# =============================================================================
# SECURE STANDALONE AUTHENTICATION
# =============================================================================
# PostgreSQL remains the durable, multi-instance production backend. This file
# supplies account creation and sign-in when no PostgreSQL server is available.
# Passwords are always stored as sodium/Argon2 password hashes, never plaintext.
# On shinyapps.io the local filesystem belongs to an individual app instance and
# may be reset on restart or redeployment; use PostgreSQL for durable accounts.
# =============================================================================

sap_auth_storage_directory <- function() {
  requested <- trimws(Sys.getenv("SAP_AUTH_DATA_DIR", ""))
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  target <- if (nzchar(requested)) {
    requested
  } else if (file.access(root, mode = 2L) == 0L) {
    file.path(root, ".sap-private", "accounts")
  } else {
    file.path(tempdir(), "survey-analytics-private", "accounts")
  }

  target <- normalizePath(target, winslash = "/", mustWork = FALSE)
  web_root <- normalizePath(file.path(root, "www"), winslash = "/", mustWork = FALSE)
  if (identical(target, web_root) || startsWith(target, paste0(web_root, "/"))) {
    stop("SAP_AUTH_DATA_DIR must not be inside the publicly served www directory.", call. = FALSE)
  }

  if (!dir.exists(target) && !dir.create(target, recursive = TRUE, mode = "0700")) {
    fallback <- file.path(tempdir(), "survey-analytics-private", "accounts")
    if (!dir.exists(fallback) && !dir.create(fallback, recursive = TRUE, mode = "0700")) {
      stop("No writable secure account-storage directory is available.", call. = FALSE)
    }
    target <- fallback
  }
  try(Sys.chmod(target, mode = "0700"), silent = TRUE)
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

sap_account_storage_label <- function() {
  if (exists("SAP_POSTGRES_ENABLED", inherits = TRUE) && isTRUE(SAP_POSTGRES_ENABLED)) {
    "PostgreSQL-backed account storage"
  } else {
    "secure application-instance account storage"
  }
}

FileAuthService <- R6::R6Class(
  "FileAuthService",
  public = list(
    initialize = function(app_state, logger = NULL, notifier = NULL, storage_dir = NULL) {
      if (!isTRUE(requireNamespace("sodium", quietly = TRUE))) {
        stop("The sodium package is required for secure account authentication.", call. = FALSE)
      }
      private$app_state <- app_state
      private$logger <- logger
      private$notifier <- notifier
      private$tick <- shiny::reactiveVal(0L)
      private$session_id <- NULL
      private$session_expires <- NULL
      directory <- if (is.null(storage_dir)) sap_auth_storage_directory() else storage_dir
      if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, mode = "0700")) {
        stop("The standalone account-storage directory could not be created.", call. = FALSE)
      }
      try(Sys.chmod(directory, mode = "0700"), silent = TRUE)
      private$path <- file.path(directory, "users.rds")
      private$dummy_hash <- sodium::password_store(
        paste0("sap-dummy-", sodium::bin2hex(sodium::random(32L)))
      )
      if (!file.exists(private$path)) {
        private$with_lock(function() {
          if (!file.exists(private$path)) private$write_users(private$empty_users())
          invisible(TRUE)
        })
      }
      private$log("Secure standalone account authentication is available.")
    },

    version = function() private$tick(),
    is_available = function() isTRUE(requireNamespace("sodium", quietly = TRUE)),
    current_user_id = function() private$app_state$current_user,
    current_session_id = function() private$session_id,
    storage_path = function() private$path,

    current_user = function() {
      uid <- self$current_user_id()
      if (is.null(uid) || !length(uid) || is.na(uid[[1]])) return(NULL)
      users <- private$read_users()
      match <- which(users$id == as.integer(uid[[1]]))
      if (!length(match)) return(NULL)
      users[match[[1]], , drop = FALSE]
    },

    is_authenticated = function() {
      if (is.null(private$session_id) || is.null(private$session_expires)) return(FALSE)
      if (!is.finite(as.numeric(private$session_expires)) || Sys.time() >= private$session_expires) {
        self$logout()
        return(FALSE)
      }
      user <- self$current_user()
      !is.null(user) && identical(suppressWarnings(as.integer(user$is_active[[1]])), 1L)
    },

    is_admin = function() {
      user <- self$current_user()
      !is.null(user) && identical(as.character(user$role[[1]]), "admin")
    },

    set_request_context = function(user_agent = NULL, ip_hash = NULL) {
      private$user_agent <- substr(as.character(user_agent %||% ""), 1L, 512L)
      private$ip_hash <- substr(as.character(ip_hash %||% ""), 1L, 128L)
      invisible(TRUE)
    },

    hash_password = function(password) {
      sodium::password_store(enc2utf8(as.character(password)[[1]]))
    },

    verify_password = function(password, hash) {
      if (is.null(hash) || !length(hash) || is.na(hash[[1]]) || !nzchar(hash[[1]])) return(FALSE)
      tryCatch(
        sodium::password_verify(as.character(hash[[1]]), enc2utf8(as.character(password)[[1]])),
        error = function(e) FALSE
      )
    },

    register = function(username, password, email = NULL, full_name = NULL,
                        country = NULL, institution = NULL, organization = NULL,
                        department = NULL, position = NULL) {
      username <- tolower(trimws(as.character(username %||% "")))
      email <- tolower(trimws(as.character(email %||% "")))
      full_name <- trimws(as.character(full_name %||% ""))
      password <- enc2utf8(as.character(password %||% ""))

      if (!grepl("^[a-z0-9][a-z0-9._-]{2,39}$", username)) {
        return(list(ok = FALSE, error = "Use 3–40 lowercase letters, numbers, dots, underscores, or hyphens for the username."))
      }
      if (!nzchar(full_name)) return(list(ok = FALSE, error = "Full name is required."))
      if (!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", email)) {
        return(list(ok = FALSE, error = "Enter a valid email address."))
      }
      password_length <- nchar(password, type = "chars", allowNA = FALSE)
      if (password_length < 15L || password_length > 128L) {
        return(list(ok = FALSE, error = "Use a passphrase containing 15–128 characters."))
      }

      result <- tryCatch(private$with_lock(function() {
        users <- private$read_users()
        if (any(tolower(users$username) == username) || any(tolower(users$email) == email)) {
          return(list(ok = FALSE, error = "The account could not be created. Try another username or email."))
        }

        bootstrap_admin <- nrow(users) == 0L
        user_id <- if (nrow(users)) max(users$id, na.rm = TRUE) + 1L else 1L
        now <- private$timestamp()
        row <- data.frame(
          id = as.integer(user_id), username = username,
          password_hash = self$hash_password(password), email = email,
          full_name = full_name, role = if (bootstrap_admin) "admin" else "user",
          is_active = 1L, failed_attempts = 0L, locked_until = NA_character_,
          created_at = now, updated_at = now, last_login = NA_character_,
          email_verified_at = now, must_reset_password = 0L, mfa_enabled = 0L,
          account_status = "active", country = private$optional_text(country),
          institution = private$optional_text(institution),
          organization = private$optional_text(organization),
          department = private$optional_text(department),
          position = private$optional_text(position),
          stringsAsFactors = FALSE, check.names = FALSE
        )
        private$write_users(rbind(users, row))
        list(ok = TRUE, user_id = as.integer(user_id), pending_approval = FALSE,
             bootstrap_admin = bootstrap_admin)
      }), error = function(e) {
        private$log(paste("Standalone account registration failed:", conditionMessage(e)))
        list(ok = FALSE, error = "The account could not be created. Check that secure application storage is writable.")
      })

      if (isTRUE(result$ok)) {
        private$bump()
        private$log(sprintf("Registered standalone user id %s.", result$user_id))
      }
      result
    },

    login = function(login, password) {
      generic_error <- "Sign-in failed. Check your username or email and password, then try again."
      identifier <- tolower(trimws(as.character(login %||% "")))
      password <- enc2utf8(as.character(password %||% ""))

      result <- tryCatch(private$with_lock(function() {
        users <- private$read_users()
        matches <- which(tolower(users$username) == identifier | tolower(users$email) == identifier)
        if (!length(matches)) {
          self$verify_password(password, private$dummy_hash)
          return(list(ok = FALSE, error = generic_error))
        }
        index <- matches[[1]]
        locked_raw <- users$locked_until[[index]]
        locked_until <- if (is.na(locked_raw) || !nzchar(locked_raw)) NA else {
          suppressWarnings(as.POSIXct(locked_raw, tz = "UTC"))
        }
        if (!is.na(locked_until) && Sys.time() < locked_until) {
          self$verify_password(password, private$dummy_hash)
          return(list(ok = FALSE, error = generic_error))
        }
        valid <- identical(as.integer(users$is_active[[index]]), 1L) &&
          self$verify_password(password, users$password_hash[[index]])
        if (!isTRUE(valid)) {
          users$failed_attempts[[index]] <- as.integer(users$failed_attempts[[index]]) + 1L
          if (users$failed_attempts[[index]] >= private$max_attempts) {
            users$locked_until[[index]] <- private$timestamp(Sys.time() + 15 * 60)
            users$failed_attempts[[index]] <- 0L
          }
          users$updated_at[[index]] <- private$timestamp()
          private$write_users(users)
          return(list(ok = FALSE, error = generic_error))
        }
        users$failed_attempts[[index]] <- 0L
        users$locked_until[[index]] <- NA_character_
        users$last_login[[index]] <- private$timestamp()
        users$updated_at[[index]] <- users$last_login[[index]]
        private$write_users(users)
        list(ok = TRUE, user_id = as.integer(users$id[[index]]))
      }), error = function(e) {
        private$log(paste("Standalone account sign-in failed:", conditionMessage(e)))
        list(ok = FALSE, error = generic_error)
      })

      if (!isTRUE(result$ok)) return(result)
      private$start_session(result$user_id)
      private$log(sprintf("Standalone user id %s signed in.", result$user_id))
      list(ok = TRUE, user_id = result$user_id, session_id = private$session_id)
    },

    restore_user = function(user_id) {
      users <- private$read_users()
      matches <- which(users$id == as.integer(user_id[[1]]) & users$is_active == 1L)
      if (!length(matches)) return(FALSE)
      if (is.null(private$session_id)) private$start_session(as.integer(user_id[[1]]))
      private$app_state$current_user <- as.integer(user_id[[1]])
      private$bump()
      TRUE
    },

    logout = function() {
      private$session_id <- NULL
      private$session_expires <- NULL
      private$app_state$current_user <- NULL
      private$app_state$current_project <- NULL
      private$app_state$current_project_name <- "Presentation Workspace"
      private$bump()
      invisible(TRUE)
    },

    change_password = function(old_password, new_password) {
      user <- self$current_user()
      if (is.null(user)) return(list(ok = FALSE, error = "Not signed in."))
      if (!self$verify_password(old_password, user$password_hash[[1]])) {
        return(list(ok = FALSE, error = "The current password is incorrect."))
      }
      new_password <- enc2utf8(as.character(new_password %||% ""))
      length_password <- nchar(new_password, type = "chars", allowNA = FALSE)
      if (length_password < 15L || length_password > 128L) {
        return(list(ok = FALSE, error = "Use a passphrase containing 15–128 characters."))
      }
      private$with_lock(function() {
        users <- private$read_users()
        index <- match(as.integer(user$id[[1]]), users$id)
        if (is.na(index)) stop("The account no longer exists.", call. = FALSE)
        users$password_hash[[index]] <- self$hash_password(new_password)
        users$updated_at[[index]] <- private$timestamp()
        private$write_users(users)
      })
      private$bump()
      list(ok = TRUE)
    },

    update_profile = function(fields = list()) {
      user <- self$current_user()
      if (is.null(user)) return(list(ok = FALSE, error = "Not signed in."))
      allowed <- intersect(names(fields), c("full_name", "email", "country", "institution", "organization", "department", "position"))
      if ("email" %in% allowed) {
        fields$email <- tolower(trimws(as.character(fields$email %||% "")))
        if (!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", fields$email)) {
          return(list(ok = FALSE, error = "Enter a valid email address."))
        }
      }
      result <- tryCatch(private$with_lock(function() {
        users <- private$read_users()
        index <- match(as.integer(user$id[[1]]), users$id)
        if (is.na(index)) return(list(ok = FALSE, error = "The account no longer exists."))
        if ("email" %in% allowed && any(tolower(users$email[-index]) == fields$email)) {
          return(list(ok = FALSE, error = "That email address is already in use."))
        }
        for (field in allowed) users[[field]][[index]] <- private$optional_text(fields[[field]])
        users$updated_at[[index]] <- private$timestamp()
        private$write_users(users)
        list(ok = TRUE)
      }), error = function(e) list(ok = FALSE, error = "The profile could not be updated."))
      if (isTRUE(result$ok)) private$bump()
      result
    },

    request_password_reset = function(login) {
      list(ok = TRUE, message = "Email password recovery requires a PostgreSQL deployment with transactional email. A signed-in account can change its passphrase in Profile.")
    },
    reset_password_with_token = function(token, new_password) {
      list(ok = FALSE, error = "Token-based password recovery requires PostgreSQL and configured transactional email.")
    },
    request_email_verification = function(user_id = self$current_user_id()) {
      list(ok = FALSE, error = "Email verification requires PostgreSQL and configured transactional email.")
    },
    verify_email_with_token = function(token) {
      list(ok = FALSE, error = "Email verification requires PostgreSQL and configured transactional email.")
    },
    complete_mfa = function(user_id, code) {
      list(ok = FALSE, error = "Multi-factor authentication requires the PostgreSQL production backend.")
    },
    login_local = function() FALSE
  ),

  private = list(
    app_state = NULL, logger = NULL, notifier = NULL, tick = NULL,
    path = NULL, session_id = NULL, session_expires = NULL,
    dummy_hash = NULL, user_agent = NULL, ip_hash = NULL, max_attempts = 5L,

    empty_users = function() {
      data.frame(
        id = integer(), username = character(), password_hash = character(),
        email = character(), full_name = character(), role = character(),
        is_active = integer(), failed_attempts = integer(), locked_until = character(),
        created_at = character(), updated_at = character(), last_login = character(),
        email_verified_at = character(), must_reset_password = integer(),
        mfa_enabled = integer(), account_status = character(), country = character(),
        institution = character(), organization = character(), department = character(),
        position = character(), stringsAsFactors = FALSE, check.names = FALSE
      )
    },

    read_users = function() {
      if (!file.exists(private$path)) return(private$empty_users())
      users <- readRDS(private$path)
      template <- private$empty_users()
      if (!is.data.frame(users) || !all(names(template) %in% names(users))) {
        stop("The secure standalone account database has an invalid format.", call. = FALSE)
      }
      users[, names(template), drop = FALSE]
    },

    write_users = function(users) {
      temporary <- tempfile(pattern = "users-", tmpdir = dirname(private$path))
      on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
      saveRDS(users, temporary, version = 2L)
      try(Sys.chmod(temporary, mode = "0600"), silent = TRUE)
      if (!file.rename(temporary, private$path)) {
        if (!file.copy(temporary, private$path, overwrite = TRUE)) {
          stop("The secure standalone account database could not be saved.", call. = FALSE)
        }
        unlink(temporary)
      }
      try(Sys.chmod(private$path, mode = "0600"), silent = TRUE)
      invisible(TRUE)
    },

    with_lock = function(callback) {
      lock <- paste0(private$path, ".lock")
      expires <- Sys.time() + 5
      while (!isTRUE(suppressWarnings(dir.create(lock, mode = "0700")))) {
        if (Sys.time() >= expires) stop("The account database is temporarily busy.", call. = FALSE)
        Sys.sleep(0.03)
      }
      on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
      callback()
    },

    start_session = function(user_id) {
      private$session_id <- sample.int(.Machine$integer.max, 1L)
      hours <- suppressWarnings(as.numeric(Sys.getenv("SAP_SESSION_HOURS", "12")))
      if (!is.finite(hours) || hours <= 0 || hours > 720) hours <- 12
      private$session_expires <- Sys.time() + hours * 60 * 60
      private$app_state$current_user <- as.integer(user_id)
      private$app_state$current_project <- NULL
      private$app_state$current_project_name <- "Presentation Workspace"
      private$bump()
      invisible(private$session_id)
    },

    optional_text = function(value) {
      text <- trimws(as.character(value %||% ""))
      if (!length(text) || is.na(text[[1]])) "" else text[[1]]
    },
    timestamp = function(value = Sys.time()) {
      format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    },
    bump = function() private$tick(shiny::isolate(private$tick()) + 1L),
    log = function(message) {
      if (!is.null(private$logger)) private$logger$info(message, "auth")
      invisible(TRUE)
    }
  )
)
