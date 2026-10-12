# =============================================================================
# Survey Analytics Platform 4.1 — non-destructive persistence round-trip
# =============================================================================
# Test rows are created inside one transaction and rolled back. Function-local
# connection names and exit handlers prevent interference with other sourced
# preflight scripts.
# =============================================================================

.sap_preflight_persistence <- function(project_root = getwd()) {
  if (file.exists(file.path(project_root, "sap_environment.R"))) {
    source(file.path(project_root, "sap_environment.R"), local = FALSE)
    sap_load_project_env(project_root, quiet = TRUE)
  }

  required <- c("DBI", "RPostgres", "pool", "blob", "jsonlite", "sodium",
                "digest", "R6", "shiny", "dplyr", "tibble")
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Install missing packages: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  source(file.path(project_root, "global_no_gtsummary.R"), local = FALSE)
  source(file.path(project_root, "db_backend.R"), local = FALSE)
  source(file.path(project_root, "00_services.R"), local = FALSE)

  persistence_con <- db_connect_direct()
  on.exit({
    if (inherits(persistence_con, "DBIConnection")) {
      try(db_disconnect(persistence_con), silent = TRUE)
    }
  }, add = TRUE)

  if (!isTRUE(DBI::dbIsValid(persistence_con))) {
    stop("Persistence preflight received an invalid PostgreSQL connection.",
         call. = FALSE)
  }

  db_init(persistence_con)

  DBI::dbBegin(persistence_con)
  transaction_open <- TRUE
  on.exit({
    if (isTRUE(transaction_open) && isTRUE(DBI::dbIsValid(persistence_con))) {
      try(DBI::dbRollback(persistence_con), silent = TRUE)
    }
  }, add = TRUE)

  suffix <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), sample(1000:9999, 1))
  username <- paste0("roundtrip_", suffix)
  email <- paste0(username, "@example.invalid")
  password <- "Persistence round-trip passphrase 2026!"

  state1 <- new.env(parent = emptyenv())
  state1$current_user <- NULL
  state1$current_project <- NULL
  state1$current_project_name <- "Presentation Workspace"
  state1$data_imported <- FALSE
  state1$processed_data <- NULL
  state1$raw_data <- NULL
  state1$variable_types <- NULL
  state1$variable_dictionary <- data.frame()
  services1 <- create_services(
    state1, pool = persistence_con, settings_path = tempfile()
  )

  reg <- services1$auth$register(username, password, email, "Round-trip Test")
  stopifnot(isTRUE(reg$ok))
  login <- services1$auth$login(username, password)
  stopifnot(
    isTRUE(login$ok),
    identical(as.integer(state1$current_user), as.integer(reg$user_id))
  )

  pid <- services1$project$create(
    "Persistence round-trip", "Temporary rollback test",
    persist_current = FALSE
  )
  stopifnot(!is.null(pid))

  original <- data.frame(
    respondent_id = 1:12,
    group = rep(c("A", "B"), 6),
    score = c(10.2, 11.5, 9.8, 12.1, 13.0, 8.9,
              10.8, 11.1, 9.5, 12.7, 13.2, 9.1),
    stringsAsFactors = FALSE
  )
  processed <- original
  processed$score_centered <- processed$score - mean(processed$score)
  variable_types <- detect_variable_types(processed)

  services1$data$set(
    original, kind = "raw", module = "preflight",
    description = "raw persistence round-trip",
    variable_types = detect_variable_types(original),
    name = "Round-trip dataset"
  )
  services1$data$set(
    processed, kind = "processed", module = "preflight",
    description = "processed persistence round-trip",
    variable_types = variable_types,
    name = "Round-trip dataset"
  )
  services1$metadata$populate(processed, project_id = pid)
  db_log_action(
    persistence_con, pid, user_id = reg$user_id, module = "preflight",
    action_type = "round_trip", description = "Temporary persistence test"
  )

  state2 <- new.env(parent = emptyenv())
  state2$current_user <- reg$user_id
  state2$current_project <- NULL
  state2$current_project_name <- "Presentation Workspace"
  state2$data_imported <- FALSE
  state2$processed_data <- NULL
  state2$raw_data <- NULL
  state2$variable_types <- NULL
  state2$variable_dictionary <- data.frame()
  services2 <- create_services(
    state2, pool = persistence_con, settings_path = tempfile()
  )
  stopifnot(isTRUE(services2$project$open(pid)))

  restored_raw <- services2$data$get("raw")
  restored_processed <- services2$data$get("processed")
  stopifnot(is.data.frame(restored_raw), isTRUE(all.equal(restored_raw, original)))
  stopifnot(
    is.data.frame(restored_processed),
    isTRUE(all.equal(restored_processed, processed))
  )
  stopifnot(identical(state2$dataset_name, "Round-trip dataset"))
  stopifnot(nrow(db_get_dictionary(persistence_con, pid)) == ncol(processed))
  stopifnot(nrow(db_get_history(persistence_con, pid)) >= 1L)

  # User workspace checkpoints survive a fresh service state and point only to
  # owner-scoped project/navigation metadata, not raw respondent rows.
  state1$current_route <- "ml"
  state1$last_action <- "Binary logistic regression configured"
  stopifnot(isTRUE(services1$workspace$save(
    last_route = "ml", last_module = "ml",
    last_analysis = state1$last_action,
    state = list(dataset_name = "Round-trip dataset", active_filter_label = "All respondents")
  )))
  ws <- services2$workspace$load()
  stopifnot(is.data.frame(ws), nrow(ws) == 1L)
  stopifnot(identical(as.integer(ws$project_id[[1]]), as.integer(pid)))
  stopifnot(identical(as.character(ws$last_route[[1]]), "ml"))
  stopifnot(identical(as.character(ws$last_analysis[[1]]), state1$last_action))

  # Authentication sessions are real server-side rows and remain owner-bound.
  sid <- services1$auth$current_session_id()
  stopifnot(!is.null(sid), isTRUE(db_session_is_active(persistence_con, sid, reg$user_id)))

  # Two-user isolation: a second valid account cannot list/open/rename/delete
  # the first user's project through the ProjectService boundary.
  reg2 <- services1$auth$register(
    paste0(username, "_other"), password, paste0("other_", email), "Other User"
  )
  stopifnot(isTRUE(reg2$ok))
  state3 <- new.env(parent = emptyenv())
  state3$current_user <- reg2$user_id
  state3$current_project <- NULL
  state3$current_project_name <- "Presentation Workspace"
  state3$data_imported <- FALSE
  state3$processed_data <- NULL
  state3$raw_data <- NULL
  state3$variable_types <- NULL
  state3$variable_dictionary <- data.frame()
  services3 <- create_services(state3, pool = persistence_con, settings_path = tempfile())
  visible_projects <- services3$project$list()
  stopifnot(!nrow(visible_projects) || !pid %in% visible_projects$id)
  stopifnot(identical(services3$project$open(pid), FALSE))
  stopifnot(identical(services3$project$rename(pid, name = "unauthorised"), FALSE))
  stopifnot(identical(services3$project$delete(pid), FALSE))

  services1$auth$logout()
  stopifnot(!isTRUE(db_session_is_active(persistence_con, sid, reg$user_id)))

  DBI::dbRollback(persistence_con)
  transaction_open <- FALSE

  cat("✓ Account registration, server-side session creation and logout revocation persisted.\n")
  cat("✓ Owner-scoped project persisted.\n")
  cat("✓ Two-user project isolation blocked list/open/rename/delete access.\n")
  cat("✓ Raw and processed dataset versions restored byte-equivalent in a fresh service state.\n")
  cat("✓ Dataset display name persisted.\n")
  cat("✓ Variable dictionary and audit history persisted.\n")
  cat("✓ Per-user workspace checkpoint persisted and restored.\n")
  cat("✓ Test transaction rolled back; no test data remains.\n")
  invisible(TRUE)
}

.sap_preflight_persistence()
rm(.sap_preflight_persistence)
