# =============================================================================
# Survey Analytics Platform — project-local environment loader
# =============================================================================
# Loads .Renviron from the application directory even when R was started from
# another folder. Existing non-empty process/deployment variables take
# precedence over values in the local file. Values are never printed.

sap_load_project_env <- function(project_root = getwd(), quiet = FALSE) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  env_file <- file.path(project_root, ".Renviron")

  if (!file.exists(env_file)) {
    if (!quiet) message("[SAP ENV] No project-local .Renviron found at ", env_file)
    return(invisible(FALSE))
  }

  lines <- readLines(env_file, warn = FALSE, encoding = "UTF-8")
  trimmed <- trimws(lines)
  assignments <- trimmed[
    nzchar(trimmed) &
      !startsWith(trimmed, "#") &
      grepl("^[A-Za-z_][A-Za-z0-9_]*[[:space:]]*=", trimmed)
  ]
  keys <- unique(trimws(sub("[[:space:]]*=.*$", "", assignments)))

  # Environment variables supplied by the operating system, Posit Connect,
  # shinyapps.io, or the current R process must win over a local file.
  existing <- if (length(keys)) Sys.getenv(keys, unset = NA_character_) else character(0)
  base::readRenviron(env_file)

  restore <- length(keys) && any(!is.na(existing) & nzchar(existing))
  if (isTRUE(restore)) {
    keep <- !is.na(existing) & nzchar(existing)
    do.call(Sys.setenv, as.list(stats::setNames(existing[keep], keys[keep])))
  }

  if (!quiet) {
    message("[SAP ENV] Loaded project-local .Renviron from ", env_file,
            " (existing non-empty process variables retained).")
  }
  invisible(TRUE)
}


# Translate the standard DATABASE_URL used by many managed hosting platforms
# (Render, Railway, Heroku-style services, etc.) into this application's
# SAP_PG_* variables. Explicit SAP_PG_* values always take precedence.
sap_apply_database_url <- function(quiet = FALSE) {
  candidates <- c(
    Sys.getenv("DATABASE_URL", ""),
    Sys.getenv("POSTGRES_URL", ""),
    Sys.getenv("POSTGRESQL_URL", "")
  )
  url <- candidates[nzchar(trimws(candidates))]
  if (!length(url)) return(invisible(FALSE))
  url <- trimws(url[[1]])

  # postgres://user:password@host:5432/database?sslmode=require
  match <- regexec(
    "^postgres(?:ql)?://([^:]+):([^@]+)@([^/:?#]+)(?::([0-9]+))?/([^?]+)(?:\\?(.*))?$",
    url, perl = TRUE, ignore.case = TRUE
  )
  parts <- regmatches(url, match)[[1]]
  if (length(parts) < 6L) {
    if (!quiet) message("[SAP ENV] DATABASE_URL was present but could not be parsed; use SAP_PG_* variables instead.")
    return(invisible(FALSE))
  }

  # Avoid relying on the application's later helpers here.
  decoded <- function(x) utils::URLdecode(if (is.null(x) || !length(x) || is.na(x)) "" else as.character(x))
  values <- list(
    SAP_PG_USER = decoded(parts[[2]]),
    SAP_PG_PASSWORD = decoded(parts[[3]]),
    SAP_PG_HOST = decoded(parts[[4]]),
    SAP_PG_PORT = if (length(parts) >= 5L && nzchar(parts[[5]])) parts[[5]] else "5432",
    SAP_PG_DB = decoded(parts[[6]])
  )

  query <- if (length(parts) >= 7L) parts[[7]] else ""
  if (nzchar(query)) {
    fields <- strsplit(query, "&", fixed = TRUE)[[1]]
    ssl <- fields[startsWith(tolower(fields), "sslmode=")]
    if (length(ssl)) values$SAP_PG_SSLMODE <- decoded(sub("^[^=]+=", "", ssl[[1]]))
  }
  if (is.null(values$SAP_PG_SSLMODE)) values$SAP_PG_SSLMODE <- "require"
  values$SAP_PG_SCHEMA <- Sys.getenv("SAP_PG_SCHEMA", "sap")

  existing <- Sys.getenv(names(values), unset = "")
  missing <- !nzchar(trimws(existing))
  if (any(missing)) do.call(Sys.setenv, values[missing])
  Sys.setenv(SAP_DATABASE_URL_APPLIED = "true")
  if (!quiet) message("[SAP ENV] PostgreSQL settings detected from a managed DATABASE_URL.")
  invisible(TRUE)
}
