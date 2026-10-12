# =============================================================================
# AUTHENTICATION PREFLIGHT v1.8 — persistent accounts + session lifecycle
# =============================================================================
# Connected mode verifies PostgreSQL authentication. Explicit Demo Mode is
# Guest-only and intentionally has no account-registration adapter.
# =============================================================================

.sap_preflight_auth <- function(project_root = getwd()) {
  if (file.exists(file.path(project_root, "sap_environment.R"))) {
    source(file.path(project_root, "sap_environment.R"), local = FALSE)
    sap_load_project_env(project_root, quiet = TRUE)
  }

  demo_mode <- !tolower(Sys.getenv("SAP_DEMO_MODE", "false")) %in%
    c("false", "0", "no")

  if (demo_mode) {
    cat("✓ Explicit Demo Mode detected: account creation is intentionally disabled.\n")
    cat("  Set SAP_DEMO_MODE=false for persistent PostgreSQL accounts.\n")
    return(invisible(TRUE))
  }

  required <- c("DBI", "pool", "blob", "RPostgres", "sodium", "digest",
                "R6", "shiny", "bslib")
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Install missing packages: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  required_env <- c("SAP_PG_HOST", "SAP_PG_DB", "SAP_PG_USER",
                    "SAP_PG_PASSWORD", "SAP_PG_SCHEMA")
  empty_env <- required_env[!nzchar(Sys.getenv(required_env, ""))]
  if (length(empty_env)) {
    stop("Configure these environment variables outside the deployment bundle: ",
         paste(empty_env, collapse = ", "), call. = FALSE)
  }

  source(file.path(project_root, "db_backend.R"), local = FALSE)
  auth_con <- db_connect_direct()
  on.exit({
    if (inherits(auth_con, "DBIConnection")) {
      try(db_disconnect(auth_con), silent = TRUE)
    }
  }, add = TRUE)

  if (!isTRUE(DBI::dbIsValid(auth_con))) {
    stop("PostgreSQL authentication preflight received an invalid connection.",
         call. = FALSE)
  }

  db_init(auth_con)
  columns <- DBI::dbGetQuery(
    auth_con,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = current_schema() AND table_name = 'users';"
  )$column_name
  needed <- c(
    "id", "username", "email", "password_hash", "role", "is_active",
    "failed_login_count", "locked_until", "created_at", "last_login"
  )
  missing_columns <- setdiff(needed, columns)
  if (length(missing_columns)) {
    stop("The users table is missing columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  session_columns <- DBI::dbGetQuery(
    auth_con,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = current_schema() AND table_name = 'user_sessions';"
  )$column_name
  session_needed <- c("id", "user_id", "token_hash", "expires_at", "last_seen_at", "revoked_at", "remember")
  missing_session_columns <- setdiff(session_needed, session_columns)
  if (length(missing_session_columns)) {
    stop("The user_sessions table is missing columns: ", paste(missing_session_columns, collapse = ", "), call. = FALSE)
  }
  workspace_columns <- DBI::dbGetQuery(
    auth_con,
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = current_schema() AND table_name = 'user_workspace_state';"
  )$column_name
  workspace_needed <- c("user_id", "project_id", "last_route", "last_module", "last_analysis", "workspace_state", "updated_at")
  missing_workspace_columns <- setdiff(workspace_needed, workspace_columns)
  if (length(missing_workspace_columns)) {
    stop("The user_workspace_state table is missing columns: ", paste(missing_workspace_columns, collapse = ", "), call. = FALSE)
  }
  cat("✓ PostgreSQL authentication, session and workspace-resume schema is ready.\n")
  invisible(TRUE)
}

.sap_preflight_auth()
rm(.sap_preflight_auth)
