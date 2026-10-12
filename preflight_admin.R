# =============================================================================
# Survey Analytics Platform 4.1 — Admin Control Center preflight
# =============================================================================

.sap_preflight_admin <- function(project_root = getwd()) {
  if (file.exists(file.path(project_root, "sap_environment.R"))) {
    source(file.path(project_root, "sap_environment.R"), local = FALSE)
    sap_load_project_env(project_root, quiet = TRUE)
  }

  demo_mode <- !tolower(Sys.getenv("SAP_DEMO_MODE", "false")) %in% c("false", "0", "no")
  if (demo_mode) {
    cat("✓ Admin Control Center correctly disabled in demo mode.\n")
    return(invisible(TRUE))
  }

  required <- c("DBI", "RPostgres", "pool", "blob", "jsonlite", "digest")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "), call. = FALSE)

  source(file.path(project_root, "db_backend.R"), local = FALSE)
  con <- db_connect_direct()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  db_init(con)

  table_exists <- DBI::dbGetQuery(con, "SELECT to_regclass(current_schema() || '.admin_audit')::text AS table_name;")$table_name[[1]]
  if (is.na(table_exists) || !nzchar(table_exists)) stop("admin_audit table is missing; migration 003 was not applied.", call. = FALSE)

  admins <- DBI::dbGetQuery(con, "SELECT id, username FROM users WHERE role = 'admin' AND is_active = 1 ORDER BY id;")
  if (!nrow(admins)) {
    stop(
      "No active administrator account exists. Register a trusted account in connected mode, then run ",
      "db/pgadmin/02_promote_first_admin.sql in pgAdmin before production launch.",
      call. = FALSE
    )
  }

  users <- db_admin_list_users(con)
  forbidden <- intersect(c("password_hash", "token_hash", "data"), names(users))
  if (length(forbidden)) stop("Admin user query exposes forbidden sensitive fields: ", paste(forbidden, collapse = ", "), call. = FALSE)

  overview <- db_admin_overview(con)
  needed <- c("total_users", "active_users", "active_admins", "active_sessions", "total_projects", "dataset_versions", "dataset_bytes")
  missing_cols <- setdiff(needed, names(overview))
  if (length(missing_cols)) stop("Admin overview is missing fields: ", paste(missing_cols, collapse = ", "), call. = FALSE)

  cat("✓ Admin audit schema is present.\n")
  cat("✓ At least one active administrator exists: ", paste(admins$username, collapse = ", "), "\n", sep = "")
  cat("✓ Control Center user query excludes password hashes, token hashes, and dataset blobs.\n")
  cat("✓ Administrative overview query passed.\n")
  invisible(TRUE)
}

.sap_preflight_admin()
rm(.sap_preflight_admin)
