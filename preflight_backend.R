# =============================================================================
# Survey Analytics Platform 4.1 — PostgreSQL backend preflight
# =============================================================================
# All runtime objects live inside a function-local scope. This prevents a
# connection opened by one sourced preflight from being overwritten or closed
# by another preflight that happens to use the same symbol name.
# =============================================================================

.sap_preflight_backend <- function(project_root = getwd()) {
  if (file.exists(file.path(project_root, "sap_environment.R"))) {
    source(file.path(project_root, "sap_environment.R"), local = FALSE)
    sap_load_project_env(project_root, quiet = TRUE)
  }

  required_packages <- c("DBI", "RPostgres", "pool", "blob", "jsonlite",
                         "sodium", "digest")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    stop("Install backend packages first: install.packages(c(",
         paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
         call. = FALSE)
  }

  required_files <- c(
    "db_backend.R", "db/migrate.R",
    "db/migrations/001_core_platform.sql",
    "db/migrations/002_project_ownership.sql",
    "db/migrations/003_admin_control_center.sql"
  )
  required_paths <- file.path(project_root, required_files)
  missing_files <- required_files[!file.exists(required_paths)]
  if (length(missing_files)) {
    stop("Missing backend files: ", paste(missing_files, collapse = ", "),
         call. = FALSE)
  }

  required_env <- c("SAP_PG_HOST", "SAP_PG_PORT", "SAP_PG_DB",
                    "SAP_PG_USER", "SAP_PG_PASSWORD", "SAP_PG_SCHEMA")
  missing_env <- required_env[!nzchar(Sys.getenv(required_env))]
  if (length(missing_env)) {
    stop("Configure these variables in .Renviron and restart R: ",
         paste(missing_env, collapse = ", "), call. = FALSE)
  }
  if (identical(Sys.getenv("SAP_PG_USER"), "postgres")) {
    stop("Do not run the application as the postgres superuser. Use sap_app.",
         call. = FALSE)
  }
  if (Sys.getenv("SAP_PG_PASSWORD") %in% c(
      "REPLACE_WITH_THE_PASSWORD_USED_IN_PGADMIN",
      "CHANGE_THIS_PASSWORD_BEFORE_RUNNING")) {
    stop("Replace the PostgreSQL password placeholder in .Renviron.",
         call. = FALSE)
  }

  source(file.path(project_root, "db_backend.R"), local = FALSE)

  # Verify that migration SQL is split into one command per DBI call while a
  # dollar-quoted PL/pgSQL body remains intact.
  splitter_probe <- paste(
    "CREATE TABLE splitter_probe(id integer);",
    "CREATE OR REPLACE FUNCTION splitter_probe_fn()",
    "RETURNS trigger LANGUAGE plpgsql AS $fn$",
    "BEGIN",
    "  PERFORM 1;",
    "  RETURN NEW;",
    "END",
    "$fn$;",
    "DROP TABLE splitter_probe;",
    sep = "\n"
  )
  splitter_statements <- sap_sql_split_statements(splitter_probe)
  if (length(splitter_statements) != 3L ||
      !grepl("PERFORM 1;", splitter_statements[[2L]], fixed = TRUE)) {
    stop("PostgreSQL migration statement splitter failed its self-test.",
         call. = FALSE)
  }
  message("[SAP DB] Migration statement splitter self-test passed.")

  backend_con <- db_connect_direct()
  on.exit({
    if (inherits(backend_con, "DBIConnection")) {
      try(db_disconnect(backend_con), silent = TRUE)
    }
  }, add = TRUE)

  if (!isTRUE(DBI::dbIsValid(backend_con))) {
    stop("RPostgres returned an invalid connection immediately after dbConnect(). ",
         "Restart R and reinstall/update RPostgres if this persists.", call. = FALSE)
  }

  health <- tryCatch(
    db_health_check(backend_con),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("bad_weak_ptr", msg, fixed = TRUE)) {
        stop(
          "The PostgreSQL connection was closed before the backend health check. ",
          "This usually indicates a connection-lifecycle collision between sourced scripts. ",
          "Survey Analytics Platform isolates every preflight connection; restart R and rerun the release gate.",
          call. = FALSE
        )
      }
      stop(e)
    }
  )

  message(sprintf("[SAP DB] Connected to database '%s' as '%s' (PostgreSQL %s).",
                  health$database_name[[1]], health$database_user[[1]],
                  health$server_version[[1]]))
  message("[SAP DB] Active schema: ", health$active_schema[[1]])

  role <- DBI::dbGetQuery(backend_con, "
    SELECT rolname, rolsuper, rolcreatedb, rolcreaterole
    FROM pg_roles WHERE rolname = current_user;")
  if (!nrow(role)) stop("Could not inspect the application role.", call. = FALSE)
  if (isTRUE(role$rolsuper[[1]]) || isTRUE(role$rolcreatedb[[1]]) ||
      isTRUE(role$rolcreaterole[[1]])) {
    stop("The application role has excessive privileges. Re-run the pgAdmin bootstrap script.",
         call. = FALSE)
  }

  schema_name <- sap_db_schema()
  schema_access <- DBI::dbGetQuery(backend_con, "
    SELECT n.nspname AS schema_name, r.rolname AS schema_owner,
           has_schema_privilege(current_user, n.oid, 'USAGE') AS can_use,
           has_schema_privilege(current_user, n.oid, 'CREATE') AS can_create
    FROM pg_namespace n JOIN pg_roles r ON r.oid = n.nspowner
    WHERE n.nspname = $1;", params = list(schema_name))
  if (!nrow(schema_access)) {
    stop("Configured schema does not exist: ", schema_name,
         ". Run db/pgadmin/00_create_app_role.sql in pgAdmin.", call. = FALSE)
  }
  if (!isTRUE(schema_access$can_use[[1]]) ||
      !isTRUE(schema_access$can_create[[1]])) {
    stop("The application role lacks USAGE/CREATE on schema '", schema_name, "'.",
         call. = FALSE)
  }
  if (!identical(as.character(schema_access$schema_owner[[1]]),
                 as.character(role$rolname[[1]]))) {
    stop("Schema '", schema_name, "' is not owned by the application role. ",
         "Re-run the pgAdmin bootstrap script.", call. = FALSE)
  }
  if (!identical(as.character(health$active_schema[[1]]), schema_name)) {
    stop("The active PostgreSQL schema is '", health$active_schema[[1]],
         "' instead of '", schema_name, "'. Check SAP_PG_SCHEMA/search_path.",
         call. = FALSE)
  }

  migrations <- sap_db_migrate(
    backend_con,
    migrations_dir = file.path(project_root, "db", "migrations")
  )
  expected_tables <- c(
    "schema_migrations", "users", "user_sessions", "projects", "project_members",
    "variable_dictionary", "datasets", "analysis_history", "models", "reports",
    "ai_audit", "health_measurements", "app_settings", "admin_audit"
  )
  tables <- DBI::dbGetQuery(backend_con, "
    SELECT table_name FROM information_schema.tables
    WHERE table_schema = current_schema() AND table_type = 'BASE TABLE';")$table_name
  missing_tables <- setdiff(expected_tables, tables)
  if (length(missing_tables)) {
    stop("Database schema is incomplete. Missing: ",
         paste(missing_tables, collapse = ", "), call. = FALSE)
  }

  # Non-destructive write/rollback verification.
  DBI::dbBegin(backend_con)
  transaction_open <- TRUE
  on.exit({
    if (isTRUE(transaction_open) && isTRUE(DBI::dbIsValid(backend_con))) {
      try(DBI::dbRollback(backend_con), silent = TRUE)
    }
  }, add = TRUE)

  write_ok <- FALSE
  tryCatch({
    DBI::dbExecute(
      backend_con,
      "CREATE TEMP TABLE sap_preflight_write_test(id integer, note text);"
    )
    DBI::dbExecute(
      backend_con,
      "INSERT INTO sap_preflight_write_test VALUES (1, 'ok');"
    )
    check <- DBI::dbGetQuery(
      backend_con,
      "SELECT note FROM sap_preflight_write_test WHERE id = 1;"
    )
    write_ok <- nrow(check) == 1L && identical(check$note[[1]], "ok")
  }, error = function(e) {
    if (isTRUE(transaction_open)) {
      try(DBI::dbRollback(backend_con), silent = TRUE)
      transaction_open <<- FALSE
    }
    stop("Database write test failed: ", conditionMessage(e), call. = FALSE)
  })

  DBI::dbRollback(backend_con)
  transaction_open <- FALSE
  if (!write_ok) stop("Database write verification failed.", call. = FALSE)

  message("[SAP DB] Applied migrations: ", paste(migrations$version, collapse = ", "))
  message("[SAP DB] All expected tables are present.")
  message("[SAP DB] Transactional write test passed and was rolled back.")
  message("✓ PostgreSQL backend preflight passed.")
  invisible(TRUE)
}

.sap_preflight_backend()
rm(.sap_preflight_backend)
