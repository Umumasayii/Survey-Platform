# =============================================================================
# BACKEND: DATA ACCESS LAYER (DAL)
# =============================================================================
# Durable persistence for Survey Analytics Platform.
#
# Design principles:
#   - The front-end NEVER touches storage directly. It calls db_* functions.
#   - PostgreSQL backend (multi-user / server). db_connect() reads connection
#     details from SAP_PG_* environment variables.
#   - Connection POOLING via {pool}. In Shiny you create ONE pool at app
#     startup and share it; you do NOT open a connection per query or per
#     session. Close it in onStop().
#   - Parameterised queries everywhere (params = list(...)). No paste()/glue
#     into SQL -> no injection, no quoting bugs.
#   - R objects (data frames, fitted models) are stored as serialised BLOBs.
#     NOTE: serialize() output is R-only. If Python must read the *data*,
#     store datasets as Parquet instead (see db_save_dataset_arrow() stub).
#
# Dependencies: DBI, pool, blob, jsonlite, RPostgres
#   install.packages(c("DBI", "pool", "blob", "jsonlite", "RPostgres"))
# =============================================================================

library(DBI)
library(pool)
library(blob)
library(jsonlite)

if (file.exists(file.path("db", "migrate.R"))) {
  source(file.path("db", "migrate.R"), local = FALSE)
}

# =============================================================================
# CONNECTION MANAGEMENT
# =============================================================================

# Return TRUE for common truthy environment values.
.sap_env_true <- function(name, default = FALSE) {
  raw <- trimws(tolower(Sys.getenv(name, if (isTRUE(default)) "true" else "false")))
  raw %in% c("true", "1", "yes", "y", "on")
}

# Create the target PostgreSQL database when explicitly enabled. This is used
# for zero-SQL local setup. Remote creation remains opt-in because managed
# providers normally provision the database themselves.
sap_ensure_database_available <- function(host, port, dbname, user, password, sslmode) {
  if (!.sap_env_true("SAP_PG_AUTO_CREATE_DB", FALSE)) return(invisible(FALSE))
  if (!nzchar(password) || !nzchar(dbname)) return(invisible(FALSE))

  local_host <- tolower(trimws(host)) %in% c("localhost", "127.0.0.1", "::1", "postgres")
  if (!local_host && !.sap_env_true("SAP_PG_AUTO_CREATE_DB_REMOTE", FALSE)) {
    return(invisible(FALSE))
  }

  can_open_target <- tryCatch({
    con <- DBI::dbConnect(
      RPostgres::Postgres(), host = host, port = port, dbname = dbname,
      user = user, password = password, sslmode = sslmode,
      connect_timeout = min(5L, as.integer(Sys.getenv("SAP_PG_CONNECT_TIMEOUT", "10")))
    )
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(can_open_target)) return(invisible(TRUE))

  admin_db <- trimws(Sys.getenv("SAP_PG_ADMIN_DB", "postgres"))
  if (!nzchar(admin_db)) admin_db <- "postgres"
  admin_con <- DBI::dbConnect(
    RPostgres::Postgres(), host = host, port = port, dbname = admin_db,
    user = user, password = password, sslmode = sslmode,
    connect_timeout = min(5L, as.integer(Sys.getenv("SAP_PG_CONNECT_TIMEOUT", "10")))
  )
  on.exit(try(DBI::dbDisconnect(admin_con), silent = TRUE), add = TRUE)

  exists <- DBI::dbGetQuery(
    admin_con, "SELECT 1 AS ok FROM pg_database WHERE datname = $1;",
    params = list(dbname)
  )
  if (!nrow(exists)) {
    db_ident <- as.character(DBI::dbQuoteIdentifier(admin_con, dbname))
    DBI::dbExecute(admin_con, paste("CREATE DATABASE", db_ident))
    message("[SAP DB] Created PostgreSQL database: ", dbname)
  }
  invisible(TRUE)
}

#' Open a pooled connection to the backend.
#'
#' @param host,port,dbname,user,password PostgreSQL connection details
#'   (default to SAP_PG_* environment variables).
#' @param ... Additional args passed to RPostgres::Postgres().
#' @return A pool object. Share this across the app; close with pool::poolClose().
#' Connect to the PostgreSQL database and return a pool.
#'
#' Connection details come from environment variables (safe for a shared /
#' server deployment — nothing is hardcoded and no secret sits in the repo):
#'   SAP_PG_HOST      (default "localhost")
#'   SAP_PG_PORT      (default 5432)
#'   SAP_PG_DB        (default "survey_platform")
#'   SAP_PG_USER      (default "sap_app")
#'   SAP_PG_PASSWORD  (required — no sensible default)
#'
#' Put these in a .Renviron file and restart R. Example:
#'   SAP_PG_HOST=localhost
#'   SAP_PG_PORT=5432
#'   SAP_PG_DB=survey_platform
#'   SAP_PG_USER=sap_app
#'   SAP_PG_PASSWORD=your_password_here
#' Direct single DBI connection — for scripts and testing where a pool is
#' overkill or causes issues. Passes straight to RPostgres::Postgres().
#' Use db_disconnect() to close. NOT for production (no pooling, not thread-safe).
db_connect_direct <- function(
    host = Sys.getenv("SAP_PG_HOST", "localhost"),
    port = as.integer(Sys.getenv("SAP_PG_PORT", "5432")),
    dbname = Sys.getenv("SAP_PG_DB", "survey_platform"),
    user = Sys.getenv("SAP_PG_USER", "sap_app"),
    password = Sys.getenv("SAP_PG_PASSWORD", ""),
    schema = Sys.getenv("SAP_PG_SCHEMA", "sap"),
    sslmode = Sys.getenv("SAP_PG_SSLMODE", "prefer")) {
  if (!requireNamespace("RPostgres", quietly = TRUE))
    stop("The 'RPostgres' package is required.", call. = FALSE)
  if (!nzchar(password))
    stop("SAP_PG_PASSWORD is empty. Configure it outside the project source code.", call. = FALSE)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", schema))
    stop("SAP_PG_SCHEMA contains invalid characters.", call. = FALSE)

  sap_ensure_database_available(host, port, dbname, user, password, sslmode)

  DBI::dbConnect(
    RPostgres::Postgres(), host = host, port = port, dbname = dbname,
    user = user, password = password, sslmode = sslmode,
    application_name = "survey-analytics-platform",
    connect_timeout = as.integer(Sys.getenv("SAP_PG_CONNECT_TIMEOUT", "10")),
    timezone = "UTC",
    options = sprintf("-c search_path=%s,public", schema)
  )
}

#' Close a pool or direct connection.
#' Invalid RPostgres connections are left alone: querying or disconnecting an
#' already-destroyed external pointer can surface the C++ `bad_weak_ptr` error.
db_disconnect <- function(connection_or_pool) {
  tryCatch({
    if (inherits(connection_or_pool, "Pool")) {
      pool::poolClose(connection_or_pool)
    } else if (inherits(connection_or_pool, "DBIConnection")) {
      if (isTRUE(DBI::dbIsValid(connection_or_pool))) {
        DBI::dbDisconnect(connection_or_pool)
      }
    }
    invisible(NULL)
  }, error = function(e) invisible(NULL))
}

#' Create the shared connection pool used by Shiny.
db_connect <- function(
    host = Sys.getenv("SAP_PG_HOST", "localhost"),
    port = as.integer(Sys.getenv("SAP_PG_PORT", "5432")),
    dbname = Sys.getenv("SAP_PG_DB", "survey_platform"),
    user = Sys.getenv("SAP_PG_USER", "sap_app"),
    password = Sys.getenv("SAP_PG_PASSWORD", ""),
    schema = Sys.getenv("SAP_PG_SCHEMA", "sap"),
    sslmode = Sys.getenv("SAP_PG_SSLMODE", "prefer"),
    min_size = as.integer(Sys.getenv("SAP_PG_POOL_MIN", "1")),
    max_size = as.integer(Sys.getenv("SAP_PG_POOL_MAX", "8")),
    ...) {
  if (!requireNamespace("RPostgres", quietly = TRUE))
    stop("The 'RPostgres' package is required. Run install.packages('RPostgres').", call. = FALSE)
  if (!nzchar(password))
    stop("SAP_PG_PASSWORD is empty. Configure it outside the project source code.", call. = FALSE)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", schema))
    stop("SAP_PG_SCHEMA contains invalid characters.", call. = FALSE)

  sap_ensure_database_available(host, port, dbname, user, password, sslmode)

  if (is.na(min_size) || min_size < 0L) min_size <- 1L
  if (is.na(max_size) || max_size < max(1L, min_size)) max_size <- max(8L, min_size)

  pool::dbPool(
    drv = RPostgres::Postgres(),
    host = host, port = port, dbname = dbname,
    user = user, password = password, sslmode = sslmode,
    application_name = "survey-analytics-platform",
    connect_timeout = as.integer(Sys.getenv("SAP_PG_CONNECT_TIMEOUT", "10")),
    timezone = "UTC",
    options = sprintf("-c search_path=%s,public", schema),
    minSize = min_size, maxSize = max_size,
    validationInterval = 30, idleTimeout = 60,
    validateQuery = "SELECT 1",
    ...
  )
}

db_health_check <- function(pool_or_connection) {
  row <- DBI::dbGetQuery(pool_or_connection, paste(
    "SELECT current_database() AS database_name,",
    "current_user AS database_user,",
    "current_schema() AS active_schema,",
    "current_setting('server_version') AS server_version,",
    "CURRENT_TIMESTAMP AS checked_at"))
  if (!nrow(row)) stop("PostgreSQL health check returned no result.", call. = FALSE)
  row
}

# =============================================================================
# SCHEMA INITIALISATION (idempotent)
# =============================================================================

#' Create tables if they don't already exist. Safe to call on every startup.
#'
#' Column types (PostgreSQL):
#'   - TEXT              text
#'   - INTEGER / SERIAL  ids and counts
#'   - timestamps        stored as TIMESTAMPTZ in UTC
#'   - JSON fields       stored as JSONB
#'   - BYTEA             serialized R objects (datasets, models)
db_init <- function(pool) {
  if (!exists("sap_db_migrate", mode = "function")) {
    stop("Database migration runner is unavailable (db/migrate.R).", call. = FALSE)
  }
  sap_db_migrate(pool)

  # No default database account is created. Connected deployments require users
  # to register/sign in through the in-tool account dialog. This avoids shipping
  # a privileged recovery identity or a bypass login.
  invisible(db_health_check(pool))
}

#' Add a column to a table only if it does not already exist (Postgres native
#' ADD COLUMN IF NOT EXISTS)
#' so we try that form there.
#' Add a column only if it doesn't exist. Postgres supports this natively.
.add_column_if_missing <- function(pool, table, column, type) {
  ok <- tryCatch({
    DBI::dbExecute(pool, sprintf(
      "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;", table, column, type))
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}

# =============================================================================
# SERIALISATION HELPERS
# =============================================================================

# R object <-> raw BYTEA. serialize(NULL connection) returns a raw vector;
# blob::blob() wraps it as a length-1 value that RPostgres binds to BYTEA.
.r_to_blob <- function(obj) blob::blob(serialize(obj, connection = NULL))

.blob_to_r <- function(x) {
  # Normalize the many shapes a serialized column comes back as:
  #  - blob object (blob::blob)        -> x[[1]] is the raw vector
  #  - list of raw (RPostgres BYTEA)   -> x[[1]] is the raw vector
  #  - a bare raw vector               -> use as-is
  raw_vec <- if (inherits(x, "blob")) x[[1]]
             else if (is.list(x))     x[[1]]
             else                     x
  if (is.null(raw_vec)) return(NULL)
  unserialize(raw_vec)
}

.now <- function() as.POSIXct(Sys.time(), tz = "UTC")

.to_json <- function(x) {
  if (is.null(x)) return(NA_character_)
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

.from_json <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  jsonlite::fromJSON(x)
}

# Insert + fetch the new row id ON THE SAME CONNECTION.
# With {pool}, dbExecute() and a subsequent dbGetQuery() may use DIFFERENT
# pooled connections. INSERT ... RETURNING id gets the id in one statement,
# connection, run both statements on it, return it.
#' Insert a row and return its generated id (Postgres RETURNING).
#' Appends "RETURNING id" to the INSERT if the caller didn't include it, then
#' reads the id back in the same statement — no separate lookup, so it's safe
#' under connection pooling.
#' Convert ? placeholders to Postgres $1, $2, $3... notation.
#' Uses a simple loop: sub() replaces one ? at a time (left to right),
#' fixed = TRUE so ? is literal (not a regex quantifier).
.pg_sql <- function(sql) {
  i <- 0L
  while (grepl("?", sql, fixed = TRUE)) {
    i <- i + 1L
    sql <- sub("?", paste0("$", i), sql, fixed = TRUE)
  }
  sql
}

.insert_returning_id <- function(pool, sql, params) {
  params <- .na_params(params)
  sql <- sub(";\\s*$", "", sql)
  if (!grepl("RETURNING", sql, ignore.case = TRUE)) {
    sql <- paste0(sql, " RETURNING id")
  }
  sql <- .pg_sql(sql)
  if (inherits(pool, "Pool")) {
    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con), add = TRUE)
    res <- DBI::dbGetQuery(con, sql, params = params)
  } else {
    res <- DBI::dbGetQuery(pool, sql, params = params)
  }
  if (nrow(res) == 0) NA_integer_ else res$id[1]
}

#' Coerce NULL / zero-length params to NA. DBI requires every bound parameter
#' to be length 1; a NULL (length 0) optional field like email = NULL raises a
#' bind error. Mapping NULL -> NA lets the column store SQL NULL as intended.
#' Normalize params for RPostgres binding. Each param must be a length-1 value.
#' - NULL / empty -> NA (RPostgres binds NA to NULL correctly)
#' - factors -> character
#' - length > 1 -> first element (defensive; params should already be scalar)
#' RPostgres binds each R type to the matching Postgres type; the main job here
#' is guaranteeing scalars and turning absent values into NA rather than
#' zero-length vectors (which RPostgres rejects).
.na_params <- function(params) {
  lapply(params, function(x) {
    if (is.null(x) || length(x) == 0L) return(NA)
    if (is.factor(x)) x <- as.character(x)
    if (length(x) > 1L) x <- x[[1]]
    x
  })
}

# =============================================================================
# PROJECTS
# =============================================================================

#' Create a project. Returns the new project id.
# =============================================================================
# USERS
# -----------------------------------------------------------------------------
# Password hashing lives in the AuthService (Phase 2). The DAL stores and
# retrieves the hash verbatim; it never sees a plain-text password.
# =============================================================================

db_active_admin_count <- function(pool) {
  row <- DBI::dbGetQuery(
    pool,
    "SELECT COUNT(*)::integer AS n FROM users WHERE role = 'admin' AND is_active = 1;"
  )
  if (!nrow(row)) 0L else as.integer(row$n[[1]])
}

db_any_admin_count <- function(pool) {
  row <- DBI::dbGetQuery(
    pool,
    "SELECT COUNT(*)::integer AS n FROM users WHERE role = 'admin';"
  )
  if (!nrow(row)) 0L else as.integer(row$n[[1]])
}

db_create_user <- function(pool, username, password_hash, email = NULL,
                           full_name = NULL, country = NULL, institution = NULL,
                           organization = NULL, department = NULL,
                           position = NULL, role = "user", is_active = TRUE) {
  .insert_returning_id(
    pool,
    "INSERT INTO users
       (username, email, password_hash, full_name, country, institution,
        organization, department, position, role, is_active, account_status,
        failed_login_count, locked_until, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, NULL, ?);",
    params = list(username, email, password_hash, full_name, country,
                  institution, organization, department, position, role,
                  as.integer(isTRUE(is_active)), if (isTRUE(is_active)) "active" else "pending", .now())
  )
}

#' Fetch a user by username OR email (case-insensitive). Returns a one-row
#' data.frame including password_hash, or NULL.
db_get_user <- function(pool, login) {
  row <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT * FROM users
             WHERE LOWER(username) = LOWER(?) OR LOWER(email) = LOWER(?)
             LIMIT 1;"),
    params = list(login, login))
  if (nrow(row) == 0) NULL else row
}

db_get_user_by_id <- function(pool, user_id) {
  row <- DBI::dbGetQuery(pool, .pg_sql("SELECT * FROM users WHERE id = ?;"),
                         params = list(user_id))
  if (nrow(row) == 0) NULL else row
}

db_username_exists <- function(pool, username) {
  n <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT COUNT(*)::integer AS n FROM users WHERE LOWER(username) = LOWER(?);"),
    params = list(username))$n
  as.integer(n) > 0
}

db_email_exists <- function(pool, email) {
  if (is.null(email) || !nzchar(email)) return(FALSE)
  n <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT COUNT(*)::integer AS n FROM users WHERE LOWER(email) = LOWER(?);"),
    params = list(email))$n
  as.integer(n) > 0
}

db_update_user_password <- function(pool, user_id, password_hash) {
  DBI::dbExecute(
    pool,
    .pg_sql("UPDATE users
             SET password_hash = ?, failed_login_count = 0, locked_until = NULL, must_reset_password = 0
             WHERE id = ?;"),
    params = list(password_hash, user_id))
  invisible(TRUE)
}

db_update_user_profile <- function(pool, user_id, fields = list()) {
  allowed <- c("full_name", "email", "country", "institution",
               "organization", "department", "position")
  fields  <- fields[intersect(names(fields), allowed)]
  if ("email" %in% names(fields)) {
    fields$email <- trimws(tolower(as.character(if (is.null(fields$email)) "" else fields$email)))
    if (!nzchar(fields$email)) fields$email <- NA_character_
  }
  if (!length(fields)) return(invisible(FALSE))
  sets   <- paste(sprintf("%s = ?", names(fields)), collapse = ", ")
  params <- c(unname(fields), list(user_id))
  DBI::dbExecute(pool, .pg_sql(sprintf("UPDATE users SET %s WHERE id = ?;", sets)),
                 params = .na_params(params))
  invisible(TRUE)
}

db_touch_last_login <- function(pool, user_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET last_login = ? WHERE id = ?;"),
                 params = list(.now(), user_id))
  invisible(TRUE)
}

#' Increment failed authentication attempts and apply a temporary lock.
db_record_login_failure <- function(pool, user_id, max_attempts = 5L,
                                    lock_minutes = 15L) {
  row <- db_get_user_by_id(pool, user_id)
  if (is.null(row)) return(invisible(FALSE))
  failures <- suppressWarnings(as.integer(if (is.null(row$failed_login_count[[1]]) || is.na(row$failed_login_count[[1]])) 0L else row$failed_login_count[[1]])) + 1L
  locked_until <- if (failures >= as.integer(max_attempts)) {
    .now() + as.difftime(lock_minutes, units = "mins")
  } else as.POSIXct(NA, tz = "UTC")
  if (failures >= as.integer(max_attempts)) failures <- 0L
  DBI::dbExecute(
    pool,
    .pg_sql("UPDATE users SET failed_login_count = ?, locked_until = ? WHERE id = ?;"),
    params = .na_params(list(failures, locked_until, user_id)))
  invisible(TRUE)
}

db_clear_login_failures <- function(pool, user_id) {
  DBI::dbExecute(
    pool,
    .pg_sql("UPDATE users SET failed_login_count = 0, locked_until = NULL WHERE id = ?;"),
    params = list(user_id))
  invisible(TRUE)
}

#' Create an authenticated application session. Only a SHA-256 token hash is stored.
db_create_user_session <- function(pool, user_id, token_hash, expires_at,
                                   remember = FALSE, user_agent = NULL, ip_hash = NULL) {
  .insert_returning_id(
    pool,
    "INSERT INTO user_sessions
       (user_id, token_hash, created_at, expires_at, last_seen_at, revoked_at, remember, user_agent, ip_hash)
     VALUES (?, ?, ?, ?, ?, NULL, ?, ?, ?);",
    params = list(user_id, token_hash, .now(), expires_at, .now(),
                  if (isTRUE(remember)) 1L else 0L, user_agent, ip_hash)
  )
}

db_session_is_active <- function(pool, session_id, user_id) {
  if (is.null(session_id) || is.null(user_id)) return(FALSE)
  n <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT COUNT(*)::integer AS n FROM user_sessions
             WHERE id = ? AND user_id = ? AND revoked_at IS NULL AND expires_at > ?;"),
    params = list(session_id, user_id, .now()))$n
  as.integer(n[[1]]) == 1L
}

db_touch_user_session <- function(pool, session_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE user_sessions SET last_seen_at = ? WHERE id = ? AND revoked_at IS NULL;"),
                 params = list(.now(), session_id))
  invisible(TRUE)
}

db_revoke_user_session <- function(pool, session_id) {
  if (is.null(session_id)) return(invisible(FALSE))
  DBI::dbExecute(pool, .pg_sql("UPDATE user_sessions SET revoked_at = COALESCE(revoked_at, ?) WHERE id = ?;"),
                 params = list(.now(), session_id))
  invisible(TRUE)
}

db_revoke_other_user_sessions <- function(pool, user_id, except_session_id = NULL) {
  if (is.null(except_session_id)) {
    DBI::dbExecute(pool, .pg_sql("UPDATE user_sessions SET revoked_at = COALESCE(revoked_at, ?) WHERE user_id = ? AND revoked_at IS NULL;"),
                   params = list(.now(), user_id))
  } else {
    DBI::dbExecute(pool, .pg_sql("UPDATE user_sessions SET revoked_at = COALESCE(revoked_at, ?) WHERE user_id = ? AND id <> ? AND revoked_at IS NULL;"),
                   params = list(.now(), user_id, except_session_id))
  }
  invisible(TRUE)
}

#' Count registered users.
db_user_count <- function(pool) {
  as.integer(DBI::dbGetQuery(pool, "SELECT COUNT(*)::integer AS n FROM users;")$n)
}

# =============================================================================
# USER WORKSPACE CHECKPOINTS
# =============================================================================

#' Read the latest lightweight workspace checkpoint for one user.
db_get_user_workspace_state <- function(pool, user_id) {
  row <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT user_id, project_id, last_route, last_module, last_analysis, workspace_state, updated_at
             FROM user_workspace_state WHERE user_id = ? LIMIT 1;"),
    params = list(user_id)
  )
  if (!nrow(row)) return(NULL)
  row$workspace_state_parsed <- list(.from_json(row$workspace_state[[1]]))
  row
}

#' Upsert one user's checkpoint. This never stores dataset BLOBs or credentials.
db_upsert_user_workspace_state <- function(pool, user_id, project_id = NULL,
                                           last_route = "home", last_module = NULL,
                                           last_analysis = NULL, workspace_state = list()) {
  if (is.null(user_id)) return(invisible(FALSE))
  project_id <- if (is.null(project_id) || length(project_id) == 0L || is.na(project_id[[1]])) NA_integer_ else as.integer(project_id[[1]])
  DBI::dbExecute(
    pool,
    .pg_sql("INSERT INTO user_workspace_state
               (user_id, project_id, last_route, last_module, last_analysis, workspace_state, updated_at)
             VALUES (?, ?, ?, ?, ?, ?, ?)
             ON CONFLICT (user_id) DO UPDATE SET
               project_id = EXCLUDED.project_id,
               last_route = EXCLUDED.last_route,
               last_module = EXCLUDED.last_module,
               last_analysis = EXCLUDED.last_analysis,
               workspace_state = EXCLUDED.workspace_state,
               updated_at = EXCLUDED.updated_at;"),
    params = .na_params(list(
      as.integer(user_id), project_id,
      as.character(if (is.null(last_route) || length(last_route) == 0L) "home" else last_route),
      if (is.null(last_module)) NA_character_ else as.character(last_module),
      if (is.null(last_analysis)) NA_character_ else substr(as.character(last_analysis), 1L, 2000L),
      .to_json(if (is.null(workspace_state)) list() else workspace_state), .now()
    ))
  )
  invisible(TRUE)
}

# =============================================================================
# ADMIN CONTROL CENTER
# =============================================================================
# Privileged administration queries intentionally exclude password hashes,
# session token hashes and serialized dataset blobs. The control center is for
# account/ownership/operations administration, not silent respondent-data access.

.db_admin_require_role <- function(role) {
  role <- tolower(trimws(as.character(if (is.null(role) || !length(role)) "" else role[[1]])))
  allowed <- c("user", "admin", "reviewer", "viewer")
  if (!role %in% allowed) stop("Invalid account role.", call. = FALSE)
  role
}

db_admin_connection_info <- function(pool) {
  DBI::dbGetQuery(pool, "SELECT current_database() AS database_name, current_user AS database_user, current_schema() AS schema_name, current_setting('server_version') AS server_version;")
}

db_admin_overview <- function(pool) {
  DBI::dbGetQuery(pool, "
    SELECT
      (SELECT COUNT(*)::integer FROM users) AS total_users,
      (SELECT COUNT(*)::integer FROM users WHERE is_active = 1) AS active_users,
      (SELECT COUNT(DISTINCT user_id)::integer FROM user_sessions
         WHERE revoked_at IS NULL AND expires_at > CURRENT_TIMESTAMP
           AND last_seen_at >= CURRENT_TIMESTAMP - INTERVAL '15 minutes') AS online_users,
      (SELECT COUNT(*)::integer FROM users WHERE role = 'admin' AND is_active = 1) AS active_admins,
      (SELECT COUNT(*)::integer FROM users WHERE created_at >= CURRENT_TIMESTAMP - INTERVAL '30 days') AS new_users_30d,
      (SELECT COUNT(*)::integer FROM user_sessions WHERE revoked_at IS NULL AND expires_at > CURRENT_TIMESTAMP) AS active_sessions,
      (SELECT COUNT(*)::integer FROM projects) AS total_projects,
      (SELECT COUNT(*)::integer FROM datasets) AS dataset_versions,
      (SELECT COALESCE(SUM(octet_length(data)), 0)::bigint FROM datasets) AS dataset_bytes,
      (SELECT COUNT(*)::integer FROM analysis_history WHERE ts >= CURRENT_TIMESTAMP - INTERVAL '24 hours') AS actions_24h;")
}

db_admin_list_users <- function(pool, search = NULL, role = NULL, status = "all") {
  where <- character()
  params <- list()
  search <- trimws(as.character(if (is.null(search) || !length(search)) "" else search[[1]]))
  role <- trimws(tolower(as.character(if (is.null(role) || !length(role)) "" else role[[1]])))
  status <- trimws(tolower(as.character(if (is.null(status) || !length(status)) "all" else status[[1]])))

  if (nzchar(search)) {
    where <- c(where, "(u.username ILIKE ? OR COALESCE(u.email, '') ILIKE ? OR COALESCE(u.full_name, '') ILIKE ? OR COALESCE(u.organization, '') ILIKE ?)")
    needle <- paste0("%", search, "%")
    params <- c(params, list(needle, needle, needle, needle))
  }
  if (nzchar(role) && !identical(role, "all")) {
    role <- .db_admin_require_role(role)
    where <- c(where, "u.role = ?")
    params <- c(params, list(role))
  }
  if (identical(status, "active")) where <- c(where, "u.account_status = 'active'")
  if (status %in% c("pending","suspended","disabled","archived")) { where <- c(where, "u.account_status = ?"); params <- c(params, list(status)) }
  if (identical(status, "inactive")) where <- c(where, "u.is_active = 0")
  if (identical(status, "locked")) where <- c(where, "u.locked_until IS NOT NULL AND u.locked_until > CURRENT_TIMESTAMP")

  sql <- paste0(
    "SELECT u.id, u.username, u.email, u.full_name, u.country, u.institution,\n",
    "       u.organization, u.department, u.position, u.role, u.is_active,\n",
    "       u.account_status, u.email_verified_at, u.mfa_enabled, u.must_reset_password,\n",
    "       u.storage_quota_bytes, u.project_quota,\n",
    "       u.failed_login_count, u.locked_until, u.created_at, u.last_login,\n",
    "       COALESCE(p.project_count, 0)::integer AS project_count,\n",
    "       COALESCE(d.dataset_count, 0)::integer AS dataset_count,\n",
    "       COALESCE(s.active_session_count, 0)::integer AS active_sessions,\n",
    "       CASE WHEN COALESCE(s.last_seen_at, TIMESTAMP '1970-01-01') >= CURRENT_TIMESTAMP - INTERVAL '15 minutes' THEN 1 ELSE 0 END AS online_now\n",
    "  FROM users u\n",
    "  LEFT JOIN (SELECT owner_id, COUNT(*) AS project_count FROM projects GROUP BY owner_id) p ON p.owner_id = u.id\n",
    "  LEFT JOIN (SELECT p2.owner_id, COUNT(d2.id) AS dataset_count FROM projects p2 LEFT JOIN datasets d2 ON d2.project_id = p2.id GROUP BY p2.owner_id) d ON d.owner_id = u.id\n",
    "  LEFT JOIN (SELECT user_id, COUNT(*) AS active_session_count, MAX(last_seen_at) AS last_seen_at FROM user_sessions WHERE revoked_at IS NULL AND expires_at > CURRENT_TIMESTAMP GROUP BY user_id) s ON s.user_id = u.id",
    if (length(where)) paste0(" WHERE ", paste(where, collapse = " AND ")) else "",
    " ORDER BY u.created_at DESC, u.id DESC;"
  )
  DBI::dbGetQuery(pool, .pg_sql(sql), params = if (length(params)) params else NULL)
}

db_admin_get_user <- function(pool, user_id) {
  out <- db_admin_list_users(pool)
  hit <- out[out$id == as.integer(user_id), , drop = FALSE]
  if (!nrow(hit)) NULL else hit[1, , drop = FALSE]
}

db_admin_list_user_projects <- function(pool, user_id) {
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT p.id, p.name, p.description, p.is_archived, p.is_favourite,
                    p.created_at, p.modified_at,
                    COALESCE(ds.dataset_versions, 0)::integer AS dataset_versions,
                    COALESCE(ds.total_rows, 0)::bigint AS stored_rows,
                    ds.last_dataset_at
               FROM projects p
               LEFT JOIN (
                 SELECT project_id, COUNT(*) AS dataset_versions,
                        COALESCE(SUM(n_rows), 0) AS total_rows,
                        MAX(created_at) AS last_dataset_at
                   FROM datasets GROUP BY project_id
               ) ds ON ds.project_id = p.id
              WHERE p.owner_id = ?
              ORDER BY p.modified_at DESC;"),
    params = list(as.integer(user_id))
  )
}

db_admin_list_user_datasets <- function(pool, user_id) {
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT d.id, d.project_id, p.name AS project_name, d.name AS dataset_name,
                    d.kind, d.version_no, d.n_rows, d.n_cols,
                    octet_length(d.data)::bigint AS stored_bytes,
                    d.created_at, d.created_by
               FROM datasets d
               JOIN projects p ON p.id = d.project_id
              WHERE p.owner_id = ?
              ORDER BY d.created_at DESC;"),
    params = list(as.integer(user_id))
  )
}

db_admin_list_user_sessions <- function(pool, user_id) {
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT id, created_at, expires_at, last_seen_at, revoked_at,
                    remember, user_agent,
                    CASE WHEN revoked_at IS NULL AND expires_at > CURRENT_TIMESTAMP THEN 1 ELSE 0 END AS is_active
               FROM user_sessions
              WHERE user_id = ?
              ORDER BY created_at DESC
              LIMIT 250;"),
    params = list(as.integer(user_id))
  )
}

db_admin_list_user_activity <- function(pool, user_id, limit = 250L) {
  limit <- suppressWarnings(as.integer(limit))
  if (is.na(limit) || limit < 1L) limit <- 250L
  limit <- min(limit, 1000L)
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT h.id, h.ts, h.project_id, p.name AS project_name,
                    h.module, h.action_type, h.description, h.reversible
               FROM analysis_history h
               LEFT JOIN projects p ON p.id = h.project_id
              WHERE h.user_id = ?
              ORDER BY h.ts DESC
              LIMIT ?;"),
    params = list(as.integer(user_id), limit)
  )
}

db_admin_list_audit <- function(pool, limit = 250L) {
  limit <- suppressWarnings(as.integer(limit))
  if (is.na(limit) || limit < 1L) limit <- 250L
  limit <- min(limit, 1000L)
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT a.id, a.created_at, a.action_type, a.description,
                    au.username AS admin_username, tu.username AS target_username
               FROM admin_audit a
               LEFT JOIN users au ON au.id = a.admin_user_id
               LEFT JOIN users tu ON tu.id = a.target_user_id
              ORDER BY a.created_at DESC
              LIMIT ?;"),
    params = list(limit)
  )
}

db_admin_log_action <- function(pool, admin_user_id, target_user_id = NULL,
                                action_type, description = NULL, metadata = NULL) {
  DBI::dbExecute(
    pool,
    .pg_sql("INSERT INTO admin_audit
               (admin_user_id, target_user_id, action_type, description, metadata, created_at)
             VALUES (?, ?, ?, ?, ?, ?);"),
    params = .na_params(list(as.integer(admin_user_id),
                             if (is.null(target_user_id)) NA_integer_ else as.integer(target_user_id),
                             as.character(action_type), description, .to_json(metadata), .now()))
  )
  invisible(TRUE)
}

db_admin_set_user_role <- function(pool, admin_user_id, target_user_id, role) {
  admin_user_id <- as.integer(admin_user_id)
  target_user_id <- as.integer(target_user_id)
  role <- .db_admin_require_role(role)
  target <- db_get_user_by_id(pool, target_user_id)
  if (is.null(target)) stop("The selected user no longer exists.", call. = FALSE)
  if (identical(admin_user_id, target_user_id) && !identical(role, "admin")) {
    stop("You cannot remove your own administrator role.", call. = FALSE)
  }
  old_role <- as.character(target$role[[1]])
  if (identical(old_role, "admin") && !identical(role, "admin")) {
    n_admin <- DBI::dbGetQuery(pool, "SELECT COUNT(*)::integer AS n FROM users WHERE role = 'admin' AND is_active = 1;")$n[[1]]
    if (as.integer(n_admin) <= 1L) stop("At least one active administrator must remain.", call. = FALSE)
  }
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET role = ? WHERE id = ?;"), params = list(role, target_user_id))
  if (!identical(old_role, role)) db_revoke_other_user_sessions(pool, target_user_id, except_session_id = NULL)
  db_admin_log_action(pool, admin_user_id, target_user_id, "role_changed",
                      paste0("Role changed from ", old_role, " to ", role, "; active sessions revoked."),
                      list(old_role = old_role, new_role = role, sessions_revoked = !identical(old_role, role)))
  invisible(TRUE)
}

db_admin_set_user_active <- function(pool, admin_user_id, target_user_id, active) {
  admin_user_id <- as.integer(admin_user_id)
  target_user_id <- as.integer(target_user_id)
  active <- isTRUE(active)
  target <- db_get_user_by_id(pool, target_user_id)
  if (is.null(target)) stop("The selected user no longer exists.", call. = FALSE)
  if (identical(admin_user_id, target_user_id) && !active) {
    stop("You cannot deactivate your own administrator account.", call. = FALSE)
  }
  if (!active && identical(as.character(target$role[[1]]), "admin")) {
    n_admin <- DBI::dbGetQuery(pool, "SELECT COUNT(*)::integer AS n FROM users WHERE role = 'admin' AND is_active = 1;")$n[[1]]
    if (as.integer(n_admin) <= 1L) stop("At least one active administrator must remain.", call. = FALSE)
  }
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET is_active = ? WHERE id = ?;"),
                 params = list(if (active) 1L else 0L, target_user_id))
  if (!active) db_revoke_other_user_sessions(pool, target_user_id, except_session_id = NULL)
  db_admin_log_action(pool, admin_user_id, target_user_id,
                      if (active) "account_activated" else "account_deactivated",
                      if (active) "Account activated." else "Account deactivated and active sessions revoked.")
  invisible(TRUE)
}

db_admin_unlock_user <- function(pool, admin_user_id, target_user_id) {
  target_user_id <- as.integer(target_user_id)
  if (is.null(db_get_user_by_id(pool, target_user_id))) stop("The selected user no longer exists.", call. = FALSE)
  db_clear_login_failures(pool, target_user_id)
  db_admin_log_action(pool, as.integer(admin_user_id), target_user_id, "account_unlocked", "Login lock and failed-attempt counter cleared.")
  invisible(TRUE)
}

db_admin_revoke_user_sessions <- function(pool, admin_user_id, target_user_id) {
  target_user_id <- as.integer(target_user_id)
  if (identical(as.integer(admin_user_id), target_user_id)) {
    stop("Use Sign out for your own session; bulk self-revocation is disabled in the Control Center.", call. = FALSE)
  }
  if (is.null(db_get_user_by_id(pool, target_user_id))) stop("The selected user no longer exists.", call. = FALSE)
  db_revoke_other_user_sessions(pool, target_user_id, except_session_id = NULL)
  db_admin_log_action(pool, as.integer(admin_user_id), target_user_id, "sessions_revoked", "All active sessions revoked by an administrator.")
  invisible(TRUE)
}

db_admin_revoke_all_other_sessions <- function(pool, admin_user_id) {
  admin_user_id <- as.integer(admin_user_id)
  n <- DBI::dbExecute(
    pool,
    .pg_sql("UPDATE user_sessions SET revoked_at = COALESCE(revoked_at, ?) WHERE user_id <> ? AND revoked_at IS NULL AND expires_at > ?;"),
    params = list(.now(), admin_user_id, .now())
  )
  db_admin_log_action(pool, admin_user_id, NULL, "system_sessions_revoked",
                      paste0("Revoked ", n, " active session(s) for other users."), list(session_count = n))
  as.integer(n)
}

db_admin_cleanup_expired_sessions <- function(pool, admin_user_id, retention_days = 90L) {
  retention_days <- max(1L, suppressWarnings(as.integer(retention_days %||% 90L)))
  cutoff <- .now() - as.difftime(retention_days, units = "days")
  n <- DBI::dbExecute(
    pool,
    .pg_sql("DELETE FROM user_sessions WHERE (expires_at < ? OR revoked_at < ?) AND created_at < ?;"),
    params = list(cutoff, cutoff, cutoff)
  )
  db_admin_log_action(pool, as.integer(admin_user_id), NULL, "expired_sessions_cleaned",
                      paste0("Removed ", n, " expired/revoked session record(s) older than ", retention_days, " days."),
                      list(session_count = n, retention_days = retention_days))
  as.integer(n)
}

# =============================================================================
# VARIABLE DICTIONARY (Metadata Engine)
# -----------------------------------------------------------------------------
# One row per variable per project. upsert_variable() is idempotent on
# (project_id, variable), so re-importing or re-detecting refreshes a row
# without duplicating it and without clobbering user-entered fields it isn't
# given (label, description, etc. are only overwritten when explicitly passed).
# =============================================================================

#' Insert or update a single variable's metadata. Only non-NULL fields are
#' written on update, so a re-detect that supplies data_type/n_missing won't
#' wipe a label the user typed earlier.
db_upsert_variable <- function(pool, project_id, variable, fields = list()) {
  now <- .now()
  existing <- DBI::dbGetQuery(
    pool,
    .pg_sql('SELECT id FROM variable_dictionary WHERE project_id = ? AND "variable" = ?;'),
    params = list(project_id, variable))

  # JSON-encode the structured columns if provided as R objects.
  for (k in c("value_labels", "missing_codes", "validation_rules")) {
    if (!is.null(fields[[k]]) && !is.character(fields[[k]])) {
      fields[[k]] <- .to_json(fields[[k]])
    }
  }

  cols_all <- c("label", "description", "units", "data_type",
                "measurement_level", "value_labels", "missing_codes",
                "survey_role", "default_chart", "default_test",
                "validation_rules", "n_missing", "n_unique")

  # Quote all identifiers so Postgres reserved words (e.g. "variable",
  # "label", "default") are treated as column names, not keywords.
  .q <- function(x) sprintf('"%s"', x)

  if (nrow(existing) == 0) {
    set_cols <- intersect(names(fields), cols_all)
    all_cols <- c("project_id", "variable", set_cols, "created_at", "modified_at")
    ph       <- paste(rep("?", length(all_cols)), collapse = ", ")
    params   <- c(list(project_id, variable),
                  lapply(set_cols, function(k) fields[[k]]),
                  list(now, now))
    DBI::dbExecute(
      pool,
      .pg_sql(sprintf('INSERT INTO variable_dictionary (%s) VALUES (%s);',
              paste(.q(all_cols), collapse = ", "), ph)),
      params = .na_params(params))
  } else {
    set_cols <- intersect(names(fields), cols_all)
    if (!length(set_cols)) return(invisible(existing$id[1]))
    sets   <- paste(sprintf('%s = ?', .q(set_cols)), collapse = ", ")
    params <- c(lapply(set_cols, function(k) fields[[k]]),
                list(now, project_id, variable))
    DBI::dbExecute(
      pool,
      .pg_sql(sprintf('UPDATE variable_dictionary SET %s, "modified_at" = ?
                WHERE project_id = ? AND "variable" = ?;', sets)),
      params = .na_params(params))
  }
  invisible(TRUE)
}

#' Full dictionary for a project as a data.frame (JSON columns left as text).
db_get_dictionary <- function(pool, project_id) {
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT * FROM variable_dictionary
      WHERE project_id = ? ORDER BY \"variable\";"),
    params = list(project_id))
}

#' One variable's row, or NULL.
db_get_variable <- function(pool, project_id, variable) {
  row <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT * FROM variable_dictionary WHERE project_id = ? AND \"variable\" = ?;"),
    params = list(project_id, variable))
  if (nrow(row) == 0) NULL else row
}

#' Remove dictionary rows whose variable is no longer in `keep` (columns that
#' were dropped during cleaning). Keeps the dictionary in sync with the data.
db_prune_dictionary <- function(pool, project_id, keep) {
  have <- db_get_dictionary(pool, project_id)$variable
  gone <- setdiff(have, keep)
  for (v in gone) {
    DBI::dbExecute(
      pool,
      .pg_sql("DELETE FROM variable_dictionary WHERE project_id = ? AND \"variable\" = ?;"),
      params = list(project_id, v))
  }
  invisible(length(gone))
}

# =============================================================================
# PROJECTS
# =============================================================================

db_create_project <- function(pool, name, description = "", settings = NULL,
                              owner_id = NULL) {
  if (!is.null(owner_id) && exists("db_assert_user_quota", mode = "function")) db_assert_user_quota(pool, owner_id, additional_bytes = 0, adding_project = TRUE)
  now <- .now()
  .insert_returning_id(
    pool,
    "INSERT INTO projects (owner_id, name, description, settings, created_at, modified_at)
     VALUES (?, ?, ?, ?, ?, ?);",
    params = list(owner_id, name, description, .to_json(settings), now, now)
  )
}

#' List projects (most-recently-modified first).
#' When `owner_id` is supplied, only that user's non-archived projects are
#' returned — this is the per-user workspace boundary. NULL returns everything
#' (single-user / admin view).
db_list_projects <- function(pool, owner_id = NULL, include_archived = FALSE) {
  where  <- character(0)
  params <- list()
  if (!is.null(owner_id)) { where <- c(where, "owner_id = ?"); params <- c(params, owner_id) }
  if (!include_archived)  { where <- c(where, "COALESCE(is_archived, 0) = 0") }
  where <- c(where, "deleted_at IS NULL")
  sql <- paste0(
    "SELECT id, owner_id, name, description,
            COALESCE(is_archived, 0)  AS is_archived,
            COALESCE(is_favourite, 0) AS is_favourite,
            created_at, modified_at
       FROM projects",
    if (length(where)) paste0(" WHERE ", paste(where, collapse = " AND ")) else "",
    " ORDER BY modified_at DESC;")
  DBI::dbGetQuery(pool, .pg_sql(sql), params = if (length(params)) params else NULL)
}

#' List every project the user can access, including shared projects.
#' Administrators receive owner-equivalent access for operational support.
db_list_accessible_projects <- function(pool, user_id, include_archived = FALSE) {
  if (is.null(user_id)) return(data.frame())
  archive_clause <- if (isTRUE(include_archived)) "" else " AND COALESCE(p.is_archived, 0) = 0"
  sql <- paste0(
    "SELECT DISTINCT p.id, p.owner_id, p.name, p.description,
            COALESCE(p.is_archived, 0) AS is_archived,
            COALESCE(pref.is_favourite, p.is_favourite, 0) AS is_favourite,
            p.created_at, p.modified_at,
            owner_u.username AS owner_username,
            CASE
              WHEN current_u.role = 'admin' OR p.owner_id = ? THEN 'owner'
              WHEN pm.permission IS NOT NULL THEN pm.permission
              ELSE team_access.permission
            END AS permission
       FROM projects p
       JOIN users current_u ON current_u.id = ?
       JOIN users owner_u ON owner_u.id = p.owner_id
       LEFT JOIN project_members pm
         ON pm.project_id = p.id AND pm.user_id = ?
       LEFT JOIN project_preferences pref ON pref.project_id = p.id AND pref.user_id = ?
       LEFT JOIN LATERAL (
         SELECT CASE MAX(CASE tpa.permission WHEN 'editor' THEN 3 WHEN 'reviewer' THEN 2 ELSE 1 END)
                  WHEN 3 THEN 'editor' WHEN 2 THEN 'reviewer' WHEN 1 THEN 'viewer' END AS permission
         FROM team_project_access tpa
         JOIN team_members tm ON tm.team_id = tpa.team_id
         WHERE tpa.project_id = p.id AND tm.user_id = ?
       ) team_access ON TRUE
      WHERE p.deleted_at IS NULL AND (current_u.role = 'admin' OR p.owner_id = ? OR pm.user_id = ? OR team_access.permission IS NOT NULL)",
    archive_clause,
    " ORDER BY p.modified_at DESC;"
  )
  DBI::dbGetQuery(pool, .pg_sql(sql), params = list(user_id, user_id, user_id, user_id, user_id, user_id, user_id))
}

#' Flip archive / favourite flags.
db_set_project_flag <- function(pool, project_id, flag = c("is_archived", "is_favourite"),
                                value = TRUE) {
  flag <- match.arg(flag)
  DBI::dbExecute(
    pool,
    .pg_sql(sprintf("UPDATE projects SET %s = ?, modified_at = ? WHERE id = ?;", flag)),
    params = list(as.integer(isTRUE(value)), .now(), project_id))
  invisible(TRUE)
}

#' Fetch a single project's metadata (settings parsed from JSON).
db_get_project <- function(pool, project_id) {
  row <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT * FROM projects WHERE id = ?;"),
    params = list(project_id)
  )
  if (nrow(row) == 0) return(NULL)
  row$settings_parsed <- list(.from_json(row$settings))
  row
}

#' Touch modified_at + optionally update fields.
db_update_project <- function(pool, project_id, name = NULL,
                              description = NULL, settings = NULL) {
  sets   <- c("modified_at = ?")
  params <- list(.now())
  if (!is.null(name))        { sets <- c(sets, "name = ?");        params <- c(params, name) }
  if (!is.null(description)) { sets <- c(sets, "description = ?"); params <- c(params, description) }
  if (!is.null(settings))    { sets <- c(sets, "settings = ?");    params <- c(params, .to_json(settings)) }
  params <- c(params, project_id)

  DBI::dbExecute(
    pool,
    .pg_sql(sprintf("UPDATE projects SET %s WHERE id = ?;", paste(sets, collapse = ", "))),
    params = .na_params(params)
  )
  invisible(TRUE)
}

#' Delete a project. ON DELETE CASCADE removes its datasets/history/models.
db_delete_project <- function(pool, project_id) {
  DBI::dbExecute(pool, .pg_sql("DELETE FROM projects WHERE id = ?;"),
                 params = list(project_id))
  invisible(TRUE)
}

# =============================================================================
# DATASETS
# =============================================================================

#' Persist a data frame under a project. Returns dataset id.
db_save_dataset <- function(pool, project_id, df, name,
                            kind = c("raw", "processed"),
                            variable_types = NULL,
                            created_by = NULL) {
  kind <- match.arg(kind)
  if (!is.data.frame(df)) stop("df must be a data.frame", call. = FALSE)
  if (!is.null(created_by)) db_require_project_permission(pool, project_id, created_by, "editor")
  if (!requireNamespace("digest", quietly = TRUE))
    stop("The digest package is required for dataset versioning.", call. = FALSE)

  # Version selection and insertion must be atomic. A project/kind-scoped
  # transaction advisory lock prevents simultaneous sessions from selecting
  # the same next version number. When the caller already owns a transaction
  # (for example the persistence preflight), that transaction remains in charge.
  holder <- if (inherits(pool, "Pool")) {
    con <- pool::poolCheckout(pool)
    list(con = con, release = function() pool::poolReturn(con))
  } else list(con = pool, release = function() invisible(NULL))
  con <- holder$con
  on.exit(holder$release(), add = TRUE)

  already_transacting <- tryCatch(
    isTRUE(RPostgres::postgresIsTransacting(con)),
    error = function(e) FALSE
  )
  started_transaction <- !already_transacting
  completed <- FALSE
  if (started_transaction) DBI::dbBegin(con)
  on.exit({
    if (started_transaction && !completed) try(DBI::dbRollback(con), silent = TRUE)
  }, add = TRUE)

  result <- tryCatch({
    lock_part <- if (identical(kind, "raw")) 1L else 2L
    DBI::dbGetQuery(
      con, "SELECT pg_advisory_xact_lock($1, $2);",
      params = list(as.integer(project_id), lock_part)
    )

    content_hash <- digest::digest(df, algo = "xxhash64", serialize = TRUE)
    blob_payload <- .r_to_blob(df)
    owner_row <- DBI::dbGetQuery(con, .pg_sql("SELECT owner_id FROM projects WHERE id=?;"), params=list(project_id))
    if (nrow(owner_row) && exists("db_assert_user_quota", mode="function")) db_assert_user_quota(con, owner_row$owner_id[[1]], additional_bytes = length(blob_payload), adding_project = FALSE)
    latest <- DBI::dbGetQuery(
      con,
      .pg_sql("SELECT id, version_no, content_hash FROM datasets
               WHERE project_id = ? AND kind = ?
               ORDER BY version_no DESC LIMIT 1;"),
      params = list(project_id, kind))

    if (nrow(latest) && identical(as.character(latest$content_hash[[1]]), content_hash)) {
      as.integer(latest$id[[1]])
    } else {
      next_version <- if (nrow(latest)) as.integer(latest$version_no[[1]]) + 1L else 1L
      id <- .insert_returning_id(
        con,
        "INSERT INTO datasets
           (project_id, name, kind, version_no, content_hash, data, variable_types,
            n_rows, n_cols, created_by, created_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
        params = list(
          project_id, name, kind, next_version, content_hash,
          blob_payload, .to_json(variable_types), nrow(df), ncol(df),
          created_by, .now()
        )
      )

      keep <- suppressWarnings(as.integer(Sys.getenv("SAP_DATASET_VERSION_LIMIT", "25")))
      if (is.na(keep) || keep < 2L) keep <- 25L
      DBI::dbExecute(
        con,
        .pg_sql("DELETE FROM datasets WHERE id IN (
          SELECT id FROM datasets WHERE project_id = ? AND kind = ?
          ORDER BY version_no DESC OFFSET ?
        );"),
        params = list(project_id, kind, keep))
      id
    }
  }, error = function(e) {
    stop("Dataset persistence failed: ", conditionMessage(e), call. = FALSE)
  })

  if (started_transaction) DBI::dbCommit(con)
  completed <- TRUE
  result
}

#' Load the most recent dataset of a given kind for a project (or by id).
db_get_dataset <- function(pool, project_id = NULL, kind = "processed",
                           dataset_id = NULL, user_id = NULL) {
  if (!is.null(project_id) && !is.null(user_id)) {
    db_require_project_permission(pool, project_id, user_id, "viewer")
  }
  if (!is.null(dataset_id)) {
    row <- DBI::dbGetQuery(pool, .pg_sql("SELECT * FROM datasets WHERE id = ?;"),
                           params = list(dataset_id))
  } else {
    row <- DBI::dbGetQuery(
      pool,
      .pg_sql("SELECT * FROM datasets
        WHERE project_id = ? AND kind = ?
        ORDER BY version_no DESC, created_at DESC LIMIT 1;"),
      params = list(project_id, kind)
    )
  }
  if (nrow(row) == 0) return(NULL)
  list(
    id             = row$id,
    name           = row$name,
    kind           = row$kind,
    data           = .blob_to_r(row$data),
    variable_types = .from_json(row$variable_types),
    n_rows         = row$n_rows,
    n_cols         = row$n_cols
  )
}

#' List dataset metadata for a project (no blobs pulled).
db_list_datasets <- function(pool, project_id, user_id = NULL) {
  if (!is.null(user_id)) db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT id, name, kind, version_no, content_hash, n_rows, n_cols, created_at
       FROM datasets WHERE project_id = ?
      ORDER BY created_at DESC;"),
    params = list(project_id)
  )
}

# =============================================================================
# ANALYSIS HISTORY  (durable audit trail)
# =============================================================================

#' Append one action to a project's history. This is the single call every
#' analysis module should make after doing something meaningful.
db_log_action <- function(pool, project_id, module, action_type,
                          description, r_code = "", reversible = FALSE,
                          metadata = NULL, user_id = NULL) {
  .insert_returning_id(
    pool,
    "INSERT INTO analysis_history
       (project_id, user_id, ts, module, action_type, description, r_code, reversible, metadata)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);",
    params = list(
      project_id, user_id, .now(), module, action_type, description,
      r_code, as.integer(isTRUE(reversible)), .to_json(metadata)
    )
  )
}

#' Full history for a project, chronological.
db_get_history <- function(pool, project_id, limit = NULL) {
  sql <- "SELECT id, ts, module, action_type, description, reversible
            FROM analysis_history WHERE project_id = ? ORDER BY ts ASC"
  if (!is.null(limit)) sql <- paste0(sql, sprintf(" LIMIT %d", as.integer(limit)))
  DBI::dbGetQuery(pool, .pg_sql(paste0(sql, ";")), params = list(project_id))
}

#' Concatenate all stored r_code for a project -> reproducible script.
db_get_history_code <- function(pool, project_id) {
  rows <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT description, r_code FROM analysis_history
      WHERE project_id = ? AND r_code IS NOT NULL AND r_code <> ''
      ORDER BY ts ASC;"),
    params = list(project_id)
  )
  if (nrow(rows) == 0) return("# No code recorded yet.")
  paste(
    mapply(function(d, c) sprintf("# %s\n%s\n", d, c),
           rows$description, rows$r_code),
    collapse = "\n"
  )
}

# =============================================================================
# MODELS
# =============================================================================

#' Persist a fitted model + its evaluation. Returns model id.
db_save_model <- function(pool, project_id, model_obj, algorithm,
                          metrics = NULL, predictors = NULL,
                          outcome = NULL, name = NULL) {
  .insert_returning_id(
    pool,
    "INSERT INTO models
       (project_id, name, algorithm, model_blob, metrics, predictors, outcome, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?);",
    params = list(
      project_id, name, algorithm,
      .r_to_blob(model_obj),
      .to_json(metrics), .to_json(predictors), outcome, .now()
    )
  )
}

#' Load a stored model object by id.
db_get_model <- function(pool, model_id) {
  row <- DBI::dbGetQuery(pool, .pg_sql("SELECT * FROM models WHERE id = ?;"),
                         params = list(model_id))
  if (nrow(row) == 0) return(NULL)
  list(
    id         = row$id,
    name       = row$name,
    algorithm  = row$algorithm,
    model      = .blob_to_r(row$model_blob),
    metrics    = .from_json(row$metrics),
    predictors = .from_json(row$predictors),
    outcome    = row$outcome
  )
}

#' List model metadata for a project (no blobs).
db_list_models <- function(pool, project_id) {
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT id, name, algorithm, outcome, created_at
       FROM models WHERE project_id = ?
      ORDER BY created_at DESC;"),
    params = list(project_id)
  )
}

# =============================================================================
# NEXUS STUDIO: AUTHORIZATION, SETTINGS, MULTI-TABLE MODELS AND PUBLISHING
# =============================================================================

.db_permission_rank <- c(viewer = 1L, reviewer = 2L, editor = 3L, owner = 4L)

db_project_permission <- function(pool, project_id, user_id) {
  if (is.null(project_id) || is.null(user_id)) return(NULL)
  row <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT
               CASE
                 WHEN p.owner_id = ? THEN 'owner'
                 WHEN pm.permission IS NOT NULL THEN pm.permission
                 ELSE team_access.permission
               END AS permission,
               u.role AS account_role
             FROM projects p
             JOIN users u ON u.id = ?
             LEFT JOIN project_members pm ON pm.project_id = p.id AND pm.user_id = ?
             LEFT JOIN LATERAL (
               SELECT CASE MAX(CASE tpa.permission WHEN 'editor' THEN 3 WHEN 'reviewer' THEN 2 ELSE 1 END)
                        WHEN 3 THEN 'editor' WHEN 2 THEN 'reviewer' WHEN 1 THEN 'viewer' END AS permission
               FROM team_project_access tpa
               JOIN team_members tm ON tm.team_id = tpa.team_id
               WHERE tpa.project_id = p.id AND tm.user_id = ?
             ) team_access ON TRUE
             WHERE p.id = ? AND p.is_archived = 0 AND p.deleted_at IS NULL
             LIMIT 1;"),
    params = list(user_id, user_id, user_id, user_id, project_id)
  )
  if (!nrow(row)) return(NULL)
  if (identical(as.character(row$account_role[[1]]), "admin")) return("owner")
  permission <- as.character(row$permission[[1]])
  if (is.na(permission) || !nzchar(permission)) NULL else permission
}

db_require_project_permission <- function(pool, project_id, user_id,
                                          minimum = "viewer") {
  permission <- db_project_permission(pool, project_id, user_id)
  actual <- unname(.db_permission_rank[as.character(permission %||% "")])
  required <- unname(.db_permission_rank[as.character(minimum %||% "")])
  if (!length(actual) || is.na(actual[[1]])) actual <- 0L else actual <- actual[[1]]
  if (!length(required) || is.na(required[[1]])) required <- 99L else required <- required[[1]]
  if (actual < required) stop("You do not have permission to perform this project action.", call. = FALSE)
  permission
}

db_get_user_settings <- function(pool, user_id) {
  rows <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT setting_key, setting_value FROM app_settings WHERE user_id = ? ORDER BY setting_key;"),
    params = list(user_id)
  )
  if (!nrow(rows)) return(list())
  out <- lapply(rows$setting_value, .from_json)
  stats::setNames(out, rows$setting_key)
}

db_upsert_user_setting <- function(pool, user_id, key, value) {
  DBI::dbExecute(
    pool,
    .pg_sql("INSERT INTO app_settings(user_id, setting_key, setting_value, modified_at)
             VALUES (?, ?, ?, ?)
             ON CONFLICT(user_id, setting_key) DO UPDATE SET
               setting_value = EXCLUDED.setting_value,
               modified_at = EXCLUDED.modified_at;"),
    params = list(user_id, as.character(key), .to_json(value), .now())
  )
  invisible(TRUE)
}

db_delete_user_settings <- function(pool, user_id, key = NULL) {
  if (is.null(key)) {
    DBI::dbExecute(pool, .pg_sql("DELETE FROM app_settings WHERE user_id = ?;"), params = list(user_id))
  } else {
    DBI::dbExecute(pool, .pg_sql("DELETE FROM app_settings WHERE user_id = ? AND setting_key = ?;"), params = list(user_id, key))
  }
  invisible(TRUE)
}

db_upsert_model_table <- function(pool, project_id, name, data, schema = NULL,
                                  source_type = "memory", source_config = list(),
                                  created_by = NULL) {
  db_require_project_permission(pool, project_id, created_by, "editor")
  if (!is.data.frame(data)) data <- as.data.frame(data, stringsAsFactors = FALSE)
  content_hash <- digest::digest(data, algo = "sha256")
  DBI::dbGetQuery(
    pool,
    .pg_sql("INSERT INTO model_tables
               (project_id, name, source_type, source_config, schema_spec,
                source_data, content_hash, n_rows, n_cols, created_by,
                created_at, updated_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
             ON CONFLICT(project_id, name) DO UPDATE SET
               source_type = EXCLUDED.source_type,
               source_config = EXCLUDED.source_config,
               schema_spec = EXCLUDED.schema_spec,
               source_data = EXCLUDED.source_data,
               content_hash = EXCLUDED.content_hash,
               n_rows = EXCLUDED.n_rows,
               n_cols = EXCLUDED.n_cols,
               updated_at = EXCLUDED.updated_at
             RETURNING id;"),
    params = list(
      project_id, as.character(name), as.character(source_type),
      .to_json(if (is.null(source_config)) list() else source_config),
      .to_json(if (is.null(schema)) list() else schema),
      .r_to_blob(data), content_hash, nrow(data), ncol(data), created_by,
      .now(), .now()
    )
  )$id[[1]]
}

db_load_model_tables <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  rows <- DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT id, name, source_type, source_config, schema_spec,
                    source_data, n_rows, n_cols, created_at, updated_at
             FROM model_tables WHERE project_id = ? ORDER BY name;"),
    params = list(project_id)
  )
  if (!nrow(rows)) return(list())
  out <- lapply(seq_len(nrow(rows)), function(i) list(
    id = rows$id[[i]], name = rows$name[[i]], source_type = rows$source_type[[i]],
    source_config = .from_json(rows$source_config[[i]]) %||% list(),
    schema = .from_json(rows$schema_spec[[i]]) %||% data.frame(),
    data = .blob_to_r(rows$source_data[i]),
    n_rows = rows$n_rows[[i]], n_cols = rows$n_cols[[i]],
    created_at = rows$created_at[[i]], updated_at = rows$updated_at[[i]]
  ))
  stats::setNames(out, rows$name)
}

db_delete_model_table <- function(pool, project_id, name, user_id) {
  db_require_project_permission(pool, project_id, user_id, "editor")
  DBI::dbExecute(pool, .pg_sql("DELETE FROM model_tables WHERE project_id = ? AND name = ?;"),
                 params = list(project_id, name))
  invisible(TRUE)
}

db_upsert_studio_document <- function(pool, project_id, document_type, name,
                                      specification, created_by = NULL) {
  db_require_project_permission(pool, project_id, created_by, "editor")
  .insert_returning_id(
    pool,
    "INSERT INTO studio_documents
       (project_id, document_type, name, version_no, specification, created_by, created_at)
     VALUES (?, ?, ?,
       (SELECT COALESCE(MAX(version_no), 0) + 1 FROM studio_documents
        WHERE project_id = ? AND document_type = ? AND name = ?),
       ?, ?, ?);",
    params = list(project_id, document_type, name, project_id, document_type,
                  name, .to_json(specification %||% list()), created_by, .now())
  )
}

db_get_latest_studio_document <- function(pool, project_id, document_type,
                                          name = NULL, user_id = NULL) {
  if (!is.null(user_id)) db_require_project_permission(pool, project_id, user_id, "viewer")
  if (is.null(name)) {
    row <- DBI::dbGetQuery(
      pool,
      .pg_sql("SELECT * FROM studio_documents
               WHERE project_id = ? AND document_type = ?
               ORDER BY version_no DESC, created_at DESC LIMIT 1;"),
      params = list(project_id, document_type)
    )
  } else {
    row <- DBI::dbGetQuery(
      pool,
      .pg_sql("SELECT * FROM studio_documents
               WHERE project_id = ? AND document_type = ? AND name = ?
               ORDER BY version_no DESC LIMIT 1;"),
      params = list(project_id, document_type, name)
    )
  }
  if (!nrow(row)) return(NULL)
  row$specification_parsed <- list(.from_json(row$specification[[1]]) %||% list())
  row
}

db_list_studio_documents <- function(pool, project_id, document_type = NULL,
                                     user_id = NULL) {
  if (!is.null(user_id)) db_require_project_permission(pool, project_id, user_id, "viewer")
  if (is.null(document_type)) {
    DBI::dbGetQuery(pool, .pg_sql("SELECT id, document_type, name, version_no, created_by, created_at
                                  FROM studio_documents WHERE project_id = ?
                                  ORDER BY created_at DESC;"), params = list(project_id))
  } else {
    DBI::dbGetQuery(pool, .pg_sql("SELECT id, document_type, name, version_no, created_by, created_at
                                  FROM studio_documents WHERE project_id = ? AND document_type = ?
                                  ORDER BY created_at DESC;"), params = list(project_id, document_type))
  }
}

db_list_project_members <- function(pool, project_id, requesting_user_id) {
  db_require_project_permission(pool, project_id, requesting_user_id, "viewer")
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT u.id AS user_id, u.username, u.full_name, u.email,
                    pm.permission, pm.added_at
             FROM project_members pm
             JOIN users u ON u.id = pm.user_id
             WHERE pm.project_id = ?
             ORDER BY CASE pm.permission WHEN 'owner' THEN 1 WHEN 'editor' THEN 2
                       WHEN 'reviewer' THEN 3 ELSE 4 END, u.username;"),
    params = list(project_id)
  )
}

db_add_project_member <- function(pool, project_id, username, permission,
                                  requesting_user_id) {
  db_require_project_permission(pool, project_id, requesting_user_id, "owner")
  permission <- tolower(trimws(as.character(permission)))
  if (!permission %in% c("editor", "reviewer", "viewer")) stop("Choose editor, reviewer, or viewer.", call. = FALSE)
  user <- db_get_user(pool, username)
  if (is.null(user)) stop("No active account matches that username or email.", call. = FALSE)
  if ("is_active" %in% names(user) && !isTRUE(as.integer(user$is_active[[1]]) == 1L)) {
    stop("That account is inactive and cannot be added to a project.", call. = FALSE)
  }
  DBI::dbExecute(
    pool,
    .pg_sql("INSERT INTO project_members(project_id, user_id, permission, added_at)
             VALUES (?, ?, ?, ?)
             ON CONFLICT(project_id, user_id) DO UPDATE SET
               permission = EXCLUDED.permission,
               added_at = EXCLUDED.added_at;"),
    params = list(project_id, user$id[[1]], permission, .now())
  )
  invisible(user$id[[1]])
}

db_update_project_member <- function(pool, project_id, member_user_id,
                                     permission, requesting_user_id) {
  db_require_project_permission(pool, project_id, requesting_user_id, "owner")
  permission <- tolower(trimws(as.character(permission)))
  if (!permission %in% c("editor", "reviewer", "viewer")) stop("Invalid member permission.", call. = FALSE)
  owner <- DBI::dbGetQuery(pool, .pg_sql("SELECT owner_id FROM projects WHERE id = ?;"), params = list(project_id))$owner_id[[1]]
  if (identical(as.integer(owner), as.integer(member_user_id))) stop("Project ownership cannot be changed here.", call. = FALSE)
  DBI::dbExecute(pool, .pg_sql("UPDATE project_members SET permission = ? WHERE project_id = ? AND user_id = ?;"),
                 params = list(permission, project_id, member_user_id))
  invisible(TRUE)
}

db_remove_project_member <- function(pool, project_id, member_user_id,
                                     requesting_user_id) {
  db_require_project_permission(pool, project_id, requesting_user_id, "owner")
  owner <- DBI::dbGetQuery(pool, .pg_sql("SELECT owner_id FROM projects WHERE id = ?;"), params = list(project_id))$owner_id[[1]]
  if (identical(as.integer(owner), as.integer(member_user_id))) stop("The project owner cannot be removed.", call. = FALSE)
  DBI::dbExecute(pool, .pg_sql("DELETE FROM project_members WHERE project_id = ? AND user_id = ?;"),
                 params = list(project_id, member_user_id))
  invisible(TRUE)
}

db_publish_dashboard <- function(pool, project_id, title, description = "",
                                 audience = "workspace", specification,
                                 published_by) {
  db_require_project_permission(pool, project_id, published_by, "editor")
  .insert_returning_id(
    pool,
    "INSERT INTO dashboard_publications
       (project_id, title, description, audience, status, version_no,
        specification, published_by, published_at)
     VALUES (?, ?, ?, ?, 'published',
       (SELECT COALESCE(MAX(version_no), 0) + 1 FROM dashboard_publications
        WHERE project_id = ? AND title = ?), ?, ?, ?);",
    params = list(project_id, title, description, audience, project_id, title,
                  .to_json(specification), published_by, .now())
  )
}

db_list_dashboard_publications <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT p.id, p.title, p.description, p.audience, p.status,
                    p.version_no, p.published_at, u.username AS published_by
             FROM dashboard_publications p
             LEFT JOIN users u ON u.id = p.published_by
             WHERE p.project_id = ? ORDER BY p.published_at DESC;"),
    params = list(project_id)
  )
}

db_add_report_comment <- function(pool, project_id, publication_id,
                                  user_id, comment_text) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  .insert_returning_id(
    pool,
    "INSERT INTO report_comments(project_id, publication_id, user_id,
                                  comment_text, status, created_at)
     VALUES (?, ?, ?, ?, 'open', ?);",
    params = list(project_id, publication_id, user_id,
                  substr(trimws(comment_text), 1L, 5000L), .now())
  )
}

db_list_report_comments <- function(pool, project_id, publication_id,
                                    user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(
    pool,
    .pg_sql("SELECT c.id, c.comment_text, c.status, c.created_at,
                    c.resolved_at, u.username
             FROM report_comments c LEFT JOIN users u ON u.id = c.user_id
             WHERE c.project_id = ? AND c.publication_id = ?
             ORDER BY c.created_at DESC;"),
    params = list(project_id, publication_id)
  )
}

db_upsert_refresh_schedule <- function(pool, project_id, table_name,
                                       source_type, source_config, frequency,
                                       scheduled_time = NULL, weekday = NULL,
                                       timezone = "UTC", incremental_field = NULL,
                                       credential_ref = NULL, enabled = TRUE,
                                       next_run_at = NULL, created_by = NULL) {
  db_require_project_permission(pool, project_id, created_by, "editor")
  DBI::dbGetQuery(
    pool,
    .pg_sql("INSERT INTO refresh_schedules
               (project_id, table_name, source_type, source_config, frequency,
                scheduled_time, weekday, timezone, incremental_field,
                credential_ref, enabled, next_run_at, created_by, created_at, updated_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
             ON CONFLICT(project_id, table_name) DO UPDATE SET
               source_type = EXCLUDED.source_type,
               source_config = EXCLUDED.source_config,
               frequency = EXCLUDED.frequency,
               scheduled_time = EXCLUDED.scheduled_time,
               weekday = EXCLUDED.weekday,
               timezone = EXCLUDED.timezone,
               incremental_field = EXCLUDED.incremental_field,
               credential_ref = EXCLUDED.credential_ref,
               enabled = EXCLUDED.enabled,
               next_run_at = EXCLUDED.next_run_at,
               updated_at = EXCLUDED.updated_at
             RETURNING id;"),
    params = list(project_id, table_name, source_type, .to_json(source_config %||% list()),
                  frequency, scheduled_time, weekday, timezone, incremental_field,
                  credential_ref, as.integer(isTRUE(enabled)), next_run_at,
                  created_by, .now(), .now())
  )$id[[1]]
}

db_list_refresh_schedules <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  rows <- DBI::dbGetQuery(pool, .pg_sql("SELECT * FROM refresh_schedules WHERE project_id = ? ORDER BY table_name;"), params = list(project_id))
  if (nrow(rows)) rows$source_config_parsed <- lapply(rows$source_config, .from_json)
  rows
}

db_start_refresh_run <- function(pool, project_id, table_name,
                                 trigger_type = "manual", triggered_by = NULL,
                                 schedule_id = NULL) {
  if (!is.null(triggered_by)) db_require_project_permission(pool, project_id, triggered_by, "editor")
  .insert_returning_id(pool,
    "INSERT INTO refresh_runs(schedule_id, project_id, table_name,
                              trigger_type, status, started_at, triggered_by)
     VALUES (?, ?, ?, ?, 'running', ?, ?);",
    params = list(schedule_id, project_id, table_name, trigger_type, .now(), triggered_by))
}

db_finish_refresh_run <- function(pool, run_id, status, rows_loaded = NULL,
                                  message = NULL) {
  if (!status %in% c("succeeded", "failed", "cancelled")) stop("Invalid refresh completion status.", call. = FALSE)
  DBI::dbExecute(pool, .pg_sql("UPDATE refresh_runs SET status = ?, rows_loaded = ?, message = ?, finished_at = ? WHERE id = ?;"),
                 params = .na_params(list(status, rows_loaded, substr(message %||% "", 1L, 5000L), .now(), run_id)))
  invisible(TRUE)
}

db_list_refresh_runs <- function(pool, project_id, user_id, limit = 100L) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(pool, .pg_sql("SELECT id, table_name, trigger_type, status, rows_loaded,
                                        started_at, finished_at, message
                                 FROM refresh_runs WHERE project_id = ?
                                 ORDER BY started_at DESC LIMIT ?;"),
                  params = list(project_id, as.integer(limit)))
}

db_add_row_security_rule <- function(pool, project_id, table_name, field_name,
                                     allowed_values, subject_type, subject_value,
                                     created_by) {
  db_require_project_permission(pool, project_id, created_by, "owner")
  .insert_returning_id(pool,
    "INSERT INTO row_security_rules(project_id, table_name, field_name,
                                    allowed_values, subject_type, subject_value,
                                    enabled, created_by, created_at)
     VALUES (?, ?, ?, ?, ?, ?, 1, ?, ?);",
    params = list(project_id, table_name, field_name, .to_json(as.character(allowed_values)),
                  subject_type, subject_value, created_by, .now()))
}

db_list_row_security_rules <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  rows <- DBI::dbGetQuery(pool, .pg_sql("SELECT * FROM row_security_rules WHERE project_id = ? ORDER BY created_at DESC;"), params = list(project_id))
  if (nrow(rows)) rows$allowed_values_parsed <- lapply(rows$allowed_values, .from_json)
  rows
}

db_delete_row_security_rule <- function(pool, project_id, rule_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "owner")
  DBI::dbExecute(pool, .pg_sql("DELETE FROM row_security_rules WHERE id = ? AND project_id = ?;"), params = list(rule_id, project_id))
  invisible(TRUE)
}

# =============================================================================
# ARROW / PARQUET STUB  (only if Python needs to read the raw data)
# =============================================================================
# serialize() blobs above are R-only. If a Python service must consume the
# datasets, store them as Parquet instead and keep only a path in the DB:
#
# db_save_dataset_arrow <- function(pool, project_id, df, name, dir = "data_store") {
#   if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
#   path <- file.path(dir, sprintf("proj%d_%s_%s.parquet",
#                                  project_id, name, format(Sys.time(), "%Y%m%d%H%M%S")))
#   arrow::write_parquet(df, path)
#   # then store `path` in a TEXT column instead of the BLOB
#   path
# }
#
# This makes the data language-neutral (R via arrow::read_parquet,
# Python via pandas.read_parquet / pyarrow) at the cost of a file store
# alongside the DB.

# =============================================================================
# END OF DATA ACCESS LAYER
# =============================================================================

message("[SAP] Database Backend v4.0.0-20260815 loaded")  # SAP_MODULE_BANNER

# =============================================================================
# ENTERPRISE PLATFORM 5.0 — identity, teams, collaboration and operations
# =============================================================================

sap_secure_token <- function(bytes = 32L) {
  bytes <- max(16L, as.integer(bytes))
  raw <- if (requireNamespace("sodium", quietly = TRUE)) sodium::random(bytes) else as.raw(sample.int(256L, bytes, replace = TRUE) - 1L)
  paste(sprintf("%02x", as.integer(raw)), collapse = "")
}

sap_token_hash <- function(token) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("The digest package is required.", call. = FALSE)
  digest::digest(enc2utf8(as.character(token)), algo = "sha256", serialize = FALSE)
}

db_update_account_status <- function(pool, user_id, status) {
  status <- match.arg(as.character(status), c("pending", "active", "suspended", "disabled", "archived"))
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET account_status = ?, is_active = CASE WHEN ? = 'active' THEN 1 ELSE 0 END WHERE id = ?;"),
                 params = list(status, status, as.integer(user_id)))
  invisible(TRUE)
}

db_mark_email_verified <- function(pool, user_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET email_verified_at = CURRENT_TIMESTAMP WHERE id = ?;"), params = list(as.integer(user_id)))
  invisible(TRUE)
}

db_create_account_token <- function(pool, user_id, purpose, ttl_minutes = 60L) {
  purpose <- match.arg(purpose, c("email_verification", "password_reset"))
  ttl_minutes <- max(5L, min(10080L, as.integer(ttl_minutes)))
  token <- sap_secure_token(32L)
  hash <- sap_token_hash(token)
  DBI::dbExecute(pool, .pg_sql("UPDATE account_tokens SET used_at = CURRENT_TIMESTAMP WHERE user_id = ? AND purpose = ? AND used_at IS NULL;"),
                 params = list(as.integer(user_id), purpose))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO account_tokens (user_id, purpose, token_hash, expires_at) VALUES (?, ?, ?, CURRENT_TIMESTAMP + (? * INTERVAL '1 minute'));"),
                 params = list(as.integer(user_id), purpose, hash, ttl_minutes))
  token
}

db_consume_account_token <- function(pool, token, purpose) {
  purpose <- match.arg(purpose, c("email_verification", "password_reset"))
  hash <- sap_token_hash(token)
  row <- DBI::dbGetQuery(pool, .pg_sql("SELECT id, user_id FROM account_tokens WHERE token_hash = ? AND purpose = ? AND used_at IS NULL AND expires_at > CURRENT_TIMESTAMP LIMIT 1;"),
                         params = list(hash, purpose))
  if (!nrow(row)) return(NULL)
  DBI::dbExecute(pool, .pg_sql("UPDATE account_tokens SET used_at = CURRENT_TIMESTAMP WHERE id = ?;"), params = list(as.integer(row$id[[1]])))
  as.integer(row$user_id[[1]])
}

db_set_password_hash <- function(pool, user_id, password_hash, revoke_sessions = TRUE) {
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET password_hash = ?, failed_login_count = 0, locked_until = NULL, must_reset_password = 0 WHERE id = ?;"),
                 params = list(as.character(password_hash), as.integer(user_id)))
  if (isTRUE(revoke_sessions)) db_revoke_other_user_sessions(pool, as.integer(user_id), except_session_id = NULL)
  invisible(TRUE)
}

db_queue_email <- function(pool, recipient, subject, body_text, template_key = NULL, metadata = NULL) {
  DBI::dbExecute(pool, .pg_sql("INSERT INTO email_outbox (recipient, subject, body_text, template_key, metadata) VALUES (?, ?, ?, ?, ?);"),
                 params = .na_params(list(tolower(trimws(recipient)), subject, body_text, template_key, .to_json(metadata %||% list()))))
  invisible(TRUE)
}

db_list_email_outbox <- function(pool, limit = 100L) {
  DBI::dbGetQuery(pool, .pg_sql("SELECT id, recipient, subject, template_key, status, attempts, last_error, created_at, sent_at FROM email_outbox ORDER BY created_at DESC LIMIT ?;"),
                  params = list(min(1000L, max(1L, as.integer(limit)))))
}

db_create_notification <- function(pool, user_id, title, body = NULL, notification_type = "info", project_id = NULL, action_url = NULL, metadata = NULL) {
  DBI::dbExecute(pool, .pg_sql("INSERT INTO notifications (user_id, notification_type, title, body, project_id, action_url, metadata) VALUES (?, ?, ?, ?, ?, ?, ?);"),
                 params = .na_params(list(as.integer(user_id), notification_type, title, body,
                                          if (is.null(project_id)) NA_integer_ else as.integer(project_id), action_url, .to_json(metadata %||% list()))))
  invisible(TRUE)
}

db_list_notifications <- function(pool, user_id, unread_only = FALSE, limit = 100L) {
  sql <- if (isTRUE(unread_only))
    "SELECT * FROM notifications WHERE user_id = ? AND read_at IS NULL ORDER BY created_at DESC LIMIT ?;" else
    "SELECT * FROM notifications WHERE user_id = ? ORDER BY created_at DESC LIMIT ?;"
  DBI::dbGetQuery(pool, .pg_sql(sql), params = list(as.integer(user_id), min(500L, max(1L, as.integer(limit)))))
}

db_mark_notification_read <- function(pool, notification_id, user_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE notifications SET read_at = COALESCE(read_at, CURRENT_TIMESTAMP) WHERE id = ? AND user_id = ?;"),
                 params = list(as.integer(notification_id), as.integer(user_id)))
  invisible(TRUE)
}

db_mark_all_notifications_read <- function(pool, user_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE notifications SET read_at = COALESCE(read_at, CURRENT_TIMESTAMP) WHERE user_id = ?;"), params = list(as.integer(user_id)))
  invisible(TRUE)
}

db_create_organization <- function(pool, owner_user_id, name, description = NULL) {
  name <- trimws(as.character(name))
  if (!nzchar(name)) stop("Organization name is required.", call. = FALSE)
  slug <- tolower(gsub("[^a-z0-9]+", "-", name))
  slug <- gsub("(^-|-$)", "", slug)
  if (!nzchar(slug)) slug <- paste0("organization-", as.integer(Sys.time()))
  base <- slug; i <- 1L
  while (nrow(DBI::dbGetQuery(pool, .pg_sql("SELECT 1 FROM organizations WHERE slug = ? LIMIT 1;"), params = list(slug)))) {
    i <- i + 1L; slug <- paste0(base, "-", i)
  }
  row <- DBI::dbGetQuery(pool, .pg_sql("INSERT INTO organizations (name, slug, description, owner_user_id) VALUES (?, ?, ?, ?) RETURNING id;"),
                         params = .na_params(list(name, slug, description, as.integer(owner_user_id))))
  oid <- as.integer(row$id[[1]])
  DBI::dbExecute(pool, .pg_sql("INSERT INTO organization_members (organization_id, user_id, role) VALUES (?, ?, 'owner') ON CONFLICT DO NOTHING;"), params = list(oid, as.integer(owner_user_id)))
  db_log_audit_event(pool, owner_user_id, "organization_created", "organization", oid, paste("Created organization", name), organization_id = oid)
  oid
}

db_list_user_organizations <- function(pool, user_id) {
  DBI::dbGetQuery(pool, .pg_sql("SELECT o.id, o.name, o.slug, o.description, om.role, o.owner_user_id, o.storage_quota_bytes, o.project_quota, o.created_at FROM organizations o JOIN organization_members om ON om.organization_id = o.id WHERE om.user_id = ? ORDER BY o.name;"), params = list(as.integer(user_id)))
}

db_organization_role <- function(pool, organization_id, user_id) {
  r <- DBI::dbGetQuery(pool, .pg_sql("SELECT role FROM organization_members WHERE organization_id = ? AND user_id = ?;"), params = list(as.integer(organization_id), as.integer(user_id)))
  if (!nrow(r)) NULL else as.character(r$role[[1]])
}

db_require_organization_role <- function(pool, organization_id, user_id, allowed = c("owner", "admin")) {
  role <- db_organization_role(pool, organization_id, user_id)
  if (is.null(role) || !role %in% allowed) stop("You do not have permission to manage this organization.", call. = FALSE)
  invisible(role)
}

db_add_organization_member <- function(pool, organization_id, actor_user_id, member_user_id, role = "member") {
  db_require_organization_role(pool, organization_id, actor_user_id, c("owner", "admin", "manager"))
  role <- match.arg(role, c("admin", "manager", "member", "viewer"))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO organization_members (organization_id, user_id, role) VALUES (?, ?, ?) ON CONFLICT (organization_id, user_id) DO UPDATE SET role = EXCLUDED.role;"),
                 params = list(as.integer(organization_id), as.integer(member_user_id), role))
  db_create_notification(pool, member_user_id, "Organization access updated", paste("You were added to an organization as", role), "access")
  db_log_audit_event(pool, actor_user_id, "organization_member_updated", "user", member_user_id, paste("Organization role set to", role), organization_id = organization_id)
  invisible(TRUE)
}

db_list_organization_members <- function(pool, organization_id, user_id) {
  role <- db_organization_role(pool, organization_id, user_id)
  if (is.null(role)) stop("Organization access is required.", call. = FALSE)
  DBI::dbGetQuery(pool, .pg_sql("SELECT u.id, u.username, u.full_name, u.email, om.role, om.joined_at FROM organization_members om JOIN users u ON u.id = om.user_id WHERE om.organization_id = ? ORDER BY om.role, u.username;"), params = list(as.integer(organization_id)))
}

db_create_team <- function(pool, organization_id, actor_user_id, name, description = NULL) {
  db_require_organization_role(pool, organization_id, actor_user_id, c("owner", "admin", "manager"))
  row <- DBI::dbGetQuery(pool, .pg_sql("INSERT INTO teams (organization_id, name, description, created_by) VALUES (?, ?, ?, ?) RETURNING id;"),
                         params = .na_params(list(as.integer(organization_id), trimws(name), description, as.integer(actor_user_id))))
  as.integer(row$id[[1]])
}

db_list_teams <- function(pool, organization_id, user_id) {
  if (is.null(db_organization_role(pool, organization_id, user_id))) stop("Organization access is required.", call. = FALSE)
  DBI::dbGetQuery(pool, .pg_sql("SELECT t.id, t.name, t.description, COUNT(tm.user_id)::integer AS members, t.created_at FROM teams t LEFT JOIN team_members tm ON tm.team_id = t.id WHERE t.organization_id = ? GROUP BY t.id ORDER BY t.name;"), params = list(as.integer(organization_id)))
}

db_add_team_member <- function(pool, team_id, actor_user_id, member_user_id, role = "member") {
  t <- DBI::dbGetQuery(pool, .pg_sql("SELECT organization_id FROM teams WHERE id = ?;"), params = list(as.integer(team_id)))
  if (!nrow(t)) stop("Team not found.", call. = FALSE)
  db_require_organization_role(pool, t$organization_id[[1]], actor_user_id, c("owner", "admin", "manager"))
  role <- match.arg(role, c("lead", "member", "viewer"))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO team_members (team_id, user_id, role) VALUES (?, ?, ?) ON CONFLICT (team_id, user_id) DO UPDATE SET role = EXCLUDED.role;"), params = list(as.integer(team_id), as.integer(member_user_id), role))
  invisible(TRUE)
}

db_assign_project_team <- function(pool, team_id, project_id, actor_user_id, permission = "viewer") {
  db_require_project_permission(pool, project_id, actor_user_id, "owner")
  team <- DBI::dbGetQuery(pool, .pg_sql("SELECT organization_id FROM teams WHERE id=?;"), params = list(as.integer(team_id)))
  project <- DBI::dbGetQuery(pool, .pg_sql("SELECT organization_id FROM projects WHERE id=?;"), params = list(as.integer(project_id)))
  if (!nrow(team) || !nrow(project)) stop("Team or project not found.", call. = FALSE)
  team_org <- as.integer(team$organization_id[[1]])
  project_org <- suppressWarnings(as.integer(project$organization_id[[1]]))
  if (is.na(project_org) || !identical(project_org, team_org)) {
    stop("Assign the project to the team's organization before granting team access.", call. = FALSE)
  }
  db_require_organization_role(pool, team_org, actor_user_id, c("owner", "admin", "manager"))
  permission <- match.arg(permission, c("editor", "reviewer", "viewer"))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO team_project_access (team_id, project_id, permission) VALUES (?, ?, ?) ON CONFLICT (team_id, project_id) DO UPDATE SET permission = EXCLUDED.permission;"), params = list(as.integer(team_id), as.integer(project_id), permission))
  db_log_audit_event(pool, actor_user_id, "team_project_access_updated", "project", project_id, paste("Team access set to", permission), project_id = project_id, organization_id = team_org)
  invisible(TRUE)
}

db_create_project_invitation <- function(pool, project_id, inviter_user_id, email, permission = "viewer", ttl_days = 7L) {
  db_require_project_permission(pool, project_id, inviter_user_id, "owner")
  email <- tolower(trimws(as.character(email)))
  if (!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", email)) stop("Enter a valid email address.", call. = FALSE)
  permission <- match.arg(permission, c("editor", "reviewer", "viewer"))
  token <- sap_secure_token(32L); hash <- sap_token_hash(token)
  DBI::dbExecute(pool, .pg_sql("INSERT INTO project_invitations (project_id, email, permission, token_hash, invited_by, expires_at) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP + (? * INTERVAL '1 day'));"), params = list(as.integer(project_id), email, permission, hash, as.integer(inviter_user_id), max(1L, min(30L, as.integer(ttl_days)))))
  db_queue_email(pool, email, "Survey Analytics project invitation", paste("You were invited to a Survey Analytics project with", permission, "access. Invitation token:", token), "project_invitation", list(project_id = project_id))
  token
}

db_accept_project_invitation <- function(pool, user_id, token) {
  hash <- sap_token_hash(token)
  user <- db_get_user_by_id(pool, user_id)
  if (is.null(user)) stop("Account not found.", call. = FALSE)
  email <- tolower(trimws(as.character(user$email[[1]] %||% "")))
  inv <- DBI::dbGetQuery(pool, .pg_sql("SELECT * FROM project_invitations WHERE token_hash = ? AND accepted_at IS NULL AND revoked_at IS NULL AND expires_at > CURRENT_TIMESTAMP LIMIT 1;"), params = list(hash))
  if (!nrow(inv) || !identical(tolower(inv$email[[1]]), email)) stop("Invitation is invalid, expired, or belongs to another email address.", call. = FALSE)
  DBI::dbExecute(pool, .pg_sql("INSERT INTO project_members (project_id, user_id, permission) VALUES (?, ?, ?) ON CONFLICT (project_id, user_id) DO UPDATE SET permission = EXCLUDED.permission;"), params = list(as.integer(inv$project_id[[1]]), as.integer(user_id), as.character(inv$permission[[1]])))
  DBI::dbExecute(pool, .pg_sql("UPDATE project_invitations SET accepted_at = CURRENT_TIMESTAMP WHERE id = ?;"), params = list(as.integer(inv$id[[1]])))
  db_create_notification(pool, user_id, "Project invitation accepted", "The shared project is now available in Project Manager.", "access", inv$project_id[[1]])
  as.integer(inv$project_id[[1]])
}

db_list_project_members <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(pool, .pg_sql("SELECT u.id, u.username, u.full_name, u.email, pm.permission, pm.added_at FROM project_members pm JOIN users u ON u.id = pm.user_id WHERE pm.project_id = ? ORDER BY CASE pm.permission WHEN 'owner' THEN 1 WHEN 'editor' THEN 2 WHEN 'reviewer' THEN 3 ELSE 4 END, u.username;"), params = list(as.integer(project_id)))
}

db_set_project_member <- function(pool, project_id, actor_user_id, member_user_id, permission) {
  db_require_project_permission(pool, project_id, actor_user_id, "owner")
  if (identical(as.integer(actor_user_id), as.integer(member_user_id))) stop("The project owner role cannot be changed here.", call. = FALSE)
  permission <- match.arg(permission, c("editor", "reviewer", "viewer"))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO project_members (project_id, user_id, permission) VALUES (?, ?, ?) ON CONFLICT (project_id, user_id) DO UPDATE SET permission = EXCLUDED.permission;"), params = list(as.integer(project_id), as.integer(member_user_id), permission))
  db_create_notification(pool, member_user_id, "Project access changed", paste("Your project permission is now", permission), "access", project_id)
  invisible(TRUE)
}

db_remove_project_member <- function(pool, project_id, actor_user_id, member_user_id) {
  db_require_project_permission(pool, project_id, actor_user_id, "owner")
  DBI::dbExecute(pool, .pg_sql("DELETE FROM project_members WHERE project_id = ? AND user_id = ? AND permission <> 'owner';"), params = list(as.integer(project_id), as.integer(member_user_id)))
  db_create_notification(pool, member_user_id, "Project access removed", "Your access to a shared project was removed.", "access", project_id)
  invisible(TRUE)
}

db_soft_delete_project <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "owner")
  DBI::dbExecute(pool, .pg_sql("UPDATE projects SET deleted_at = CURRENT_TIMESTAMP, deleted_by = ?, modified_at = CURRENT_TIMESTAMP WHERE id = ?;"), params = list(as.integer(user_id), as.integer(project_id)))
  db_log_audit_event(pool, user_id, "project_recycled", "project", project_id, "Project moved to recycle bin", project_id = project_id)
  invisible(TRUE)
}

db_list_recycled_projects <- function(pool, user_id) {
  DBI::dbGetQuery(pool, .pg_sql("SELECT id, name, description, deleted_at, modified_at FROM projects WHERE owner_id = ? AND deleted_at IS NOT NULL ORDER BY deleted_at DESC;"), params = list(as.integer(user_id)))
}

db_restore_project <- function(pool, project_id, user_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE projects SET deleted_at = NULL, deleted_by = NULL, modified_at = CURRENT_TIMESTAMP WHERE id = ? AND owner_id = ?;"), params = list(as.integer(project_id), as.integer(user_id)))
  invisible(TRUE)
}

db_purge_project <- function(pool, project_id, user_id) {
  DBI::dbExecute(pool, .pg_sql("DELETE FROM projects WHERE id = ? AND owner_id = ? AND deleted_at IS NOT NULL;"), params = list(as.integer(project_id), as.integer(user_id)))
  invisible(TRUE)
}

db_create_folder <- function(pool, user_id, name, organization_id = NULL) {
  row <- DBI::dbGetQuery(pool, .pg_sql("INSERT INTO project_folders (owner_user_id, organization_id, name) VALUES (?, ?, ?) ON CONFLICT (owner_user_id, organization_id, name) DO UPDATE SET name = EXCLUDED.name RETURNING id;"), params = .na_params(list(as.integer(user_id), if (is.null(organization_id)) NA_integer_ else as.integer(organization_id), trimws(name))))
  as.integer(row$id[[1]])
}

db_list_folders <- function(pool, user_id) DBI::dbGetQuery(pool, .pg_sql("SELECT id, name, organization_id, created_at FROM project_folders WHERE owner_user_id = ? ORDER BY name;"), params = list(as.integer(user_id)))

db_move_project_to_folder <- function(pool, project_id, user_id, folder_id = NULL) {
  db_require_project_permission(pool, project_id, user_id, "owner")
  DBI::dbExecute(pool, .pg_sql("UPDATE projects SET folder_id = ?, modified_at = CURRENT_TIMESTAMP WHERE id = ?;"), params = .na_params(list(if (is.null(folder_id)) NA_integer_ else as.integer(folder_id), as.integer(project_id))))
  invisible(TRUE)
}

db_add_project_tag <- function(pool, project_id, user_id, tag_name) {
  db_require_project_permission(pool, project_id, user_id, "editor")
  tag_name <- trimws(tag_name); if (!nzchar(tag_name)) stop("Tag name is required.", call. = FALSE)
  tag <- DBI::dbGetQuery(pool, .pg_sql("INSERT INTO tags (owner_user_id, name) VALUES (?, ?) ON CONFLICT (owner_user_id, name) DO UPDATE SET name = EXCLUDED.name RETURNING id;"), params = list(as.integer(user_id), tag_name))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO project_tags (project_id, tag_id) VALUES (?, ?) ON CONFLICT DO NOTHING;"), params = list(as.integer(project_id), as.integer(tag$id[[1]])))
  invisible(TRUE)
}

db_set_project_favourite <- function(pool, project_id, user_id, favourite = TRUE) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbExecute(pool, .pg_sql("INSERT INTO project_preferences (user_id, project_id, is_favourite, last_opened_at) VALUES (?, ?, ?, CURRENT_TIMESTAMP) ON CONFLICT (user_id, project_id) DO UPDATE SET is_favourite = EXCLUDED.is_favourite, last_opened_at = CURRENT_TIMESTAMP;"), params = list(as.integer(user_id), as.integer(project_id), if (isTRUE(favourite)) 1L else 0L))
  invisible(TRUE)
}

db_assert_user_quota <- function(pool, user_id, additional_bytes = 0, adding_project = FALSE) {
  u <- DBI::dbGetQuery(pool, .pg_sql("SELECT storage_quota_bytes, project_quota FROM users WHERE id=?;"), params=list(as.integer(user_id)))
  if (!nrow(u)) return(invisible(TRUE))
  if (isTRUE(adding_project) && !is.na(u$project_quota[[1]])) {
    n <- DBI::dbGetQuery(pool, .pg_sql("SELECT COUNT(*)::integer AS n FROM projects WHERE owner_id=? AND deleted_at IS NULL;"), params=list(as.integer(user_id)))$n[[1]]
    if (as.integer(n) >= as.integer(u$project_quota[[1]])) stop("Your project quota has been reached. Contact an administrator.", call. = FALSE)
  }
  if (!is.na(u$storage_quota_bytes[[1]])) {
    used <- DBI::dbGetQuery(pool, .pg_sql("SELECT COALESCE(SUM(octet_length(d.data)),0)::bigint AS bytes FROM datasets d JOIN projects p ON p.id=d.project_id WHERE p.owner_id=?;"), params=list(as.integer(user_id)))$bytes[[1]]
    if (as.numeric(used) + as.numeric(additional_bytes) > as.numeric(u$storage_quota_bytes[[1]])) stop("Your storage quota would be exceeded. Remove old data or contact an administrator.", call. = FALSE)
  }
  invisible(TRUE)
}

db_user_usage <- function(pool, user_id) {
  DBI::dbGetQuery(pool, .pg_sql("SELECT u.storage_quota_bytes, u.project_quota, COALESCE(p.project_count,0)::integer AS project_count, COALESCE(d.storage_bytes,0)::bigint AS storage_bytes FROM users u LEFT JOIN (SELECT owner_id, COUNT(*) FILTER (WHERE deleted_at IS NULL) AS project_count FROM projects GROUP BY owner_id) p ON p.owner_id=u.id LEFT JOIN (SELECT p.owner_id, SUM(octet_length(d.data)) AS storage_bytes FROM projects p JOIN datasets d ON d.project_id=p.id GROUP BY p.owner_id) d ON d.owner_id=u.id WHERE u.id=?;"), params = list(as.integer(user_id)))
}

db_global_search <- function(pool, user_id, query, limit = 50L) {
  q <- paste0("%", tolower(trimws(query)), "%")
  limit <- min(200L, max(5L, as.integer(limit)))
  DBI::dbGetQuery(pool, .pg_sql("WITH accessible AS (SELECT p.id, p.name FROM projects p WHERE p.deleted_at IS NULL AND (p.owner_id = ? OR EXISTS (SELECT 1 FROM project_members pm WHERE pm.project_id=p.id AND pm.user_id=?) OR EXISTS (SELECT 1 FROM team_project_access tpa JOIN team_members tm ON tm.team_id=tpa.team_id WHERE tpa.project_id=p.id AND tm.user_id=?))) SELECT 'project'::text AS result_type, p.id::text AS result_id, p.name AS title, COALESCE(p.description,'') AS detail, p.id AS project_id FROM projects p JOIN accessible a ON a.id=p.id WHERE lower(p.name) LIKE ? OR lower(COALESCE(p.description,'')) LIKE ? UNION ALL SELECT 'dataset', d.id::text, d.name, d.kind || ' v' || d.version_no::text, d.project_id FROM datasets d JOIN accessible a ON a.id=d.project_id WHERE lower(d.name) LIKE ? UNION ALL SELECT 'variable', vd.id::text, vd.variable, COALESCE(vd.label,vd.description,''), vd.project_id FROM variable_dictionary vd JOIN accessible a ON a.id=vd.project_id WHERE lower(vd.variable) LIKE ? OR lower(COALESCE(vd.label,'')) LIKE ? ORDER BY title LIMIT ?;"), params = list(as.integer(user_id), as.integer(user_id), as.integer(user_id), q, q, q, q, q, limit))
}

db_create_api_token <- function(pool, user_id, name, scopes = c("read"), expires_days = 90L) {
  token <- paste0("sap_", sap_secure_token(32L)); prefix <- substr(token, 1L, 12L)
  DBI::dbExecute(pool, .pg_sql("INSERT INTO api_tokens (user_id, name, token_prefix, token_hash, scopes, expires_at) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP + (? * INTERVAL '1 day'));"), params = list(as.integer(user_id), trimws(name), prefix, sap_token_hash(token), .to_json(unique(as.character(scopes))), max(1L, min(3650L, as.integer(expires_days)))))
  token
}

db_list_api_tokens <- function(pool, user_id) DBI::dbGetQuery(pool, .pg_sql("SELECT id, name, token_prefix, scopes, last_used_at, expires_at, revoked_at, created_at FROM api_tokens WHERE user_id=? ORDER BY created_at DESC;"), params = list(as.integer(user_id)))

db_revoke_api_token <- function(pool, user_id, token_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE api_tokens SET revoked_at = CURRENT_TIMESTAMP WHERE id=? AND user_id=?;"), params = list(as.integer(token_id), as.integer(user_id)))
  invisible(TRUE)
}

db_add_project_comment <- function(pool, project_id, user_id, text, object_type = "project", object_key = NULL, assigned_to = NULL) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  text <- trimws(text); if (!nzchar(text)) stop("Comment cannot be empty.", call. = FALSE)
  row <- DBI::dbGetQuery(pool, .pg_sql("INSERT INTO project_comments (project_id, author_user_id, object_type, object_key, comment_text, assigned_to) VALUES (?, ?, ?, ?, ?, ?) RETURNING id;"), params = .na_params(list(as.integer(project_id), as.integer(user_id), object_type, object_key, text, if (is.null(assigned_to)) NA_integer_ else as.integer(assigned_to))))
  notified <- integer()
  if (!is.null(assigned_to)) {
    db_create_notification(pool, assigned_to, "New review comment", text, "comment", project_id)
    notified <- c(notified, as.integer(assigned_to))
  }
  raw_mentions <- regmatches(text, gregexpr("@[A-Za-z0-9._-]{3,40}", text, perl = TRUE))[[1]]
  mentions <- if (length(raw_mentions) && !identical(raw_mentions[[1]], "")) unique(sub("^@", "", raw_mentions)) else character()
  for (username in mentions) {
    hit <- DBI::dbGetQuery(pool, .pg_sql("SELECT id FROM users WHERE lower(username)=lower(?) AND is_active=1 LIMIT 1;"), params = list(username))
    if (nrow(hit) && !as.integer(hit$id[[1]]) %in% c(as.integer(user_id), notified)) {
      allowed <- tryCatch({ db_require_project_permission(pool, project_id, hit$id[[1]], "viewer"); TRUE }, error = function(e) FALSE)
      if (allowed) db_create_notification(pool, hit$id[[1]], paste0("@", username, " mentioned you"), text, "mention", project_id)
    }
  }
  as.integer(row$id[[1]])
}

db_list_project_comments <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(pool, .pg_sql("SELECT c.id, c.object_type, c.object_key, c.comment_text, c.status, c.created_at, u.username AS author, au.username AS assigned_to FROM project_comments c LEFT JOIN users u ON u.id=c.author_user_id LEFT JOIN users au ON au.id=c.assigned_to WHERE c.project_id=? ORDER BY c.created_at DESC;"), params = list(as.integer(project_id)))
}

db_resolve_project_comment <- function(pool, comment_id, user_id) {
  c <- DBI::dbGetQuery(pool, .pg_sql("SELECT project_id FROM project_comments WHERE id=?;"), params = list(as.integer(comment_id)))
  if (!nrow(c)) stop("Comment not found.", call. = FALSE)
  db_require_project_permission(pool, c$project_id[[1]], user_id, "reviewer")
  DBI::dbExecute(pool, .pg_sql("UPDATE project_comments SET status='resolved', resolved_at=CURRENT_TIMESTAMP WHERE id=?;"), params = list(as.integer(comment_id)))
  invisible(TRUE)
}

db_create_review_request <- function(pool, project_id, user_id, object_type, object_key, reviewer_user_id = NULL) {
  db_require_project_permission(pool, project_id, user_id, "editor")
  row <- DBI::dbGetQuery(pool, .pg_sql("INSERT INTO review_requests (project_id, object_type, object_key, status, requested_by, reviewer_user_id, requested_at) VALUES (?, ?, ?, 'in_review', ?, ?, CURRENT_TIMESTAMP) RETURNING id;"), params = .na_params(list(as.integer(project_id), object_type, object_key, as.integer(user_id), if (is.null(reviewer_user_id)) NA_integer_ else as.integer(reviewer_user_id))))
  if (!is.null(reviewer_user_id)) db_create_notification(pool, reviewer_user_id, "Review requested", paste("Review requested for", object_type, object_key), "review", project_id)
  as.integer(row$id[[1]])
}

db_update_review_status <- function(pool, review_id, user_id, status, note = NULL) {
  status <- match.arg(status, c("changes_requested", "approved", "published"))
  r <- DBI::dbGetQuery(pool, .pg_sql("SELECT project_id FROM review_requests WHERE id=?;"), params = list(as.integer(review_id)))
  if (!nrow(r)) stop("Review request not found.", call. = FALSE)
  db_require_project_permission(pool, r$project_id[[1]], user_id, if (status == "published") "owner" else "reviewer")
  DBI::dbExecute(pool, .pg_sql("UPDATE review_requests SET status=?, decision_note=?, decided_at=CURRENT_TIMESTAMP, updated_at=CURRENT_TIMESTAMP WHERE id=?;"), params = .na_params(list(status, note, as.integer(review_id))))
  invisible(TRUE)
}

db_list_review_requests <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(pool, .pg_sql("SELECT r.*, rq.username AS requested_by_username, rv.username AS reviewer_username FROM review_requests r LEFT JOIN users rq ON rq.id=r.requested_by LEFT JOIN users rv ON rv.id=r.reviewer_user_id WHERE r.project_id=? ORDER BY r.updated_at DESC;"), params = list(as.integer(project_id)))
}

db_log_audit_event <- function(pool, actor_user_id, event_type, object_type = NULL, object_id = NULL, description = NULL, metadata = NULL, project_id = NULL, organization_id = NULL) {
  DBI::dbExecute(pool, .pg_sql("INSERT INTO audit_events (actor_user_id, project_id, organization_id, event_type, object_type, object_id, description, metadata) VALUES (?, ?, ?, ?, ?, ?, ?, ?);"), params = .na_params(list(if (is.null(actor_user_id)) NA_integer_ else as.integer(actor_user_id), if (is.null(project_id)) NA_integer_ else as.integer(project_id), if (is.null(organization_id)) NA_integer_ else as.integer(organization_id), event_type, object_type, if (is.null(object_id)) NULL else as.character(object_id), description, .to_json(metadata %||% list()))))
  invisible(TRUE)
}

db_list_audit_events <- function(pool, user_id, project_id = NULL, limit = 250L) {
  limit <- min(1000L, max(10L, as.integer(limit)))
  if (!is.null(project_id)) {
    db_require_project_permission(pool, project_id, user_id, "viewer")
    return(DBI::dbGetQuery(pool, .pg_sql("SELECT ae.id, ae.created_at, ae.event_type, ae.object_type, ae.object_id, ae.description, u.username AS actor FROM audit_events ae LEFT JOIN users u ON u.id=ae.actor_user_id WHERE ae.project_id=? ORDER BY ae.created_at DESC LIMIT ?;"), params = list(as.integer(project_id), limit)))
  }
  DBI::dbGetQuery(pool, .pg_sql("SELECT ae.id, ae.created_at, ae.event_type, ae.object_type, ae.object_id, ae.description, u.username AS actor, ae.project_id FROM audit_events ae LEFT JOIN users u ON u.id=ae.actor_user_id WHERE ae.actor_user_id=? ORDER BY ae.created_at DESC LIMIT ?;"), params = list(as.integer(user_id), limit))
}

db_list_dataset_versions <- function(pool, project_id, user_id) {
  db_require_project_permission(pool, project_id, user_id, "viewer")
  DBI::dbGetQuery(pool, .pg_sql("SELECT d.id, d.name, d.kind, d.version_no, d.n_rows, d.n_cols, d.content_hash, d.created_at, u.username AS created_by_username FROM datasets d LEFT JOIN users u ON u.id=d.created_by WHERE d.project_id=? ORDER BY d.kind, d.version_no DESC, d.created_at DESC;"), params = list(as.integer(project_id)))
}

db_record_backup_run <- function(pool, status, backup_type = "database", location = NULL, checksum = NULL, size_bytes = NULL, initiated_by = NULL, error_message = NULL, run_id = NULL) {
  if (is.null(run_id)) {
    row <- DBI::dbGetQuery(pool, .pg_sql("INSERT INTO backup_runs (backup_type, status, storage_location, checksum_sha256, size_bytes, initiated_by, error_message) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id;"), params = .na_params(list(backup_type, status, location, checksum, size_bytes, initiated_by, error_message)))
    return(as.integer(row$id[[1]]))
  }
  DBI::dbExecute(pool, .pg_sql("UPDATE backup_runs SET status=?, storage_location=?, checksum_sha256=?, size_bytes=?, error_message=?, finished_at=CURRENT_TIMESTAMP WHERE id=?;"), params = .na_params(list(status, location, checksum, size_bytes, error_message, as.integer(run_id))))
  invisible(run_id)
}

db_list_backup_runs <- function(pool, limit = 100L) DBI::dbGetQuery(pool, .pg_sql("SELECT * FROM backup_runs ORDER BY started_at DESC LIMIT ?;"), params = list(min(500L, max(1L, as.integer(limit)))))

db_list_active_sessions <- function(pool, user_id) {
  DBI::dbGetQuery(pool, .pg_sql("SELECT id, created_at, expires_at, last_seen_at, remember, user_agent, CASE WHEN revoked_at IS NULL AND expires_at > CURRENT_TIMESTAMP THEN 1 ELSE 0 END AS is_active FROM user_sessions WHERE user_id=? ORDER BY last_seen_at DESC;"), params = list(as.integer(user_id)))
}

db_revoke_session_by_id <- function(pool, user_id, session_id) {
  DBI::dbExecute(pool, .pg_sql("UPDATE user_sessions SET revoked_at=COALESCE(revoked_at,CURRENT_TIMESTAMP) WHERE id=? AND user_id=?;"), params = list(as.integer(session_id), as.integer(user_id)))
  invisible(TRUE)
}

db_search_users <- function(pool, query, limit = 50L) {
  q <- paste0("%", tolower(trimws(query)), "%")
  DBI::dbGetQuery(pool, .pg_sql("SELECT id, username, full_name, email, role, account_status FROM users WHERE lower(username) LIKE ? OR lower(COALESCE(full_name,'')) LIKE ? OR lower(COALESCE(email,'')) LIKE ? ORDER BY username LIMIT ?;"), params = list(q, q, q, min(200L, max(5L, as.integer(limit)))))
}

db_set_user_quota <- function(pool, admin_user_id, target_user_id, storage_quota_bytes = NULL, project_quota = NULL) {
  admin <- db_get_user_by_id(pool, admin_user_id)
  if (is.null(admin) || !identical(as.character(admin$role[[1]]), "admin")) stop("Administrator access is required.", call. = FALSE)
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET storage_quota_bytes=?, project_quota=? WHERE id=?;"), params = .na_params(list(storage_quota_bytes, project_quota, as.integer(target_user_id))))
  db_admin_log_action(pool, admin_user_id, target_user_id, "quota_changed", "User storage/project quota changed.", list(storage_quota_bytes = storage_quota_bytes, project_quota = project_quota))
  invisible(TRUE)
}

db_register_sso_provider <- function(pool, admin_user_id, organization_id, provider_type, name, issuer_url = NULL, client_id = NULL, client_secret_ref = NULL, scopes = "openid email profile", enabled = FALSE) {
  admin <- db_get_user_by_id(pool, admin_user_id)
  if (is.null(admin) || !identical(as.character(admin$role[[1]]), "admin")) stop("Administrator access is required.", call. = FALSE)
  provider_type <- match.arg(provider_type, c("oidc", "azure_ad", "google"))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO sso_providers (organization_id, provider_type, name, issuer_url, client_id, client_secret_ref, scopes, enabled) VALUES (?, ?, ?, ?, ?, ?, ?, ?);"), params = .na_params(list(organization_id, provider_type, name, issuer_url, client_id, client_secret_ref, scopes, if (isTRUE(enabled)) 1L else 0L)))
  invisible(TRUE)
}

db_list_sso_providers <- function(pool) DBI::dbGetQuery(pool, "SELECT id, organization_id, provider_type, name, issuer_url, client_id, client_secret_ref, scopes, enabled, created_at, updated_at FROM sso_providers ORDER BY name;")

# Opt-in PostgreSQL RLS readiness check. Application-layer permission checks remain
# mandatory; native policies should only be enabled after the deployment role is
# configured to SET LOCAL sap.user_id on every checked-out connection.
db_native_rls_readiness <- function(pool) {
  DBI::dbGetQuery(pool, "SELECT current_user AS database_user, current_setting('sap.user_id', true) AS sap_user_id, current_setting('row_security', true) AS row_security;")
}

# ---- Authenticator-app MFA --------------------------------------------------
.sap_raw_to_hex <- function(x) paste(sprintf("%02x", as.integer(x)), collapse = "")
.sap_hex_to_raw <- function(x) {
  x <- gsub("[^0-9A-Fa-f]", "", as.character(x))
  if (nchar(x) %% 2L) stop("Invalid encrypted MFA payload.", call. = FALSE)
  as.raw(strtoi(substring(x, seq(1L, nchar(x), 2L), seq(2L, nchar(x), 2L)), 16L))
}
.sap_base32_alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", "", fixed = TRUE)[[1]]
.sap_base32_encode <- function(raw) {
  bits <- paste(vapply(as.integer(raw), function(v) paste(rev(as.integer(intToBits(v))[1:8]), collapse = ""), character(1)), collapse = "")
  pad <- (5L - nchar(bits) %% 5L) %% 5L
  if (pad) bits <- paste0(bits, strrep("0", pad))
  idx <- vapply(seq(1L, nchar(bits), 5L), function(i) strtoi(substr(bits, i, i + 4L), 2L) + 1L, integer(1))
  paste0(.sap_base32_alphabet[idx], collapse = "")
}
.sap_base32_decode <- function(text) {
  chars <- strsplit(toupper(gsub("[^A-Z2-7]", "", text)), "", fixed = TRUE)[[1]]
  idx <- match(chars, .sap_base32_alphabet) - 1L
  if (anyNA(idx)) stop("Invalid Base32 MFA secret.", call. = FALSE)
  bits <- paste(vapply(idx, function(v) sprintf("%05d", as.integer(intToBits(v))[1] * 0L), character(1)), collapse = "")
  # sprintf cannot format binary; build explicitly.
  bits <- paste(vapply(idx, function(v) paste(rev(as.integer(intToBits(v))[1:5]), collapse = ""), character(1)), collapse = "")
  nbytes <- floor(nchar(bits) / 8L)
  if (!nbytes) return(raw())
  as.raw(vapply(seq_len(nbytes), function(i) strtoi(substr(bits, (i-1L)*8L+1L, i*8L), 2L), integer(1)))
}
.sap_mfa_key <- function() {
  secret <- Sys.getenv("SAP_MFA_ENCRYPTION_KEY", "")
  if (nchar(secret) < 24L) stop("Set SAP_MFA_ENCRYPTION_KEY to a strong secret before enabling MFA.", call. = FALSE)
  if (!requireNamespace("sodium", quietly = TRUE)) stop("The sodium package is required for MFA encryption.", call. = FALSE)
  sodium::hash(charToRaw(enc2utf8(secret)))
}
.sap_mfa_encrypt <- function(secret) .sap_raw_to_hex(sodium::data_encrypt(charToRaw(secret), .sap_mfa_key()))
.sap_mfa_decrypt <- function(ciphertext) rawToChar(sodium::data_decrypt(.sap_hex_to_raw(ciphertext), .sap_mfa_key()))

sap_totp_code <- function(secret, at = Sys.time(), digits = 6L, period = 30L) {
  if (!requireNamespace("openssl", quietly = TRUE)) stop("The openssl package is required for MFA.", call. = FALSE)
  counter <- floor(as.numeric(at) / period)
  # Counter fits in 32 bits for current epochs; TOTP requires 8-byte big endian.
  lo <- as.integer(counter %% 2^32)
  msg <- as.raw(c(0,0,0,0, bitwAnd(bitwShiftR(lo,24),255), bitwAnd(bitwShiftR(lo,16),255), bitwAnd(bitwShiftR(lo,8),255), bitwAnd(lo,255)))
  digest <- as.raw(openssl::sha1(msg, key = .sap_base32_decode(secret)))
  offset <- bitwAnd(as.integer(digest[[length(digest)]]), 15L)
  p <- as.integer(digest[(offset + 1L):(offset + 4L)])
  binary <- bitwAnd(p[[1]], 127L) * 2^24 + p[[2]] * 2^16 + p[[3]] * 2^8 + p[[4]]
  sprintf(paste0("%0", digits, "d"), as.integer(binary %% (10^digits)))
}

db_begin_mfa_enrollment <- function(pool, user_id) {
  secret <- .sap_base32_encode(sodium::random(20L))
  recovery <- replicate(8L, paste(sample(c(LETTERS, 2:9), 10L, replace = TRUE), collapse = ""))
  hashes <- vapply(recovery, sodium::password_store, character(1))
  DBI::dbExecute(pool, .pg_sql("INSERT INTO user_mfa (user_id, secret_ciphertext, recovery_hashes, verified_at) VALUES (?, ?, ?, NULL) ON CONFLICT (user_id) DO UPDATE SET secret_ciphertext=EXCLUDED.secret_ciphertext, recovery_hashes=EXCLUDED.recovery_hashes, verified_at=NULL, enrolled_at=CURRENT_TIMESTAMP;"),
                 params = list(as.integer(user_id), .sap_mfa_encrypt(secret), .to_json(as.list(hashes))))
  list(secret = secret, recovery_codes = recovery)
}

db_verify_mfa_enrollment <- function(pool, user_id, code) {
  row <- DBI::dbGetQuery(pool, .pg_sql("SELECT secret_ciphertext FROM user_mfa WHERE user_id=?;"), params = list(as.integer(user_id)))
  if (!nrow(row)) return(FALSE)
  secret <- .sap_mfa_decrypt(row$secret_ciphertext[[1]])
  ok <- any(vapply(c(-1,0,1), function(w) identical(sprintf("%06d", as.integer(code)), sap_totp_code(secret, Sys.time() + w*30)), logical(1)))
  if (ok) {
    DBI::dbExecute(pool, .pg_sql("UPDATE user_mfa SET verified_at=CURRENT_TIMESTAMP WHERE user_id=?;"), params = list(as.integer(user_id)))
    DBI::dbExecute(pool, .pg_sql("UPDATE users SET mfa_enabled=1 WHERE id=?;"), params = list(as.integer(user_id)))
  }
  ok
}

db_verify_mfa_code <- function(pool, user_id, code) {
  row <- DBI::dbGetQuery(pool, .pg_sql("SELECT secret_ciphertext, recovery_hashes FROM user_mfa WHERE user_id=? AND verified_at IS NOT NULL;"), params = list(as.integer(user_id)))
  if (!nrow(row)) return(FALSE)
  code <- trimws(as.character(code))
  if (grepl("^[0-9]{6}$", code)) {
    secret <- .sap_mfa_decrypt(row$secret_ciphertext[[1]])
    return(any(vapply(c(-1,0,1), function(w) identical(code, sap_totp_code(secret, Sys.time() + w*30)), logical(1))))
  }
  hashes <- .from_json(row$recovery_hashes[[1]])
  if (!length(hashes)) return(FALSE)
  hit <- which(vapply(hashes, function(h) tryCatch(sodium::password_verify(as.character(h), code), error = function(e) FALSE), logical(1)))[1]
  if (is.na(hit)) return(FALSE)
  hashes <- hashes[-hit]
  DBI::dbExecute(pool, .pg_sql("UPDATE user_mfa SET recovery_hashes=? WHERE user_id=?;"), params = list(.to_json(as.list(hashes)), as.integer(user_id)))
  TRUE
}

db_disable_mfa <- function(pool, user_id) {
  DBI::dbExecute(pool, .pg_sql("DELETE FROM user_mfa WHERE user_id=?;"), params = list(as.integer(user_id)))
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET mfa_enabled=0 WHERE id=?;"), params = list(as.integer(user_id)))
  invisible(TRUE)
}

db_authenticate_api_token <- function(pool, token, required_scope = "read") {
  token <- trimws(as.character(token %||% ""))
  if (!nzchar(token)) return(NULL)
  row <- DBI::dbGetQuery(pool, .pg_sql("SELECT at.id, at.user_id, at.scopes, u.username, u.role, u.is_active FROM api_tokens at JOIN users u ON u.id=at.user_id WHERE at.token_hash=? AND at.revoked_at IS NULL AND (at.expires_at IS NULL OR at.expires_at > CURRENT_TIMESTAMP) LIMIT 1;"), params = list(sap_token_hash(token)))
  if (!nrow(row) || !identical(as.integer(row$is_active[[1]]), 1L)) return(NULL)
  scopes <- .from_json(row$scopes[[1]])
  if (!required_scope %in% scopes && !"admin" %in% scopes) return(NULL)
  DBI::dbExecute(pool, .pg_sql("UPDATE api_tokens SET last_used_at=CURRENT_TIMESTAMP WHERE id=?;"), params = list(as.integer(row$id[[1]])))
  list(token_id=as.integer(row$id[[1]]), user_id=as.integer(row$user_id[[1]]), username=as.character(row$username[[1]]), role=as.character(row$role[[1]]), scopes=scopes)
}

db_admin_issue_password_reset <- function(pool, admin_user_id, target_user_id, ttl_minutes = 60L) {
  admin_user_id <- as.integer(admin_user_id)
  target_user_id <- as.integer(target_user_id)
  admin <- db_get_user_by_id(pool, admin_user_id)
  target <- db_get_user_by_id(pool, target_user_id)
  if (is.null(admin) || !identical(as.character(admin$role[[1]]), "admin")) stop("Administrator access is required.", call. = FALSE)
  if (is.null(target)) stop("The selected user no longer exists.", call. = FALSE)
  email <- if (!is.null(target$email) && !is.na(target$email[[1]])) trimws(as.character(target$email[[1]])) else ""
  token <- db_create_account_token(pool, target_user_id, "password_reset", ttl_minutes)
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET must_reset_password=1 WHERE id=?;"), params = list(target_user_id))
  db_revoke_other_user_sessions(pool, target_user_id, except_session_id = NULL)
  if (nzchar(email)) {
    try(db_queue_email(pool, email, "Survey Analytics password reset required",
      paste("An administrator requires you to reset your password. Use this one-time token within", ttl_minutes, "minutes:", token),
      "admin_password_reset", list(user_id = target_user_id, admin_user_id = admin_user_id)), silent = TRUE)
  }
  db_admin_log_action(pool, admin_user_id, target_user_id, "password_reset_required",
    "Administrator required a password reset; active sessions were revoked.", list(ttl_minutes = ttl_minutes, email_queued = nzchar(email)))
  token
}

db_admin_set_account_status <- function(pool, admin_user_id, target_user_id, status) {
  admin_user_id <- as.integer(admin_user_id); target_user_id <- as.integer(target_user_id)
  status <- match.arg(status, c("pending","active","suspended","disabled","archived"))
  target <- db_get_user_by_id(pool, target_user_id)
  if (is.null(target)) stop("The selected user no longer exists.", call. = FALSE)
  if (identical(admin_user_id, target_user_id) && !identical(status, "active")) stop("You cannot suspend, disable, or archive your own administrator account.", call. = FALSE)
  active <- identical(status, "active")
  DBI::dbExecute(pool, .pg_sql("UPDATE users SET account_status=?, is_active=? WHERE id=?;"), params=list(status, if (active) 1L else 0L, target_user_id))
  if (!active) db_revoke_other_user_sessions(pool, target_user_id, except_session_id=NULL)
  db_admin_log_action(pool, admin_user_id, target_user_id, "account_status_changed", paste("Account status changed to", status), list(status=status))
  invisible(TRUE)
}

db_assign_project_organization <- function(pool, project_id, actor_user_id, organization_id = NULL) {
  db_require_project_permission(pool, project_id, actor_user_id, "owner")
  if (!is.null(organization_id)) db_require_organization_role(pool, organization_id, actor_user_id, c("owner","admin","manager"))
  DBI::dbExecute(pool, .pg_sql("UPDATE projects SET organization_id=?, modified_at=CURRENT_TIMESTAMP WHERE id=?;"), params=.na_params(list(if (is.null(organization_id)) NA_integer_ else as.integer(organization_id), as.integer(project_id))))
  invisible(TRUE)
}
