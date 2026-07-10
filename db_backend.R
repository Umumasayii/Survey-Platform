# =============================================================================
# BACKEND: DATA ACCESS LAYER (DAL)
# =============================================================================
# Durable persistence for the Survey Analytics Platform.
#
# Design principles:
#   - The front-end NEVER touches storage directly. It calls db_* functions.
#   - Driver-agnostic: SQLite today, PostgreSQL later, same code. Only
#     db_connect() knows which driver is in play.
#   - Connection POOLING via {pool}. In Shiny you create ONE pool at app
#     startup and share it; you do NOT open a connection per query or per
#     session. Close it in onStop().
#   - Parameterised queries everywhere (params = list(...)). No paste()/glue
#     into SQL -> no injection, no quoting bugs.
#   - R objects (data frames, fitted models) are stored as serialised BLOBs.
#     NOTE: serialize() output is R-only. If Python must read the *data*,
#     store datasets as Parquet instead (see db_save_dataset_arrow() stub).
#
# Dependencies: DBI, pool, blob, jsonlite
#   install.packages(c("DBI", "pool", "blob", "jsonlite", "RSQLite"))
#   # for Postgres later: install.packages("RPostgres")
# =============================================================================

library(DBI)
library(pool)
library(blob)
library(jsonlite)

# =============================================================================
# CONNECTION MANAGEMENT
# =============================================================================

#' Open a pooled connection to the backend.
#'
#' @param backend "sqlite" or "postgres".
#' @param sqlite_path File path for the SQLite database (backend = "sqlite").
#' @param ... Postgres connection args (dbname, host, port, user, password)
#'   passed through when backend = "postgres".
#' @return A pool object. Share this across the app; close with pool::poolClose().
db_connect <- function(backend = c("sqlite", "postgres"),
                       sqlite_path = "survey_platform.sqlite",
                       ...) {
  backend <- match.arg(backend)

  if (backend == "sqlite") {
    pool::dbPool(
      drv     = RSQLite::SQLite(),
      dbname  = sqlite_path,
      # SQLite: enforce foreign keys (off by default) on every checkout
      onCreate = function(con) {
        DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
      }
    )
  } else {
    # Same DAL, different driver. Nothing downstream changes.
    pool::dbPool(
      drv = RPostgres::Postgres(),
      ...
    )
  }
}

#' Close the pool. Call in app's onStop(function() db_disconnect(pool)).
db_disconnect <- function(pool) {
  if (!is.null(pool)) pool::poolClose(pool)
  invisible(NULL)
}

# =============================================================================
# SCHEMA INITIALISATION (idempotent)
# =============================================================================

#' Create tables if they don't already exist. Safe to call on every startup.
#'
#' Types are written to be valid in BOTH SQLite and Postgres:
#'   - TEXT              portable
#'   - INTEGER           portable
#'   - timestamps        stored as ISO-8601 TEXT (driver-neutral, sortable)
#'   - JSON fields       stored as TEXT (parse with jsonlite on read)
#'   - BLOB (SQLite) / BYTEA (Postgres) differ -> chosen per backend
db_init <- function(pool, backend = c("sqlite", "postgres")) {
  backend  <- match.arg(backend)
  blob_type <- if (backend == "sqlite") "BLOB" else "BYTEA"
  pk        <- if (backend == "sqlite") {
    "INTEGER PRIMARY KEY AUTOINCREMENT"
  } else {
    "SERIAL PRIMARY KEY"
  }

  statements <- c(
    sprintf("
      CREATE TABLE IF NOT EXISTS users (
        id            %s,
        username      TEXT NOT NULL UNIQUE,
        email         TEXT UNIQUE,
        password_hash TEXT NOT NULL,
        full_name     TEXT,
        country       TEXT,
        institution   TEXT,
        organization  TEXT,
        department    TEXT,
        position      TEXT,
        role          TEXT NOT NULL DEFAULT 'user',   -- 'user' | 'admin'
        is_active     INTEGER NOT NULL DEFAULT 1,
        created_at    TEXT NOT NULL,
        last_login    TEXT
      );", pk),

    sprintf("
      CREATE TABLE IF NOT EXISTS user_sessions (
        id          %s,
        user_id     INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        token       TEXT NOT NULL UNIQUE,
        created_at  TEXT NOT NULL,
        expires_at  TEXT,
        remember    INTEGER NOT NULL DEFAULT 0
      );", pk),

    sprintf("
      CREATE TABLE IF NOT EXISTS projects (
        id           %s,
        owner_id     INTEGER REFERENCES users(id) ON DELETE CASCADE,
        name         TEXT NOT NULL,
        description  TEXT,
        settings     TEXT,               -- JSON
        is_archived  INTEGER NOT NULL DEFAULT 0,
        is_favourite INTEGER NOT NULL DEFAULT 0,
        created_at   TEXT NOT NULL,
        modified_at  TEXT NOT NULL
      );", pk),

    sprintf("
      CREATE TABLE IF NOT EXISTS datasets (
        id             %s,
        project_id     INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
        name           TEXT NOT NULL,
        kind           TEXT NOT NULL,     -- 'raw' | 'processed'
        data           %s   NOT NULL,     -- serialised data.frame
        variable_types TEXT,             -- JSON
        n_rows         INTEGER,
        n_cols         INTEGER,
        created_at     TEXT NOT NULL
      );", pk, blob_type),

    sprintf("
      CREATE TABLE IF NOT EXISTS analysis_history (
        id           %s,
        project_id   INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
        ts           TEXT NOT NULL,       -- ISO-8601
        module       TEXT,
        action_type  TEXT,
        description  TEXT,
        r_code       TEXT,
        reversible   INTEGER DEFAULT 0,   -- 0/1 boolean
        metadata     TEXT                 -- JSON
      );", pk),

    sprintf("
      CREATE TABLE IF NOT EXISTS models (
        id          %s,
        project_id  INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
        name        TEXT,
        algorithm   TEXT NOT NULL,
        model_blob  %s NOT NULL,          -- serialised fitted model
        metrics     TEXT,                 -- JSON
        predictors  TEXT,                 -- JSON array
        outcome     TEXT,
        created_at  TEXT NOT NULL
      );", pk, blob_type)
  )

  # Helpful indexes for the common access pattern (everything by project)
  indexes <- c(
    "CREATE INDEX IF NOT EXISTS idx_datasets_project ON datasets(project_id);",
    "CREATE INDEX IF NOT EXISTS idx_history_project  ON analysis_history(project_id);",
    "CREATE INDEX IF NOT EXISTS idx_models_project   ON models(project_id);",
    "CREATE INDEX IF NOT EXISTS idx_projects_owner   ON projects(owner_id);",
    "CREATE INDEX IF NOT EXISTS idx_sessions_user    ON user_sessions(user_id);",
    "CREATE INDEX IF NOT EXISTS idx_users_username   ON users(username);"
  )

  for (s in c(statements, indexes)) {
    DBI::dbExecute(pool, s)
  }

  # ---- Non-destructive column migration --------------------------------------
  # CREATE TABLE IF NOT EXISTS never alters an existing table, so a database
  # created before this version keeps its old `projects` shape. Add any missing
  # columns in place; existing rows get the DEFAULT. This is why we can extend
  # the schema without a rewrite or data loss.
  .add_column_if_missing(pool, "projects", "owner_id",     "INTEGER")
  .add_column_if_missing(pool, "projects", "is_archived",  "INTEGER NOT NULL DEFAULT 0")
  .add_column_if_missing(pool, "projects", "is_favourite", "INTEGER NOT NULL DEFAULT 0")

  # ---- Seed the default single-user account ----------------------------------
  # foreign_keys is ON (see db_connect), so a project with owner_id = 1 requires
  # a users row with id = 1 to exist. Version 1.0 is single-user, and even after
  # auth lands the app needs a valid owner before anyone logs in (session
  # restore, auto-create-on-import). Seed a placeholder local account with a
  # non-loginable password hash; AuthService (Phase 2) will let the real user
  # claim/replace it. Only runs when the users table is empty.
  seeded <- tryCatch(db_user_count(pool) == 0, error = function(e) FALSE)
  if (isTRUE(seeded)) {
    tryCatch(
      db_create_user(pool,
                     username      = "local",
                     password_hash = "!",          # not a valid hash -> no login
                     email         = NULL,
                     full_name     = "Local User",
                     role          = "admin"),
      error = function(e) NULL)
  }

  invisible(TRUE)
}

#' Add a column to a table only if it does not already exist. SQLite-focused
#' (uses PRAGMA table_info); on Postgres the ADD COLUMN IF NOT EXISTS is native
#' so we try that form there.
.add_column_if_missing <- function(pool, table, column, type) {
  cols <- tryCatch(
    DBI::dbGetQuery(pool, sprintf("PRAGMA table_info(%s);", table))$name,
    error = function(e) character(0))
  if (length(cols) && column %in% cols) return(invisible(FALSE))
  ok <- tryCatch({
    DBI::dbExecute(pool, sprintf("ALTER TABLE %s ADD COLUMN %s %s;",
                                 table, column, type))
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}

# =============================================================================
# SERIALISATION HELPERS
# =============================================================================

# R object <-> raw BLOB. serialize(NULL connection) returns a raw vector;
# blob::blob() wraps it as a length-1 blob column value that DBI binds cleanly
# across both RSQLite (BLOB) and RPostgres (BYTEA).
.r_to_blob <- function(obj) blob::blob(serialize(obj, connection = NULL))

.blob_to_r <- function(x) {
  # DBI returns a blob (list of raw). Grab the single element and unserialise.
  raw_vec <- if (inherits(x, "blob")) x[[1]] else x
  unserialize(raw_vec)
}

.now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")

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
# pooled connections, making last_insert_rowid() unreliable. Check out one
# connection, run both statements on it, return it.
.insert_returning_id <- function(pool, sql, params) {
  params <- .na_params(params)
  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbExecute(con, sql, params = params)
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id;")$id
}

#' Coerce NULL / zero-length params to NA. DBI requires every bound parameter
#' to be length 1; a NULL (length 0) optional field like email = NULL raises a
#' bind error. Mapping NULL -> NA lets the column store SQL NULL as intended.
.na_params <- function(params) {
  lapply(params, function(x) if (is.null(x) || length(x) == 0L) NA else x)
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

db_create_user <- function(pool, username, password_hash, email = NULL,
                           full_name = NULL, country = NULL, institution = NULL,
                           organization = NULL, department = NULL,
                           position = NULL, role = "user") {
  .insert_returning_id(
    pool,
    "INSERT INTO users
       (username, email, password_hash, full_name, country, institution,
        organization, department, position, role, is_active, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?);",
    params = list(username, email, password_hash, full_name, country,
                  institution, organization, department, position, role, .now())
  )
}

#' Fetch a user by username OR email (login accepts either). Returns a one-row
#' data.frame including password_hash, or NULL.
db_get_user <- function(pool, login) {
  row <- DBI::dbGetQuery(
    pool,
    "SELECT * FROM users WHERE username = ? OR email = ? LIMIT 1;",
    params = list(login, login))
  if (nrow(row) == 0) NULL else row
}

db_get_user_by_id <- function(pool, user_id) {
  row <- DBI::dbGetQuery(pool, "SELECT * FROM users WHERE id = ?;",
                         params = list(user_id))
  if (nrow(row) == 0) NULL else row
}

db_username_exists <- function(pool, username) {
  n <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM users WHERE username = ?;",
                       params = list(username))$n
  n > 0
}

db_email_exists <- function(pool, email) {
  if (is.null(email) || !nzchar(email)) return(FALSE)
  n <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM users WHERE email = ?;",
                       params = list(email))$n
  n > 0
}

db_update_user_password <- function(pool, user_id, password_hash) {
  DBI::dbExecute(pool, "UPDATE users SET password_hash = ? WHERE id = ?;",
                 params = list(password_hash, user_id))
  invisible(TRUE)
}

db_update_user_profile <- function(pool, user_id, fields = list()) {
  allowed <- c("full_name", "email", "country", "institution",
               "organization", "department", "position")
  fields  <- fields[intersect(names(fields), allowed)]
  if (!length(fields)) return(invisible(FALSE))
  sets   <- paste(sprintf("%s = ?", names(fields)), collapse = ", ")
  params <- c(unname(fields), list(user_id))
  DBI::dbExecute(pool, sprintf("UPDATE users SET %s WHERE id = ?;", sets),
                 params = .na_params(params))
  invisible(TRUE)
}

db_touch_last_login <- function(pool, user_id) {
  DBI::dbExecute(pool, "UPDATE users SET last_login = ? WHERE id = ?;",
                 params = list(.now(), user_id))
  invisible(TRUE)
}

#' Count users — used to decide whether to seed the first admin account.
db_user_count <- function(pool) {
  DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM users;")$n
}

# =============================================================================
# PROJECTS
# =============================================================================

db_create_project <- function(pool, name, description = "", settings = NULL,
                              owner_id = NULL) {
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
  sql <- paste0(
    "SELECT id, owner_id, name, description,
            COALESCE(is_archived, 0)  AS is_archived,
            COALESCE(is_favourite, 0) AS is_favourite,
            created_at, modified_at
       FROM projects",
    if (length(where)) paste0(" WHERE ", paste(where, collapse = " AND ")) else "",
    " ORDER BY modified_at DESC;")
  DBI::dbGetQuery(pool, sql, params = if (length(params)) params else NULL)
}

#' Flip archive / favourite flags.
db_set_project_flag <- function(pool, project_id, flag = c("is_archived", "is_favourite"),
                                value = TRUE) {
  flag <- match.arg(flag)
  DBI::dbExecute(
    pool,
    sprintf("UPDATE projects SET %s = ?, modified_at = ? WHERE id = ?;", flag),
    params = list(as.integer(isTRUE(value)), .now(), project_id))
  invisible(TRUE)
}

#' Fetch a single project's metadata (settings parsed from JSON).
db_get_project <- function(pool, project_id) {
  row <- DBI::dbGetQuery(
    pool,
    "SELECT * FROM projects WHERE id = ?;",
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
    sprintf("UPDATE projects SET %s WHERE id = ?;", paste(sets, collapse = ", ")),
    params = .na_params(params)
  )
  invisible(TRUE)
}

#' Delete a project. ON DELETE CASCADE removes its datasets/history/models.
db_delete_project <- function(pool, project_id) {
  DBI::dbExecute(pool, "DELETE FROM projects WHERE id = ?;",
                 params = list(project_id))
  invisible(TRUE)
}

# =============================================================================
# DATASETS
# =============================================================================

#' Persist a data frame under a project. Returns dataset id.
db_save_dataset <- function(pool, project_id, df, name,
                            kind = c("raw", "processed"),
                            variable_types = NULL) {
  kind <- match.arg(kind)
  .insert_returning_id(
    pool,
    "INSERT INTO datasets
       (project_id, name, kind, data, variable_types, n_rows, n_cols, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?);",
    params = list(
      project_id, name, kind,
      .r_to_blob(df),
      .to_json(variable_types),
      nrow(df), ncol(df), .now()
    )
  )
}

#' Load the most recent dataset of a given kind for a project (or by id).
db_get_dataset <- function(pool, project_id = NULL, kind = "processed",
                           dataset_id = NULL) {
  if (!is.null(dataset_id)) {
    row <- DBI::dbGetQuery(pool, "SELECT * FROM datasets WHERE id = ?;",
                           params = list(dataset_id))
  } else {
    row <- DBI::dbGetQuery(
      pool,
      "SELECT * FROM datasets
        WHERE project_id = ? AND kind = ?
        ORDER BY created_at DESC LIMIT 1;",
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
db_list_datasets <- function(pool, project_id) {
  DBI::dbGetQuery(
    pool,
    "SELECT id, name, kind, n_rows, n_cols, created_at
       FROM datasets WHERE project_id = ?
      ORDER BY created_at DESC;",
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
                          metadata = NULL) {
  .insert_returning_id(
    pool,
    "INSERT INTO analysis_history
       (project_id, ts, module, action_type, description, r_code, reversible, metadata)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?);",
    params = list(
      project_id, .now(), module, action_type, description,
      r_code, as.integer(isTRUE(reversible)), .to_json(metadata)
    )
  )
}

#' Full history for a project, chronological.
db_get_history <- function(pool, project_id, limit = NULL) {
  sql <- "SELECT id, ts, module, action_type, description, reversible
            FROM analysis_history WHERE project_id = ? ORDER BY ts ASC"
  if (!is.null(limit)) sql <- paste0(sql, sprintf(" LIMIT %d", as.integer(limit)))
  DBI::dbGetQuery(pool, paste0(sql, ";"), params = list(project_id))
}

#' Concatenate all stored r_code for a project -> reproducible script.
db_get_history_code <- function(pool, project_id) {
  rows <- DBI::dbGetQuery(
    pool,
    "SELECT description, r_code FROM analysis_history
      WHERE project_id = ? AND r_code IS NOT NULL AND r_code <> ''
      ORDER BY ts ASC;",
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
  row <- DBI::dbGetQuery(pool, "SELECT * FROM models WHERE id = ?;",
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
    "SELECT id, name, algorithm, outcome, created_at
       FROM models WHERE project_id = ?
      ORDER BY created_at DESC;",
    params = list(project_id)
  )
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

message("[SAP] Database Backend v2.3.2-20260708 loaded")  # SAP_MODULE_BANNER
