# =============================================================================
# 00_services.R — SERVICE LAYER FOUNDATION (v2.2-20260706)
# =============================================================================
# The spine of the enterprise architecture: centralized services that modules
# call instead of manipulating state directly.
#
# Services provided:
#   LoggingService       levels, ring buffer, timing, export
#   SettingsService      defaults + overrides, file persistence
#   NotificationService  UI notifications + durable history
#   CacheService         hash-keyed memoisation with version invalidation
#   DatasetService       THE gateway for reading/writing the active dataset
#
# Design notes:
#   - R6 classes: services are single-instance, mutable, long-lived. R6 is the
#     natural fit and is already a project dependency.
#   - This file is ADDITIVE. Sourcing it changes nothing until app.R calls
#     create_services() and modules opt in. Migration is incremental by design.
#   - No reactive reads at top level; DatasetService methods that touch
#     app_state must be called from reactive contexts, same as today.
#   - Hashing uses rlang::hash() (ships with shiny) -> zero new dependencies
#     beyond R6, which is already required.
#
# Dependencies: R6 (existing), rlang (via shiny), jsonlite (existing)
# =============================================================================

# =============================================================================
# 1. LOGGING SERVICE
# =============================================================================

LoggingService <- R6::R6Class(
  "LoggingService",
  public = list(
    #' @field min_level Minimum level recorded (10=DEBUG 20=INFO 30=WARN 40=ERROR)
    min_level = 20L,

    initialize = function(min_level = 20L, max_entries = 2000L,
                          log_file = NULL) {
      self$min_level        <- as.integer(min_level)
      private$max_entries   <- as.integer(max_entries)
      private$log_file      <- log_file
      private$entries       <- vector("list", 0L)
    },

    log = function(level, msg, module = NA_character_, ...) {
      if (level < self$min_level) return(invisible(NULL))
      extra <- list(...)
      entry <- list(
        ts     = Sys.time(),
        level  = private$level_name(level),
        module = module,
        msg    = msg,
        extra  = if (length(extra)) extra else NULL
      )
      private$entries[[length(private$entries) + 1L]] <- entry
      # ring buffer trim
      n <- length(private$entries)
      if (n > private$max_entries) {
        private$entries <- private$entries[(n - private$max_entries + 1L):n]
      }
      if (!is.null(private$log_file)) {
        line <- sprintf("%s [%s] %s %s",
                        format(entry$ts, "%Y-%m-%d %H:%M:%OS3"),
                        entry$level,
                        ifelse(is.na(module), "-", module),
                        msg)
        try(cat(line, "\n", file = private$log_file, append = TRUE),
            silent = TRUE)
      }
      invisible(entry)
    },

    debug = function(msg, module = NA_character_, ...) self$log(10L, msg, module, ...),
    info  = function(msg, module = NA_character_, ...) self$log(20L, msg, module, ...),
    warn  = function(msg, module = NA_character_, ...) self$log(30L, msg, module, ...),
    error = function(msg, module = NA_character_, ...) self$log(40L, msg, module, ...),

    #' Time an expression; logs elapsed seconds at INFO. Returns expr's value.
    time = function(label, expr, module = NA_character_) {
      t0  <- proc.time()[["elapsed"]]
      val <- force(expr)
      dt  <- proc.time()[["elapsed"]] - t0
      self$info(sprintf("%s completed in %.3fs", label, dt), module,
                elapsed = dt)
      val
    },

    #' Entries as a data.frame (newest last)
    get_entries = function(min_level = 10L) {
      keep <- Filter(function(e) private$level_num(e$level) >= min_level,
                     private$entries)
      if (length(keep) == 0L) {
        return(data.frame(ts = as.POSIXct(character()),
                          level = character(), module = character(),
                          msg = character(), stringsAsFactors = FALSE))
      }
      data.frame(
        ts     = as.POSIXct(vapply(keep, function(e) as.numeric(e$ts),
                                   numeric(1)), origin = "1970-01-01"),
        level  = vapply(keep, function(e) e$level,  character(1)),
        module = vapply(keep, function(e) as.character(e$module), character(1)),
        msg    = vapply(keep, function(e) e$msg,    character(1)),
        stringsAsFactors = FALSE
      )
    },

    export = function(path) {
      df <- self$get_entries()
      utils::write.csv(df, path, row.names = FALSE)
      invisible(path)
    },

    clear = function() { private$entries <- vector("list", 0L); invisible(NULL) }
  ),
  private = list(
    entries     = NULL,
    max_entries = 2000L,
    log_file    = NULL,
    level_name  = function(l) c(`10` = "DEBUG", `20` = "INFO",
                                `30` = "WARN",  `40` = "ERROR")[[as.character(l)]],
    level_num   = function(nm) c(DEBUG = 10L, INFO = 20L,
                                 WARN = 30L, ERROR = 40L)[[nm]]
  )
)

# =============================================================================
# 2. SETTINGS SERVICE
# =============================================================================

SettingsService <- R6::R6Class(
  "SettingsService",
  public = list(
    initialize = function(defaults = list(), path = NULL, logger = NULL) {
      private$defaults  <- defaults
      private$overrides <- list()
      private$path      <- path
      private$logger    <- logger
      if (!is.null(path) && file.exists(path)) self$load()
    },

    #' Override -> default -> fallback, in that order.
    get = function(key, fallback = NULL) {
      if (!is.null(private$overrides[[key]])) return(private$overrides[[key]])
      if (!is.null(private$defaults[[key]]))  return(private$defaults[[key]])
      fallback
    },

    set = function(key, value, persist = TRUE) {
      private$overrides[[key]] <- value
      if (!is.null(private$logger)) {
        private$logger$debug(sprintf("setting '%s' updated", key), "settings")
      }
      if (persist && !is.null(private$path)) self$save()
      invisible(value)
    },

    #' Full effective settings (defaults overlaid with overrides)
    all = function() utils::modifyList(private$defaults, private$overrides),

    reset = function(key = NULL) {
      if (is.null(key)) private$overrides <- list()
      else private$overrides[[key]] <- NULL
      if (!is.null(private$path)) self$save()
      invisible(NULL)
    },

    save = function() {
      if (is.null(private$path)) return(invisible(FALSE))
      ok <- tryCatch({
        saveRDS(private$overrides, private$path)
        TRUE
      }, error = function(e) {
        if (!is.null(private$logger)) {
          private$logger$error(paste("settings save failed:", e$message),
                               "settings")
        }
        FALSE
      })
      invisible(ok)
    },

    load = function() {
      ok <- tryCatch({
        private$overrides <- readRDS(private$path)
        TRUE
      }, error = function(e) FALSE)
      invisible(ok)
    }
  ),
  private = list(
    defaults  = NULL,
    overrides = NULL,
    path      = NULL,
    logger    = NULL
  )
)

# =============================================================================
# 3. NOTIFICATION SERVICE
# =============================================================================

NotificationService <- R6::R6Class(
  "NotificationService",
  public = list(
    initialize = function(logger = NULL, max_history = 500L) {
      private$logger      <- logger
      private$max_history <- as.integer(max_history)
      private$history     <- vector("list", 0L)
    },

    #' Show a UI notification (when a Shiny session is live) and record it.
    #' type: "message" | "warning" | "error" | "default"
    notify = function(msg, type = "message", duration = 5,
                      module = NA_character_) {
      entry <- list(ts = Sys.time(), type = type, module = module, msg = msg)
      private$history[[length(private$history) + 1L]] <- entry
      n <- length(private$history)
      if (n > private$max_history) {
        private$history <- private$history[(n - private$max_history + 1L):n]
      }
      if (!is.null(private$logger)) {
        lvl <- switch(type, error = 40L, warning = 30L, 20L)
        private$logger$log(lvl, msg, module)
      }
      # Only touch the UI if we're inside a live session
      dom <- shiny::getDefaultReactiveDomain()
      if (!is.null(dom)) {
        shiny::showNotification(msg, type = type, duration = duration,
                                session = dom)
      }
      invisible(entry)
    },

    success = function(msg, module = NA_character_)
      self$notify(msg, "message", module = module),
    warning = function(msg, module = NA_character_)
      self$notify(msg, "warning", module = module),
    error   = function(msg, module = NA_character_)
      self$notify(msg, "error",   module = module),

    get_history = function() {
      if (length(private$history) == 0L) {
        return(data.frame(ts = as.POSIXct(character()), type = character(),
                          module = character(), msg = character(),
                          stringsAsFactors = FALSE))
      }
      data.frame(
        ts     = as.POSIXct(vapply(private$history,
                                   function(e) as.numeric(e$ts), numeric(1)),
                            origin = "1970-01-01"),
        type   = vapply(private$history, function(e) e$type, character(1)),
        module = vapply(private$history,
                        function(e) as.character(e$module), character(1)),
        msg    = vapply(private$history, function(e) e$msg, character(1)),
        stringsAsFactors = FALSE
      )
    }
  ),
  private = list(
    logger      = NULL,
    max_history = 500L,
    history     = NULL
  )
)

# =============================================================================
# 4. CACHE SERVICE
# =============================================================================

CacheService <- R6::R6Class(
  "CacheService",
  public = list(
    initialize = function(max_entries = 200L, logger = NULL) {
      private$store       <- new.env(parent = emptyenv())
      private$max_entries <- as.integer(max_entries)
      private$logger      <- logger
    },

    #' Memoise a computation. Key = hash(name + args + version). Bump the
    #' version (e.g. DatasetService$version()) to invalidate everything that
    #' depends on the data.
    cached = function(name, args, version, compute_fn) {
      key <- rlang::hash(list(name = name, args = args, version = version))
      if (!is.null(private$store[[key]])) {
        if (!is.null(private$logger)) {
          private$logger$debug(sprintf("cache HIT: %s", name), "cache")
        }
        return(private$store[[key]]$value)
      }
      val <- compute_fn()
      private$store[[key]] <- list(name = name, value = val,
                                   created = Sys.time())
      private$evict_if_needed()
      if (!is.null(private$logger)) {
        private$logger$debug(sprintf("cache MISS -> stored: %s", name), "cache")
      }
      val
    },

    #' Drop all entries for a given computation name (or everything).
    invalidate = function(name = NULL) {
      keys <- ls(private$store)
      for (k in keys) {
        if (is.null(name) || identical(private$store[[k]]$name, name)) {
          rm(list = k, envir = private$store)
        }
      }
      invisible(NULL)
    },

    size = function() length(ls(private$store))
  ),
  private = list(
    store       = NULL,
    max_entries = 200L,
    logger      = NULL,
    evict_if_needed = function() {
      keys <- ls(private$store)
      if (length(keys) <= private$max_entries) return(invisible(NULL))
      # evict oldest first
      created <- vapply(keys,
                        function(k) as.numeric(private$store[[k]]$created),
                        numeric(1))
      drop_n  <- length(keys) - private$max_entries
      for (k in keys[order(created)][seq_len(drop_n)]) {
        rm(list = k, envir = private$store)
      }
      invisible(NULL)
    }
  )
)

# =============================================================================
# 5. DATASET SERVICE — the single gateway to the active dataset
# =============================================================================
# Wraps app_state so modules stop writing to it directly. Every write:
#   1. updates app_state (reactivity preserved — callers stay reactive)
#   2. bumps an integer data version (drives cache invalidation)
#   3. logs the action
#   4. optionally persists via the DAL (db_backend.R) when a pool is supplied
#
# IMPORTANT: get()/set() read/write reactiveValues, so they must be called
# from reactive contexts (observers, reactives, renderers) — exactly the same
# contract modules already follow when touching app_state directly.

DatasetService <- R6::R6Class(
  "DatasetService",
  public = list(
    initialize = function(app_state, logger = NULL, cache = NULL,
                          pool = NULL, project_id_fn = NULL) {
      private$app_state     <- app_state
      private$logger        <- logger
      private$cache         <- cache
      private$pool          <- pool
      private$project_id_fn <- project_id_fn
      private$data_version  <- 0L
    },

    #' Current data version — use as the `version` arg to CacheService$cached()
    version = function() private$data_version,

    is_loaded = function() {
      isTRUE(private$app_state$data_imported)
    },

    get = function(kind = c("processed", "raw")) {
      kind <- match.arg(kind)
      if (kind == "processed") private$app_state$processed_data
      else                     private$app_state$raw_data
    },

    variable_types = function() {
      private$app_state$variable_types
    },

    #' The one sanctioned write path for the active dataset.
    set = function(df, kind = c("processed", "raw"),
                   module = NA_character_, description = "dataset updated",
                   variable_types = NULL) {
      kind <- match.arg(kind)
      stopifnot(is.data.frame(df))

      if (kind == "processed") private$app_state$processed_data <- df
      else                     private$app_state$raw_data       <- df
      private$app_state$data_imported <- TRUE
      if (!is.null(variable_types)) {
        private$app_state$variable_types <- variable_types
      }

      private$data_version <- private$data_version + 1L
      if (!is.null(private$cache)) private$cache$invalidate()
      if (!is.null(private$logger)) {
        private$logger$info(
          sprintf("%s [%s: %d x %d, v%d]",
                  description, kind, nrow(df), ncol(df), private$data_version),
          module
        )
      }

      # Optional durable persistence through the DAL
      if (!is.null(private$pool) && !is.null(private$project_id_fn)) {
        pid <- private$project_id_fn()
        if (!is.null(pid)) {
          ok <- tryCatch({
            db_save_dataset(private$pool, pid, df,
                            name = kind, kind = kind,
                            variable_types = private$app_state$variable_types)
            TRUE
          }, error = function(e) {
            if (!is.null(private$logger)) private$logger$error(
              sprintf("Dataset persist failed (project %s): %s",
                      pid, conditionMessage(e)), "dataset")
            FALSE
          })
          if (isTRUE(ok) && !is.null(private$logger)) {
            private$logger$debug(
              sprintf("Persisted %s dataset to project %s", kind, pid), "dataset")
          }
        }
      }
      invisible(private$data_version)
    },

    #' Load the most recent dataset of `kind` for a project back into app_state.
    #' This is the "reopen and continue" path — the mirror of set()'s persist.
    #' Returns TRUE if a dataset was restored, FALSE otherwise.
    load = function(project_id, kind = c("processed", "raw")) {
      kind <- match.arg(kind)
      if (is.null(private$pool) || is.null(project_id)) return(FALSE)

      rec <- tryCatch(
        db_get_dataset(private$pool, project_id = project_id, kind = kind),
        error = function(e) NULL)

      # Fall back to raw if no processed dataset was ever saved.
      if (is.null(rec) && kind == "processed") {
        rec <- tryCatch(
          db_get_dataset(private$pool, project_id = project_id, kind = "raw"),
          error = function(e) NULL)
      }
      if (is.null(rec) || is.null(rec$data)) return(FALSE)

      if (identical(rec$kind, "raw")) private$app_state$raw_data       <- rec$data
      else                            private$app_state$processed_data <- rec$data
      private$app_state$data_imported <- TRUE
      if (!is.null(rec$variable_types)) {
        private$app_state$variable_types <- rec$variable_types
      }

      private$data_version <- private$data_version + 1L
      if (!is.null(private$cache)) private$cache$invalidate()
      if (!is.null(private$logger)) {
        private$logger$info(
          sprintf("Restored %s dataset [%d x %d] for project %s",
                  rec$kind, rec$n_rows %||% nrow(rec$data),
                  rec$n_cols %||% ncol(rec$data), project_id),
          "dataset")
      }
      TRUE
    }
  ),
  private = list(
    app_state     = NULL,
    logger        = NULL,
    cache         = NULL,
    pool          = NULL,
    project_id_fn = NULL,
    data_version  = 0L
  )
)

# =============================================================================
# 5a-ii. PROJECT SERVICE — the current-project boundary + CRUD
# -----------------------------------------------------------------------------
# This is the missing link that makes persistence actually fire. Nothing was
# writing app_state$current_project, so DatasetService$set()'s project_id_fn()
# always returned NULL and nothing was ever saved. ProjectService owns that
# value: open()/create() set it, and every DatasetService write then lands
# under the right project. `owner_id_fn` scopes projects to the logged-in user
# (Phase 2); for now it defaults to a single hardcoded user.
# =============================================================================

ProjectService <- R6::R6Class(
  "ProjectService",
  public = list(
    initialize = function(app_state, pool = NULL, dataset_service = NULL,
                          logger = NULL, notifier = NULL,
                          owner_id_fn = function() app_state$current_user %||% 1L) {
      private$app_state <- app_state
      private$pool      <- pool
      private$data_svc  <- dataset_service
      private$logger    <- logger
      private$notifier  <- notifier
      private$owner_fn  <- owner_id_fn
      private$tick      <- shiny::reactiveVal(0L)
    },

    #' Reactive trigger: read in a render/observer to refresh on project change.
    version = function() private$tick(),

    current_id = function() private$app_state$current_project,

    current = function() {
      pid <- self$current_id()
      if (is.null(pid) || is.null(private$pool)) return(NULL)
      tryCatch(db_get_project(private$pool, pid), error = function(e) NULL)
    },

    list = function(include_archived = FALSE) {
      if (is.null(private$pool)) return(data.frame())
      tryCatch(
        db_list_projects(private$pool, owner_id = private$resolve_owner(),
                         include_archived = include_archived),
        error = function(e) data.frame())
    },

    #' Create a project, make it current, and (optionally) persist the active
    #' dataset into it so a brand-new project isn't empty.
    create = function(name, description = "", persist_current = TRUE) {
      if (is.null(private$pool)) return(NULL)
      pid <- tryCatch(
        db_create_project(private$pool, name = name, description = description,
                          owner_id = private$resolve_owner()),
        error = function(e) { private$warn_fail("create", e); NULL })
      if (is.null(pid)) return(NULL)

      private$app_state$current_project <- pid
      private$bump()
      private$log(sprintf("Created project '%s' (id %s)", name, pid))

      # Seed the new project with whatever is currently loaded.
      if (isTRUE(persist_current) && !is.null(private$data_svc) &&
          isTRUE(private$app_state$data_imported)) {
        df <- private$data_svc$get("processed")
        if (is.data.frame(df)) {
          private$data_svc$set(df, kind = "processed", module = "project",
                               description = "seed new project")
        }
      }
      if (!is.null(private$notifier)) private$notifier$success(
        sprintf("Project '%s' created.", name))
      pid
    },

    #' Open an existing project: set it current AND restore its dataset.
    open = function(project_id) {
      if (is.null(private$pool) || is.null(project_id)) return(FALSE)
      proj <- tryCatch(db_get_project(private$pool, project_id),
                       error = function(e) NULL)
      if (is.null(proj)) {
        if (!is.null(private$notifier)) private$notifier$error("Project not found.")
        return(FALSE)
      }
      private$app_state$current_project <- project_id
      private$bump()

      restored <- FALSE
      if (!is.null(private$data_svc)) {
        restored <- private$data_svc$load(project_id, kind = "processed")
      }
      private$log(sprintf("Opened project id %s (dataset restored: %s)",
                          project_id, restored))
      if (!is.null(private$notifier)) private$notifier$success(
        sprintf("Opened '%s'%s", proj$name,
                if (restored) " and restored its dataset." else "."))
      TRUE
    },

    rename = function(project_id, name = NULL, description = NULL) {
      if (is.null(private$pool)) return(FALSE)
      tryCatch({
        db_update_project(private$pool, project_id, name = name,
                          description = description)
        private$bump(); TRUE
      }, error = function(e) { private$warn_fail("update", e); FALSE })
    },

    archive = function(project_id, value = TRUE) {
      if (is.null(private$pool)) return(FALSE)
      tryCatch({
        db_set_project_flag(private$pool, project_id, "is_archived", value)
        private$bump(); TRUE
      }, error = function(e) FALSE)
    },

    favourite = function(project_id, value = TRUE) {
      if (is.null(private$pool)) return(FALSE)
      tryCatch({
        db_set_project_flag(private$pool, project_id, "is_favourite", value)
        private$bump(); TRUE
      }, error = function(e) FALSE)
    },

    delete = function(project_id) {
      if (is.null(private$pool)) return(FALSE)
      tryCatch({
        db_delete_project(private$pool, project_id)
        if (identical(private$app_state$current_project, project_id)) {
          private$app_state$current_project <- NULL
        }
        private$bump(); TRUE
      }, error = function(e) { private$warn_fail("delete", e); FALSE })
    }
  ),
  private = list(
    app_state = NULL, pool = NULL, data_svc = NULL, logger = NULL,
    notifier = NULL, owner_fn = NULL, tick = NULL,
    #' Resolve the owner id, but only if it actually exists as a user. With
    #' foreign_keys ON, inserting a project whose owner_id has no users row is
    #' an FK violation. Returning NULL (a valid, unconstrained owner) is safer
    #' than crashing when the id is stale or the account was never seeded.
    resolve_owner = function() {
      oid <- tryCatch(private$owner_fn(), error = function(e) NULL)
      if (is.null(oid) || is.null(private$pool)) return(NULL)
      exists_user <- tryCatch(
        !is.null(db_get_user_by_id(private$pool, oid)),
        error = function(e) FALSE)
      if (isTRUE(exists_user)) oid else NULL
    },
    bump = function() private$tick(shiny::isolate(private$tick()) + 1L),
    log  = function(msg) if (!is.null(private$logger)) private$logger$info(msg, "project"),
    warn_fail = function(op, e) {
      if (!is.null(private$logger)) private$logger$error(
        sprintf("Project %s failed: %s", op, conditionMessage(e)), "project")
    }
  )
)

# =============================================================================
# 5b. REPORT SERVICE — buffer of analysis blocks for report construction
# =============================================================================
# Every "Add to Report" click pushes a typed block here. Each block carries its
# rendered payload AND the R code that produced it, so the same buffer serves
# both report construction (Feature B) and pipeline code-gen (Feature A).
#
# Block schema: list(id, kind, title, payload, code, ts)
#   kind = "table"    -> payload is a data.frame
#          "verbatim" -> payload is a character vector (printed output)
#          "text"     -> payload is a character string (a note/paragraph)
#          "plot"     -> payload is a PNG file path (or NULL placeholder)

ReportService <- R6::R6Class(
  "ReportService",
  public = list(
    initialize = function(logger = NULL) {
      private$blocks <- list()
      private$seq    <- 0L
      private$logger <- logger
      # Reactive invalidation trigger. Any reactive consumer that calls
      # $version() re-runs when a block is added/removed/reordered/cleared,
      # so the Report Builder in 08 refreshes without a manual button.
      private$tick   <- shiny::reactiveVal(0L)
    },

    #' Reactive dependency: read this inside a render/observer to auto-refresh.
    version = function() private$tick(),

    #' @param payload table -> data.frame; verbatim/text -> character;
    #'   plot -> a ggplot object (rendered to PNG here) OR an existing .png path.
    add_block = function(kind, title, payload, code = "") {
      kind <- match.arg(kind, c("table", "verbatim", "text", "plot"))

      if (identical(kind, "plot")) {
        payload <- private$plot_png(payload)
        if (is.null(payload)) {
          if (!is.null(private$logger)) {
            private$logger$warn(sprintf("Could not render plot '%s' to PNG", title),
                                "report")
          }
          return(invisible(NULL))
        }
      } else if (is.null(payload)) {
        return(invisible(NULL))
      }

      private$seq <- private$seq + 1L
      id <- sprintf("blk_%03d", private$seq)
      private$blocks[[id]] <- list(
        id = id, kind = kind, title = title,
        payload = payload, code = code, ts = Sys.time()
      )
      if (!is.null(private$logger)) {
        private$logger$info(sprintf("Added %s to report: %s", kind, title),
                            "report")
      }
      private$bump()
      invisible(id)
    },

    #' Move a block one slot up (-1) or down (+1) in render order.
    move_block = function(id, dir = -1L) {
      ids <- names(private$blocks)
      i <- match(id, ids)
      if (is.na(i)) return(invisible(NULL))
      j <- i + as.integer(dir)
      if (j < 1L || j > length(ids)) return(invisible(NULL))
      ids[c(i, j)] <- ids[c(j, i)]
      private$blocks <- private$blocks[ids]
      private$bump()
      invisible(NULL)
    },

    get_blocks = function() private$blocks,
    count      = function() length(private$blocks),

    remove_block = function(id) {
      private$blocks[[id]] <- NULL
      private$bump()
      invisible(NULL)
    },

    clear = function() { private$blocks <- list(); private$bump(); invisible(NULL) },

    #' Concatenated reproducible code for all blocks (Feature A payoff).
    get_code = function() {
      if (length(private$blocks) == 0L) return("# No steps recorded yet.")
      parts <- lapply(private$blocks, function(b) {
        if (nzchar(b$code %||% "")) sprintf("# %s\n%s\n", b$title, b$code)
        else NULL
      })
      paste(Filter(Negate(is.null), parts), collapse = "\n")
    },

    #' Render all blocks to an HTML fragment (used by the HTML report and the
    #' in-app preview). Self-contained, no external deps.
    to_html = function() {
      if (length(private$blocks) == 0L) {
        return("<p><em>No content added yet. Use \"Add to Report\" on any result.</em></p>")
      }
      frags <- lapply(private$blocks, function(b) {
        header <- sprintf("<h2>%s</h2>", private$esc(b$title))
        body <- switch(b$kind,
          "table"    = private$df_to_html(b$payload),
          "verbatim" = sprintf("<pre style='background:#f6f8fa;padding:10px;border-radius:6px;'>%s</pre>",
                               private$esc(paste(b$payload, collapse = "\n"))),
          "text"     = sprintf("<p>%s</p>", private$esc(paste(b$payload, collapse = " "))),
          "plot"     = private$plot_html(b),
          sprintf("<p>%s</p>", private$esc(as.character(b$payload)))
        )
        code <- if (nzchar(b$code %||% ""))
          sprintf("<details><summary>R code</summary><pre style='background:#f6f8fa;padding:10px;'>%s</pre></details>",
                  private$esc(b$code)) else ""
        paste0("<section>", header, body, code, "</section>")
      })
      paste(frags, collapse = "\n<hr/>\n")
    },

    #' Simple summary for the in-app builder list.
    summary_df = function() {
      if (length(private$blocks) == 0L) {
        return(data.frame(Order = integer(), Type = character(),
                          Title = character(), stringsAsFactors = FALSE))
      }
      data.frame(
        Order = seq_along(private$blocks),
        Type  = vapply(private$blocks, function(b) b$kind, character(1)),
        Title = vapply(private$blocks, function(b) b$title, character(1)),
        stringsAsFactors = FALSE, row.names = NULL
      )
    }
  ),
  private = list(
    blocks = NULL, seq = 0L, logger = NULL, tick = NULL,

    bump = function() {
      if (is.null(private$tick)) return(invisible(NULL))
      private$tick(shiny::isolate(private$tick()) + 1L)
      invisible(NULL)
    },

    #' Normalize a plot payload to a PNG path on disk.
    #' Accepts: ggplot object | existing .png path | plotly widget built from
    #' ggplotly() (we recover $x$source_ggplot if the caller stashed it).
    #' Returns NULL if it cannot be rendered statically.
    plot_png = function(p) {
      if (is.null(p)) return(NULL)
      if (is.character(p) && length(p) == 1L && file.exists(p)) return(p)
      if (!inherits(p, "ggplot")) return(NULL)
      f <- tempfile(pattern = "sap_blk_", fileext = ".png")
      ok <- tryCatch({
        ggplot2::ggsave(f, plot = p, width = 9, height = 5.5, dpi = 120,
                        bg = "white")
        TRUE
      }, error = function(e) FALSE, warning = function(w) TRUE)
      if (isTRUE(ok) && file.exists(f)) f else NULL
    },

    plot_html = function(b) {
      if (is.null(b$payload) || !file.exists(b$payload)) {
        return(sprintf("<p><em>[plot unavailable: %s]</em></p>", private$esc(b$title)))
      }
      if (!requireNamespace("base64enc", quietly = TRUE)) {
        return(sprintf("<p><em>[plot: %s &mdash; install 'base64enc' to embed]</em></p>",
                       private$esc(b$title)))
      }
      sprintf("<img src='data:image/png;base64,%s' style='max-width:100%%;'/>",
              base64enc::base64encode(b$payload))
    },

    esc = function(x) {
      x <- as.character(x)
      x <- gsub("&", "&amp;", x, fixed = TRUE)
      x <- gsub("<", "&lt;",  x, fixed = TRUE)
      gsub(">", "&gt;",  x, fixed = TRUE)
    },
    df_to_html = function(df) {
      if (!is.data.frame(df) || nrow(df) == 0) return("<p><em>(empty table)</em></p>")
      th <- paste(sprintf("<th>%s</th>", private$esc(names(df))), collapse = "")
      rows <- apply(df, 1, function(r) {
        paste0("<tr>",
               paste(sprintf("<td>%s</td>", private$esc(r)), collapse = ""),
               "</tr>")
      })
      sprintf("<table border='1' cellspacing='0' cellpadding='6' style='border-collapse:collapse;'><thead><tr>%s</tr></thead><tbody>%s</tbody></table>",
              th, paste(rows, collapse = ""))
    }
  )
)

# =============================================================================
# 6. SERVICE REGISTRY
# =============================================================================

#' Build the full service set. Call ONCE in app.R's server function:
#'
#'   services <- create_services(app_state)                # in-memory only
#'   services <- create_services(app_state, pool = app_pool)  # + DAL persistence
#'
#' Then pass `services` to module servers as an optional argument. Modules
#' that don't take it keep working unchanged — migration is incremental.
create_services <- function(app_state,
                            pool = NULL,
                            settings_defaults = if (exists("DEFAULT_SETTINGS"))
                              DEFAULT_SETTINGS else list(),
                            settings_path = "sap_settings.rds",
                            log_file = NULL,
                            project_id_fn = function() {
                              # default: read current project id from app_state
                              app_state$current_project
                            }) {
  log_svc      <- LoggingService$new(log_file = log_file)
  settings_svc <- SettingsService$new(defaults = settings_defaults,
                                      path = settings_path,
                                      logger = log_svc)
  notify_svc   <- NotificationService$new(logger = log_svc)
  cache_svc    <- CacheService$new(logger = log_svc)
  report_svc   <- ReportService$new(logger = log_svc)
  data_svc     <- DatasetService$new(app_state,
                                     logger = log_svc,
                                     cache  = cache_svc,
                                     pool   = pool,
                                     project_id_fn = project_id_fn)
  project_svc  <- ProjectService$new(app_state,
                                     pool = pool,
                                     dataset_service = data_svc,
                                     logger = log_svc,
                                     notifier = notify_svc)

  list(
    log      = log_svc,
    settings = settings_svc,
    notify   = notify_svc,
    cache    = cache_svc,
    report   = report_svc,
    data     = data_svc,
    project  = project_svc
  )
}

# =============================================================================
# ADD-TO-REPORT WIRING HELPERS
# -----------------------------------------------------------------------------
# Turn any module output into a report block with two lines:
#
#   UI:      report_button(ns, "add_dist_plot")
#   server:  register_report_button(input, services, "add_dist_plot",
#              kind    = "plot",
#              title   = function() paste("Distribution of", input$dist_var),
#              payload = pc$get("dist_plot"),
#              code    = function() sprintf("ggplot(df, aes(%s)) + geom_histogram()",
#                                            input$dist_var))
#
# `title`, `payload` and `code` may each be a value or a zero-arg function
# (evaluated at click time, isolated).
# =============================================================================

#' Small green "Add to Report" button.
report_button <- function(ns, id, label = "Add to Report") {
  shiny::actionButton(
    ns(id), label,
    icon  = shiny::icon("circle-plus"),
    class = "btn btn-sm btn-success sap-add-report"
  )
}

#' Bind an actionButton to services$report$add_block().
#' No-ops safely when `services` is NULL (module used standalone / not migrated).
register_report_button <- function(input, services, id, kind, title, payload,
                                   code = "") {
  force(id); force(kind)
  resolve <- function(x) if (is.function(x)) x() else x

  shiny::observeEvent(input[[id]], {
    if (is.null(services) || is.null(services$report)) return(invisible(NULL))

    pl <- tryCatch(shiny::isolate(resolve(payload)), error = function(e) NULL)
    if (is.null(pl)) {
      if (!is.null(services$notify)) {
        services$notify$warning("Nothing to add yet - run/render the result first.")
      }
      return(invisible(NULL))
    }

    ttl <- tryCatch(shiny::isolate(resolve(title)),  error = function(e) "Result")
    cd  <- tryCatch(shiny::isolate(resolve(code)),   error = function(e) "")

    blk <- services$report$add_block(kind = kind, title = ttl, payload = pl,
                                     code = cd %||% "")
    if (!is.null(services$notify)) {
      if (is.null(blk)) services$notify$warning(sprintf("Could not add '%s' to report.", ttl))
      else              services$notify$success(sprintf("Added to report: %s", ttl))
    }
  }, ignoreInit = TRUE)
}

#' Capture store for ggplot objects that get handed to ggplotly().
#'
#' Renders throw the ggplot away; the report button needs it. Wrap the object
#' at the point it is passed to ggplotly:
#'
#'   pc <- plot_capture()
#'   output$dist_plot <- plotly::renderPlotly({
#'     p <- plot_distribution(df_data(), input$dist_var)
#'     plotly::ggplotly(pc$keep("dist_plot", p))
#'   })
#'
#' Guarantees the captured plot IS the rendered plot. Writes happen inside the
#' render (an observer); reads happen isolated inside the button observer, so
#' there is no self-invalidating loop.
plot_capture <- function() {
  store <- shiny::reactiveValues()
  list(
    keep = function(name, p) { store[[name]] <- p; p },
    get  = function(name) function() shiny::isolate(store[[name]]),
    has  = function(name) !is.null(shiny::isolate(store[[name]]))
  )
}

#' Coerce a statistical-result named list into a tidy 2-column data.frame.
#'
#' The test helpers in 05/06 return named lists of scalars (plus the occasional
#' htest object or nested post-hoc matrix), not printable `htest` objects, so
#' `capture.output(print(x))` yields `$statistic \\n [1] 2.31` noise. Rendering
#' them as tables instead means officer's body_add_table() path in 08 works for
#' free.
#'
#' @param res   named list
#' @param drop  names to exclude (nested objects, raw vectors)
#' @param digits rounding for numeric scalars
stat_result_df <- function(res, drop = c("post_hoc", "model", "data", "error"),
                           digits = 4) {
  if (is.null(res) || !is.list(res) || is.null(names(res))) return(NULL)
  res <- res[setdiff(names(res), drop)]
  keep <- vapply(res, function(v) is.atomic(v) && length(v) == 1L, logical(1))
  res <- res[keep]
  if (!length(res)) return(NULL)

  pretty <- function(v) {
    if (is.numeric(v)) {
      if (is.na(v)) return("NA")
      return(format(round(v, digits), nsmall = 0, trim = TRUE))
    }
    if (is.na(v)) return("NA")
    as.character(v)
  }
  label <- function(n) {
    n <- gsub("_", " ", n)
    paste0(toupper(substring(n, 1, 1)), substring(n, 2))
  }
  data.frame(
    Statistic = vapply(names(res), label, character(1), USE.NAMES = FALSE),
    Value     = vapply(res, pretty, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

message("[SAP] Service Layer v2.5-20260708 loaded")  # SAP_MODULE_BANNER
