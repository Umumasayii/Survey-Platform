# =============================================================================
# preflight_persistence.R  —  Phase 1 round-trip test. Run from the project
# folder:  source("preflight_persistence.R")
#
# Proves the persistence spine end to end WITHOUT starting Shiny:
#   1. build a pool on a throwaway SQLite file
#   2. db_init (schema + migration)
#   3. create a project, mark it current
#   4. DatasetService$set() a data.frame -> should persist to the DAL
#   5. simulate "reopen": fresh app_state, ProjectService$open()
#   6. assert the dataset came back byte-identical
# =============================================================================

suppressMessages({
  library(DBI); library(RSQLite); library(pool); library(R6)
  library(blob); library(jsonlite)
})

cat("\n--- Phase 1 persistence round-trip ---\n\n")

# Minimal shiny::reactiveVal / isolate shims so the R6 services load headless.
if (!requireNamespace("shiny", quietly = TRUE)) {
  shiny <- local({
    reactiveVal <- function(x) { v <- x; function(new) if (missing(new)) v else { v <<- new; invisible(v) } }
    isolate <- function(x) x
    list(reactiveVal = reactiveVal, isolate = isolate)
  })
}
`%||%` <- function(x, y) if (is.null(x)) y else x

source("db_backend.R")
source("00_services.R")

tmp <- tempfile(fileext = ".sqlite")
p   <- db_connect(backend = "sqlite", sqlite_path = tmp)
db_init(p, backend = "sqlite")
cat("1. schema built\n")

# users table + migration present?
tbls <- DBI::dbListTables(p)
stopifnot("users" %in% tbls, "projects" %in% tbls, "datasets" %in% tbls)
cols <- DBI::dbGetQuery(p, "PRAGMA table_info(projects);")$name
stopifnot(all(c("owner_id", "is_archived", "is_favourite") %in% cols))
cat("2. users table + owner_id migration OK\n")

# db_init now seeds a default 'local' user (id 1) so owner_id = 1 is a valid FK.
n_seed <- db_user_count(p)
stopifnot(n_seed == 1)
cat("2b. default user seeded (count =", n_seed, ")\n")

# Fake app_state as a plain environment (reactiveValues not needed headless).
app_state <- new.env()
app_state$current_project <- NULL
app_state$current_user    <- 1L
app_state$data_imported   <- FALSE
app_state$processed_data  <- NULL
app_state$raw_data        <- NULL
app_state$variable_types  <- NULL

services <- create_services(app_state, pool = p, settings_path = tempfile())
cat("3. services created (project svc present: ",
    !is.null(services$project), ")\n", sep = "")

# Create a project, then persist a dataset through the service.
pid <- services$project$create("Round-trip test", persist_current = FALSE)
stopifnot(!is.null(pid), identical(app_state$current_project, pid))
cat("4. project created, id =", pid, "\n")

original <- data.frame(
  id    = 1:200,
  group = rep(c("a", "b"), 100),
  value = rnorm(200),
  stringsAsFactors = FALSE
)
services$data$set(original, kind = "processed", module = "test",
                  description = "seed", variable_types = list(value = "numeric"))
cat("5. dataset set + persisted\n")

# Simulate reopening: brand-new app_state, brand-new services, same pool.
app_state2 <- new.env()
app_state2$current_project <- NULL
app_state2$current_user    <- 1L
app_state2$data_imported   <- FALSE
services2 <- create_services(app_state2, pool = p, settings_path = tempfile())

recent <- services2$project$list()
stopifnot(nrow(recent) >= 1)
services2$project$open(recent$id[1])
cat("6. reopened project", recent$id[1], "\n")

restored <- services2$data$get("processed")
stopifnot(is.data.frame(restored))
stopifnot(identical(dim(restored), dim(original)))
stopifnot(isTRUE(all.equal(restored, original)))
cat("7. dataset restored byte-identical: PASS\n")

# users DAL smoke test (a 'local' user already exists from the seed -> count 2)
uid <- db_create_user(p, "tester", password_hash = "x", email = "t@e.com")
stopifnot(db_username_exists(p, "tester"), db_email_exists(p, "t@e.com"),
          !is.null(db_get_user(p, "tester")), db_user_count(p) == 2)
cat("8. users DAL OK (created id", uid, ", total users", db_user_count(p), ")\n")

db_disconnect(p)
unlink(tmp)
cat("\n--- ALL PHASE 1 CHECKS PASSED ---\n\n")
