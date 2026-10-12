# Survey Analytics Platform v6.1.0 — full PostgreSQL release gate

.sap_release_script <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NA_character_
)

.sap_run_release_gate <- function(script_file = .sap_release_script) {
  project_root <- if (!is.na(script_file)) {
    dirname(script_file)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
  if (!file.exists(file.path(project_root, "app.R"))) {
    stop("Could not locate app.R beside preflight_release.R. Project root: ",
         project_root, call. = FALSE)
  }

  previous_wd <- setwd(project_root)
  on.exit(setwd(previous_wd), add = TRUE)

  source("sap_environment.R", local = FALSE)
  sap_load_project_env(project_root, quiet = FALSE)

  source("preflight_frontend.R", local = FALSE)
  source("preflight_unit.R", local = FALSE)

  # Backend comes first so schema, role and transaction checks are complete
  # before authentication and full persistence exercise the tables.
  source("preflight_backend.R", local = FALSE)
  source("preflight_auth.R", local = FALSE)
  source("preflight_admin.R", local = FALSE)
  source("preflight_persistence.R", local = FALSE)

  cat("\n✓ Full Survey Analytics Platform v6.1.0 release gate completed.\n")
  cat("Next: source(\"preflight_ollama.R\") and source(\"startup_backend.R\")\n")
  invisible(TRUE)
}

.sap_run_release_gate()
rm(.sap_run_release_gate, .sap_release_script)
