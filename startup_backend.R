# =============================================================================
# Survey Analytics Platform — PostgreSQL backend startup
# =============================================================================
# 1. Configure .Renviron from .Renviron.example and restart R.
# 2. Run the pgAdmin role bootstrap script once.
# 3. Source this file.

source("sap_environment.R", local = FALSE)
sap_load_project_env(getwd(), quiet = FALSE)

Sys.setenv(SAP_DEMO_MODE = "false", SAP_DB_REQUIRED = "true", SAP_AUTH_REQUIRED = "true")
source("preflight_backend.R")

.sap_port_text <- trimws(Sys.getenv("PORT", ""))
.sap_port <- suppressWarnings(as.integer(.sap_port_text))
.sap_hosted_port <- nzchar(.sap_port_text) && !is.na(.sap_port) && .sap_port > 0L
if (.sap_hosted_port) {
  shiny::runApp(
    appDir = getwd(),
    host = Sys.getenv("HOST", "0.0.0.0"),
    port = .sap_port,
    launch.browser = FALSE
  )
} else {
  shiny::runApp(appDir = getwd(), launch.browser = interactive())
}
