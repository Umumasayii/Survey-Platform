# =============================================================================
# SURVEY ANALYTICS PLATFORM — LOCAL STARTUP
# =============================================================================
# Open this file in RStudio and click Source, or run:
#   source("startup.R")
#
# Local startup keeps sign-in and signup available. PostgreSQL is used when
# configured; otherwise secure standalone app-instance accounts are enabled.
# =============================================================================

if (!nzchar(Sys.getenv("SAP_RUNTIME_PROFILE"))) {
  Sys.setenv(SAP_RUNTIME_PROFILE = "standalone")
}

installer <- file.path(getwd(), "INSTALL_SURVEY_ANALYTICS_PACKAGES_FIXED.R")
if (file.exists(installer)) {
  source(installer, local = FALSE)
}

startup_version <- tryCatch(trimws(readLines(file.path(getwd(), "VERSION"), warn = FALSE)[[1]]), error = function(e) "unknown")
message("[SAP] Starting Survey Analytics Platform — Data Cleaning Studio ", startup_version, ".")

# Generic cloud hosts expose the listening port through PORT and require the
# process to bind to 0.0.0.0. Local/RStudio launches keep Shiny's normal port
# selection and may open a browser.
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

message("[SAP] Local Startup ", startup_version, " completed")  # SAP_MODULE_BANNER
