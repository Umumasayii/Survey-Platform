# =============================================================================
# Survey Analytics Platform — generic cloud deployment entry point
# =============================================================================
# Use this command on hosts that ask for a start command:
#   Rscript start_deployment.R
#
# The app itself chooses Guest/Demo mode only when no explicit runtime flags and
# no complete PostgreSQL configuration are present. Persistent deployments should
# configure SAP_DEMO_MODE=false plus the SAP_PG_* environment variables.
# =============================================================================

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The 'shiny' package is required. Run source('INSTALL_DEPENDENCIES.R') first.",
       call. = FALSE)
}

.sap_port <- suppressWarnings(as.integer(Sys.getenv("PORT", "3838")))
if (is.na(.sap_port) || .sap_port < 1L || .sap_port > 65535L) .sap_port <- 3838L
.sap_host <- Sys.getenv("HOST", "0.0.0.0")
if (!nzchar(trimws(.sap_host))) .sap_host <- "0.0.0.0"

message(sprintf("[SAP] Deployment listener: http://%s:%d", .sap_host, .sap_port))
shiny::runApp(
  appDir = getwd(),
  host = .sap_host,
  port = .sap_port,
  launch.browser = FALSE
)
