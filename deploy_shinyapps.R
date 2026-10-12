# Deploy the cleaned application to shinyapps.io from a fresh R session.
# Usage: source("deploy_shinyapps.R")
# Optional: Sys.setenv(SAP_SHINY_ACCOUNT = "account", SAP_SHINY_APP_ID = "id")

options(repos = c(CRAN = "https://cloud.r-project.org"))
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
required_root <- c("app.R", "auth.R", "14_auth.R", "package_manifest.R", "CHECK_APP.R")
missing_root <- required_root[!file.exists(file.path(project_root, required_root))]
if (length(missing_root)) {
  stop("Run this script from the extracted application folder. Missing: ",
       paste(missing_root, collapse = ", "), call. = FALSE)
}

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
source("INSTALL_DEPENDENCIES.R", local = FALSE)
source("CHECK_APP.R", local = FALSE)

postgres_bundle <- tolower(trimws(Sys.getenv("SAP_DEPLOY_POSTGRESQL", "false"))) %in%
  c("true", "1", "yes") || all(nzchar(trimws(Sys.getenv(
    c("SAP_PG_HOST", "SAP_PG_DB", "SAP_PG_USER", "SAP_PG_PASSWORD"), unset = ""
  )))) || nzchar(trimws(Sys.getenv("DATABASE_URL", "")))

module_files <- list.files(project_root, pattern = "^[0-9]{2}[a-z]?_.*\\.R$", full.names = FALSE)
static_files <- c(
  "app.R", "auth.R", "package_manifest.R", "dependencies.R", "sap_environment.R",
  "release_metadata.R", "global_no_gtsummary.R", "project_setup.R",
  "frontend_components.R", "VERSION", "DESCRIPTION", "LICENSE"
)
if (file.exists("renv.lock")) static_files <- c(static_files, "renv.lock")
asset_files <- c(
  list.files("www", recursive = TRUE, full.names = TRUE),
  list.files("data", recursive = TRUE, full.names = TRUE)
)
postgres_files <- if (postgres_bundle) c(
  "db_backend.R",
  list.files("db", recursive = TRUE, full.names = TRUE)
) else character()

application_files <- unique(c(static_files, module_files, asset_files, postgres_files))
missing_files <- application_files[!file.exists(application_files)]
if (length(missing_files)) {
  stop("The deployment bundle is incomplete: ", paste(missing_files, collapse = ", "),
       call. = FALSE)
}
private_pattern <- "(^|/)(\\.env|\\.Renviron|\\.Rhistory|\\.RData|users\\.rds|\\.sap-private)(/|$)"
if (any(grepl(private_pattern, application_files, perl = TRUE))) {
  stop("A private configuration or account file was included in the deployment bundle.", call. = FALSE)
}

account <- trimws(Sys.getenv("SAP_SHINY_ACCOUNT", ""))
application_id <- trimws(Sys.getenv("SAP_SHINY_APP_ID", ""))
if (!nzchar(account)) {
  stop("Set SAP_SHINY_ACCOUNT to your shinyapps.io account name before deployment.", call. = FALSE)
}
deploy_arguments <- list(
  appDir = project_root,
  appFiles = application_files,
  account = account,
  server = "shinyapps.io",
  launch.browser = TRUE
)
if (nzchar(application_id)) deploy_arguments$appId <- application_id

supported_arguments <- names(formals(rsconnect::deployApp))
if ("forceUpdate" %in% supported_arguments) deploy_arguments$forceUpdate <- TRUE

message(sprintf(
  "[SAP] Deploying %d application files to shinyapps.io (%s account mode).",
  length(application_files), if (postgres_bundle) "PostgreSQL" else "standalone"
))
message("[SAP] Windows package libraries, private environment files, histories and account databases are excluded.")
do.call(rsconnect::deployApp, deploy_arguments)
