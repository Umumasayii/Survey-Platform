# Survey Analytics Platform — safe installation and deployment dependencies.
# Run from the extracted project folder: source("INSTALL_DEPENDENCIES.R")
# Optional extras: Sys.setenv(SAP_INSTALL_OPTIONAL = "true")
# PostgreSQL tools: Sys.setenv(SAP_INSTALL_POSTGRESQL = "true")

options(repos = c(CRAN = Sys.getenv("SAP_CRAN_REPOSITORY",
                                     "https://cloud.r-project.org")))
manifest <- file.path(getwd(), "package_manifest.R")
if (!file.exists(manifest)) {
  stop("Run this installer from the extracted application folder containing package_manifest.R.", call. = FALSE)
}
source(manifest, local = FALSE)

flag <- function(name) {
  tolower(trimws(Sys.getenv(name, "false"))) %in% c("true", "1", "yes")
}
postgres_configured <- all(nzchar(trimws(Sys.getenv(
  c("SAP_PG_HOST", "SAP_PG_DB", "SAP_PG_USER", "SAP_PG_PASSWORD"), unset = ""
)))) || nzchar(trimws(Sys.getenv("DATABASE_URL", "")))

packages <- unique(c(
  SAP_CORE_PACKAGES,
  SAP_RECOMMENDED_PACKAGES,
  if (flag("SAP_INSTALL_POSTGRESQL") || postgres_configured) SAP_POSTGRESQL_PACKAGES,
  if (flag("SAP_INSTALL_OPTIONAL")) SAP_OPTIONAL_PACKAGES
))

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing ", length(missing), " Survey Analytics package(s): ", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = c("Depends", "Imports", "LinkingTo"))
} else {
  message("All selected Survey Analytics packages are already installed.")
}

missing_core <- SAP_CORE_PACKAGES[
  !vapply(SAP_CORE_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_core)) {
  stop(
    "Core package installation is incomplete: ", paste(missing_core, collapse = ", "),
    ". Restart R/RStudio and run this installer again.", call. = FALSE
  )
}

missing_recommended <- setdiff(
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)],
  missing_core
)
if (length(missing_recommended)) {
  warning(
    "Some non-essential features are unavailable until these packages are installed: ",
    paste(missing_recommended, collapse = ", "), call. = FALSE
  )
}
message(
  "Core application dependencies are ready. ",
  "Run source('CHECK_APP.R') before deployment. ",
  "After verification, source('LOCK_DEPENDENCIES.R') records the actual installed package versions."
)
