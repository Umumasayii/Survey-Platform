# Central package manifest for the Survey Analytics Platform.
# Keep startup requirements separate from optional, resource-intensive features.

SAP_CORE_PACKAGES <- unique(c(
  "shiny", "bslib", "shinyWidgets", "shinyjs", "shinyFiles",
  "shinycssloaders", "bsicons", "fontawesome", "DT", "reactable",
  "plotly", "gt", "dplyr", "tidyr", "readr", "readxl", "haven",
  "janitor", "stringr", "stringi", "forcats", "lubridate", "rlang",
  "R6", "jsonlite", "openxlsx", "ggplot2", "broom", "officer",
  "rmarkdown", "sodium", "digest", "htmltools"
))

SAP_RECOMMENDED_PACKAGES <- unique(c(
  "survey", "survival", "MASS", "nnet", "rpart", "class", "e1071",
  "naivebayes", "randomForest", "sandwich", "lmtest", "gridExtra",
  "knitr", "httr", "curl", "base64enc", "testthat"
))

SAP_POSTGRESQL_PACKAGES <- c("DBI", "pool", "blob", "RPostgres", "openssl")

SAP_OPTIONAL_PACKAGES <- c(
  "arrow", "xgboost", "lme4", "geepack", "car", "ggcorrplot",
  "writexl", "tinytex", "plumber", "sf", "terra", "leaflet"
)

sap_optional_available <- function(package) {
  isTRUE(requireNamespace(as.character(package)[[1]], quietly = TRUE))
}

sap_optional_call <- function(package, function_name, ..., feature = NULL) {
  package <- as.character(package)[[1]]
  function_name <- as.character(function_name)[[1]]
  if (!sap_optional_available(package)) {
    feature <- if (is.null(feature) || !nzchar(as.character(feature)[[1]])) {
      "This feature"
    } else {
      as.character(feature)[[1]]
    }
    stop(
      sprintf(
        "%s requires the optional '%s' package. Install it with install.packages('%s'), then restart the application.",
        feature, package, package
      ),
      call. = FALSE
    )
  }
  do.call(getExportedValue(package, function_name), list(...))
}
