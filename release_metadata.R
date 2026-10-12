# Central release metadata. Keep VERSION as the single semantic-version source.
.sap_version_file <- file.path(getwd(), "VERSION")
if (!file.exists(.sap_version_file)) stop("VERSION file is missing.", call. = FALSE)
SAP_VERSION <- trimws(readLines(.sap_version_file, warn = FALSE, encoding = "UTF-8")[[1]])
if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", SAP_VERSION)) stop("VERSION is not a semantic version.", call. = FALSE)
SAP_RELEASE_NAME <- "Multilingual Professional Monitoring & Evaluation Edition"
SAP_RELEASE_DATE <- "2026-09-12"
SAP_RELEASE_SLUG <- "multilingual-professional-monitoring-evaluation"
SAP_RELEASE_ID <- paste0(SAP_VERSION, "-", SAP_RELEASE_SLUG, "-20260912")
SAP_PRESENTATION_VERSION <- paste(SAP_VERSION, SAP_RELEASE_NAME)
rm(.sap_version_file)
