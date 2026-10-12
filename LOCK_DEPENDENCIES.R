# Record exact installed R package versions after a successful preflight.
source("package_manifest.R", local = FALSE)
packages <- unique(c(SAP_CORE_PACKAGES, SAP_RECOMMENDED_PACKAGES, SAP_POSTGRESQL_PACKAGES, SAP_OPTIONAL_PACKAGES))
installed <- utils::installed.packages()
keep <- intersect(packages, rownames(installed))
lock <- data.frame(package=keep, version=installed[keep,"Version"], stringsAsFactors=FALSE)
utils::write.csv(lock, "PACKAGE_VERSIONS.csv", row.names=FALSE)
message("Recorded ", nrow(lock), " installed package versions in PACKAGE_VERSIONS.csv")
