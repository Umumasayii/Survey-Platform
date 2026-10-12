# Backwards-compatible entry point for the previous Windows package installer.
# Core packages are installed once; heavyweight optional features are opt-in.
installer <- file.path(getwd(), "INSTALL_DEPENDENCIES.R")
if (!file.exists(installer)) {
  stop("Run this script from the extracted Survey Analytics application folder.", call. = FALSE)
}
source(installer, local = FALSE)
rm(installer)
