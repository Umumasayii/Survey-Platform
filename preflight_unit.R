# Survey Analytics Platform v6.1.0 — automated unit/static release checks
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required. Run INSTALL_DEPENDENCIES.R first.", call. = FALSE)
}
files <- list.files(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
files <- files[!grepl("(^|/)renv/", files)]
for (f in files) parse(file = f)
message(sprintf("[SAP TEST] Parsed %d R files successfully.", length(files)))
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
message("[SAP TEST] v6.1.0 professional MEL unit/static checks passed.")
