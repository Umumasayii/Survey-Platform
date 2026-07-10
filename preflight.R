# =============================================================================
# preflight.R — RUN THIS BEFORE app.R TO LOCATE ANY STARTUP PROBLEM
# =============================================================================
# Purpose: stop guessing. This sources every file in isolation, reports the
# EXACT file + line + message on failure, verifies you're running the v2.1
# files (not drifted copies), and checks required packages.
#
# Usage (from the project folder, fresh R session):
#   setwd("C:/Users/branc/Desktop/AI In Work/Dashboard Prompts/R-Shiny")
#   source("preflight.R")
#
# It does NOT launch the app. It tells you whether the app CAN launch.
# =============================================================================

cat("\n===========================================================\n")
cat("  SURVEY ANALYTICS PLATFORM — PREFLIGHT DIAGNOSTIC\n")
cat("  Expected version: v2.1-20260705\n")
cat("===========================================================\n\n")

EXPECTED_VERSION <- "v2.1-20260705"

# ---- 0. Working directory --------------------------------------------------
cat("[0] Working directory\n")
cat("    ", getwd(), "\n\n")

# ---- 1. File presence ------------------------------------------------------
required_files <- c(
  "global_no_gtsummary.R",
  "00_services.R",
  "01_import_wizard.R", "02_data_explorer.R", "03_data_cleaning.R",
  "04_dashboard.R", "05_descriptive_stats.R", "06_statistical_tests.R",
  "07_visualization.R", "08_report_generator.R", "09_project_manager.R",
  "10_analysis_history.R", "11_machine_learning.R", "16_code_generator.R",
  "db_backend.R", "app.R"
)

cat("[1] File presence\n")
missing <- required_files[!file.exists(required_files)]
if (length(missing)) {
  cat("    MISSING FILES:\n")
  for (f in missing) cat("      -", f, "\n")
  cat("\n    STOP: extract the v2.1 zip into THIS folder, then re-run.\n")
} else {
  cat("    All", length(required_files), "files present.\n")
}
cat("\n")

# ---- 2. Version banner check (detects drifted / stale files) ---------------
cat("[2] Version fingerprint (detects stale files)\n")
version_files <- setdiff(required_files, c("db_backend.R", "app.R"))
stale <- character(0)
for (f in version_files) {
  if (!file.exists(f)) next
  txt <- readLines(f, warn = FALSE)
  has_ver <- any(grepl(EXPECTED_VERSION, txt, fixed = TRUE))
  if (!has_ver) stale <- c(stale, f)
}
if (length(stale)) {
  cat("    STALE / WRONG-VERSION files (no", EXPECTED_VERSION, "banner):\n")
  for (f in stale) cat("      -", f, "\n")
  cat("\n    STOP: these are old copies. Replace them from the v2.1 zip.\n")
} else {
  cat("    All module files carry the", EXPECTED_VERSION, "banner.\n")
}
cat("\n")

# ---- 3. Required packages --------------------------------------------------
cat("[3] Required packages\n")
required_pkgs <- c(
  # core UI
  "shiny", "bslib", "shinyWidgets", "bsicons", "fontawesome",
  "shinyjs", "shinyFiles", "shinycssloaders",
  # data
  "dplyr", "tidyr", "janitor", "haven", "readxl", "stringr", "forcats",
  "lubridate", "jsonlite",
  # viz / tables
  "ggplot2", "plotly", "DT", "gt", "reactable",
  # stats / models
  "broom",
  # reporting
  "officer", "rmarkdown",
  # services / backend
  "R6", "rlang", "DBI", "pool", "blob", "RSQLite"
)
inst <- rownames(installed.packages())
absent <- required_pkgs[!required_pkgs %in% inst]
if (length(absent)) {
  cat("    MISSING PACKAGES:\n")
  cat("      install.packages(c(",
      paste(sprintf('"%s"', absent), collapse = ", "), "))\n")
} else {
  cat("    All", length(required_pkgs), "required packages installed.\n")
}
# ML packages are optional at startup (only needed when training)
ml_pkgs <- c("rpart", "randomForest", "e1071", "class", "naivebayes", "xgboost")
ml_absent <- ml_pkgs[!ml_pkgs %in% inst]
if (length(ml_absent)) {
  cat("    (optional, ML only): ",
      paste(ml_absent, collapse = ", "), "\n")
}
cat("\n")

# ---- 4. Check whether observe is masked (the original mystery) -------------
cat("[4] Is shiny::observe masked?\n")
if ("shiny" %in% inst) {
  suppressMessages(suppressWarnings(require(shiny, quietly = TRUE)))
  where <- utils::find("observe")
  cat("    observe resolves to:", paste(where, collapse = ", "), "\n")
  if (length(where) && where[1] != "package:shiny") {
    cat("    WARNING: observe is masked by", where[1],
        "- this breaks reactivity!\n")
  } else {
    cat("    OK — resolves to package:shiny.\n")
  }
}
cat("\n")

# ---- 5. Source each file in isolation, report exact failure ----------------
cat("[5] Source each file in isolation (locates exact failure)\n")
source_order <- c(
  "global_no_gtsummary.R",
  "00_services.R",
  "db_backend.R",
  "01_import_wizard.R", "02_data_explorer.R", "03_data_cleaning.R",
  "04_dashboard.R", "05_descriptive_stats.R", "06_statistical_tests.R",
  "07_visualization.R", "08_report_generator.R", "09_project_manager.R",
  "10_analysis_history.R", "11_machine_learning.R", "16_code_generator.R"
)
fail <- FALSE
for (f in source_order) {
  if (!file.exists(f)) { cat("    SKIP (absent):", f, "\n"); next }
  res <- tryCatch({
    # keepSource so error srcrefs report real line numbers
    source(f, local = new.env(), keep.source = TRUE)
    "ok"
  }, error = function(e) {
    conditionMessage(e)
  })
  if (identical(res, "ok")) {
    cat("    OK   ", f, "\n")
  } else {
    fail <- TRUE
    cat("    FAIL ", f, "\n")
    cat("         ->", res, "\n")
    break  # stop at first failure — that's the one to fix
  }
}
cat("\n")

# ---- 6. Verdict ------------------------------------------------------------
cat("===========================================================\n")
if (length(missing) || length(stale) || length(absent) || fail) {
  cat("  RESULT: NOT READY. Fix the item(s) flagged above.\n")
  cat("  Fix the FIRST failure only, then re-run preflight.\n")
} else {
  cat("  RESULT: READY. All files valid, packages present, no\n")
  cat("  isolated source failures. You can run app.R.\n")
}
cat("===========================================================\n\n")
