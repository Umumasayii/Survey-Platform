# =============================================================================
# preflight_report.R  —  run BEFORE launching, from the project folder.
#   source("preflight_report.R")
# Verifies the Add-to-Report wiring. Static checks only; no app is started.
# =============================================================================

cat("\n--- SAP Add-to-Report preflight ---\n\n")

files <- c("00_services.R", "02_data_explorer.R", "03_data_cleaning.R",
           "05_descriptive_stats.R", "06_statistical_tests.R",
           "07_visualization.R", "08_report_generator.R",
           "11_machine_learning.R", "app.R")

missing <- files[!file.exists(files)]
if (length(missing)) stop("Missing files: ", paste(missing, collapse = ", "))

# 1. Banners -----------------------------------------------------------------
cat("1. Version banners\n")
for (f in files) {
  ln <- grep("SAP_MODULE_BANNER", readLines(f, warn = FALSE), value = TRUE)
  cat(sprintf("   %-26s %s\n", f,
              if (length(ln)) sub('.*"(\\[SAP\\][^"]*)".*', "\\1", ln[1]) else "(none)"))
}

# 2. Signature drift ---------------------------------------------------------
cat("\n2. Module servers must accept `services`\n")
for (f in setdiff(files, c("00_services.R", "app.R"))) {
  ln <- grep("_server <- function", readLines(f, warn = FALSE), value = TRUE)[1]
  ok <- grepl("services", ln)
  cat(sprintf("   %-26s %s\n", f, if (ok) "migrated OK" else "OLD - will error"))
}

# 3. app.R must pass services ------------------------------------------------
cat("\n3. app.R passes services to every wired module\n")
app <- readLines("app.R", warn = FALSE)
for (m in c("data_explorer", "data_cleaning", "descriptive_stats",
            "statistical_tests", "visualization", "machine_learning",
            "report_generator")) {
  ln <- grep(sprintf("mod_%s_server\\(", m), app, value = TRUE)
  cat(sprintf("   %-20s %s\n", m,
              if (length(ln) && grepl("services", ln[1])) "OK" else "MISSING services="))
}

# 4. Helpers exist after sourcing 00_services.R ------------------------------
cat("\n4. Shared helpers\n")
e <- new.env()
suppressMessages(sys.source("global_no_gtsummary.R", envir = e))
suppressMessages(sys.source("00_services.R", envir = e))
for (fn in c("report_button", "register_report_button", "plot_capture",
             "stat_result_df", "create_services")) {
  cat(sprintf("   %-24s %s\n", fn,
              if (exists(fn, envir = e, inherits = FALSE)) "defined" else "MISSING"))
}

# 5. Button / registration pairing per module --------------------------------
cat("\n5. Every button has exactly one registration\n")
for (f in c("02_data_explorer.R", "03_data_cleaning.R", "05_descriptive_stats.R",
            "06_statistical_tests.R", "07_visualization.R",
            "11_machine_learning.R")) {
  txt  <- paste(readLines(f, warn = FALSE), collapse = "\n")
  btns <- unique(c(
    regmatches(txt, gregexpr('report_button\\(ns, "[a-z_0-9]+"', txt))[[1]],
    regmatches(txt, gregexpr('actionButton\\(ns\\("add_[a-z_0-9]+"\\)', txt))[[1]]))
  btns <- gsub('.*"([a-z_0-9]+)".*', "\\1", btns)
  regs <- regmatches(txt, gregexpr('register_report_button\\(\\s*\n?\\s*input, services, "[a-z_0-9]+"', txt))[[1]]
  regs <- gsub('.*"([a-z_0-9]+)".*', "\\1", regs)
  cat(sprintf("   %-26s %2d buttons, %2d registrations  %s\n", f,
              length(btns), length(regs),
              if (setequal(btns, regs)) "OK" else "MISMATCH"))
}

# 6. Optional packages -------------------------------------------------------
cat("\n6. Optional packages for report output\n")
for (pk in c("base64enc", "officer", "rmarkdown", "openxlsx", "writexl")) {
  cat(sprintf("   %-12s %s\n", pk,
              if (requireNamespace(pk, quietly = TRUE)) "installed"
              else "NOT installed (feature degrades)"))
}

cat("\n--- preflight complete ---\n\n")
