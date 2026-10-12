# =============================================================================
# MODULE 12: SYSTEM DIAGNOSTICS  (v2.2-20260706)
# =============================================================================
# Verifies application health and environment integrity:
#   - File integrity: every module file present AND at the expected version
#     (catches the "stale file being sourced" failure mode definitively)
#   - Symbol resolution: observe/reactive/req etc. resolve to package:shiny
#     (catches masking by other packages)
#   - Package installation & versions (required vs optional)
#   - External tools: pandoc, LaTeX
#   - Session resources: R memory, project file footprint
# Produces a plain-text diagnostic report suitable for sharing/debugging.
# =============================================================================

SAP_EXPECTED_VERSION <- "v2.2-20260706"

# -----------------------------------------------------------------------------
# CHECK PRIMITIVES
# -----------------------------------------------------------------------------

#' One diagnostic result row
diag_row <- function(section, check, status, detail, recommendation = "") {
  data.frame(section = section, check = check, status = status,
             detail = detail, recommendation = recommendation,
             stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# ENVIRONMENT
# -----------------------------------------------------------------------------

run_environment_checks <- function() {
  rows <- list()

  rows[[length(rows) + 1]] <- diag_row(
    "Environment", "R version", "ok",
    R.version$version.string
  )

  rows[[length(rows) + 1]] <- diag_row(
    "Environment", "Working directory", "ok",
    normalizePath(getwd(), winslash = "/"),
    "All module files must live in THIS directory."
  )

  shiny_ver <- tryCatch(as.character(utils::packageVersion("shiny")),
                        error = function(e) NA_character_)
  rows[[length(rows) + 1]] <- diag_row(
    "Environment", "shiny version",
    if (!is.na(shiny_ver)) "ok" else "fail",
    ifelse(is.na(shiny_ver), "not installed", shiny_ver)
  )

  do.call(rbind, rows)
}

# -----------------------------------------------------------------------------
# SYMBOL RESOLUTION (masking detection)
# -----------------------------------------------------------------------------

run_masking_checks <- function() {
  syms <- c("observe", "observeEvent", "reactive", "eventReactive",
            "isolate", "req", "renderUI", "validate", "need")
  rows <- lapply(syms, function(s) {
    hits <- utils::find(s)
    top  <- if (length(hits) > 0) hits[1] else "(not found)"
    ok   <- identical(top, "package:shiny")
    diag_row(
      "Symbol resolution",
      sprintf("`%s` resolves to", s),
      if (ok) "ok" else "fail",
      top,
      if (ok) "" else sprintf(
        "'%s' is masking shiny::%s. Load that package BEFORE shiny, or call shiny::%s explicitly.",
        top, s, s)
    )
  })
  do.call(rbind, rows)
}

# -----------------------------------------------------------------------------
# FILE INTEGRITY (the deployment-drift catcher)
# -----------------------------------------------------------------------------

run_file_checks <- function(expected_version = SAP_EXPECTED_VERSION) {
  required <- c(
    "app.R", "global_no_gtsummary.R",
    "01_import_wizard.R", "02_data_explorer.R", "03_data_cleaning.R",
    "04_dashboard.R", "05_descriptive_stats.R", "06_statistical_tests.R",
    "07_visualization.R", "08_report_generator.R", "09_project_manager.R",
    "10_analysis_history.R", "11_machine_learning.R",
    "12_system_diagnostics.R", "16_code_generator.R"
  )
  optional <- c("00_services.R", "db_backend.R")

  check_one <- function(f, is_required) {
    if (!file.exists(f)) {
      return(diag_row(
        "File integrity", f,
        if (is_required) "fail" else "warn",
        "MISSING",
        if (is_required)
          "Extract this file from survey_platform_v2.1.zip into this directory."
        else "Optional file; not yet installed."
      ))
    }
    txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    has_version <- any(grepl(expected_version, txt, fixed = TRUE))
    mtime <- format(file.mtime(f), "%Y-%m-%d %H:%M")
    if (has_version) {
      diag_row("File integrity", f, "ok",
               sprintf("%s (modified %s)", expected_version, mtime))
    } else {
      diag_row("File integrity", f, "fail",
               sprintf("STALE - no %s stamp (modified %s)",
                       expected_version, mtime),
               "Old version. Overwrite with the copy from survey_platform_v2.1.zip.")
    }
  }

  rows <- c(lapply(required, check_one, is_required = TRUE),
            lapply(optional, check_one, is_required = FALSE))
  do.call(rbind, rows)
}

# -----------------------------------------------------------------------------
# PACKAGES
# -----------------------------------------------------------------------------

run_package_checks <- function() {
  required <- c("shiny", "bslib", "shinyWidgets", "bsicons", "fontawesome",
                "plotly", "DT", "janitor", "dplyr", "tidyr", "readxl",
                "jsonlite", "stringr", "forcats", "ggplot2", "broom",
                "officer", "rmarkdown", "shinycssloaders", "shinyjs",
                "shinyFiles", "R6")
  optional <- c("rpart", "randomForest", "e1071", "class", "naivebayes",
                "xgboost", "car", "DBI", "pool", "RSQLite", "blob",
                "haven", "openxlsx", "gt", "reactable", "lubridate")

  check_pkg <- function(p, is_required) {
    ok <- requireNamespace(p, quietly = TRUE)
    if (ok) {
      diag_row("Packages", p, "ok",
               as.character(utils::packageVersion(p)))
    } else {
      diag_row("Packages", p,
               if (is_required) "fail" else "warn",
               "not installed",
               sprintf('install.packages("%s")%s', p,
                       if (is_required) "" else "  (needed only for some features)"))
    }
  }

  rows <- c(lapply(required, check_pkg, is_required = TRUE),
            lapply(optional, check_pkg, is_required = FALSE))
  do.call(rbind, rows)
}

# -----------------------------------------------------------------------------
# EXTERNAL TOOLS
# -----------------------------------------------------------------------------

run_tool_checks <- function() {
  rows <- list()

  pandoc_ok <- tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)
  rows[[length(rows) + 1]] <- diag_row(
    "External tools", "Pandoc",
    if (pandoc_ok) "ok" else "warn",
    if (pandoc_ok)
      as.character(tryCatch(rmarkdown::pandoc_version(),
                            error = function(e) "available"))
    else "not found",
    if (pandoc_ok) "" else
      "HTML/Word report rendering needs pandoc. RStudio bundles it; standalone R does not."
  )

  latex_path <- Sys.which("pdflatex")
  tinytex_ok <- sap_optional_available("tinytex") &&
    tryCatch(sap_optional_call("tinytex", "is_tinytex"), error = function(e) FALSE)
  latex_ok <- nzchar(latex_path) || tinytex_ok
  rows[[length(rows) + 1]] <- diag_row(
    "External tools", "LaTeX (PDF reports)",
    if (latex_ok) "ok" else "warn",
    if (tinytex_ok) "tinytex" else if (nzchar(latex_path)) latex_path else "not found",
    if (latex_ok) "" else
      'PDF report export needs LaTeX: install.packages("tinytex"); tinytex::install_tinytex()'
  )

  do.call(rbind, rows)
}

# -----------------------------------------------------------------------------
# RESOURCES
# -----------------------------------------------------------------------------

run_resource_checks <- function() {
  g <- gc(verbose = FALSE)
  mem_mb <- round(sum(g[, 2]), 1)

  r_files  <- list.files(".", pattern = "\\.R$")
  proj_kb  <- round(sum(file.size(r_files), na.rm = TRUE) / 1024, 1)

  rbind(
    diag_row("Resources", "R session memory", "ok",
             sprintf("%.1f MB in use", mem_mb)),
    diag_row("Resources", "Project source files", "ok",
             sprintf("%d .R files, %.1f KB", length(r_files), proj_kb))
  )
}

# -----------------------------------------------------------------------------
# AGGREGATE + REPORT
# -----------------------------------------------------------------------------

run_all_diagnostics <- function() {
  res <- rbind(
    run_environment_checks(),
    run_masking_checks(),
    run_file_checks(),
    run_package_checks(),
    run_tool_checks(),
    run_resource_checks()
  )
  res$ran_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  res
}

build_diagnostic_report <- function(res) {
  n_fail <- sum(res$status == "fail")
  n_warn <- sum(res$status == "warn")
  header <- c(
    "==============================================================",
    sprintf(" SURVEY ANALYTICS PLATFORM - DIAGNOSTIC REPORT (%s)",
            SAP_EXPECTED_VERSION),
    sprintf(" Generated: %s", res$ran_at[1]),
    sprintf(" Result: %d FAIL / %d WARN / %d OK",
            n_fail, n_warn, sum(res$status == "ok")),
    "=============================================================="
  )
  body <- unlist(lapply(split(res, res$section), function(sec) {
    c(sprintf("\n## %s", sec$section[1]),
      vapply(seq_len(nrow(sec)), function(i) {
        line <- sprintf("[%s] %-32s %s",
                        toupper(sec$status[i]), sec$check[i], sec$detail[i])
        if (nzchar(sec$recommendation[i])) {
          line <- paste0(line, "\n        -> ", sec$recommendation[i])
        }
        line
      }, character(1)))
  }), use.names = FALSE)
  paste(c(header, body), collapse = "\n")
}

# =============================================================================
# UI
# =============================================================================

mod_system_diagnostics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "d-flex justify-content-between align-items-center mb-4",
      div(
        h2(icon("stethoscope"), "System Diagnostics"),
        p("Verify files, packages, environment, and application health",
          class = "text-muted mb-0")
      ),
      actionButton(ns("btn_run"), "Re-run Checks",
                   class = "btn-primary", icon = icon("rotate"))
    ),

    # Summary badges
    uiOutput(ns("summary_badges")),
    br(),

    # Results table
    div(
      class = "card",
      div(class = "card-header", h5("Check Results")),
      div(class = "card-body p-0",
        div(style = "max-height: 480px; overflow-y: auto;",
          uiOutput(ns("results_table"))
        )
      )
    ),
    br(),

    # Pasteable report
    div(
      class = "card",
      div(
        class = "card-header d-flex justify-content-between align-items-center",
        h5("Diagnostic Report (copy/paste this when asking for help)"),
        downloadButton(ns("dl_report"), "Download .txt", class = "btn-sm")
      ),
      div(class = "card-body",
        tags$pre(
          style = "max-height: 320px; overflow-y: auto; font-size: 0.8rem;",
          textOutput(ns("report_text"))
        )
      )
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

mod_system_diagnostics_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {

    # Runs once at startup (ignoreNULL = FALSE) and on every button press.
    results <- eventReactive(input$btn_run, {
      run_all_diagnostics()
    }, ignoreNULL = FALSE)

    output$summary_badges <- renderUI({
      res <- results()
      n_ok   <- sum(res$status == "ok")
      n_warn <- sum(res$status == "warn")
      n_fail <- sum(res$status == "fail")
      overall <- if (n_fail > 0) {
        span(class = "badge bg-danger fs-6", sprintf("%d FAILURES", n_fail))
      } else if (n_warn > 0) {
        span(class = "badge bg-warning text-dark fs-6",
             sprintf("Healthy with %d warnings", n_warn))
      } else {
        span(class = "badge bg-success fs-6", "All checks passed")
      }
      div(
        class = "d-flex gap-2 align-items-center",
        overall,
        span(class = "badge bg-success",  sprintf("%d ok", n_ok)),
        span(class = "badge bg-warning text-dark", sprintf("%d warn", n_warn)),
        span(class = "badge bg-danger",   sprintf("%d fail", n_fail))
      )
    })

    output$results_table <- renderUI({
      res <- results()
      badge <- function(st) {
        cls <- switch(st, ok = "bg-success", warn = "bg-warning text-dark",
                      fail = "bg-danger", "bg-secondary")
        span(class = paste("badge", cls), toupper(st))
      }
      tags$table(
        class = "table table-sm table-hover mb-0",
        tags$thead(tags$tr(
          tags$th("Status"), tags$th("Section"), tags$th("Check"),
          tags$th("Detail"), tags$th("Recommendation")
        )),
        tags$tbody(
          lapply(seq_len(nrow(res)), function(i) {
            tags$tr(
              tags$td(badge(res$status[i])),
              tags$td(res$section[i]),
              tags$td(res$check[i]),
              tags$td(tags$code(res$detail[i])),
              tags$td(class = "text-muted small", res$recommendation[i])
            )
          })
        )
      )
    })

    output$report_text <- renderText({
      build_diagnostic_report(results())
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        paste0("sap_diagnostics_", format(Sys.Date(), "%Y%m%d"), ".txt")
      },
      content = function(file) {
        if (!is.null(services) && !is.null(services$privacy)) {
          services$privacy$assert_content_export_allowed(
            build_diagnostic_report(results()), "system diagnostics export"
          )
        }
        writeLines(build_diagnostic_report(results()), file)
      }
    )
  })
}

message("[SAP] System Diagnostics v2.2-20260706 loaded")  # SAP_MODULE_BANNER
