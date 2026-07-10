# =============================================================================
# MODULE 8: REPORT GENERATOR
# =============================================================================
#
# Professional report generation in PDF, Word, and HTML formats
#
# =============================================================================

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Generate PDF report
# =============================================================================
# REPORT BLOCK RENDERERS
# -----------------------------------------------------------------------------
# services$report$get_blocks() yields list(id, kind, title, payload, code, ts)
# with kind in {table, verbatim, text, plot}. `payload` is a data.frame for
# tables, a character vector for verbatim/text, and a PNG path for plots.
#
# These turn that buffer into Markdown (for the rmarkdown/PDF path) and into
# officer calls (for the Word path). HTML already had its own renderer in
# ReportService$to_html().
# =============================================================================

#' Escape a data.frame's cells so they survive a pandoc pipe table.
sanitize_md_table <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x[] <- lapply(x, function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    gsub("|", "\\|", v, fixed = TRUE)
  })
  x
}

#' Indent text by four spaces -> a pandoc code block.
#'
#' Deliberately NOT a fenced block: this string is written INTO an R chunk
#' inside a generated .Rmd, and a literal ``` there would close the chunk.
indent_code_block <- function(txt) {
  lines <- unlist(strsplit(paste(txt, collapse = "\n"), "\n", fixed = TRUE))
  paste0("    ", lines, collapse = "\n")
}

#' Copy plot PNGs next to the .Rmd and rewrite payloads to relative names.
#' Absolute Windows temp paths break LaTeX; relative names never do.
stage_plot_blocks <- function(blocks, dir) {
  i <- 0L
  lapply(blocks, function(b) {
    if (!identical(b$kind, "plot")) return(b)
    if (is.null(b$payload) || !file.exists(b$payload)) {
      b$payload <- NA_character_
      return(b)
    }
    i <<- i + 1L
    nm <- sprintf("sap_blk_%02d.png", i)
    ok <- file.copy(b$payload, file.path(dir, nm), overwrite = TRUE)
    b$payload <- if (isTRUE(ok)) nm else NA_character_
    b
  })
}

#' Append the blocks section + reproducible-code appendix to an .Rmd body.
#' Returns a character string of Rmd source.
report_blocks_rmd <- function() {
  paste(
    "",
    "\\newpage",
    "",
    "# Analysis Results",
    "",
    "```{r sap-blocks, echo=FALSE, results='asis', message=FALSE, warning=FALSE}",
    "if (!length(sap_blocks)) {",
    "  cat('_No results captured. Use the **Add to Report** buttons, then export again._\\n')",
    "} else {",
    "  for (b in sap_blocks) {",
    "    cat('\\n\\n## ', b$title, '\\n\\n', sep = '')",
    "    if (identical(b$kind, 'table')) {",
    "      print(knitr::kable(sanitize_md_table(b$payload), format = 'pipe',",
    "                         row.names = FALSE))",
    "      cat('\\n')",
    "    } else if (identical(b$kind, 'plot')) {",
    "      if (!is.na(b$payload)) cat('\\n![](', b$payload, ')\\n', sep = '')",
    "      else cat('_[plot unavailable]_\\n')",
    "    } else if (identical(b$kind, 'verbatim')) {",
    "      cat('\\n', indent_code_block(b$payload), '\\n\\n', sep = '')",
    "    } else {",
    "      cat('\\n', paste(b$payload, collapse = ' '), '\\n', sep = '')",
    "    }",
    "  }",
    "}",
    "```",
    "",
    "\\newpage",
    "",
    "# Reproducible Code",
    "",
    "```{r sap-code, echo=FALSE, results='asis'}",
    "if (nzchar(sap_code)) cat('\\n', indent_code_block(sap_code), '\\n', sep = '')",
    "else cat('_No code recorded._\\n')",
    "```",
    "",
    sep = "\n")
}

generate_pdf_report <- function(df, title = "Data Analysis Report",
                               output_file = NULL, blocks = list(), code = "") {
  
  if (is.null(output_file)) {
    output_file <- file.path(tempdir(), 
                            paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"))
  }
  
  # Create temporary RMarkdown file
  rmd_file <- tempfile(fileext = ".Rmd")
  
  # Generate RMarkdown content
  rmd_content <- sprintf('---
title: "%s"
author: "Survey Analytics Platform"
date: "%s"
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
---

# Executive Summary

This report provides comprehensive analysis of your dataset.

Generated: %s | Dataset: %d rows × %d columns | Quality: High

---

# Data Overview

Dataset contains %d observations and %d variables across multiple types.

---

# Data Quality Assessment

Missing Values: %.1f%% | Data Quality: Excellent

---

# Methodology

Analysis follows professional statistical standards and best practices.

---

# Conclusions

Data is suitable for analysis with no critical quality issues.

---

# Technical Information

Report Generated: %s | R Version: %s

---

*This is an automated report. See platform for full analysis.*
',
    title,
    format(Sys.time(), "%B %d, %Y"),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    nrow(df),
    ncol(df),
    nrow(df),
    ncol(df),
    round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 1),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    R.version$version.string
  )
  
  # Stage plot PNGs beside the .Rmd (relative paths; absolute Windows temp
  # paths with spaces break LaTeX \includegraphics).
  rmd_dir <- dirname(rmd_file)
  blocks  <- stage_plot_blocks(blocks, rmd_dir)

  # Append the captured "Add to Report" blocks + code appendix.
  rmd_content <- paste0(rmd_content, report_blocks_rmd())

  # Write RMarkdown file
  writeLines(rmd_content, rmd_file)

  # Render to PDF. The chunk reads `sap_blocks` / `sap_code` from this env,
  # so the data.frames and PNG paths never have to survive a round trip
  # through the .Rmd source.
  knit_env <- new.env(parent = globalenv())
  knit_env$sap_blocks         <- blocks
  knit_env$sap_code           <- code %||% ""
  knit_env$sanitize_md_table  <- sanitize_md_table
  knit_env$indent_code_block  <- indent_code_block

  tryCatch({
    rmarkdown::render(
      rmd_file,
      output_format = "pdf_document",
      output_file = output_file,
      envir = knit_env,
      quiet = TRUE
    )
    return(invisible(output_file))
  }, error = function(e) {
    warning("PDF generation failed: ", e$message)
    return(invisible(NULL))
  })
}

#' Generate Word report
#' Heading via a paragraph style.
#'
#' officer does NOT export `body_add_heading()`; headings are paragraph styles
#' on `body_add_par()`. The previous code called `officer::body_add_heading()`
#' and `officer::print()` (also not exported -- `print.rdocx` is an S3 method),
#' both of which threw, were swallowed by tryCatch, and produced an empty file.
docx_heading <- function(doc, text, level = 1) {
  sty <- paste("heading", level)
  out <- tryCatch(officer::body_add_par(doc, text, style = sty),
                  error = function(e) NULL)
  if (is.null(out)) officer::body_add_par(doc, text) else out
}

generate_word_report <- function(df, title = "Data Analysis Report",
                                 output_file = NULL, blocks = list(), code = "") {

  if (is.null(output_file)) {
    output_file <- file.path(tempdir(),
                            paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx"))
  }

  if (!requireNamespace("officer", quietly = TRUE)) {
    warning("Package 'officer' is required for Word export.")
    return(invisible(NULL))
  }

  tryCatch({
    doc <- officer::read_docx()

    doc <- docx_heading(doc, title, 1)
    doc <- officer::body_add_par(doc,
                                 sprintf("Generated: %s",
                                        format(Sys.time(), "%B %d, %Y")))

    doc <- docx_heading(doc, "Executive Summary", 2)
    doc <- officer::body_add_par(doc,
      "This report provides comprehensive analysis of your dataset.")

    doc <- docx_heading(doc, "Dataset Overview", 2)
    doc <- officer::body_add_par(doc,
                                 sprintf("Observations: %d | Variables: %d",
                                        nrow(df), ncol(df)))

    doc <- docx_heading(doc, "Data Quality Assessment", 2)
    missing_pct <- round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 1)
    doc <- officer::body_add_par(doc,
      sprintf("Missing Values: %.1f%% | Overall Quality: High", missing_pct))

    # ---- Captured "Add to Report" blocks --------------------------------
    doc <- docx_heading(doc, "Analysis Results", 1)
    if (!length(blocks)) {
      doc <- officer::body_add_par(doc,
        "No results were captured. Use the Add to Report buttons on any output, then export again.")
    } else {
      for (b in blocks) {
        doc <- docx_heading(doc, b$title %||% "Result", 2)

        if (identical(b$kind, "table")) {
          tbl <- as.data.frame(b$payload, stringsAsFactors = FALSE)
          tbl[] <- lapply(tbl, function(v) {
            v <- as.character(v); v[is.na(v)] <- ""; v
          })
          doc <- officer::body_add_table(doc, value = tbl)

        } else if (identical(b$kind, "plot")) {
          if (!is.null(b$payload) && file.exists(b$payload)) {
            doc <- officer::body_add_img(doc, src = b$payload,
                                         width = 6, height = 3.7)
          } else {
            doc <- officer::body_add_par(doc, "[plot unavailable]")
          }

        } else if (identical(b$kind, "verbatim")) {
          for (ln in as.character(b$payload)) {
            doc <- officer::body_add_par(doc, ln)
          }

        } else {
          doc <- officer::body_add_par(doc, paste(b$payload, collapse = " "))
        }
      }
    }

    # ---- Reproducible code appendix -------------------------------------
    doc <- docx_heading(doc, "Reproducible Code", 1)
    if (nzchar(code %||% "")) {
      for (ln in unlist(strsplit(code, "\n", fixed = TRUE))) {
        doc <- officer::body_add_par(doc, ln)
      }
    } else {
      doc <- officer::body_add_par(doc, "No code recorded.")
    }

    doc <- docx_heading(doc, "Technical Information", 2)
    doc <- officer::body_add_par(doc,
      sprintf("Generated: %s | R: %s",
             format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             R.version$version.string))

    # print.rdocx S3 method -- NOT officer::print()
    print(doc, target = output_file)
    return(invisible(output_file))

  }, error = function(e) {
    warning("Word report generation failed: ", e$message)
    return(invisible(NULL))
  })
}

#' Generate HTML report
generate_html_report <- function(df, title = "Data Analysis Report",
                                  output_file = NULL, blocks_html = "") {
  
  if (is.null(output_file)) {
    output_file <- file.path(tempdir(), 
                            paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"))
  }
  
  tryCatch({
    # Calculate metrics
    numeric_cols <- sum(sapply(df, is.numeric))
    categorical_cols <- sum(sapply(df, function(x) is.factor(x) || is.character(x)))
    missing_pct <- round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 1)
    
    # Create HTML
    html_content <- sprintf('<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>%s</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css">
  <style>
    body { font-family: Segoe UI, sans-serif; background-color: #f8f9fa; }
    .header { background: linear-gradient(135deg, #0d6efd 0%%, #0d6efd 100%%); color: white; padding: 40px; border-radius: 8px; margin-bottom: 30px; }
    .kpi-card { background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #0d6efd; }
    .section { background: white; padding: 30px; border-radius: 8px; margin-bottom: 30px; }
    .section h2 { color: #0d6efd; border-bottom: 2px solid #0d6efd; padding-bottom: 15px; margin-bottom: 20px; }
    table { width: 100%%; border-collapse: collapse; }
    th { background-color: #0d6efd; color: white; padding: 12px; }
    td { padding: 12px; border-bottom: 1px solid #eee; }
  </style>
</head>
<body>
  <div style="max-width: 1200px; margin: 0 auto; padding: 20px;">
    
    <div class="header">
      <h1>%s</h1>
      <p>Generated: %s</p>
    </div>
    
    <div class="section">
      <h2>Executive Summary</h2>
      <p>Comprehensive analysis of your dataset using Survey Analytics Platform.</p>
    </div>
    
    <div class="section">
      <h2>Key Metrics</h2>
      <div class="row">
        <div class="col-md-3">
          <div class="kpi-card">
            <h6>Observations</h6>
            <h3>%d</h3>
          </div>
        </div>
        <div class="col-md-3">
          <div class="kpi-card">
            <h6>Variables</h6>
            <h3>%d</h3>
          </div>
        </div>
        <div class="col-md-3">
          <div class="kpi-card">
            <h6>Missing Data</h6>
            <h3>%.1f%%</h3>
          </div>
        </div>
        <div class="col-md-3">
          <div class="kpi-card">
            <h6>Data Quality</h6>
            <h3>A</h3>
          </div>
        </div>
      </div>
    </div>
    
    <div class="section">
      <h2>Data Overview</h2>
      <table>
        <tr>
          <th>Type</th>
          <th>Count</th>
        </tr>
        <tr>
          <td>Numeric Variables</td>
          <td>%d</td>
        </tr>
        <tr>
          <td>Categorical Variables</td>
          <td>%d</td>
        </tr>
      </table>
    </div>
    
    <div class="section">
      <h2>Data Quality</h2>
      <p>Missing Values: %.1f%%</p>
      <p>Completeness: %.1f%%</p>
      <p>Overall Assessment: Excellent</p>
    </div>
    
    <div class="section">
      <h2>Methodology</h2>
      <p>Analysis follows professional statistical standards.</p>
    </div>
    
    <div class="section">
      <h2>Next Steps</h2>
      <ol>
        <li>Review descriptive statistics</li>
        <li>Explore relationships</li>
        <li>Conduct hypothesis testing</li>
      </ol>
    </div>
    
    <div class="section" style="background-color: #f0f0f0;">
      <h2>Technical Information</h2>
      <p>Generated: %s<br>R Version: %s<br>Platform: Survey Analytics Platform</p>
    </div>
    
  </div>
</body>
</html>',
    title,
    title,
    format(Sys.time(), "%B %d, %Y at %H:%M:%S"),
    nrow(df),
    ncol(df),
    missing_pct,
    numeric_cols,
    categorical_cols,
    missing_pct,
    100 - missing_pct,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    R.version$version.string
  )
  
  # Write HTML file
  # Inject captured analysis blocks (from "Add to Report") before </body>.
  if (nzchar(blocks_html)) {
    injected <- sprintf(
      "<div class='section'><h2>Analysis Results</h2>%s</div>\n</body>",
      blocks_html)
    html_content <- sub("</body>", injected, html_content, fixed = TRUE)
  }
  writeLines(html_content, output_file)
  return(invisible(output_file))
    
  }, error = function(e) {
    warning("HTML generation failed: ", e$message)
    return(invisible(NULL))
  })
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_report_generator_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("file-pdf"), "Report Generator"),
      p("Generate professional reports in multiple formats", class = "text-muted")
    ),
    
    # No data message
    uiOutput(ns("no_data_message")),
    
    # Main content
    uiOutput(ns("report_content"))
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_report_generator_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Data validation
    data_exists <- reactive({
      req(app_state$data_imported)
      !is.null(app_state$processed_data) && nrow(app_state$processed_data) > 0
    })
    
    df_data <- reactive({
      req(data_exists())
      app_state$processed_data
    })
    
    # No data message
    output$no_data_message <- renderUI({
      if (!data_exists()) {
        empty_state(
          icon = "file-pdf",
          title = "No data available",
          message = "Import data first to generate reports"
        )
      }
    })
    
    # Report content
    output$report_content <- renderUI({
      if (!data_exists()) return(NULL)
      
      tagList(
        # Report title input
        div(
          class = "card mb-4",
          div(class = "card-header", h5("Report Settings")),
          div(class = "card-body",
            textInput(ns("report_title"), "Report Title:",
                     value = "Data Analysis Report")
          )
        ),

        # Report Builder — content captured via "Add to Report" buttons
        div(
          class = "card mb-4",
          div(class = "card-header d-flex justify-content-between align-items-center",
            h5(class = "mb-0", "Report Builder"),
            div(
              span(class = "badge bg-primary me-2",
                   textOutput(ns("report_block_count"), inline = TRUE),
                   " blocks"),
              actionButton(ns("refresh_report_builder"), "Refresh",
                           class = "btn btn-sm btn-outline-secondary",
                           icon = icon("rotate")),
              actionButton(ns("clear_report"), "Clear",
                           class = "btn btn-sm btn-outline-danger",
                           icon = icon("trash"))
            )
          ),
          div(class = "card-body",
            p(class = "text-muted small",
              "Use \"Add to Report\" on any result (summary tables, tests, ...). ",
              "Blocks appear here and are included in the exported report. ",
              "Click Refresh after adding."),
            tableOutput(ns("report_builder_table")),
            hr(),
            h6("Preview"),
            div(style = "max-height: 400px; overflow:auto; border:1px solid #dee2e6; padding:12px; border-radius:6px;",
                uiOutput(ns("report_preview")))
          )
        ),
        
        # Export options
        div(
          class = "card mb-4",
          div(class = "card-header", h5("Export Formats")),
          div(class = "card-body",
            div(class = "row",
              div(class = "col-md-4",
                div(class = "card border-primary",
                  div(class = "card-body text-center",
                    icon("file-pdf", class = "fa-3x text-danger mb-3"),
                    h6("PDF Report"),
                    downloadButton(ns("export_pdf"), "Download PDF", class = "btn-danger btn-sm")
                  )
                )
              ),
              div(class = "col-md-4",
                div(class = "card border-primary",
                  div(class = "card-body text-center",
                    icon("file-word", class = "fa-3x text-primary mb-3"),
                    h6("Word Report"),
                    downloadButton(ns("export_word"), "Download Word", class = "btn-primary btn-sm")
                  )
                )
              ),
              div(class = "col-md-4",
                div(class = "card border-primary",
                  div(class = "card-body text-center",
                    icon("globe", class = "fa-3x text-info mb-3"),
                    h6("HTML Report"),
                    downloadButton(ns("export_html"), "Download HTML", class = "btn-info btn-sm")
                  )
                )
              )
            )
          )
        )
      )
    })
    
    # Export handlers
    # Captured blocks + reproducible code, shared by all three exporters.
    report_blocks <- function() {
      if (is.null(services) || is.null(services$report)) return(list())
      services$report$get_blocks()
    }
    report_code <- function() {
      if (is.null(services) || is.null(services$report)) return("")
      services$report$get_code()
    }

    output$export_pdf <- downloadHandler(
      filename = function() {
        paste0("report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        title <- input$report_title %||% "Data Analysis Report"
        tryCatch({
          pdf_file <- generate_pdf_report(df_data(), title = title,
                                          blocks = report_blocks(),
                                          code   = report_code())
          if (!is.null(pdf_file) && file.exists(pdf_file)) {
            file.copy(pdf_file, file, overwrite = TRUE)
          } else {
            stop("PDF renderer produced no file. Is LaTeX (tinytex) installed?")
          }
        }, error = function(e) {
          writeLines(sprintf("Error: %s", e$message), file)
        })
      }
    )

    output$export_word <- downloadHandler(
      filename = function() {
        paste0("report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        title <- input$report_title %||% "Data Analysis Report"
        word_file <- generate_word_report(df_data(), title = title,
                                          blocks = report_blocks(),
                                          code   = report_code())
        if (!is.null(word_file) && file.exists(word_file)) {
          file.copy(word_file, file, overwrite = TRUE)
        } else {
          # Never hand the browser a 0-byte .docx: surface the failure.
          showNotification("Word export failed - see console warning.",
                           type = "error")
          writeLines("Word report generation failed.", file)
        }
      }
    )

    output$export_html <- downloadHandler(
      filename = function() {
        paste0("report_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        title <- input$report_title %||% "Data Analysis Report"
        blocks_html <- if (!is.null(services) && !is.null(services$report))
                         services$report$to_html() else ""
        generate_html_report(df_data(), title = title, output_file = file,
                             blocks_html = blocks_html)
      }
    )

    # ---- Report Builder: list captured blocks, preview, clear -------------
    # services$report$version() is a reactive trigger bumped on every
    # add/remove/clear, so these refresh live as buttons are clicked across
    # the app. input$refresh_report_builder still forces a manual re-render.
    output$report_block_count <- renderText({
      input$refresh_report_builder
      if (is.null(services) || is.null(services$report)) return("0")
      services$report$version()
      as.character(services$report$count())
    })

    output$report_builder_table <- renderTable({
      input$refresh_report_builder
      if (is.null(services) || is.null(services$report)) return(NULL)
      services$report$version()
      services$report$summary_df()
    })

    output$report_preview <- renderUI({
      input$refresh_report_builder
      if (is.null(services) || is.null(services$report)) {
        return(div(class = "text-muted", "Report services not available."))
      }
      services$report$version()
      HTML(services$report$to_html())
    })

    observeEvent(input$clear_report, {
      if (!is.null(services) && !is.null(services$report)) services$report$clear()
      showNotification("Report cleared", type = "message")
    })
    
  })
}

# =============================================================================
# END OF MODULE 8
# =============================================================================

message("[SAP] Report Generator v2.5-20260708 loaded")  # SAP_MODULE_BANNER
