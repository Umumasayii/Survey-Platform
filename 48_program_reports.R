# Professional programme reporting from governed MEL state.
sap_me_report_types <- function(language = "en") sap_report_type_labels(language)

sap_me_report_config <- function(project, prepared_by, partner, period, reviewer, report_type, language = "en") {
  list(
    project = project, prepared_by = prepared_by, partner = partner, period = period,
    reviewer = reviewer, report_type = report_type,
    language = sap_normalize_language(language),
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M")
  )
}

sap_me_report_type_label <- function(report_type, language = "en") {
  choices <- sap_report_type_labels(language)
  idx <- match(as.character(report_type %||% ""), unname(choices))
  if (is.na(idx)) as.character(report_type %||% "") else names(choices)[[idx]]
}

sap_me_render_html_report <- function(config, indicators, data = NULL, results = NULL) {
  esc <- function(x) htmltools::htmlEscape(as.character(x %||% ""))
  lang <- sap_normalize_language(config$language %||% "en")
  tr <- function(x) sap_tr(x, lang)
  indicators <- indicators %||% data.frame()
  results <- results %||% sap_me_empty_results()
  if (nrow(indicators)) {
    indicators$progress_pct <- mapply(sap_me_progress, indicators$baseline, indicators$target, indicators$actual, indicators$direction)
    indicators$status <- vapply(indicators$progress_pct, function(x) tr(sap_progress_status(x)), character(1))
  }
  tab <- function(df) {
    if (!is.data.frame(df) || !nrow(df)) return(paste0("<p>", esc(tr("No verified records available.")), "</p>"))
    display <- df
    names(display) <- tr(names(display))
    paste0(
      "<table><thead><tr>", paste0("<th>", esc(names(display)), "</th>", collapse = ""),
      "</tr></thead><tbody>",
      paste(apply(utils::head(display, 50), 1, function(r) paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>")), collapse = ""),
      "</tbody></table>"
    )
  }
  report_label <- sap_me_report_type_label(config$report_type, lang)
  paste0(
    "<!doctype html><html lang='", esc(lang), "'><head><meta charset='utf-8'>",
    "<style>body{font-family:Arial,sans-serif;margin:40px;color:#1f2937}h1,h2{color:#0f4c5c}table{border-collapse:collapse;width:100%;font-size:12px}th,td{border:1px solid #d1d5db;padding:7px;text-align:left}th{background:#f3f4f6}.meta{padding:14px;background:#f8fafc;border-radius:10px}</style></head><body>",
    "<h1>", esc(config$project), "</h1><div class='meta'><b>", esc(tr("Report")), ":</b> ", esc(report_label),
    " &nbsp; <b>", esc(tr("Period")), ":</b> ", esc(config$period),
    "<br><b>", esc(tr("Prepared by")), ":</b> ", esc(config$prepared_by),
    " &nbsp; <b>", esc(tr("Reviewer")), ":</b> ", esc(config$reviewer),
    "<br><b>", esc(tr("Partner")), ":</b> ", esc(config$partner), "</div>",
    "<h2>", esc(tr("Indicator performance against targets")), "</h2>", tab(indicators),
    "<h2>", esc(tr("Reporting-period results and verification")), "</h2>", tab(sap_me_report_result_summary(results, FALSE)),
    "<h2>", esc(tr("Data and quality context")), "</h2><p>",
    esc(tr("Records available for analysis")), ": ", if (is.data.frame(data)) nrow(data) else 0, ".</p></body></html>"
  )
}

mod_program_reports_ui<-function(id){ns<-NS(id);tagList(sap_professional_header("Programme Reports","Generate governed MEL reports from indicator definitions and verified reporting-period results.","file-circle-check","Evidence-ready"),layout_columns(card(card_header("Report configuration"),selectInput(ns("type"),"Report type",sap_me_report_types()),textInput(ns("project"),"Project / programme"),textInput(ns("period"),"Reporting period",placeholder="August 2026 / Q3 2026"),textInput(ns("prepared"),"Prepared by"),textInput(ns("reviewer"),"Reviewed by"),textInput(ns("partner"),"Partner / institution"),downloadButton(ns("download"),"Download HTML report",class="btn-primary")),card(card_header("Report readiness"),uiOutput(ns("readiness")),p("Sensitive disaggregation is excluded from shared reporting-history tables by default.")),col_widths=c(6,6)))}
mod_program_reports_server <- function(id, app_state, services = NULL) moduleServer(id, function(input, output, session) {
  observe({
    if (!nzchar(input$project %||% "")) updateTextInput(session, "project", value = app_state$current_project_name %||% "Survey Analytics Project")
  })
  observe({
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    current <- input$type %||% "monthly"
    updateSelectInput(session, "type", choices = sap_me_report_types(lang), selected = current)
  })
  output$readiness <- renderUI({
    inds <- app_state$me_indicators %||% sap_me_empty_indicators()
    res <- app_state$me_results %||% sap_me_empty_results()
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    div(
      class = "sap-pro-kpi-grid sap-pro-kpi-grid-2",
      sap_pro_kpi(sap_tr("Indicators", lang), nrow(inds), sap_tr("definitions", lang), "bullseye"),
      sap_pro_kpi(sap_tr("Verified results", lang), nrow(res), sap_tr("period records", lang), "clipboard-check")
    )
  })
  output$download <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "_", input$project %||% "programme"), "_", input$type, "_", Sys.Date(), ".html"),
    content = function(file) {
      lang <- sap_normalize_language(app_state$ui_language %||% "en")
      cfg <- sap_me_report_config(input$project, input$prepared, input$partner, input$period, input$reviewer, input$type, language = lang)
      html <- sap_me_render_html_report(
        cfg,
        app_state$me_indicators %||% sap_me_empty_indicators(),
        app_state$processed_data %||% app_state$raw_data,
        app_state$me_results %||% sap_me_empty_results()
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )
})

