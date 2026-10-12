# =============================================================================
# MODULE 19: SURVEY DESIGN & WEIGHTING — FRONTEND
# =============================================================================

mod_survey_design_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Survey Design & Weighting",
      "Define weights, strata, clusters, finite-population corrections, and analysis domains.",
      "scale-balanced"
    ),
    sap_frontend_notice("The saved design is validated centrally and reused by survey-aware tables and estimators. Weight trimming, strata, clusters, finite-population correction, and domain definitions remain attached to the active workspace."),
    uiOutput(ns("body"))
  )
}

mod_survey_design_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(app_state$processed_data, {
      vars <- sap_safe_names(app_state)
      numeric_vars <- sap_numeric_names(app_state)
      saved <- app_state$survey_design %||% list()
      updateSelectInput(session, "weight", choices = c("None" = "", numeric_vars), selected = saved$weight %||% "")
      updateSelectInput(session, "strata", choices = c("None" = "", vars), selected = saved$strata %||% "")
      updateSelectInput(session, "cluster", choices = c("None" = "", vars), selected = saved$cluster %||% "")
      updateSelectInput(session, "fpc", choices = c("None" = "", numeric_vars), selected = saved$fpc %||% "")
      updateSelectInput(session, "domain", choices = c("None" = "", vars), selected = saved$domain %||% "")
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported) || !is.data.frame(app_state$processed_data)) return(sap_no_data())
      tagList(
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("sliders"), " Design specification"),
            card_body(
              checkboxInput(ns("enabled"), "Use complex survey design", value = isTRUE((app_state$survey_design %||% list())$enabled)),
              selectInput(ns("weight"), "Sampling weight", choices = c("None" = "", sap_numeric_names(app_state)),
                          selected = (app_state$survey_design %||% list())$weight %||% ""),
              selectInput(ns("strata"), "Stratum variable", choices = c("None" = "", sap_safe_names(app_state)),
                          selected = (app_state$survey_design %||% list())$strata %||% ""),
              selectInput(ns("cluster"), "Primary sampling unit / cluster", choices = c("None" = "", sap_safe_names(app_state)),
                          selected = (app_state$survey_design %||% list())$cluster %||% ""),
              selectInput(ns("fpc"), "Finite population correction", choices = c("None" = "", sap_numeric_names(app_state)),
                          selected = (app_state$survey_design %||% list())$fpc %||% ""),
              selectInput(ns("domain"), "Default domain / subpopulation", choices = c("None" = "", sap_safe_names(app_state)),
                          selected = (app_state$survey_design %||% list())$domain %||% ""),
              sliderInput(ns("trim_pct"), "Weight trimming percentile", min = 0, max = 10,
                          value = (app_state$survey_design %||% list())$trim_pct %||% 0, step = 0.5,
                          post = "%"),
              textAreaInput(ns("notes"), "Design notes", rows = 3,
                            value = (app_state$survey_design %||% list())$notes %||% "",
                            placeholder = "Sampling frame, stages, calibration source, replicate method..."),
              div(class = "d-grid gap-2",
                  actionButton(ns("save"), "Save survey design", icon = icon("floppy-disk"), class = "btn-primary"),
                  actionButton(ns("reset"), "Reset design", icon = icon("rotate-left"), class = "btn-outline-secondary"))
            )
          ),
          div(
            uiOutput(ns("summary_cards")),
            card(
              class = "sap-card mt-3",
              card_header(icon("chart-simple"), " Weight diagnostics"),
              card_body(DTOutput(ns("weight_table")))
            ),
            card(
              class = "sap-card mt-3",
              card_header(icon("circle-check"), " Readiness review"),
              card_body(uiOutput(ns("readiness")))
            )
          )
        )
      )
    })

    selected_weight <- reactive({
      req(is.data.frame(app_state$processed_data))
      if (is.null(input$weight) || !nzchar(input$weight) || !input$weight %in% names(app_state$processed_data)) return(NULL)
      suppressWarnings(as.numeric(app_state$processed_data[[input$weight]]))
    })

    output$summary_cards <- renderUI({
      df <- app_state$processed_data
      req(is.data.frame(df))
      w <- selected_weight()
      weighted_n <- if (is.null(w)) NA_real_ else sum(w[is.finite(w) & w >= 0], na.rm = TRUE)
      eff_n <- if (is.null(w)) NA_real_ else {
        good <- is.finite(w) & w > 0
        if (!any(good)) NA_real_ else sum(w[good])^2 / sum(w[good]^2)
      }
      div(class = "sap-kpi-grid",
          sap_kpi("Unweighted respondents", format(nrow(df), big.mark = ","), "users"),
          sap_kpi("Weighted population", sap_fmt(weighted_n, 0), "weight-hanging"),
          sap_kpi("Effective sample size", sap_fmt(eff_n, 0), "users"),
          sap_kpi("Design components", sum(nzchar(c(input$weight %||% "", input$strata %||% "", input$cluster %||% ""))), "diagram-project"))
    })

    output$weight_table <- renderDT({
      w <- selected_weight()
      if (is.null(w)) {
        return(datatable(data.frame(Status = "Select a numeric weight variable to preview diagnostics."), options = list(dom = "t"), rownames = FALSE))
      }
      good <- is.finite(w) & !is.na(w)
      pos <- good & w > 0
      tab <- data.frame(
        Metric = c("Valid weights", "Missing / invalid", "Zero or negative", "Minimum", "Median", "Mean", "Maximum", "Coefficient of variation", "Effective sample size"),
        Value = c(
          sum(good), sum(!good), sum(good & w <= 0),
          sap_fmt(min(w[good], na.rm = TRUE), 3), sap_fmt(stats::median(w[good], na.rm = TRUE), 3),
          sap_fmt(mean(w[good], na.rm = TRUE), 3), sap_fmt(max(w[good], na.rm = TRUE), 3),
          if (sum(pos) > 1) sap_fmt(stats::sd(w[pos]) / mean(w[pos]), 3) else "—",
          if (any(pos)) sap_fmt(sum(w[pos])^2 / sum(w[pos]^2), 0) else "—"
        ), stringsAsFactors = FALSE
      )
      datatable(tab, options = list(dom = "t", ordering = FALSE), rownames = FALSE)
    })

    output$readiness <- renderUI({
      issues <- character(0)
      if (isTRUE(input$enabled) && (is.null(input$weight) || !nzchar(input$weight))) issues <- c(issues, "Complex design is enabled but no sampling weight is selected.")
      w <- selected_weight()
      if (!is.null(w) && any(!is.finite(w) | w <= 0, na.rm = TRUE)) issues <- c(issues, "The weight variable contains invalid, zero, or negative values.")
      if (!is.null(input$cluster) && nzchar(input$cluster) && identical(input$cluster, input$strata)) issues <- c(issues, "Cluster and stratum variables are identical; confirm that this is intentional.")
      if (length(issues) == 0L) {
        div(class = "alert alert-success mb-0", icon("circle-check"), " The current design specification is ready to save.")
      } else {
        div(class = "alert alert-warning mb-0", icon("triangle-exclamation"), tags$ul(class = "mb-0 mt-2", lapply(issues, tags$li)))
      }
    })

    observeEvent(input$save, {
      candidate <- list(
        enabled = isTRUE(input$enabled), weight = input$weight %||% "", strata = input$strata %||% "",
        cluster = input$cluster %||% "", fpc = input$fpc %||% "", domain = input$domain %||% "",
        trim_pct = input$trim_pct %||% 0, notes = input$notes %||% "", updated_at = Sys.time()
      )
      if (isTRUE(candidate$enabled) && !is.null(services) && !is.null(services$survey)) {
        chk <- services$survey$validate_spec(app_state$processed_data, candidate)
        if (!isTRUE(chk$ok)) {
          showNotification(paste("Survey design not saved:", paste(chk$errors, collapse = "; ")), type = "error", duration = 10)
          return(invisible(NULL))
        }
      }
      app_state$survey_design <- candidate
      sap_frontend_record(app_state, "Survey Design", "Survey design specification saved", app_state$survey_design)
      showNotification(if (isTRUE(candidate$enabled)) "Survey design validated and saved." else "Survey design saved in the current workspace.", type = "message")
    })

    observeEvent(input$reset, {
      app_state$survey_design <- list(enabled = FALSE, weight = "", strata = "", cluster = "", fpc = "", domain = "", trim_pct = 0, notes = "")
      updateCheckboxInput(session, "enabled", value = FALSE)
      lapply(c("weight", "strata", "cluster", "fpc", "domain"), function(x) updateSelectInput(session, x, selected = ""))
      updateSliderInput(session, "trim_pct", value = 0)
      updateTextAreaInput(session, "notes", value = "")
      sap_frontend_record(app_state, "Survey Design", "Survey design reset")
    })
  })
}

message("[SAP] Survey Design estimator foundation v1.1-20260809 loaded")
