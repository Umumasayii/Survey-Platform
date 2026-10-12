# =============================================================================
# MODULE 22: MISSING DATA & NONRESPONSE
# =============================================================================

mod_missing_nonresponse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Missing Data & Nonresponse",
      "Separate item missingness, unit nonresponse, breakoffs, and interview-quality risks before inference.",
      "user-clock"),
    uiOutput(ns("body"))
  )
}

mod_missing_nonresponse_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(app_state$processed_data, {
      vars <- sap_safe_names(app_state); nums <- sap_numeric_names(app_state)
      updateSelectInput(session, "respondent_id", choices = c("Row number" = "", vars))
      updateSelectInput(session, "duration", choices = c("None" = "", nums))
      updateSelectInput(session, "group", choices = c("None" = "", vars))
      updateSelectInput(session, "response_status", choices = c("None" = "", vars))
      updateSelectizeInput(session, "propensity_predictors", choices = vars, server = TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$response_status, {
      df <- app_state$processed_data
      values <- if (is.data.frame(df) && nzchar(input$response_status %||% "") && input$response_status %in% names(df)) {
        unique(as.character(df[[input$response_status]]))
      } else character()
      updateSelectInput(session, "responded_value", choices = values[!is.na(values)])
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      tagList(
        uiOutput(session$ns("kpis")),
        layout_columns(
          col_widths = c(4, 8),
          card(class = "sap-card",
            card_header(icon("filter"), " Diagnostic rules"),
            card_body(
              sliderInput(session$ns("variable_threshold"), "Flag variables missing above", min = 0, max = 100, value = 10, post = "%"),
              sliderInput(session$ns("respondent_threshold"), "Flag respondents completing below", min = 0, max = 100, value = 70, post = "%"),
              selectInput(session$ns("respondent_id"), "Respondent identifier", choices = c("Row number" = "", sap_safe_names(app_state))),
              selectInput(session$ns("duration"), "Interview duration variable", choices = c("None" = "", sap_numeric_names(app_state))),
              numericInput(session$ns("fast_cutoff"), "Fast interview cutoff", value = 5, min = 0, step = .5),
              selectInput(session$ns("group"), "Compare missingness by group", choices = c("None" = "", sap_safe_names(app_state))),
              checkboxGroupInput(session$ns("actions"), "Suggested actions", choices = c("Complete-case sensitivity" = "complete_case", "Multiple imputation plan" = "mi", "Nonresponse weighting" = "weighting", "Breakoff review" = "breakoff"), selected = c("complete_case", "mi")),
              div(class = "d-grid gap-2",
                  actionButton(session$ns("run"), "Run diagnostics", icon = icon("play"), class = "btn-primary"),
                  actionButton(session$ns("save"), "Save nonresponse plan", icon = icon("floppy-disk"), class = "btn-outline-primary"))
            )
          ),
          div(
            card(class = "sap-card",
                 card_header(icon("table-columns"), " Variable missingness"),
                 card_body(DTOutput(session$ns("variable_table")))),
            card(class = "sap-card mt-3",
                 card_header(icon("chart-column"), " Respondent completion"),
                 card_body(plotlyOutput(session$ns("completion_plot"), height = "300px"))),
            card(class = "sap-card mt-3",
                 card_header(icon("people-group"), " Group missingness comparison"),
                 card_body(DTOutput(session$ns("group_table"))))
          )
        ),
        card(class = "sap-card mt-3",
             card_header(icon("flag"), " Flagged respondent preview"),
             card_body(DTOutput(session$ns("respondent_table")))),
        layout_columns(
          col_widths = c(4, 8),
          card(class = "sap-card mt-3", card_header(icon("scale-balanced"), " Response propensity"), card_body(
            selectInput(session$ns("response_status"), "Response-status field", choices = c("None" = "", sap_safe_names(app_state))),
            selectInput(session$ns("responded_value"), "Value meaning responded", choices = character()),
            selectizeInput(session$ns("propensity_predictors"), "Frame / auxiliary predictors", choices = sap_safe_names(app_state), multiple = TRUE),
            numericInput(session$ns("propensity_trim"), "Trim weights at percentile", 99, min = 90, max = 100, step = .5),
            actionButton(session$ns("fit_propensity"), "Fit response model", icon = icon("play"), class = "btn-primary w-100"),
            actionButton(session$ns("create_propensity_weight"), "Create nonresponse weight", icon = icon("plus"), class = "btn-outline-primary w-100 mt-2")
          )),
          div(
            uiOutput(session$ns("propensity_kpis")),
            card(class = "sap-card mt-3", card_header(icon("table-list"), " Propensity and adjustment preview"), card_body(DTOutput(session$ns("propensity_table"))))
          )
        )
      )
    })

    diag_result <- eventReactive(input$run, {
      df <- app_state$processed_data; req(is.data.frame(df))
      miss_pct <- 100 * vapply(df, function(x) mean(is.na(x)), numeric(1))
      variable_table <- data.frame(
        Variable = names(df), Missing_n = vapply(df, function(x) sum(is.na(x)), integer(1)),
        Missing_pct = round(miss_pct, 2),
        Flag = ifelse(miss_pct >= (input$variable_threshold %||% 10), "Review", "OK"),
        stringsAsFactors = FALSE
      )
      completion <- 100 * rowMeans(!is.na(df))
      id <- if (!is.null(input$respondent_id) && nzchar(input$respondent_id)) as.character(df[[input$respondent_id]]) else as.character(seq_len(nrow(df)))
      duration <- if (!is.null(input$duration) && nzchar(input$duration)) suppressWarnings(as.numeric(df[[input$duration]])) else rep(NA_real_, nrow(df))
      flags <- completion < (input$respondent_threshold %||% 70) | (!is.na(duration) & duration < (input$fast_cutoff %||% 5))
      respondents <- data.frame(Respondent = id, Completion_pct = round(completion, 1), Duration = duration,
                                Low_completion = completion < (input$respondent_threshold %||% 70),
                                Fast_interview = !is.na(duration) & duration < (input$fast_cutoff %||% 5), stringsAsFactors = FALSE)
      group_table <- data.frame()
      if (nzchar(input$group %||% "") && input$group %in% names(df)) {
        group <- as.character(df[[input$group]]); group[is.na(group) | !nzchar(group)] <- "(Missing group)"
        group_table <- do.call(rbind, lapply(names(df), function(variable) {
          pieces <- split(df[[variable]], group)
          do.call(rbind, lapply(names(pieces), function(group_name) data.frame(
            Variable = variable, Group = group_name, Missing_n = sum(is.na(pieces[[group_name]])),
            Base = length(pieces[[group_name]]), Missing_pct = 100 * mean(is.na(pieces[[group_name]])), stringsAsFactors = FALSE
          )))
        }))
        group_table$Missing_pct <- round(group_table$Missing_pct, 2)
      }
      list(variable = variable_table, completion = completion, respondents = respondents[flags, , drop = FALSE], group = group_table)
    }, ignoreInit = TRUE)

    output$kpis <- renderUI({
      df <- app_state$processed_data; req(is.data.frame(df))
      total <- nrow(df) * ncol(df); miss <- sum(is.na(df)); completion <- 100 * rowMeans(!is.na(df))
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Missing cells", format(miss, big.mark = ","), "table-cells"),
          sap_kpi("Overall missingness", if (total) sprintf("%.1f%%", 100 * miss / total) else "0%", "percent"),
          sap_kpi("Median completion", sprintf("%.1f%%", stats::median(completion, na.rm = TRUE)), "user-check"),
          sap_kpi("Complete respondents", format(sum(completion == 100), big.mark = ","), "circle-check"))
    })

    output$variable_table <- renderDT({
      res <- diag_result()
      if (is.null(res)) return(datatable(data.frame(Instruction = "Run diagnostics to review variable missingness."), options = list(dom = "t"), rownames = FALSE))
      datatable(res$variable, options = list(pageLength = 15, dom = "tip", scrollX = TRUE), rownames = FALSE)
    })

    output$completion_plot <- renderPlotly({
      res <- diag_result(); req(!is.null(res))
      p <- ggplot(data.frame(completion = res$completion), aes(x = completion)) + geom_histogram(binwidth = 5, boundary = 0) +
        geom_vline(xintercept = input$respondent_threshold %||% 70, linetype = "dashed") +
        labs(x = "Question completion (%)", y = "Respondents") + theme_minimal()
      sap_ggplotly(p)
    })

    output$respondent_table <- renderDT({
      res <- diag_result()
      if (is.null(res)) return(datatable(data.frame(Instruction = "Run diagnostics to identify respondent-level flags."), options = list(dom = "t"), rownames = FALSE))
      datatable(res$respondents, options = list(pageLength = 10, dom = "tip", scrollX = TRUE), rownames = FALSE)
    })

    output$group_table <- renderDT({
      res <- diag_result()
      if (is.null(res) || !nrow(res$group)) return(datatable(data.frame(Instruction = "Choose a group field and run diagnostics."), options = list(dom = "t"), rownames = FALSE))
      datatable(res$group, filter = "top", rownames = FALSE, options = list(pageLength = 15, dom = "tip", scrollX = TRUE))
    })

    propensity_result <- eventReactive(input$fit_propensity, {
      df <- app_state$processed_data
      req(is.data.frame(df), nzchar(input$response_status %||% ""), nzchar(input$responded_value %||% ""), length(input$propensity_predictors) > 0)
      outcome <- as.integer(as.character(df[[input$response_status]]) == as.character(input$responded_value))
      predictors <- setdiff(input$propensity_predictors, input$response_status)
      req(length(predictors) > 0, length(unique(outcome[!is.na(outcome)])) == 2L)
      model_df <- df[, predictors, drop = FALSE]; model_df$.responded <- outcome
      formula <- stats::reformulate(predictors, response = ".responded")
      fit <- stats::glm(formula, data = model_df, family = stats::binomial(), na.action = stats::na.exclude)
      propensity <- as.numeric(stats::predict(fit, newdata = model_df, type = "response"))
      propensity <- pmin(pmax(propensity, .01), .99)
      adjustment <- ifelse(outcome == 1L, 1 / propensity, NA_real_)
      cap <- stats::quantile(adjustment, input$propensity_trim / 100, na.rm = TRUE, names = FALSE)
      adjustment <- pmin(adjustment, cap)
      brier <- mean((outcome - propensity)^2, na.rm = TRUE)
      id <- if (nzchar(input$respondent_id %||% "") && input$respondent_id %in% names(df)) as.character(df[[input$respondent_id]]) else as.character(seq_len(nrow(df)))
      list(fit = fit, outcome = outcome, propensity = propensity, adjustment = adjustment,
           cap = cap, brier = brier,
           preview = data.frame(Respondent = id, Responded = outcome == 1L,
                                Response_propensity = propensity, Nonresponse_adjustment = adjustment,
                                stringsAsFactors = FALSE))
    }, ignoreInit = TRUE)

    output$propensity_kpis <- renderUI({
      result <- propensity_result()
      if (is.null(result)) return(div(class = "alert alert-info mt-3", "Select a response indicator and auxiliary frame variables to estimate inverse-probability adjustment weights."))
      div(class = "sap-kpi-grid mt-3",
          sap_kpi("Respondents", sum(result$outcome == 1L, na.rm = TRUE), "user-check"),
          sap_kpi("Model Brier score", sap_fmt(result$brier, 3), "bullseye"),
          sap_kpi("Median propensity", sap_fmt(stats::median(result$propensity, na.rm = TRUE), 3), "percent"),
          sap_kpi("Trim cap", sap_fmt(result$cap, 2), "scissors"))
    })
    output$propensity_table <- renderDT({
      result <- propensity_result()
      if (is.null(result)) return(datatable(data.frame(Instruction = "Fit a response model to populate adjustment weights."), options = list(dom = "t"), rownames = FALSE))
      d <- result$preview; d$Response_propensity <- round(d$Response_propensity, 4); d$Nonresponse_adjustment <- round(d$Nonresponse_adjustment, 4)
      datatable(d, filter = "top", rownames = FALSE, options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })
    observeEvent(input$create_propensity_weight, {
      result <- propensity_result(); req(!is.null(result))
      df <- app_state$processed_data; df$nonresponse_adjustment <- result$adjustment
      if (!is.null(services) && !is.null(services$data)) services$data$set(
        df, kind = "processed", module = "missing_nonresponse",
        description = "Nonresponse propensity adjustment created",
        name = app_state$dataset_name %||% "Active dataset"
      ) else app_state$processed_data <- df
      app_state$transformation_count <- (app_state$transformation_count %||% 0L) + 1L
      showNotification("Column 'nonresponse_adjustment' was added for respondents; nonrespondents remain missing.", type = "message")
    })

    observeEvent(input$save, {
      app_state$nonresponse_plan <- list(variable_threshold = input$variable_threshold, respondent_threshold = input$respondent_threshold,
        respondent_id = input$respondent_id, duration = input$duration, fast_cutoff = input$fast_cutoff,
        group = input$group, actions = input$actions, updated_at = Sys.time())
      sap_frontend_record(app_state, "Missing & Nonresponse", "Nonresponse analysis plan saved", app_state$nonresponse_plan)
      showNotification("Nonresponse plan saved.", type = "message")
    })
  })
}

message("[SAP] Missing & Nonresponse engine v4.0.0 loaded")
