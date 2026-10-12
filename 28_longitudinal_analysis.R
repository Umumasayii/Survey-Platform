# =============================================================================
# MODULE 28: TREND, WAVE & PANEL ANALYSIS
# =============================================================================

mod_longitudinal_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Trend, Wave & Panel Analysis",
      "Compare survey waves, track panel respondents, inspect attrition, and prepare harmonized trend analyses.",
      "timeline"),
    uiOutput(ns("body"))
  )
}

mod_longitudinal_analysis_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(app_state$processed_data, {
      vars <- sap_safe_names(app_state); nums <- sap_numeric_names(app_state)
      updateSelectInput(session, "wave", choices = vars)
      updateSelectInput(session, "id", choices = c("None" = "", vars))
      updateSelectInput(session, "outcome", choices = vars)
      updateSelectInput(session, "group", choices = c("None" = "", vars))
      updateSelectInput(session, "weight", choices = c("None" = "", nums))
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      layout_columns(col_widths = c(4, 8),
        card(class = "sap-card", card_header(icon("sliders"), " Longitudinal specification"), card_body(
          selectInput(session$ns("design"), "Study structure", choices = c("Repeated cross-sectional surveys" = "repeated", "Panel / cohort" = "panel", "Pre/post intervention" = "prepost")),
          selectInput(session$ns("wave"), "Wave / time variable", choices = sap_safe_names(app_state)),
          selectInput(session$ns("id"), "Respondent ID", choices = c("None" = "", sap_safe_names(app_state))),
          selectInput(session$ns("outcome"), "Outcome", choices = sap_safe_names(app_state)),
          selectInput(session$ns("group"), "Comparison group", choices = c("None" = "", sap_safe_names(app_state))),
          selectInput(session$ns("weight"), "Weight", choices = c("None" = "", sap_numeric_names(app_state))),
          selectInput(session$ns("summary"), "Summary", choices = c("Mean" = "mean", "Median" = "median", "Proportion non-zero" = "proportion")),
          checkboxGroupInput(session$ns("outputs"), "Analysis outputs", choices = c("Trend estimates" = "trend", "Wave differences" = "difference", "Attrition" = "attrition", "Transition matrix" = "transition", "Harmonization checks" = "harmonization"), selected = c("trend", "difference")),
          div(class = "d-grid gap-2",
              actionButton(session$ns("analyze"), "Preview trend", icon = icon("play"), class = "btn-primary"),
              actionButton(session$ns("save"), "Save longitudinal specification", icon = icon("floppy-disk"), class = "btn-outline-primary"))
        )),
        div(
          uiOutput(session$ns("trend_kpis")),
          card(class = "sap-card", card_header(icon("chart-line"), " Trend preview"), card_body(plotlyOutput(session$ns("trend_plot"), height = "350px"))),
          navset_card_tab(
            nav_panel("Wave summary", DTOutput(session$ns("wave_table"))),
            nav_panel("Change tests", DTOutput(session$ns("difference_table"))),
            nav_panel("Panel retention", DTOutput(session$ns("attrition_table"))),
            nav_panel("Transitions", DTOutput(session$ns("transition_table")))
          )
        )
      )
    })

    trend_result <- eventReactive(input$analyze, {
      df <- app_state$processed_data; req(is.data.frame(df), nzchar(input$wave %||% ""), nzchar(input$outcome %||% ""))
      wave <- as.character(df[[input$wave]]); y <- suppressWarnings(as.numeric(df[[input$outcome]]))
      w <- rep(1, nrow(df)); if (nzchar(input$weight %||% "")) { w <- suppressWarnings(as.numeric(df[[input$weight]])); w[!is.finite(w) | w <= 0] <- NA }
      group <- if (nzchar(input$group %||% "")) as.character(df[[input$group]]) else "All respondents"
      d <- data.frame(wave = wave, group = group, y = y, w = w, stringsAsFactors = FALSE)
      d <- d[!is.na(d$wave) & !is.na(d$group) & !is.na(d$y) & !is.na(d$w), , drop = FALSE]
      if (nrow(d) == 0L) {
        return(data.frame(Wave = character(0), Group = character(0), Estimate = numeric(0),
                          SE = numeric(0), CI_Lower = numeric(0), CI_Upper = numeric(0),
                          Unweighted_n = integer(0), Weighted_n = numeric(0), Effective_n = numeric(0),
                          stringsAsFactors = FALSE))
      }
      key <- interaction(d$wave, d$group, drop = TRUE, lex.order = TRUE)
      split_d <- split(d, key)
      rows <- lapply(split_d, function(z) {
        estimate <- switch(input$summary,
          mean = stats::weighted.mean(z$y, z$w, na.rm = TRUE),
          median = stats::median(z$y, na.rm = TRUE),
          proportion = 100 * stats::weighted.mean(as.numeric(z$y != 0), z$w, na.rm = TRUE))
        effective_n <- sum(z$w)^2 / sum(z$w^2)
        se <- switch(input$summary,
          mean = sqrt(stats::weighted.mean((z$y - estimate)^2, z$w, na.rm = TRUE) / effective_n),
          median = 1.2533 * stats::sd(z$y, na.rm = TRUE) / sqrt(effective_n),
          proportion = {
            pp <- estimate / 100; 100 * sqrt(pp * (1 - pp) / effective_n)
          })
        data.frame(Wave = z$wave[1], Group = z$group[1], Estimate = estimate, SE = se,
                   CI_Lower = estimate - 1.96 * se, CI_Upper = estimate + 1.96 * se,
                   Unweighted_n = nrow(z), Weighted_n = sum(z$w, na.rm = TRUE), Effective_n = effective_n,
                   stringsAsFactors = FALSE)
      })
      out <- do.call(rbind, rows); rownames(out) <- NULL; out
    }, ignoreInit = TRUE)

    output$trend_kpis <- renderUI({
      out <- trend_result()
      if (is.null(out)) return(div(class = "alert alert-info", "Choose wave and outcome variables, then preview the trend."))
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Waves", length(unique(out$Wave)), "calendar-days"),
          sap_kpi("Groups", length(unique(out$Group)), "people-group"),
          sap_kpi("Observations summarized", format(sum(out$Unweighted_n), big.mark = ","), "users"),
          sap_kpi("Design", switch(input$design, repeated = "Repeated cross-section", panel = "Panel", prepost = "Pre/post"), "timeline"))
    })

    output$trend_plot <- renderPlotly({
      out <- trend_result(); req(is.data.frame(out), nrow(out) > 0)
      out$Wave <- factor(out$Wave, levels = unique(out$Wave))
      p <- ggplot(out, aes(x = Wave, y = Estimate, group = Group, text = paste("Group:", Group))) + geom_line(aes(linetype = Group)) + geom_point() + labs(x = "Wave", y = "Estimate") + theme_minimal()
      sap_ggplotly(p, tooltip = c("x", "y", "text"))
    })

    output$wave_table <- renderDT({
      out <- trend_result()
      if (is.null(out)) return(datatable(data.frame(Instruction = "Run a trend preview to populate wave estimates."), options = list(dom = "t"), rownames = FALSE))
      numeric <- names(out)[vapply(out, is.numeric, logical(1))]
      out[numeric] <- lapply(out[numeric], round, digits = 3)
      datatable(out, options = list(dom = "tip", scrollX = TRUE), rownames = FALSE)
    })

    difference_result <- reactive({
      out <- trend_result(); req(is.data.frame(out), nrow(out) > 0)
      rows <- lapply(split(out, out$Group), function(g) {
        g <- g[match(unique(g$Wave), g$Wave), , drop = FALSE]
        if (nrow(g) < 2L) return(NULL)
        baseline <- g[1, , drop = FALSE]
        current <- g[-1, , drop = FALSE]
        se <- sqrt(current$SE^2 + baseline$SE[[1]]^2)
        z <- (current$Estimate - baseline$Estimate[[1]]) / se
        data.frame(
          Group = current$Group, Baseline = baseline$Wave[[1]], Comparison = current$Wave,
          Difference = current$Estimate - baseline$Estimate[[1]], SE_Difference = se,
          CI_Lower = current$Estimate - baseline$Estimate[[1]] - 1.96 * se,
          CI_Upper = current$Estimate - baseline$Estimate[[1]] + 1.96 * se,
          P_value = 2 * stats::pnorm(-abs(z)), stringsAsFactors = FALSE
        )
      })
      result <- do.call(rbind, Filter(Negate(is.null), rows))
      if (is.null(result)) data.frame() else result
    })

    output$difference_table <- renderDT({
      if (!"difference" %in% (input$outputs %||% character())) return(datatable(data.frame(Instruction = "Enable Wave differences in the specification."), options = list(dom = "t"), rownames = FALSE))
      d <- difference_result()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "At least two waves are required."), options = list(dom = "t"), rownames = FALSE))
      num <- names(d)[vapply(d, is.numeric, logical(1))]; d[num] <- lapply(d[num], round, digits = 4)
      datatable(d, rownames = FALSE, options = list(dom = "tip", scrollX = TRUE))
    })

    panel_result <- reactive({
      if (!identical(input$design, "panel") || !nzchar(input$id %||% "") || !nzchar(input$wave %||% "")) return(list(attrition = data.frame(), transition = data.frame()))
      df <- app_state$processed_data
      id <- as.character(df[[input$id]]); wave <- as.character(df[[input$wave]])
      valid <- !is.na(id) & nzchar(id) & !is.na(wave) & nzchar(wave)
      id <- id[valid]; wave <- wave[valid]
      waves <- unique(wave); baseline_ids <- unique(id[wave == waves[[1]]])
      attrition <- do.call(rbind, lapply(waves, function(wv) {
        present <- unique(id[wave == wv]); retained <- length(intersect(baseline_ids, present))
        data.frame(Wave = wv, Observed_IDs = length(present), Retained_from_baseline = retained,
                   Retention_pct = if (length(baseline_ids)) 100 * retained / length(baseline_ids) else NA_real_, stringsAsFactors = FALSE)
      }))
      transition <- data.frame()
      if ("transition" %in% (input$outputs %||% character()) && length(waves) >= 2L && nzchar(input$outcome %||% "")) {
        tmp <- data.frame(id = as.character(df[[input$id]]), wave = as.character(df[[input$wave]]), value = as.character(df[[input$outcome]]), stringsAsFactors = FALSE)
        first <- tmp[tmp$wave == waves[[1]] & !is.na(tmp$value), c("id", "value")]; names(first)[[2]] <- "From"
        last <- tmp[tmp$wave == tail(waves, 1L) & !is.na(tmp$value), c("id", "value")]; names(last)[[2]] <- "To"
        paired <- merge(first[!duplicated(first$id), ], last[!duplicated(last$id), ], by = "id")
        if (nrow(paired)) {
          transition_tab <- as.data.frame.matrix(table(paired$From, paired$To))
          transition <- data.frame(From = rownames(transition_tab), transition_tab,
                                   check.names = FALSE, stringsAsFactors = FALSE)
          rownames(transition) <- NULL
        }
      }
      list(attrition = attrition, transition = transition)
    })

    output$attrition_table <- renderDT({
      d <- panel_result()$attrition
      if (!"attrition" %in% (input$outputs %||% character()) || !nrow(d)) return(datatable(data.frame(Instruction = "Choose Panel, a respondent ID, and enable Attrition."), options = list(dom = "t"), rownames = FALSE))
      d$Retention_pct <- round(d$Retention_pct, 2)
      datatable(d, rownames = FALSE, options = list(dom = "tip"))
    })
    output$transition_table <- renderDT({
      d <- panel_result()$transition
      if (!is.data.frame(d) || !nrow(d)) return(datatable(data.frame(Instruction = "Enable Transition matrix for panel data with at least two waves."), options = list(dom = "t"), rownames = FALSE))
      datatable(d, rownames = FALSE, options = list(dom = "tip", scrollX = TRUE))
    })

    observeEvent(input$save, {
      app_state$longitudinal_spec <- list(design = input$design, wave = input$wave, id = input$id, outcome = input$outcome,
        group = input$group, weight = input$weight, summary = input$summary, outputs = input$outputs, updated_at = Sys.time())
      sap_frontend_record(app_state, "Longitudinal", "Longitudinal analysis specification saved", app_state$longitudinal_spec)
      showNotification("Longitudinal specification saved.", type = "message")
    })
  })
}

message("[SAP] Longitudinal Analysis engine v4.0.0 loaded")
