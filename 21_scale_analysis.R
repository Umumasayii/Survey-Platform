# =============================================================================
# MODULE 21: PSYCHOMETRICS & SCALE ANALYSIS
# =============================================================================

mod_scale_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Psychometrics & Scale Analysis",
      "Validate multi-item constructs, inspect reliability, manage reverse coding, and create composite scores.",
      "list-ol"),
    uiOutput(ns("body"))
  )
}

mod_scale_analysis_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(app_state$processed_data, {
      nums <- sap_numeric_names(app_state)
      updateSelectizeInput(session, "items", choices = nums, server = TRUE)
      updateSelectizeInput(session, "reverse_items", choices = nums, server = TRUE)
    }, ignoreInit = FALSE)
    observeEvent(input$items, {
      k <- max(1L, length(input$items %||% character()))
      updateSliderInput(session, "min_answered", max = k, value = min(input$min_answered %||% 1L, k))
      updateNumericInput(session, "factor_count", max = max(1L, k - 1L), value = min(input$factor_count %||% 1L, max(1L, k - 1L)))
    })

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      layout_columns(
        col_widths = c(4, 8),
        card(class = "sap-card",
          card_header(icon("sliders"), " Scale definition"),
          card_body(
            textInput(session$ns("scale_name"), "Scale name", placeholder = "e.g., Service satisfaction"),
            selectizeInput(session$ns("items"), "Scale items", choices = sap_numeric_names(app_state), multiple = TRUE),
            selectizeInput(session$ns("reverse_items"), "Reverse-coded items", choices = sap_numeric_names(app_state), multiple = TRUE),
            selectInput(session$ns("score_method"), "Composite score", choices = c("Mean of answered items" = "mean", "Sum of answered items" = "sum")),
            sliderInput(session$ns("min_answered"), "Minimum answered items", min = 1, max = 10, value = 2, step = 1),
            numericInput(session$ns("factor_count"), "Exploratory factors", value = 1, min = 1, max = 9),
            div(class = "d-grid gap-2",
                actionButton(session$ns("analyze"), "Analyze scale", icon = icon("play"), class = "btn-primary"),
                actionButton(session$ns("save_definition"), "Save scale definition", icon = icon("floppy-disk"), class = "btn-outline-primary"),
                actionButton(session$ns("create_score"), "Create score in active data", icon = icon("plus"), class = "btn-success"))
          )
        ),
        div(
          uiOutput(session$ns("kpis")),
          navset_card_tab(
            nav_panel("Item diagnostics", DTOutput(session$ns("item_table"))),
            nav_panel("Inter-item matrix", plotlyOutput(session$ns("correlation_plot"), height = "360px")),
            nav_panel("Factor loadings", DTOutput(session$ns("factor_table")))
          ),
          card(class = "sap-card mt-3",
               card_header(icon("chart-line"), " Score distribution preview"),
               card_body(plotlyOutput(session$ns("score_plot"), height = "300px")))
        )
      )
    })

    prepared_items <- reactive({
      df <- app_state$processed_data
      req(is.data.frame(df), length(input$items) >= 2)
      x <- as.data.frame(lapply(df[input$items], function(v) suppressWarnings(as.numeric(v))))
      revs <- intersect(input$reverse_items %||% character(0), names(x))
      for (nm in revs) {
        rng <- range(x[[nm]], na.rm = TRUE)
        if (all(is.finite(rng))) x[[nm]] <- sum(rng) - x[[nm]]
      }
      x
    })

    scale_result <- eventReactive(input$analyze, {
      x <- prepared_items()
      k <- ncol(x)
      complete <- stats::complete.cases(x)
      alpha <- NA_real_
      if (sum(complete) >= 3 && k >= 2) {
        xc <- x[complete, , drop = FALSE]
        total <- rowSums(xc)
        vt <- stats::var(total)
        if (is.finite(vt) && vt > 0) alpha <- k / (k - 1) * (1 - sum(vapply(xc, stats::var, numeric(1)), na.rm = TRUE) / vt)
      }
      answered <- rowSums(!is.na(x))
      min_req <- min(input$min_answered %||% 1, k)
      score <- if (identical(input$score_method, "sum")) rowSums(x, na.rm = TRUE) else rowMeans(x, na.rm = TRUE)
      score[answered < min_req] <- NA_real_
      item_rest <- vapply(names(x), function(nm) {
        rest <- rowSums(x[setdiff(names(x), nm)], na.rm = FALSE)
        suppressWarnings(stats::cor(x[[nm]], rest, use = "pairwise.complete.obs"))
      }, numeric(1))
      alpha_deleted <- vapply(names(x), function(drop) {
        reduced <- x[setdiff(names(x), drop)]; kk <- ncol(reduced); cc <- stats::complete.cases(reduced)
        if (kk < 2L || sum(cc) < 3L) return(NA_real_)
        total_reduced <- rowSums(reduced[cc, , drop = FALSE]); variance <- stats::var(total_reduced)
        if (!is.finite(variance) || variance <= 0) return(NA_real_)
        kk / (kk - 1) * (1 - sum(vapply(reduced[cc, , drop = FALSE], stats::var, numeric(1)), na.rm = TRUE) / variance)
      }, numeric(1))
      correlation <- suppressWarnings(stats::cor(x, use = "pairwise.complete.obs"))
      factor_count <- min(as.integer(input$factor_count %||% 1L), max(1L, k - 1L))
      factor_fit <- if (sum(complete) > max(10L, 3L * k) && k >= 3L) tryCatch(
        stats::factanal(x[complete, , drop = FALSE], factors = factor_count, rotation = if (factor_count > 1L) "varimax" else "none"),
        error = function(e) NULL
      ) else NULL
      loadings <- if (!is.null(factor_fit)) as.data.frame(unclass(factor_fit$loadings)) else data.frame()
      if (nrow(loadings)) loadings$Item <- rownames(loadings)
      omega <- NA_real_
      one_factor <- if (sum(complete) > max(10L, 3L * k) && k >= 3L) tryCatch(stats::factanal(x[complete, , drop = FALSE], factors = 1L, rotation = "none"), error = function(e) NULL) else NULL
      if (!is.null(one_factor)) {
        lambda <- as.numeric(one_factor$loadings[, 1]); omega <- sum(lambda)^2 / (sum(lambda)^2 + sum(one_factor$uniquenesses))
      }
      list(x = x, alpha = alpha, omega = omega, score = score, item_rest = item_rest,
           alpha_deleted = alpha_deleted, correlation = correlation, loadings = loadings,
           complete_n = sum(complete))
    }, ignoreInit = TRUE)

    output$kpis <- renderUI({
      res <- scale_result()
      if (is.null(res)) return(div(class = "alert alert-info", "Select at least two numeric items and analyze the scale."))
      alpha_status <- if (is.na(res$alpha)) "Not estimable" else if (res$alpha >= .8) "Strong" else if (res$alpha >= .7) "Acceptable" else "Review"
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Items", ncol(res$x), "list-ol"),
          sap_kpi("Complete cases", format(res$complete_n, big.mark = ","), "users"),
          sap_kpi("Cronbach's alpha", sap_fmt(res$alpha, 3), "gauge-high"),
          sap_kpi("McDonald's omega", sap_fmt(res$omega, 3), "wave-square"),
          sap_kpi("Reliability status", alpha_status, "circle-check"))
    })

    output$item_table <- renderDT({
      res <- scale_result()
      if (is.null(res)) return(datatable(data.frame(Instruction = "Run the scale analysis to populate item diagnostics."), options = list(dom = "t"), rownames = FALSE))
      x <- res$x
      tab <- data.frame(
        Item = names(x),
        Mean = vapply(x, mean, numeric(1), na.rm = TRUE),
        SD = vapply(x, stats::sd, numeric(1), na.rm = TRUE),
        Missing_pct = 100 * vapply(x, function(v) mean(is.na(v)), numeric(1)),
        Item_rest_correlation = unname(res$item_rest),
        Alpha_if_deleted = unname(res$alpha_deleted),
        Reverse_coded = names(x) %in% (input$reverse_items %||% character(0)),
        stringsAsFactors = FALSE
      )
      numeric_cols <- vapply(tab, is.numeric, logical(1))
      tab[numeric_cols] <- lapply(tab[numeric_cols], round, digits = 3)
      datatable(tab, options = list(dom = "tip", pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    output$score_plot <- renderPlotly({
      res <- scale_result(); req(!is.null(res))
      d <- data.frame(score = res$score)
      p <- ggplot(d, aes(x = score)) + geom_histogram(bins = 20, na.rm = TRUE) + labs(x = "Composite score", y = "Respondents") + theme_minimal()
      sap_ggplotly(p)
    })

    output$correlation_plot <- renderPlotly({
      res <- scale_result(); req(!is.null(res), nrow(res$correlation) > 1L)
      corr <- res$correlation
      d <- expand.grid(Item_x = colnames(corr), Item_y = rownames(corr), stringsAsFactors = FALSE)
      d$Correlation <- as.numeric(corr)
      plotly::plot_ly(d, x = ~Item_x, y = ~Item_y, z = ~Correlation, type = "heatmap",
                      colors = c("#fb7185", "#f8fafc", "#5b5bd6"), zmin = -1, zmax = 1,
                      hovertemplate = "%{y} × %{x}<br>r=%{z:.3f}<extra></extra>") |>
        plotly::layout(xaxis = list(title = ""), yaxis = list(title = "")) |>
        plotly::config(displaylogo = FALSE, responsive = TRUE)
    })

    output$factor_table <- renderDT({
      res <- scale_result()
      if (is.null(res) || !nrow(res$loadings)) return(datatable(data.frame(Instruction = "Factor loadings require at least three items and sufficient complete cases."), options = list(dom = "t"), rownames = FALSE))
      d <- res$loadings[, c("Item", setdiff(names(res$loadings), "Item")), drop = FALSE]
      numeric <- names(d)[vapply(d, is.numeric, logical(1))]; d[numeric] <- lapply(d[numeric], round, digits = 3)
      datatable(d, rownames = FALSE, options = list(dom = "tip", scrollX = TRUE))
    })

    observeEvent(input$save_definition, {
      req(nzchar(input$scale_name %||% ""), length(input$items) >= 2)
      definition <- list(name = input$scale_name, items = input$items, reverse_items = input$reverse_items,
                         method = input$score_method, min_answered = input$min_answered, updated_at = Sys.time())
      defs <- app_state$scale_definitions %||% list(); defs[[input$scale_name]] <- definition; app_state$scale_definitions <- defs
      sap_frontend_record(app_state, "Scale Analysis", paste("Scale definition saved:", input$scale_name), definition)
      showNotification("Scale definition saved.", type = "message")
    })

    observeEvent(input$create_score, {
      req(nzchar(input$scale_name %||% "")); res <- scale_result(); req(!is.null(res))
      new_name <- janitor::make_clean_names(input$scale_name)
      df <- app_state$processed_data
      df[[new_name]] <- res$score
      if (!is.null(services) && !is.null(services$data)) {
        services$data$set(
          df, kind = "processed", module = "scale_analysis",
          description = paste("Composite score created:", new_name),
          name = app_state$dataset_name %||% "Active dataset"
        )
      } else {
        app_state$processed_data <- df
      }
      app_state$transformation_count <- (app_state$transformation_count %||% 0L) + 1L
      sap_frontend_record(app_state, "Scale Analysis", paste("Composite score created:", new_name))
      showNotification(sprintf("Composite score '%s' added to the active dataset.", new_name), type = "message")
    })
  })
}

message("[SAP] Scale Analysis engine v4.0.0 loaded")
