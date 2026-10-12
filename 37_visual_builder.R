# =============================================================================
# MODULE 37: VISUAL BUILDER
# =============================================================================

mod_visual_builder_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Explore",
      "Build interactive visuals from any model table, apply visual filters, and save reusable objects to dashboard pages.",
      "chart-column"
    ),
    div(
      class = "sap-builder-shell",
      tags$aside(
        class = "sap-builder-pane sap-fields-pane",
        div(class = "sap-builder-pane-head", icon("database"), strong("Data")),
        div(class = "sap-builder-pane-body",
            selectInput(ns("table"), "Model table", choices = character()),
            selectInput(ns("saved_measure"), "Reusable measure", choices = c("Build from fields" = "")),
            div(class = "sap-field-search", textInput(ns("field_search"), NULL, placeholder = "Search fields…", width = "100%")),
            uiOutput(ns("field_list")))
      ),
      tags$main(
        class = "sap-builder-canvas",
        div(
          class = "sap-canvas-toolbar",
          div(
            class = "sap-viz-type-picker",
            radioGroupButtons(
              ns("chart"), NULL,
              choices = c("Bar" = "bar", "Line" = "line", "Area" = "area", "Scatter" = "scatter",
                          "Histogram" = "histogram", "Box" = "box", "Donut" = "donut",
                          "Heatmap" = "heatmap", "KPI" = "kpi", "Table" = "table"),
              selected = "bar", justified = FALSE, size = "sm", status = "primary"
            )
          ),
          div(class = "sap-canvas-actions",
              actionButton(ns("clear_filter"), "Clear filters", icon = icon("filter-circle-xmark"), class = "btn-sm btn-outline-secondary"),
              actionButton(ns("save_visual"), NULL, icon = icon("gauge-high"), class = "btn-sm btn-primary", title = "Send to Dashboard", `aria-label` = "Send to Dashboard"))
        ),
        div(
          class = "sap-live-canvas",
          uiOutput(ns("visual_header")),
          uiOutput(ns("visual_canvas")),
          uiOutput(ns("visual_footer"))
        )
      ),
      tags$aside(
        class = "sap-builder-pane sap-build-pane",
        div(class = "sap-builder-pane-head", icon("sliders"), strong("Build")),
        div(
          class = "sap-builder-pane-body",
          textInput(ns("title"), "Visual title", value = "New insight"),
          selectInput(ns("x"), "Category / X axis", choices = character()),
          selectInput(ns("y"), "Value / Y axis", choices = character()),
          selectInput(ns("color"), "Legend / series", choices = character()),
          selectInput(ns("aggregation"), "Summarise", choices = c(
            "Count" = "count", "Distinct count" = "distinct_count",
            "Sum" = "sum", "Average" = "mean", "Median" = "median",
            "Minimum" = "min", "Maximum" = "max"
          )),
          hr(),
          selectInput(ns("filter_field"), "Visual filter field", choices = character()),
          selectizeInput(ns("filter_values"), "Keep values", choices = character(), multiple = TRUE),
          numericInput(ns("top_n"), "Maximum categories", 20, min = 3, max = 100),
          checkboxInput(ns("cross_filter"), "Click marks to cross-filter", TRUE),
          hr(),
          checkboxInput(ns("show_legend"), "Show legend", TRUE),
          checkboxInput(ns("show_labels"), "Show value labels", FALSE),
          selectInput(ns("sort"), "Sort categories", choices = c("By value, descending" = "value_desc", "By value, ascending" = "value_asc", "By category" = "category")),
          selectInput(ns("number_format"), "Number format", choices = c("Number" = "number", "Integer" = "integer", "Percent" = "percent", "Currency" = "currency")),
          numericInput(ns("decimals"), "Decimal places", 1, min = 0, max = 5)
        )
      )
    )
  )
}

mod_visual_builder_server <- function(id, app_state, services = NULL, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    sap_bi_ensure_state(app_state)
    click_filter <- reactiveVal(NULL)
    visual_setting <- function(key, fallback) {
      if (!is.null(services) && !is.null(services$settings)) {
        return(services$settings$get(key, fallback))
      }
      fallback
    }

    observeEvent(app_state$settings, {
      decimals <- suppressWarnings(as.integer(visual_setting("decimal_places", 1L)))
      if (!is.finite(decimals)) decimals <- 1L
      updateNumericInput(session, "decimals", value = min(max(decimals, 0L), 5L))
    }, ignoreInit = FALSE)

    persist_dashboard <- function() {
      if (is.null(pool) || is.null(app_state$current_project) || !exists("db_upsert_studio_document", mode = "function")) return(invisible(FALSE))
      try(db_upsert_studio_document(
        pool, app_state$current_project, "dashboard", "Dashboard Studio",
        app_state$bi$dashboard, app_state$current_user
      ), silent = TRUE)
      invisible(TRUE)
    }

    observe({
      tables <- sap_bi_table_names(app_state)
      selected <- input$table %||% app_state$bi$active_table %||% sap_bi_first(tables, "")
      updateSelectInput(session, "table", choices = tables,
                        selected = if (selected %in% tables) selected else sap_bi_first(tables, ""))
    })

    current_df <- reactive({
      df <- sap_bi_get_table(app_state, input$table)
      if (!is.data.frame(df)) return(NULL)
      df <- sap_bi_apply_global_filters(app_state, input$table, df)
      if (nzchar(input$filter_field %||% "") && input$filter_field %in% names(df) && length(input$filter_values)) {
        df <- df[as.character(df[[input$filter_field]]) %in% as.character(input$filter_values), , drop = FALSE]
      }
      df
    })

    observeEvent(input$table, {
      df <- sap_bi_get_table(app_state, input$table)
      fields <- names(df) %||% character()
      numeric_fields <- if (is.data.frame(df)) fields[vapply(df, is.numeric, logical(1))] else character()
      updateSelectInput(session, "x", choices = c("None" = "", fields), selected = sap_bi_first(fields, ""))
      updateSelectInput(session, "y", choices = c("None / row count" = "", fields), selected = sap_bi_first(numeric_fields, ""))
      updateSelectInput(session, "color", choices = c("None" = "", fields), selected = "")
      updateSelectInput(session, "filter_field", choices = c("None" = "", fields), selected = "")
      measures <- app_state$bi$measures
      m <- if (nrow(measures)) measures[measures$table == input$table, , drop = FALSE] else measures
      updateSelectInput(session, "saved_measure", choices = c("Build from fields" = "", stats::setNames(m$id, m$name)), selected = "")
    }, ignoreInit = FALSE)

    observeEvent(input$filter_field, {
      df <- sap_bi_get_table(app_state, input$table)
      choices <- if (is.data.frame(df) && input$filter_field %in% names(df)) {
        values <- unique(as.character(df[[input$filter_field]])); values <- values[!is.na(values)]
        utils::head(sort(values), 1000L)
      } else character()
      updateSelectizeInput(session, "filter_values", choices = choices, selected = character(), server = TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$saved_measure, {
      if (!nzchar(input$saved_measure %||% "")) return()
      m <- app_state$bi$measures
      row <- m[m$id == input$saved_measure, , drop = FALSE]
      if (!nrow(row)) return()
      updateSelectInput(session, "aggregation", selected = row$aggregation[[1]])
      updateSelectInput(session, "y", selected = row$column[[1]])
      updateSelectInput(session, "number_format", selected = row$format[[1]])
      if (!nzchar(input$title %||% "") || identical(input$title, "New insight")) updateTextInput(session, "title", value = row$name[[1]])
    })

    output$field_list <- renderUI({
      df <- sap_bi_get_table(app_state, input$table)
      schema <- sap_bi_schema(df)
      if (!nrow(schema)) return(p(class = "text-muted small", "Connect a model table first."))
      search <- tolower(trimws(input$field_search %||% ""))
      if (nzchar(search)) schema <- schema[grepl(search, tolower(schema$field), fixed = TRUE), , drop = FALSE]
      if (!nrow(schema)) return(p(class = "text-muted small", "No matching fields."))
      tagList(lapply(seq_len(nrow(schema)), function(i) {
        r <- schema[i, ]
        div(class = "sap-field-item",
            icon(if (r$role == "Measure") "hashtag" else if (r$role == "Date") "calendar" else "font"),
            span(r$field), span(r$type, class = "small"))
      }))
    })

    visual_spec <- reactive({
      list(
        id = "builder_preview", title = input$title %||% "New insight",
        table = input$table %||% "", chart = input$chart %||% "bar",
        x = input$x %||% "", y = input$y %||% "", color = input$color %||% "",
        aggregation = input$aggregation %||% "count",
        measure_id = input$saved_measure %||% "",
        filter_field = input$filter_field %||% "", filter_values = input$filter_values %||% character(),
        top_n = input$top_n %||% 20L, cross_filter = isTRUE(input$cross_filter),
        show_legend = isTRUE(input$show_legend), show_labels = isTRUE(input$show_labels),
        sort = input$sort %||% "value_desc", number_format = input$number_format %||% "number",
        decimals = input$decimals %||% 1L,
        accent_color = as.character(visual_setting("accent_color", "blue"))
      )
    })

    plot_object <- reactive({
      spec <- visual_spec(); df <- current_df()
      req(is.data.frame(df), nrow(df) > 0)
      chart <- spec$chart
      x <- spec$x; y <- spec$y; color <- spec$color
      legend_label <- if (nzchar(color)) color else NULL
      base_theme <- ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_blank(),
          axis.title = ggplot2::element_text(color = "#64748b"),
          legend.position = if (isTRUE(spec$show_legend)) "bottom" else "none"
        )
      accent <- as.character(visual_setting("accent_color", "blue"))
      palette <- list(
        violet = c("#5b5bd6", "#14b8a6"),
        teal = c("#0f9f92", "#5b5bd6"),
        blue = c("#2563eb", "#14b8a6"),
        coral = c("#e85d75", "#5b5bd6")
      )
      colors <- palette[[accent]] %||% palette$blue
      primary <- colors[[1]]; secondary <- colors[[2]]
      if (chart %in% c("bar", "line", "area", "donut")) {
        req(nzchar(x), x %in% names(df))
        d <- sap_bi_summarise_visual(df, x, y, color, spec$aggregation, spec$top_n)
        if (spec$sort == "value_asc") d <- d[order(d$.sap_value), , drop = FALSE]
        if (spec$sort == "category") d <- d[order(as.character(d[[x]])), , drop = FALSE]
        d[[x]] <- factor(d[[x]], levels = unique(d[[x]]))
        if (chart == "bar") {
          p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data$.sap_value,
                                               fill = if (nzchar(color)) .data[[color]] else NULL,
                                               text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
            ggplot2::geom_col(fill = if (!nzchar(color)) primary else NULL, position = "dodge", width = .72) +
            ggplot2::labs(x = NULL, y = tools::toTitleCase(gsub("_", " ", spec$aggregation)), fill = legend_label) + base_theme +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (nrow(d) > 8) 35 else 0, hjust = if (nrow(d) > 8) 1 else .5))
          if (isTRUE(spec$show_labels) && !nzchar(color)) p <- p + ggplot2::geom_text(ggplot2::aes(label = round(.data$.sap_value, spec$decimals)), vjust = -.35, size = 3.2)
        } else if (chart == "line") {
          p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data$.sap_value,
                                               color = if (nzchar(color)) .data[[color]] else NULL,
                                               group = if (nzchar(color)) .data[[color]] else 1,
                                               text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
            ggplot2::geom_line(color = if (!nzchar(color)) primary else NULL, linewidth = 1.1) +
            ggplot2::geom_point(color = if (!nzchar(color)) primary else NULL, size = 2.2) +
            ggplot2::labs(x = NULL, y = tools::toTitleCase(gsub("_", " ", spec$aggregation)), color = legend_label) + base_theme
        } else if (chart == "area") {
          p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data$.sap_value,
                                               fill = if (nzchar(color)) .data[[color]] else NULL,
                                               group = if (nzchar(color)) .data[[color]] else 1,
                                               text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
            ggplot2::geom_area(fill = if (!nzchar(color)) primary else NULL, alpha = .76, position = "identity") +
            ggplot2::labs(x = NULL, y = tools::toTitleCase(gsub("_", " ", spec$aggregation)), fill = legend_label) + base_theme
        } else {
          p <- ggplot2::ggplot(d, ggplot2::aes(x = "", y = .data$.sap_value, fill = .data[[x]],
                                               text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
            ggplot2::geom_col(width = 1) + ggplot2::coord_polar(theta = "y") +
            ggplot2::labs(x = NULL, y = NULL, fill = x) + base_theme +
            ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
        }
      } else if (chart == "scatter") {
        req(nzchar(x), nzchar(y), x %in% names(df), y %in% names(df), is.numeric(df[[x]]), is.numeric(df[[y]]))
        d <- utils::head(df, 10000L)
        p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]],
                                             color = if (nzchar(color)) .data[[color]] else NULL,
                                             text = paste0(x, ": ", signif(.data[[x]], 6), "<br>", y, ": ", signif(.data[[y]], 6)))) +
          ggplot2::geom_point(color = if (!nzchar(color)) primary else NULL, alpha = .72, size = 2.2) +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = secondary, linewidth = .8, inherit.aes = FALSE,
                               ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
          ggplot2::labs(x = x, y = y, color = legend_label) + base_theme
      } else if (chart == "histogram") {
        req(nzchar(x), x %in% names(df), is.numeric(df[[x]]))
        p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], text = paste0(x, ": ", signif(.data[[x]], 6)))) +
          ggplot2::geom_histogram(fill = primary, color = "white", bins = min(40L, max(10L, floor(sqrt(nrow(df)))))) +
          ggplot2::labs(x = x, y = "Count") + base_theme
      } else if (chart == "box") {
        req(nzchar(x), nzchar(y), x %in% names(df), y %in% names(df), is.numeric(df[[y]]))
        p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = if (nzchar(color)) .data[[color]] else NULL)) +
          ggplot2::geom_boxplot(fill = if (!nzchar(color)) primary else NULL, alpha = .78, outlier.alpha = .35) +
          ggplot2::labs(x = NULL, y = y, fill = legend_label) + base_theme
      } else if (chart == "heatmap") {
        req(nzchar(x), nzchar(color), x %in% names(df), color %in% names(df))
        d <- sap_bi_summarise_visual(df, x, y, color, spec$aggregation, Inf)
        p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[color]], fill = .data$.sap_value,
                                             text = paste0(x, ": ", .data[[x]], "<br>", color, ": ", .data[[color]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
          ggplot2::geom_tile(color = "white", linewidth = .6) +
          ggplot2::scale_fill_gradient(low = "#e0e7ff", high = primary) +
          ggplot2::labs(x = NULL, y = NULL, fill = "Value") + base_theme
      } else return(NULL)
      p
    })

    output$plot <- renderPlotly({
      p <- plot_object(); req(!is.null(p))
      interactive_plot <- plotly::ggplotly(p, tooltip = "text", source = "builder") |>
        plotly::layout(margin = list(l = 55, r = 25, b = 70, t = 15),
                       hoverlabel = list(bgcolor = "#111827", font = list(color = "white")))
      interactive_plot <- plotly::event_register(interactive_plot, "plotly_click")
      plotly::config(interactive_plot, displaylogo = FALSE, responsive = TRUE,
                     modeBarButtonsToRemove = c("lasso2d", "select2d"))
    })

    output$table_visual <- renderDT({
      df <- current_df(); req(is.data.frame(df))
      fields <- unique(c(input$x, input$y, input$color)); fields <- fields[nzchar(fields) & fields %in% names(df)]
      if (!length(fields)) fields <- names(df)
      datatable(utils::head(df[, fields, drop = FALSE], 1000L), rownames = FALSE, filter = "top",
                options = list(pageLength = 12, scrollX = TRUE, deferRender = TRUE))
    })

    output$kpi_visual <- renderUI({
      df <- current_df(); req(is.data.frame(df))
      value <- sap_bi_measure_value(df, input$aggregation, input$y)
      div(class = "sap-big-kpi",
          div(class = "sap-big-kpi-icon", icon("chart-simple")),
          div(class = "sap-big-kpi-value", sap_bi_format_value(value, input$number_format, input$decimals)),
          div(class = "sap-big-kpi-label", input$title %||% "Measure"),
          div(class = "sap-big-kpi-context", paste(format(nrow(df), big.mark = ","), "filtered records")))
    })

    output$visual_canvas <- renderUI({
      df <- current_df()
      if (!is.data.frame(df)) return(sap_empty_state("No model table", "Connect data before building a visual.", "database"))
      if (!nrow(df)) return(sap_empty_state("No rows match", "Clear or change the visual filters.", "filter-circle-xmark"))
      chart <- input$chart %||% "bar"
      if (chart == "table") return(DTOutput(session$ns("table_visual")))
      if (chart == "kpi") return(uiOutput(session$ns("kpi_visual")))
      plotlyOutput(session$ns("plot"), height = "500px")
    })

    output$visual_header <- renderUI({
      div(class = "sap-visual-title-row",
          div(strong(input$title %||% "New insight"),
              span(paste(tools::toTitleCase(input$chart %||% "Visual"), "·", input$table %||% "No table"), class = "sap-visual-subtitle")),
          uiOutput(session$ns("filter_badges")))
    })

    output$filter_badges <- renderUI({
      filters <- app_state$bi$global_filters %||% list()
      own <- if (length(input$filter_values)) list(list(field = input$filter_field, values = input$filter_values)) else list()
      all <- c(filters, own)
      if (!length(all)) return(span("All records", class = "sap-filter-badge"))
      div(class = "sap-filter-badges", lapply(all, function(x) span(
        paste0(x$field, ": ", paste(utils::head(x$values, 3L), collapse = ", "), if (length(x$values) > 3L) "…" else ""),
        class = "sap-filter-badge is-active"
      )))
    })

    output$visual_footer <- renderUI({
      df <- current_df()
      if (!is.data.frame(df)) return(NULL)
      click <- click_filter()
      div(class = "sap-visual-foot",
          span(icon("database"), paste(format(nrow(df), big.mark = ","), "rows in current filter context")),
          if (!is.null(click)) span(icon("filter"), paste("Cross-filter:", click$field, "=", click$value))
      )
    })

    # Read the underlying Plotly Shiny input directly. event_data() schedules a
    # registration warning when this output is not currently rendered (for example
    # Table/KPI mode or an empty dataset), even though the interactive plot itself
    # correctly calls event_register(). The raw event input is the same payload that
    # event_data() parses after Plotly sends it to Shiny.
    builder_click_event <- shiny::reactive({
      raw <- session$rootScope()$input[["plotly_click-builder"]]
      if (is.null(raw)) return(NULL)
      if (is.character(raw) && length(raw) == 1L) {
        return(tryCatch(jsonlite::parse_json(raw, simplifyVector = TRUE), error = function(e) NULL))
      }
      raw
    })

    observeEvent(builder_click_event(), {
      if (!isTRUE(input$cross_filter) || !nzchar(input$x %||% "")) return()
      event <- builder_click_event()
      if (is.null(event)) return()
      if (is.list(event) && !is.data.frame(event)) {
        event <- tryCatch(as.data.frame(event, stringsAsFactors = FALSE), error = function(e) NULL)
      }
      if (is.null(event) || !is.data.frame(event) || nrow(event) < 1L) return()
      value <- sap_bi_first(event$x, sap_bi_first(event$key, NULL))
      if (is.null(value)) return()
      flt <- list(id = "builder_click", table = input$table, field = input$x, values = as.character(value), source = "visual_builder")
      model <- app_state$bi
      model$global_filters <- c(Filter(function(x) !identical(x$id, "builder_click"), model$global_filters %||% list()), list(flt))
      app_state$bi <- model
      click_filter(list(field = input$x, value = as.character(value)))
    }, ignoreInit = TRUE)

    observeEvent(input$clear_filter, {
      model <- app_state$bi
      model$global_filters <- list()
      app_state$bi <- model
      click_filter(NULL)
      updateSelectizeInput(session, "filter_values", selected = character())
    })

    observeEvent(input$save_visual, {
      if (!sap_bi_has_role(app_state, "editor")) {
        return(showNotification("The editor role is required to save shared dashboard visuals.",
                                type = "error", duration = 7))
      }
      spec <- visual_spec()
      df <- current_df()
      req(is.data.frame(df), nrow(df) > 0)
      if (!spec$chart %in% c("kpi", "table") && !nzchar(spec$x)) {
        showNotification("Choose an X/category field before saving this visual.", type = "warning")
        return()
      }
      model <- app_state$bi
      spec$id <- sap_bi_id("visual")
      spec$created_at <- as.character(Sys.time())
      spec$updated_at <- spec$created_at
      model$dashboard$visuals[[spec$id]] <- spec
      active_page <- model$dashboard$active_page %||% "page_overview"
      page_index <- match(active_page, vapply(model$dashboard$pages, function(x) x$id, character(1)))
      if (is.na(page_index)) page_index <- 1L
      model$dashboard$pages[[page_index]]$visual_ids <- unique(c(model$dashboard$pages[[page_index]]$visual_ids, spec$id))
      model$dashboard$updated_at <- as.character(Sys.time())
      app_state$bi <- model
      persist_dashboard()
      app_state$last_action <- paste("Visual saved:", spec$title)
      showNotification("Visual saved to the active dashboard page.", type = "message")
    })
  })
}

message("[SAP] Visual Builder v4.0.0 loaded")
