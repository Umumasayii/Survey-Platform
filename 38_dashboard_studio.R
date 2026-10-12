# =============================================================================
# MODULE 38: INTERACTIVE DASHBOARD STUDIO
# =============================================================================

mod_dashboard_studio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Dashboard",
      "Arrange saved visuals into report pages, apply page-wide filters, cross-filter by clicking marks, and preserve bookmarks.",
      "gauge-high"
    ),
    div(
      class = "sap-dashboard-toolbar",
      div(class = "sap-dashboard-pages", uiOutput(ns("page_tabs"))),
      div(class = "sap-dashboard-actions",
          actionButton(ns("add_page"), "New page", icon = icon("plus"), class = "btn-sm btn-outline-primary"),
          actionButton(ns("rename_page"), "Rename", icon = icon("pen"), class = "btn-sm btn-outline-secondary"),
          actionButton(ns("delete_page"), "Delete", icon = icon("trash"), class = "btn-sm btn-outline-danger"),
          actionButton(ns("present"), "Present", icon = icon("expand"), class = "btn-sm btn-primary"))
    ),
    layout_columns(
      col_widths = c(9, 3),
      div(
        class = "sap-dashboard-stage",
        uiOutput(ns("dashboard_canvas")),
        uiOutput(ns("drillthrough_panel"))
      ),
      div(
        class = "sap-dashboard-side",
        card(
          class = "sap-card",
          card_header(icon("filter"), " Page filters"),
          card_body(
            selectInput(ns("filter_table"), "Table", choices = character()),
            selectInput(ns("filter_field"), "Field", choices = character()),
            selectizeInput(ns("filter_values"), "Values", choices = character(), multiple = TRUE),
            actionButton(ns("apply_filter"), "Apply filter", icon = icon("filter"), class = "btn-primary w-100"),
            uiOutput(ns("active_filters")),
            actionButton(ns("clear_filters"), "Clear all filters", icon = icon("filter-circle-xmark"), class = "btn-sm btn-outline-secondary w-100")
          )
        ),
        card(
          class = "sap-card",
          card_header(icon("layer-group"), " Visuals on page"),
          card_body(
            selectInput(ns("selected_visual"), "Selected visual", choices = character()),
            div(class = "d-grid gap-2",
                actionButton(ns("move_visual_up"), "Move earlier", icon = icon("arrow-up"), class = "btn-sm btn-outline-secondary"),
                actionButton(ns("move_visual_down"), "Move later", icon = icon("arrow-down"), class = "btn-sm btn-outline-secondary"),
                actionButton(ns("remove_visual"), "Remove from page", icon = icon("xmark"), class = "btn-sm btn-outline-danger"))
          )
        ),
        card(
          class = "sap-card",
          card_header(icon("pen-to-square"), " Edit selected item"),
          card_body(uiOutput(ns("visual_editor")))
        ),
        card(
          class = "sap-card",
          card_header(icon("note-sticky"), " Dashboard explanation"),
          card_body(
            textAreaInput(ns("page_note"), "Page note", rows = 4, placeholder = "Explain what this dashboard page shows…"),
            actionButton(ns("save_page_note"), "Save page note", icon = icon("floppy-disk"), class = "btn-sm btn-outline-primary w-100")
          )
        ),
        card(
          class = "sap-card",
          card_header(icon("bookmark"), " Filter bookmarks"),
          card_body(
            textInput(ns("bookmark_name"), "Bookmark name", placeholder = "e.g. Women-headed households"),
            actionButton(ns("save_bookmark"), "Save current view", icon = icon("bookmark"), class = "btn-sm btn-outline-primary w-100"),
            uiOutput(ns("bookmark_list"))
          )
        )
      )
    ),
    div(class = "sap-presentation-exit", actionButton(ns("exit_present"), "Exit presentation", icon = icon("compress"), class = "btn-light"))
  )
}

sap_dashboard_plot <- function(spec, df) {
  chart <- spec$chart %||% "bar"; x <- spec$x %||% ""; y <- spec$y %||% ""; color <- spec$color %||% ""
  legend_label <- if (nzchar(color)) color else NULL
  palette <- list(
    violet = c("#5b5bd6", "#14b8a6"), teal = c("#0f9f92", "#5b5bd6"),
    blue = c("#2563eb", "#14b8a6"), coral = c("#e85d75", "#5b5bd6")
  )
  colors <- palette[[as.character(spec$accent_color %||% "blue")]] %||% palette$blue
  primary <- colors[[1]]; secondary <- colors[[2]]
  base_theme <- ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(color = "#64748b"),
    legend.position = if (isTRUE(spec$show_legend %||% TRUE)) "bottom" else "none"
  )
  if (chart %in% c("bar", "line", "area", "donut")) {
    if (!nzchar(x) || !x %in% names(df)) return(NULL)
    d <- sap_bi_summarise_visual(df, x, y, color, spec$aggregation %||% "count", spec$top_n %||% 20L)
    if (!nrow(d)) return(NULL)
    if (identical(spec$sort, "value_asc")) d <- d[order(d$.sap_value), , drop = FALSE]
    if (identical(spec$sort, "category")) d <- d[order(as.character(d[[x]])), , drop = FALSE]
    d[[x]] <- factor(d[[x]], levels = unique(d[[x]]))
    if (chart == "bar") {
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data$.sap_value,
                                           fill = if (nzchar(color)) .data[[color]] else NULL,
                                           text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
        ggplot2::geom_col(fill = if (!nzchar(color)) primary else NULL, position = "dodge", width = .72) +
        ggplot2::labs(x = NULL, y = tools::toTitleCase(gsub("_", " ", spec$aggregation %||% "count")), fill = legend_label) + base_theme +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (nrow(d) > 8) 35 else 0, hjust = if (nrow(d) > 8) 1 else .5))
      if (isTRUE(spec$show_labels) && !nzchar(color)) p <- p + ggplot2::geom_text(ggplot2::aes(label = round(.data$.sap_value, spec$decimals %||% 1L)), vjust = -.35, size = 3)
      return(p)
    }
    if (chart == "line") return(
      ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data$.sap_value,
                                      color = if (nzchar(color)) .data[[color]] else NULL,
                                      group = if (nzchar(color)) .data[[color]] else 1,
                                      text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
        ggplot2::geom_line(color = if (!nzchar(color)) primary else NULL, linewidth = 1.05) +
        ggplot2::geom_point(color = if (!nzchar(color)) primary else NULL, size = 2) +
        ggplot2::labs(x = NULL, y = tools::toTitleCase(gsub("_", " ", spec$aggregation %||% "count")), color = legend_label) + base_theme
    )
    if (chart == "area") return(
      ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data$.sap_value,
                                      fill = if (nzchar(color)) .data[[color]] else NULL,
                                      group = if (nzchar(color)) .data[[color]] else 1,
                                      text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
        ggplot2::geom_area(fill = if (!nzchar(color)) primary else NULL, alpha = .75, position = "identity") +
        ggplot2::labs(x = NULL, y = tools::toTitleCase(gsub("_", " ", spec$aggregation %||% "count")), fill = legend_label) + base_theme
    )
    return(
      ggplot2::ggplot(d, ggplot2::aes(x = "", y = .data$.sap_value, fill = .data[[x]],
                                      text = paste0(x, ": ", .data[[x]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
        ggplot2::geom_col(width = 1) + ggplot2::coord_polar(theta = "y") +
        ggplot2::labs(x = NULL, y = NULL, fill = x) + base_theme +
        ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
    )
  }
  if (chart == "scatter" && x %in% names(df) && y %in% names(df) && is.numeric(df[[x]]) && is.numeric(df[[y]])) {
    d <- utils::head(df, 10000L)
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]],
                                           color = if (nzchar(color)) .data[[color]] else NULL,
                                           text = paste0(x, ": ", signif(.data[[x]], 6), "<br>", y, ": ", signif(.data[[y]], 6)))) +
             ggplot2::geom_point(color = if (!nzchar(color)) primary else NULL, alpha = .72, size = 2) +
             ggplot2::labs(x = x, y = y, color = legend_label) + base_theme)
  }
  if (chart == "histogram" && x %in% names(df) && is.numeric(df[[x]])) {
    return(ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], text = paste0(x, ": ", signif(.data[[x]], 6)))) +
             ggplot2::geom_histogram(fill = primary, color = "white", bins = min(40L, max(10L, floor(sqrt(nrow(df)))))) +
             ggplot2::labs(x = x, y = "Count") + base_theme)
  }
  if (chart == "box" && x %in% names(df) && y %in% names(df) && is.numeric(df[[y]])) {
    return(ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = if (nzchar(color)) .data[[color]] else NULL)) +
             ggplot2::geom_boxplot(fill = if (!nzchar(color)) primary else NULL, alpha = .78, outlier.alpha = .35) +
             ggplot2::labs(x = NULL, y = y, fill = legend_label) + base_theme)
  }
  if (chart == "heatmap" && x %in% names(df) && color %in% names(df)) {
    d <- sap_bi_summarise_visual(df, x, y, color, spec$aggregation %||% "count", Inf)
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[color]], fill = .data$.sap_value,
                                           text = paste0(x, ": ", .data[[x]], "<br>", color, ": ", .data[[color]], "<br>Value: ", signif(.data$.sap_value, 6)))) +
             ggplot2::geom_tile(color = "white", linewidth = .6) +
             ggplot2::scale_fill_gradient(low = "#e0e7ff", high = primary) +
             ggplot2::labs(x = NULL, y = NULL, fill = "Value") + base_theme)
  }
  NULL
}

mod_dashboard_studio_server <- function(id, app_state, services = NULL, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    sap_bi_ensure_state(app_state)
    drillthrough <- reactiveVal(NULL)
    registered_sources <- reactiveVal(character())
    registered_pages <- reactiveVal(character())
    registered_bookmarks <- reactiveVal(character())
    require_editor <- function(action = "edit the shared dashboard") {
      if (sap_bi_has_role(app_state, "editor")) return(TRUE)
      showNotification(sprintf("The editor role is required to %s.", action),
                       type = "error", duration = 7)
      FALSE
    }

    persist <- function() {
      if (is.null(pool) || is.null(app_state$current_project) || !exists("db_upsert_studio_document", mode = "function")) return(invisible(FALSE))
      try(db_upsert_studio_document(pool, app_state$current_project, "dashboard", "Dashboard Studio",
                                    app_state$bi$dashboard, app_state$current_user), silent = TRUE)
      invisible(TRUE)
    }

    active_page <- reactive({
      dash <- app_state$bi$dashboard
      id <- dash$active_page %||% dash$pages[[1]]$id
      idx <- match(id, vapply(dash$pages, function(x) x$id, character(1)))
      if (is.na(idx)) idx <- 1L
      dash$pages[[idx]]
    })

    observe({
      tables <- sap_bi_table_names(app_state)
      updateSelectInput(session, "filter_table", choices = tables,
                        selected = input$filter_table %||% app_state$bi$active_table %||% sap_bi_first(tables, ""))
      page <- active_page()
      visuals <- app_state$bi$dashboard$visuals
      ids <- page$visual_ids %||% character()
      labels <- vapply(ids, function(x) visuals[[x]]$title %||% x, character(1))
      updateSelectInput(session, "selected_visual", choices = stats::setNames(ids, labels), selected = input$selected_visual %||% sap_bi_first(ids, ""))
    })

    observeEvent(input$filter_table, {
      df <- sap_bi_get_table(app_state, input$filter_table)
      updateSelectInput(session, "filter_field", choices = names(df) %||% character())
    }, ignoreInit = FALSE)
    observeEvent(input$filter_field, {
      df <- sap_bi_get_table(app_state, input$filter_table)
      choices <- if (is.data.frame(df) && input$filter_field %in% names(df)) {
        values <- sort(unique(as.character(df[[input$filter_field]]))); values[!is.na(values)]
      } else character()
      updateSelectizeInput(session, "filter_values", choices = utils::head(choices, 2000L), selected = character(), server = TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$apply_filter, {
      req(nzchar(input$filter_table %||% ""), nzchar(input$filter_field %||% ""), length(input$filter_values))
      id <- paste(input$filter_table, input$filter_field, sep = "::")
      flt <- list(id = id, table = input$filter_table, field = input$filter_field,
                  values = input$filter_values, source = "dashboard_filter")
      model <- app_state$bi
      model$global_filters <- c(Filter(function(x) !identical(x$id, id), model$global_filters %||% list()), list(flt))
      app_state$bi <- model
    })
    observeEvent(input$clear_filters, {
      model <- app_state$bi; model$global_filters <- list(); app_state$bi <- model
      drillthrough(NULL)
      updateSelectizeInput(session, "filter_values", selected = character())
    })

    output$page_tabs <- renderUI({
      dash <- app_state$bi$dashboard
      div(class = "sap-page-pills", lapply(dash$pages, function(page) actionButton(
        session$ns(paste0("page_", page$id)), page$name,
        icon = icon(if (identical(page$id, dash$active_page)) "circle-dot" else "circle"),
        class = paste("btn-sm sap-page-pill", if (identical(page$id, dash$active_page)) "is-active" else "")
      )))
    })

    observe({
      pages <- app_state$bi$dashboard$pages
      pending <- Filter(function(page) !page$id %in% registered_pages(), pages)
      lapply(pending, function(page) local({
        p <- page
        observeEvent(input[[paste0("page_", p$id)]], {
          model <- app_state$bi; model$dashboard$active_page <- p$id; app_state$bi <- model
          drillthrough(NULL)
        }, ignoreInit = TRUE, once = FALSE)
      }))
      if (length(pending)) {
        registered_pages(unique(c(registered_pages(), vapply(pending, function(x) x$id, character(1)))))
      }
    })

    observeEvent(input$add_page, {
      if (!require_editor("create shared dashboard pages")) return()
      showModal(modalDialog(
        title = "Create dashboard page", textInput(session$ns("new_page_name"), "Page name", value = "New page"),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_add_page"), "Create page", class = "btn-primary"))
      ))
    })
    observeEvent(input$confirm_add_page, {
      if (!require_editor("create shared dashboard pages")) { removeModal(); return() }
      req(nzchar(trimws(input$new_page_name %||% "")))
      model <- app_state$bi; id <- sap_bi_id("page")
      model$dashboard$pages <- c(model$dashboard$pages, list(list(id = id, name = sap_bi_clean_name(input$new_page_name, "Page"), visual_ids = character())))
      model$dashboard$active_page <- id; app_state$bi <- model; persist(); removeModal()
    })

    observeEvent(input$rename_page, {
      if (!require_editor("rename shared dashboard pages")) return()
      showModal(modalDialog(
        title = "Rename dashboard page", textInput(session$ns("rename_page_value"), "Page name", value = active_page()$name),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_rename_page"), "Rename", class = "btn-primary"))
      ))
    })
    observeEvent(input$confirm_rename_page, {
      if (!require_editor("rename shared dashboard pages")) { removeModal(); return() }
      req(nzchar(trimws(input$rename_page_value %||% "")))
      model <- app_state$bi; idx <- match(model$dashboard$active_page, vapply(model$dashboard$pages, function(x) x$id, character(1)))
      model$dashboard$pages[[idx]]$name <- sap_bi_clean_name(input$rename_page_value, "Page")
      app_state$bi <- model; persist(); removeModal()
    })

    observeEvent(input$delete_page, {
      if (!require_editor("delete shared dashboard pages")) return()
      if (length(app_state$bi$dashboard$pages) <= 1L) return(showNotification("A dashboard must keep at least one page.", type = "warning"))
      showModal(modalDialog(title = "Delete this page?", p("Visual definitions remain in the library, but this page layout will be removed."),
                            footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_delete_page"), "Delete page", class = "btn-danger"))))
    })
    observeEvent(input$confirm_delete_page, {
      if (!require_editor("delete shared dashboard pages")) { removeModal(); return() }
      model <- app_state$bi; current <- model$dashboard$active_page
      model$dashboard$pages <- Filter(function(x) !identical(x$id, current), model$dashboard$pages)
      model$dashboard$active_page <- model$dashboard$pages[[1]]$id
      app_state$bi <- model; persist(); removeModal(); drillthrough(NULL)
    })

    move_visual <- function(direction) {
      if (!require_editor("reorder shared dashboard visuals")) return(FALSE)
      id <- input$selected_visual; page <- active_page(); ids <- page$visual_ids %||% character()
      pos <- match(id, ids); target <- pos + direction
      if (is.na(pos) || target < 1L || target > length(ids)) return(FALSE)
      ids[c(pos, target)] <- ids[c(target, pos)]
      model <- app_state$bi; idx <- match(page$id, vapply(model$dashboard$pages, function(x) x$id, character(1)))
      model$dashboard$pages[[idx]]$visual_ids <- ids; app_state$bi <- model; persist(); TRUE
    }
    observeEvent(input$move_visual_up, move_visual(-1L))
    observeEvent(input$move_visual_down, move_visual(1L))
    observeEvent(input$remove_visual, {
      if (!require_editor("remove shared dashboard visuals")) return()
      id <- input$selected_visual; if (!nzchar(id %||% "")) return()
      model <- app_state$bi; page <- active_page(); idx <- match(page$id, vapply(model$dashboard$pages, function(x) x$id, character(1)))
      model$dashboard$pages[[idx]]$visual_ids <- setdiff(page$visual_ids, id)
      app_state$bi <- model; persist()
    })

    observeEvent(input$present, shinyjs::runjs("document.body.classList.add('sap-presenting');"))
    observeEvent(input$exit_present, shinyjs::runjs("document.body.classList.remove('sap-presenting');"))

    observeEvent(input$save_bookmark, {
      if (!require_editor("save shared dashboard bookmarks")) return()
      req(nzchar(trimws(input$bookmark_name %||% "")))
      model <- app_state$bi
      bookmark <- list(id = sap_bi_id("bookmark"), name = sap_bi_clean_name(input$bookmark_name, "Bookmark"),
                       filters = model$global_filters %||% list(), page = model$dashboard$active_page,
                       created_at = as.character(Sys.time()))
      model$dashboard$bookmarks <- c(model$dashboard$bookmarks %||% list(), list(bookmark))
      app_state$bi <- model; persist(); updateTextInput(session, "bookmark_name", value = "")
    })

    output$bookmark_list <- renderUI({
      bookmarks <- app_state$bi$dashboard$bookmarks %||% list()
      if (!length(bookmarks)) return(p(class = "small text-muted mt-2 mb-0", "No saved views."))
      div(class = "sap-bookmark-list", lapply(bookmarks, function(x) actionButton(
        session$ns(paste0("bookmark_", x$id)), x$name, icon = icon("bookmark"), class = "btn-sm btn-ghost w-100 text-start"
      )))
    })

    observe({
      bookmarks <- app_state$bi$dashboard$bookmarks %||% list()
      pending <- Filter(function(item) !item$id %in% registered_bookmarks(), bookmarks)
      lapply(pending, function(item) local({
        b <- item
        observeEvent(input[[paste0("bookmark_", b$id)]], {
          model <- app_state$bi; model$global_filters <- b$filters %||% list()
          if (b$page %in% vapply(model$dashboard$pages, function(x) x$id, character(1))) model$dashboard$active_page <- b$page
          app_state$bi <- model
        }, ignoreInit = TRUE)
      }))
      if (length(pending)) {
        registered_bookmarks(unique(c(registered_bookmarks(), vapply(pending, function(x) x$id, character(1)))))
      }
    })

    output$active_filters <- renderUI({
      filters <- app_state$bi$global_filters %||% list()
      if (!length(filters)) return(p(class = "small text-muted mt-2", "No page filters."))
      div(class = "sap-filter-stack", lapply(filters, function(x) div(
        class = "sap-filter-summary", strong(x$field),
        span(paste(utils::head(x$values, 4L), collapse = ", "), if (length(x$values) > 4L) "…" else "")
      )))
    })

    visual_df <- function(spec) {
      df <- if (identical(spec$table %||% "", "__processed_data__")) app_state$processed_data else sap_bi_get_table(app_state, spec$table)
      if (!is.data.frame(df)) return(NULL)
      df <- sap_bi_apply_global_filters(app_state, spec$table, df)
      if (nzchar(spec$filter_field %||% "") && spec$filter_field %in% names(df) && length(spec$filter_values)) {
        df <- df[as.character(df[[spec$filter_field]]) %in% as.character(spec$filter_values), , drop = FALSE]
      }
      df
    }

    observe({
      page <- active_page()
      updateTextAreaInput(session, "page_note", value = page$note %||% "")
    })

    output$visual_editor <- renderUI({
      id <- input$selected_visual %||% ""
      spec <- app_state$bi$dashboard$visuals[[id]]
      if (!nzchar(id) || is.null(spec)) return(p(class = "small text-muted mb-0", "Select an item to edit its dashboard properties."))
      ns <- session$ns
      df <- visual_df(spec)
      fields <- names(df) %||% character()
      numeric_fields <- if (is.data.frame(df)) fields[vapply(df, is.numeric, logical(1))] else character()
      common <- tagList(
        textInput(ns("edit_title"), "Title", value = spec$title %||% "Dashboard item"),
        selectInput(ns("edit_width"), "Width / size", choices = c("Standard" = "standard", "Wide" = "wide", "Full width" = "full"), selected = spec$width %||% "standard")
      )
      if (identical(spec$chart %||% "", "capture")) {
        capture_kind <- spec$capture$kind %||% "output"
        tagList(common,
          div(class = "alert alert-light border small", paste("Captured", capture_kind, "output. The saved content is preserved; edit its title, size, and explanation here.")),
          textAreaInput(ns("edit_note"), "Item explanation / note", rows = 3, value = spec$note %||% ""),
          actionButton(ns("save_visual_changes"), "Save item changes", icon = icon("floppy-disk"), class = "btn-sm btn-primary w-100")
        )
      } else if (identical(spec$chart %||% "", "map")) {
        tagList(common,
          selectInput(ns("edit_basemap"), "Basemap / map type", choices = .sap_geo_basemaps, selected = spec$basemap %||% "road"),
          selectInput(ns("edit_latitude"), "Latitude field", choices = numeric_fields, selected = spec$latitude %||% ""),
          selectInput(ns("edit_longitude"), "Longitude field", choices = numeric_fields, selected = spec$longitude %||% ""),
          selectInput(ns("edit_mapped_variable"), "Mapped variable", choices = c("Respondent count" = "", fields), selected = spec$mapped_variable %||% ""),
          selectInput(ns("edit_map_style"), "Map visualization", choices = c("Points" = "points", "Proportional / bubble symbols" = "bubble", "Density / heat" = "heat"), selected = spec$map_style %||% "points"),
          textAreaInput(ns("edit_note"), "Item explanation / note", rows = 3, value = spec$note %||% ""),
          actionButton(ns("save_visual_changes"), "Save item changes", icon = icon("floppy-disk"), class = "btn-sm btn-primary w-100")
        )
      } else {
        tagList(common,
          selectInput(ns("edit_chart"), "Chart type", choices = c("Bar"="bar","Line"="line","Area"="area","Scatter"="scatter","Histogram"="histogram","Box"="box","Donut"="donut","Heatmap"="heatmap","KPI"="kpi","Table"="table"), selected = spec$chart %||% "bar"),
          selectInput(ns("edit_x"), "X / category variable", choices = c("None"="", fields), selected = spec$x %||% ""),
          selectInput(ns("edit_y"), "Y / value variable", choices = c("None / row count"="", fields), selected = spec$y %||% ""),
          selectInput(ns("edit_color"), "Series / group variable", choices = c("None"="", fields), selected = spec$color %||% ""),
          selectInput(ns("edit_aggregation"), "Aggregation method", choices = c("Count"="count","Distinct count"="distinct_count","Sum"="sum","Average"="mean","Median"="median","Minimum"="min","Maximum"="max"), selected = spec$aggregation %||% "count"),
          numericInput(ns("edit_top_n"), "Number of categories displayed", value = spec$top_n %||% 20L, min = 3, max = 100),
          checkboxInput(ns("edit_show_labels"), "Show labels", value = isTRUE(spec$show_labels)),
          checkboxInput(ns("edit_show_legend"), "Show legend", value = isTRUE(spec$show_legend %||% TRUE)),
          textAreaInput(ns("edit_note"), "Item explanation / note", rows = 3, value = spec$note %||% ""),
          actionButton(ns("save_visual_changes"), "Save item changes", icon = icon("floppy-disk"), class = "btn-sm btn-primary w-100")
        )
      }
    })

    observeEvent(input$save_page_note, {
      if (!require_editor("edit dashboard explanations")) return()
      model <- app_state$bi; page <- active_page()
      idx <- match(page$id, vapply(model$dashboard$pages, function(x) x$id, character(1)))
      if (is.na(idx)) return()
      model$dashboard$pages[[idx]]$note <- trimws(input$page_note %||% "")
      model$dashboard$updated_at <- as.character(Sys.time())
      app_state$bi <- model; persist()
      showNotification("Dashboard explanation saved.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$save_visual_changes, {
      if (!require_editor("edit dashboard items")) return()
      id <- input$selected_visual %||% ""; req(nzchar(id))
      model <- app_state$bi; spec <- model$dashboard$visuals[[id]]; req(!is.null(spec))
      spec$title <- sap_bi_clean_name(input$edit_title, spec$title %||% "Dashboard item")
      spec$width <- input$edit_width %||% "standard"
      spec$note <- trimws(input$edit_note %||% "")
      if (identical(spec$chart %||% "", "capture")) {
        # Captured outputs preserve their stored payload; only common dashboard properties are editable.
      } else if (identical(spec$chart %||% "", "map")) {
        spec$basemap <- input$edit_basemap %||% "road"
        spec$latitude <- input$edit_latitude %||% ""
        spec$longitude <- input$edit_longitude %||% ""
        spec$mapped_variable <- input$edit_mapped_variable %||% ""
        spec$map_style <- input$edit_map_style %||% "points"
      } else {
        spec$chart <- input$edit_chart %||% spec$chart
        spec$x <- input$edit_x %||% ""; spec$y <- input$edit_y %||% ""; spec$color <- input$edit_color %||% ""
        spec$aggregation <- input$edit_aggregation %||% "count"
        spec$top_n <- input$edit_top_n %||% 20L
        spec$show_labels <- isTRUE(input$edit_show_labels); spec$show_legend <- isTRUE(input$edit_show_legend)
      }
      spec$updated_at <- as.character(Sys.time())
      model$dashboard$visuals[[id]] <- spec; model$dashboard$updated_at <- as.character(Sys.time())
      app_state$bi <- model; persist()
      showNotification("Dashboard item updated.", type = "message")
    }, ignoreInit = TRUE)

    observe({
      page <- active_page(); visuals <- app_state$bi$dashboard$visuals
      ids <- page$visual_ids %||% character()
      for (id in ids) local({
        vid <- id
        output[[paste0("plot_", vid)]] <- renderPlotly({
          spec <- app_state$bi$dashboard$visuals[[vid]]; req(!is.null(spec))
          df <- visual_df(spec); req(is.data.frame(df), nrow(df) > 0)
          p <- sap_dashboard_plot(spec, df); req(!is.null(p))
          interactive_plot <- plotly::ggplotly(p, tooltip = "text", source = vid) |>
            plotly::layout(margin = list(l = 50, r = 20, b = 65, t = 10), hoverlabel = list(bgcolor = "#111827", font = list(color = "white"))) |>
            plotly::config(displaylogo = FALSE, responsive = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
          plotly::event_register(interactive_plot, "plotly_click")
        })
        output[[paste0("map_", vid)]] <- renderPlotly({
          spec <- app_state$bi$dashboard$visuals[[vid]]; req(!is.null(spec), identical(spec$chart %||% "", "map"))
          df <- visual_df(spec); req(is.data.frame(df), spec$latitude %in% names(df), spec$longitude %in% names(df))
          mapped <- spec$mapped_variable %||% ""
          variable_numeric <- nzchar(mapped) && mapped %in% names(df) && is.numeric(df[[mapped]])
          val <- if (nzchar(mapped) && mapped %in% names(df)) df[[mapped]] else rep(1, nrow(df))
          coords <- data.frame(lat = suppressWarnings(as.numeric(df[[spec$latitude]])), lon = suppressWarnings(as.numeric(df[[spec$longitude]])),
                               value = as.character(val), numeric_value = if (variable_numeric) suppressWarnings(as.numeric(val)) else 1, stringsAsFactors = FALSE)
          coords <- coords[is.finite(coords$lat) & is.finite(coords$lon) & abs(coords$lat) <= 90 & abs(coords$lon) <= 180, , drop = FALSE]
          if (isTRUE(spec$jitter) && nrow(coords)) { set.seed(20260815L); coords$lat <- jitter(coords$lat, amount=.002); coords$lon <- jitter(coords$lon, amount=.002) }
          coords$label <- paste0(if (nzchar(mapped)) paste0(mapped, ": ") else "Respondent: ", coords$value)
          coords <- utils::head(coords, 15000L)
          req(nrow(coords) > 0, exists("sap_geospatial_plotly", mode = "function"))
          sap_geospatial_plotly(coords, spec$basemap %||% "road", spec$map_style %||% "points", variable_numeric)
        })
        output[[paste0("capture_table_", vid)]] <- renderDT({
          spec <- app_state$bi$dashboard$visuals[[vid]]; req(!is.null(spec), identical(spec$chart %||% "", "capture"), identical(spec$capture$kind %||% "", "table"))
          dat <- spec$capture$data
          if (!is.data.frame(dat)) dat <- tryCatch(as.data.frame(dat, stringsAsFactors = FALSE), error = function(e) NULL)
          req(is.data.frame(dat))
          datatable(dat, rownames = FALSE, options = list(dom = "tip", pageLength = 8, scrollX = TRUE, deferRender = TRUE))
        })
        output[[paste0("capture_ui_", vid)]] <- renderUI({
          spec <- app_state$bi$dashboard$visuals[[vid]]; req(!is.null(spec), identical(spec$chart %||% "", "capture"))
          cap <- spec$capture %||% list(); kind <- cap$kind %||% "text"
          if (identical(kind, "plot") && nzchar(cap$data_uri %||% "")) {
            return(tags$img(src = cap$data_uri, alt = spec$title %||% "Dashboard plot", style = "display:block;max-width:100%;height:auto;margin:auto;"))
          }
          txt <- paste(as.character(cap$data %||% ""), collapse = "\n")
          if (identical(kind, "verbatim")) tags$pre(class = "mb-0 small", txt) else div(class = "sap-dashboard-captured-text", style = "white-space:pre-wrap;", txt)
        })
        output[[paste0("table_", vid)]] <- renderDT({
          spec <- app_state$bi$dashboard$visuals[[vid]]; req(!is.null(spec))
          df <- visual_df(spec); req(is.data.frame(df))
          fields <- unique(c(spec$x, spec$y, spec$color)); fields <- fields[nzchar(fields) & fields %in% names(df)]
          if (!length(fields)) fields <- names(df)
          datatable(utils::head(df[, fields, drop = FALSE], 1000L), rownames = FALSE,
                    options = list(dom = "tip", pageLength = 8, scrollX = TRUE, deferRender = TRUE))
        })
        output[[paste0("kpi_", vid)]] <- renderUI({
          spec <- app_state$bi$dashboard$visuals[[vid]]; req(!is.null(spec))
          df <- visual_df(spec); req(is.data.frame(df))
          value <- sap_bi_measure_value(df, spec$aggregation, spec$y)
          div(class = "sap-dashboard-kpi",
              div(class = "sap-dashboard-kpi-value", sap_bi_format_value(value, spec$number_format, spec$decimals)),
              div(class = "sap-dashboard-kpi-context", paste(format(nrow(df), big.mark = ","), "records")))
        })
        if (!vid %in% registered_sources()) {
          observeEvent(plotly::event_data("plotly_click", source = vid), {
            spec <- app_state$bi$dashboard$visuals[[vid]]
            if (is.null(spec) || !isTRUE(spec$cross_filter) || !nzchar(spec$x %||% "")) return()
            event <- plotly::event_data("plotly_click", source = vid)
            if (is.null(event) || !nrow(event)) return()
            value <- sap_bi_first(event$x, sap_bi_first(event$key, NULL))
            if (is.null(value)) return()
            id <- paste0("cross::", vid)
            flt <- list(id = id, table = spec$table, field = spec$x, values = as.character(value), source = vid)
            model <- app_state$bi
            model$global_filters <- c(Filter(function(x) !identical(x$id, id), model$global_filters %||% list()), list(flt))
            app_state$bi <- model
            drillthrough(list(table = spec$table, field = spec$x, value = as.character(value), source = spec$title))
          }, ignoreInit = TRUE)
          registered_sources(unique(c(registered_sources(), vid)))
        }
      })
    })

    output$dashboard_canvas <- renderUI({
      page <- active_page(); visuals <- app_state$bi$dashboard$visuals
      ids <- page$visual_ids %||% character()
      if (!length(ids)) return(div(class = "sap-dashboard-empty",
                                   div(class = "sap-empty-orbit", icon("chart-column")),
                                   h3("This page is ready for insight"),
                                   p("Open Explore, build a visual, and choose Save to dashboard.")))
      tagList(
        if (nzchar(trimws(page$note %||% ""))) div(class = "alert alert-light border mb-3", icon("note-sticky"), " ", page$note),
        div(class = "sap-dashboard-grid", lapply(ids, function(id) {
        spec <- visuals[[id]]
        if (is.null(spec)) return(NULL)
        content <- if (identical(spec$chart, "kpi")) uiOutput(session$ns(paste0("kpi_", id)))
                   else if (identical(spec$chart, "table")) DTOutput(session$ns(paste0("table_", id)))
                   else if (identical(spec$chart, "map")) plotlyOutput(session$ns(paste0("map_", id)), height = "360px")
                   else if (identical(spec$chart, "capture") && identical(spec$capture$kind %||% "", "table")) DTOutput(session$ns(paste0("capture_table_", id)))
                   else if (identical(spec$chart, "capture")) uiOutput(session$ns(paste0("capture_ui_", id)))
                   else plotlyOutput(session$ns(paste0("plot_", id)), height = "340px")
        width_style <- switch(spec$width %||% "standard", wide = "grid-column: span 2;", full = "grid-column: 1 / -1;", NULL)
        div(
          class = paste("sap-dashboard-visual", if (identical(spec$chart, "kpi")) "is-kpi" else ""), style = width_style,
          div(class = "sap-dashboard-visual-head",
              div(strong(spec$title), span(paste(
                if (identical(spec$chart, "capture")) paste("Captured", tools::toTitleCase(spec$capture$kind %||% "output")) else tools::toTitleCase(spec$chart),
                "·", if (identical(spec$table, "__processed_data__")) "Active dataset" else if (identical(spec$table, "__capture__")) "Analysis output" else spec$table))),
              span(icon("up-down-left-right"), class = "sap-visual-handle")),
          div(class = "sap-dashboard-visual-body", content),
          if (nzchar(trimws(spec$note %||% ""))) div(class = "small text-muted px-3 pb-3", icon("note-sticky"), " ", spec$note)
        )
      }))
      )
    })

    output$drillthrough_panel <- renderUI({
      detail <- drillthrough()
      if (is.null(detail)) return(NULL)
      div(class = "sap-drillthrough-panel",
          div(class = "sap-drillthrough-head",
              div(icon("magnifying-glass-plus"), strong(paste("Drillthrough:", detail$field, "=", detail$value)),
                  span(paste("Selected from", detail$source), class = "small text-muted")),
              actionButton(session$ns("close_drillthrough"), "Close", icon = icon("xmark"), class = "btn-sm btn-ghost")),
          DTOutput(session$ns("drillthrough_table")))
    })
    observeEvent(input$close_drillthrough, drillthrough(NULL))
    output$drillthrough_table <- renderDT({
      detail <- drillthrough(); req(!is.null(detail))
      df <- sap_bi_get_table(app_state, detail$table); req(is.data.frame(df), detail$field %in% names(df))
      out <- df[as.character(df[[detail$field]]) == detail$value, , drop = FALSE]
      datatable(utils::head(out, 500L), rownames = FALSE, filter = "top",
                options = list(pageLength = 8, scrollX = TRUE, deferRender = TRUE))
    })
  })
}

message("[SAP] Dashboard Studio v4.0.0 loaded")
