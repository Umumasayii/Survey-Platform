# =============================================================================
# MODULE 36: REPEATABLE TRANSFORM PIPELINES
# =============================================================================

mod_transform_pipeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Prepare",
      "Build an ordered, replayable data-preparation pipeline instead of losing cleaning work inside one session.",
      "wand-magic-sparkles"
    ),
    div(
      class = "sap-studio-toolbar",
      div(class = "sap-toolbar-field", selectInput(ns("table"), "Pipeline table", choices = character())),
      div(class = "sap-toolbar-actions",
          actionButton(ns("run"), "Run pipeline", icon = icon("play"), class = "btn-primary"),
          actionButton(ns("undo_last"), "Undo last step", icon = icon("rotate-left"), class = "btn-outline-secondary"),
          actionButton(ns("clear"), "Clear pipeline", icon = icon("trash"), class = "btn-outline-danger"))
    ),
    uiOutput(ns("pipeline_kpis")),
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "sap-card",
        card_header(icon("plus"), " Add transformation step"),
        card_body(
          selectInput(ns("operation"), "Operation", choices = c(
            "Filter rows" = "filter",
            "Choose fields" = "select",
            "Rename field" = "rename",
            "Replace missing values" = "replace_missing",
            "Change data type" = "convert",
            "Sort rows" = "sort",
            "Remove duplicates" = "deduplicate",
            "Safe calculated field" = "calculate",
            "Join another table" = "join",
            "Append / union table" = "union",
            "Pivot fields to rows" = "pivot_longer",
            "Pivot rows to fields" = "pivot_wider",
            "Aggregate rows" = "aggregate"
          )),
          uiOutput(ns("step_form")),
          actionButton(ns("add_step"), "Add to pipeline", icon = icon("plus"), class = "btn-primary w-100")
        )
      ),
      div(
        card(
          class = "sap-card",
          card_header(
            div(class = "d-flex justify-content-between align-items-center w-100 gap-2",
                span(icon("list-ol"), " Ordered steps"),
                span("Source → steps → model table", class = "small text-muted"))
          ),
          card_body(DTOutput(ns("steps")))
        ),
        div(class = "d-flex flex-wrap justify-content-end gap-2 mt-2",
            actionButton(ns("move_up"), "Move up", icon = icon("arrow-up"), class = "btn-sm btn-outline-secondary"),
            actionButton(ns("move_down"), "Move down", icon = icon("arrow-down"), class = "btn-sm btn-outline-secondary"),
            actionButton(ns("toggle_step"), "Enable / disable", icon = icon("toggle-on"), class = "btn-sm btn-outline-secondary"),
            actionButton(ns("remove_step"), "Remove", icon = icon("xmark"), class = "btn-sm btn-outline-danger")),
        uiOutput(ns("run_status"))
      )
    ),
    navset_card_tab(
      nav_panel("Prepared preview", card(class = "sap-card", card_body(DTOutput(ns("prepared_preview"))))),
      nav_panel("Source preview", card(class = "sap-card", card_body(DTOutput(ns("source_preview"))))),
      nav_panel("Quality comparison", card(class = "sap-card", card_body(uiOutput(ns("quality_comparison")))))
    )
  )
}

mod_transform_pipeline_server <- function(id, app_state, services = NULL, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    sap_bi_ensure_state(app_state)
    last_run <- reactiveVal(NULL)
    require_editor <- function(action = "edit the transformation pipeline") {
      tryCatch({ sap_bi_require_role(app_state, "editor", action); TRUE },
               error = function(e) {
                 showNotification(conditionMessage(e), type = "error", duration = 7)
                 FALSE
               })
    }

    table_fields <- reactive({
      df <- sap_bi_get_table(app_state, input$table, original = TRUE)
      names(df) %||% character()
    })

    observe({
      tables <- sap_bi_table_names(app_state)
      selected <- input$table %||% app_state$bi$active_table %||% sap_bi_first(tables, "")
      updateSelectInput(session, "table", choices = tables, selected = if (selected %in% tables) selected else sap_bi_first(tables, ""))
    })

    output$step_form <- renderUI({
      ns <- session$ns
      fields <- table_fields()
      numeric_fields <- {
        df <- sap_bi_get_table(app_state, input$table, original = TRUE)
        if (is.data.frame(df)) names(df)[vapply(df, is.numeric, logical(1))] else character()
      }
      tables <- setdiff(sap_bi_table_names(app_state), input$table %||% "")
      op <- input$operation %||% "filter"
      switch(
        op,
        filter = tagList(
          selectInput(ns("step_field"), "Field", choices = fields),
          selectInput(ns("filter_operator"), "Condition", choices = c(
            "Equals" = "equals", "Does not equal" = "not_equals",
            "Greater than" = "greater", "Greater than or equal" = "greater_equal",
            "Less than" = "less", "Less than or equal" = "less_equal",
            "Contains" = "contains", "Starts with" = "starts_with", "Ends with" = "ends_with",
            "Is missing" = "is_missing", "Is not missing" = "not_missing"
          )),
          textInput(ns("filter_value"), "Value")
        ),
        select = selectizeInput(ns("select_columns"), "Keep fields", choices = fields, selected = fields, multiple = TRUE),
        rename = tagList(selectInput(ns("step_field"), "Field", choices = fields), textInput(ns("new_name"), "New name")),
        replace_missing = tagList(selectInput(ns("step_field"), "Field", choices = fields), textInput(ns("replacement"), "Replacement value")),
        convert = tagList(selectInput(ns("step_field"), "Field", choices = fields), selectInput(ns("convert_type"), "New type", choices = c("Numeric" = "numeric", "Integer" = "integer", "Text" = "character", "Category" = "factor", "Logical" = "logical", "Date" = "date"))),
        sort = tagList(selectInput(ns("step_field"), "Field", choices = fields), radioButtons(ns("sort_direction"), "Direction", c("Ascending" = "ascending", "Descending" = "descending"), inline = TRUE)),
        deduplicate = selectizeInput(ns("dedupe_columns"), "Duplicate key fields (blank = entire row)", choices = fields, multiple = TRUE),
        calculate = tagList(
          textInput(ns("calculated_name"), "New field name"),
          selectInput(ns("calculation"), "Calculation / transformation method", choices = c(
            "Add" = "add", "Subtract" = "subtract", "Multiply" = "multiply", "Divide" = "divide",
            "Percentage (first ÷ second × 100)" = "percentage", "Average / mean of two fields" = "average",
            "Concatenate text" = "concat", "Coalesce / first non-missing value" = "coalesce",
            "Row sum" = "row_sum", "Row mean" = "row_mean", "Row minimum" = "row_min",
            "Row maximum" = "row_max", "Count non-missing fields" = "count_non_missing",
            "Concatenate several variables" = "concat_many"
          )),
          selectInput(ns("calc_left"), "First field", choices = fields),
          selectInput(ns("calc_mode"), "Second operand", choices = c("Another field" = "field", "Constant value" = "constant")),
          conditionalPanel(sprintf("input['%s'] == 'field'", ns("calc_mode")), selectInput(ns("calc_right"), "Second field", choices = fields)),
          conditionalPanel(sprintf("input['%s'] == 'constant'", ns("calc_mode")), textInput(ns("calc_constant"), "Constant")),
          selectizeInput(ns("calc_fields"), "Fields for row / multi-field operations", choices = fields, multiple = TRUE,
                         selected = utils::head(fields, 2L), options = list(plugins = list("remove_button"))),
          helpText("Choose fields from the list. Row sum/mean/min/max, count non-missing, and multi-variable concatenation use this field list; no R formula is required."),
          textInput(ns("calc_separator"), "Text separator", value = " ")
        ),
        join = tagList(
          selectInput(ns("other_table"), "Join table", choices = tables),
          selectInput(ns("left_key"), "Current-table key", choices = fields),
          uiOutput(ns("right_key_ui")),
          selectInput(ns("join_type"), "Join type", choices = c("Left" = "left", "Inner" = "inner", "Right" = "right", "Full" = "full"))
        ),
        union = selectInput(ns("other_table"), "Append table", choices = tables),
        pivot_longer = tagList(
          selectizeInput(ns("pivot_columns"), "Fields to pivot", choices = fields, multiple = TRUE),
          textInput(ns("pivot_names_to"), "Names field", value = "variable"),
          textInput(ns("pivot_values_to"), "Values field", value = "value")
        ),
        pivot_wider = tagList(
          selectInput(ns("wider_names"), "Names from", choices = fields),
          selectInput(ns("wider_values"), "Values from", choices = fields)
        ),
        aggregate = tagList(
          selectizeInput(ns("group_by"), "Group by", choices = fields, multiple = TRUE),
          selectInput(ns("aggregate_type"), "Aggregation", choices = c("Count" = "count", "Sum" = "sum", "Average" = "mean", "Median" = "median", "Minimum" = "min", "Maximum" = "max")),
          selectInput(ns("aggregate_measure"), "Measure field", choices = c("Not required for count" = "", numeric_fields)),
          textInput(ns("aggregate_name"), "Result field name", value = "value")
        )
      )
    })

    output$right_key_ui <- renderUI({
      other <- sap_bi_get_table(app_state, input$other_table, original = TRUE)
      selectInput(session$ns("right_key"), "Joined-table key", choices = names(other) %||% character())
    })

    observeEvent(input$filter_operator, {
      disabled <- input$filter_operator %in% c("is_missing", "not_missing")
      shinyjs::toggleState("filter_value", condition = !disabled)
    })

    step_from_inputs <- function() {
      op <- input$operation
      params <- switch(
        op,
        filter = list(field = input$step_field, operator = input$filter_operator, value = input$filter_value),
        select = list(columns = input$select_columns),
        rename = list(field = input$step_field, new_name = input$new_name),
        replace_missing = list(field = input$step_field, value = input$replacement),
        convert = list(field = input$step_field, type = input$convert_type),
        sort = list(field = input$step_field, direction = input$sort_direction),
        deduplicate = list(columns = input$dedupe_columns),
        calculate = list(
          new_field = input$calculated_name, left = input$calc_left,
          calculation = input$calculation, fields = input$calc_fields %||% character(),
          right = if (identical(input$calc_mode, "field")) input$calc_right else NULL,
          constant = if (identical(input$calc_mode, "constant")) input$calc_constant else NULL,
          separator = input$calc_separator
        ),
        join = list(with_table = input$other_table, left_key = input$left_key,
                    right_key = input$right_key, join_type = input$join_type),
        union = list(with_table = input$other_table),
        pivot_longer = list(columns = input$pivot_columns, names_to = input$pivot_names_to, values_to = input$pivot_values_to),
        pivot_wider = list(names_from = input$wider_names, values_from = input$wider_values),
        aggregate = list(group_by = input$group_by, aggregation = input$aggregate_type,
                         measure = input$aggregate_measure, new_field = input$aggregate_name),
        list()
      )
      label <- switch(
        op,
        filter = paste("Keep rows where", input$step_field, gsub("_", " ", input$filter_operator), input$filter_value %||% ""),
        select = paste("Keep", length(input$select_columns %||% character()), "fields"),
        rename = paste("Rename", input$step_field, "to", input$new_name),
        replace_missing = paste("Replace missing", input$step_field),
        convert = paste("Convert", input$step_field, "to", input$convert_type),
        sort = paste("Sort", input$step_field, input$sort_direction),
        deduplicate = paste("Remove duplicates by", paste(input$dedupe_columns %||% "entire row", collapse = ", ")),
        calculate = paste("Create", input$calculated_name, "using", input$calculation),
        join = paste(tools::toTitleCase(input$join_type), "join", input$other_table),
        union = paste("Append", input$other_table),
        pivot_longer = paste("Pivot", length(input$pivot_columns %||% character()), "fields to rows"),
        pivot_wider = paste("Pivot", input$wider_names, "to fields"),
        aggregate = paste("Aggregate by", paste(input$group_by, collapse = ", ")),
        tools::toTitleCase(gsub("_", " ", op))
      )
      list(operation = op, label = label, params = params)
    }

    observeEvent(input$add_step, {
      if (!require_editor()) return()
      req(nzchar(input$table %||% ""))
      step <- step_from_inputs()
      # Validate the candidate by replaying existing steps plus the new one on
      # a bounded preview before adding it to the authoritative pipeline.
      model <- app_state$bi
      candidate <- list(id = "candidate", operation = step$operation, label = step$label,
                        params = step$params, enabled = TRUE)
      preview_source <- utils::head(model$tables[[input$table]]$source_data, 5000L)
      validation <- sap_bi_run_pipeline(preview_source, c(model$pipelines[[input$table]] %||% list(), list(candidate)), model$tables)
      if (!isTRUE(validation$success)) {
        showNotification(paste("Step is invalid:", validation$error), type = "error", duration = 8)
        return()
      }
      sap_bi_add_step(app_state, input$table, step$operation, step$label, step$params)
      app_state$last_action <- paste("Pipeline step added:", step$label)
      showNotification("Validated step added. Run the pipeline to apply it.", type = "message")
    })

    selected_step <- function() {
      idx <- input$steps_rows_selected
      steps <- app_state$bi$pipelines[[input$table]] %||% list()
      if (!length(idx) || idx > length(steps)) return(NULL)
      steps[[idx]]
    }

    observeEvent(input$move_up, {
      if (!require_editor()) return()
      step <- selected_step(); if (is.null(step)) return(showNotification("Select a step first.", type = "warning"))
      sap_bi_move_step(app_state, input$table, step$id, -1L)
    })
    observeEvent(input$move_down, {
      if (!require_editor()) return()
      step <- selected_step(); if (is.null(step)) return(showNotification("Select a step first.", type = "warning"))
      sap_bi_move_step(app_state, input$table, step$id, 1L)
    })
    observeEvent(input$toggle_step, {
      if (!require_editor()) return()
      step <- selected_step(); if (is.null(step)) return(showNotification("Select a step first.", type = "warning"))
      model <- app_state$bi
      steps <- model$pipelines[[input$table]]
      idx <- match(step$id, vapply(steps, function(x) x$id, character(1)))
      steps[[idx]]$enabled <- !isTRUE(steps[[idx]]$enabled)
      model$pipelines[[input$table]] <- steps
      app_state$bi <- model
    })
    observeEvent(input$remove_step, {
      if (!require_editor()) return()
      step <- selected_step(); if (is.null(step)) return(showNotification("Select a step first.", type = "warning"))
      sap_bi_remove_step(app_state, input$table, step$id)
    })
    observeEvent(input$undo_last, {
      if (!require_editor()) return()
      model <- app_state$bi
      steps <- model$pipelines[[input$table]] %||% list()
      if (!length(steps)) return(showNotification("The pipeline has no steps.", type = "warning"))
      removed <- steps[[length(steps)]]$label
      model$pipelines[[input$table]] <- steps[-length(steps)]
      app_state$bi <- model
      result <- sap_bi_apply_pipeline(app_state, input$table)
      last_run(result)
      showNotification(paste("Removed and reverted:", removed), type = "message")
    })

    observeEvent(input$clear, {
      if (!require_editor()) return()
      req(nzchar(input$table %||% ""))
      showModal(modalDialog(
        title = "Clear this pipeline?",
        p("All transformation steps will be removed and the table will return to its connected source data."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_clear"), "Clear pipeline", class = "btn-danger"))
      ))
    })
    observeEvent(input$confirm_clear, {
      if (!require_editor()) {
        removeModal()
        return()
      }
      model <- app_state$bi
      model$pipelines[[input$table]] <- list()
      model$tables[[input$table]]$data <- model$tables[[input$table]]$source_data
      model$tables[[input$table]]$schema <- sap_bi_schema(model$tables[[input$table]]$data)
      model$tables[[input$table]]$rows <- nrow(model$tables[[input$table]]$data)
      model$tables[[input$table]]$cols <- ncol(model$tables[[input$table]]$data)
      app_state$bi <- model
      if (identical(model$active_table, input$table)) sap_bi_sync_active(app_state)
      last_run(NULL)
      removeModal()
      showNotification("Pipeline cleared and source restored.", type = "message")
    })

    observeEvent(input$run, {
      if (!require_editor("run the transformation pipeline")) return()
      req(nzchar(input$table %||% ""))
      result <- withProgress(message = "Running repeatable pipeline", value = 0.2, {
        out <- sap_bi_apply_pipeline(app_state, input$table)
        incProgress(0.8)
        out
      })
      last_run(result)
      if (isTRUE(result$success)) {
        app_state$transformation_count <- length(app_state$bi$pipelines[[input$table]] %||% list())
        app_state$last_action <- paste("Pipeline completed:", input$table)
        if (!is.null(pool) && !is.null(app_state$current_project)) {
          if (exists("db_upsert_studio_document", mode = "function")) try(db_upsert_studio_document(
            pool, app_state$current_project, "transform_pipeline", paste("Pipeline —", input$table),
            list(table = input$table, steps = app_state$bi$pipelines[[input$table]], updated_at = as.character(Sys.time())),
            app_state$current_user
          ), silent = TRUE)
          if (exists("db_upsert_model_table", mode = "function")) try(db_upsert_model_table(
            pool, app_state$current_project, input$table,
            app_state$bi$tables[[input$table]]$source_data,
            app_state$bi$tables[[input$table]]$schema,
            app_state$bi$tables[[input$table]]$source_type,
            app_state$bi$tables[[input$table]]$source_config,
            app_state$current_user
          ), silent = TRUE)
        }
        showNotification("Pipeline completed and the model table was refreshed.", type = "message")
      } else {
        showNotification(paste("Pipeline stopped safely:", result$error), type = "error", duration = 10)
      }
    })

    steps_data <- reactive({
      steps <- app_state$bi$pipelines[[input$table]] %||% list()
      if (!length(steps)) return(data.frame())
      log <- last_run()$log %||% list()
      log_by_id <- if (length(log)) setNames(log, vapply(log, function(x) x$id, character(1))) else list()
      do.call(rbind, lapply(seq_along(steps), function(i) {
        step <- steps[[i]]; entry <- log_by_id[[step$id]]
        data.frame(
          Order = i, Enabled = isTRUE(step$enabled), Step = step$label,
          Operation = tools::toTitleCase(gsub("_", " ", step$operation)),
          Status = entry$status %||% "Not run",
          Rows = if (is.null(entry)) "—" else paste0(format(entry$rows_before, big.mark = ","), " → ", format(entry$rows_after, big.mark = ",")),
          stringsAsFactors = FALSE
        )
      }))
    })

    output$steps <- renderDT({
      d <- steps_data()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "Add validated steps to create a repeatable preparation pipeline."), options = list(dom = "t"), rownames = FALSE))
      datatable(d, rownames = FALSE, selection = "single", options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })

    output$pipeline_kpis <- renderUI({
      source <- sap_bi_get_table(app_state, input$table, original = TRUE)
      prepared <- sap_bi_get_table(app_state, input$table)
      steps <- app_state$bi$pipelines[[input$table]] %||% list()
      if (!is.data.frame(source)) return(NULL)
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Source rows", format(nrow(source), big.mark = ","), "database"),
          sap_kpi("Prepared rows", format(nrow(prepared), big.mark = ","), "table"),
          sap_kpi("Pipeline steps", length(steps), "list-ol"),
          sap_kpi("Enabled", sum(vapply(steps, function(x) isTRUE(x$enabled), logical(1))), "toggle-on"))
    })

    output$run_status <- renderUI({
      result <- last_run()
      if (is.null(result)) return(div(class = "alert alert-light mt-3 mb-0", icon("circle-info"), " The prepared preview shows the last successful run."))
      if (isTRUE(result$success)) return(div(class = "alert alert-success mt-3 mb-0", icon("circle-check"), " Every enabled step completed successfully."))
      div(class = "alert alert-danger mt-3 mb-0", icon("circle-xmark"), strong(" Pipeline stopped without overwriting the last good table. "), result$error)
    })

    output$prepared_preview <- renderDT({
      df <- sap_bi_get_table(app_state, input$table)
      if (!is.data.frame(df)) return(datatable(data.frame(Instruction = "Choose a model table."), options = list(dom = "t"), rownames = FALSE))
      datatable(utils::head(df, 300), rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE, deferRender = TRUE))
    })
    output$source_preview <- renderDT({
      df <- sap_bi_get_table(app_state, input$table, original = TRUE)
      if (!is.data.frame(df)) return(datatable(data.frame(Instruction = "Choose a model table."), options = list(dom = "t"), rownames = FALSE))
      datatable(utils::head(df, 300), rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE, deferRender = TRUE))
    })

    output$quality_comparison <- renderUI({
      source <- sap_bi_get_table(app_state, input$table, original = TRUE)
      prepared <- sap_bi_get_table(app_state, input$table)
      if (!is.data.frame(source) || !is.data.frame(prepared)) return(p(class = "text-muted", "Choose a table."))
      metric <- function(df) list(
        rows = nrow(df), fields = ncol(df), missing = sum(is.na(df)),
        duplicates = sum(duplicated(df))
      )
      a <- metric(source); b <- metric(prepared)
      tags$table(
        class = "table table-sm sap-compare-table",
        tags$thead(tags$tr(tags$th("Metric"), tags$th("Source"), tags$th("Prepared"), tags$th("Change"))),
        tags$tbody(
          lapply(c("rows", "fields", "missing", "duplicates"), function(nm) tags$tr(
            tags$td(tools::toTitleCase(nm)),
            tags$td(format(a[[nm]], big.mark = ",")),
            tags$td(format(b[[nm]], big.mark = ",")),
            tags$td(sprintf("%+d", b[[nm]] - a[[nm]]))
          ))
        )
      )
    })
  })
}

message("[SAP] Transform Pipeline Studio v4.0.0 loaded")
