# =============================================================================
# MODULE 35: SEMANTIC MODEL STUDIO
# =============================================================================

mod_semantic_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Model",
      "Define trusted relationships, reusable measures, and drill hierarchies across all tables in the workspace.",
      "diagram-project"
    ),
    uiOutput(ns("model_kpis")),
    navset_card_tab(
      nav_panel(
        "Model view",
        card(
          class = "sap-card",
          card_header(
            div(class = "d-flex justify-content-between align-items-center gap-2 w-100",
                span(icon("diagram-project"), " Semantic model"),
                actionButton(ns("autodetect"), "Autodetect", icon = icon("wand-magic-sparkles"), class = "btn-sm btn-outline-primary"))
          ),
          card_body(uiOutput(ns("model_canvas")))
        ),
        layout_columns(
          col_widths = c(7, 5),
          card(class = "sap-card", card_header(icon("link"), " Relationships"), card_body(DTOutput(ns("relationships")))),
          card(class = "sap-card", card_header(icon("heart-pulse"), " Model health"), card_body(uiOutput(ns("model_health"))))
        )
      ),
      nav_panel(
        "Relationships",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("link"), " Create relationship"),
            card_body(
              selectInput(ns("from_table"), "From table", choices = character()),
              selectInput(ns("from_key"), "From field", choices = character()),
              selectInput(ns("to_table"), "To table", choices = character()),
              selectInput(ns("to_key"), "To field", choices = character()),
              selectInput(ns("cardinality"), "Cardinality", choices = c(
                "Many to one (*:1)" = "many-to-one",
                "One to many (1:*)" = "one-to-many",
                "One to one (1:1)" = "one-to-one",
                "Many to many (*:*)" = "many-to-many"
              )),
              selectInput(ns("cross_filter"), "Filter direction", choices = c("Single direction" = "single", "Both directions" = "both")),
              div(class = "d-grid gap-2",
                  actionButton(ns("validate_relationship"), "Validate", icon = icon("list-check"), class = "btn-outline-primary"),
                  actionButton(ns("add_relationship"), "Create relationship", icon = icon("plus"), class = "btn-primary"))
            )
          ),
          div(
            uiOutput(ns("relationship_validation")),
            card(class = "sap-card", card_header(icon("table-list"), " Relationship registry"), card_body(DTOutput(ns("relationship_registry")))),
            div(class = "d-flex justify-content-end mt-2",
                actionButton(ns("delete_relationship"), "Delete selected", icon = icon("trash"), class = "btn-outline-danger"))
          )
        )
      ),
      nav_panel(
        "Measures",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("calculator"), " New reusable measure"),
            card_body(
              textInput(ns("measure_name"), "Measure name", placeholder = "e.g. Average household size"),
              selectInput(ns("measure_table"), "Home table", choices = character()),
              selectInput(ns("measure_aggregation"), "Aggregation", choices = c(
                "Count rows" = "count", "Distinct count" = "distinct_count",
                "Sum" = "sum", "Average" = "mean", "Median" = "median",
                "Minimum" = "min", "Maximum" = "max"
              )),
              selectInput(ns("measure_column"), "Field", choices = character()),
              selectInput(ns("measure_format"), "Display format", choices = c("Number" = "number", "Integer" = "integer", "Percent" = "percent", "Currency" = "currency")),
              textAreaInput(ns("measure_description"), "Definition", rows = 3),
              actionButton(ns("add_measure"), "Save measure", icon = icon("floppy-disk"), class = "btn-primary w-100")
            )
          ),
          div(
            card(class = "sap-card", card_header(icon("book"), " Measure catalog"), card_body(DTOutput(ns("measures")))),
            div(class = "d-flex justify-content-end mt-2",
                actionButton(ns("delete_measure"), "Delete selected", icon = icon("trash"), class = "btn-outline-danger"))
          )
        )
      ),
      nav_panel(
        "Hierarchies & fields",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("sitemap"), " Drill hierarchy"),
            card_body(
              textInput(ns("hierarchy_name"), "Hierarchy name", placeholder = "Geography"),
              selectInput(ns("hierarchy_table"), "Table", choices = character()),
              selectizeInput(ns("hierarchy_fields"), "Levels, broad to detailed", choices = character(), multiple = TRUE),
              actionButton(ns("add_hierarchy"), "Save hierarchy", icon = icon("plus"), class = "btn-primary w-100"),
              hr(),
              uiOutput(ns("hierarchy_list"))
            )
          ),
          card(class = "sap-card", card_header(icon("list"), " Field catalog"), card_body(
            selectInput(ns("catalog_table"), "Table", choices = character()),
            DTOutput(ns("field_catalog"))
          ))
        )
      )
    )
  )
}

mod_semantic_model_server <- function(id, app_state, services = NULL, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    sap_bi_ensure_state(app_state)
    last_validation <- reactiveVal(NULL)
    require_editor <- function(action = "edit the semantic model") {
      tryCatch({ sap_bi_require_role(app_state, "editor", action); TRUE },
               error = function(e) {
                 showNotification(conditionMessage(e), type = "error", duration = 7)
                 FALSE
               })
    }

    persist_model <- function() {
      if (is.null(pool) || is.null(app_state$current_project) || !exists("db_upsert_studio_document", mode = "function")) return(invisible(FALSE))
      try(db_upsert_studio_document(
        pool, project_id = app_state$current_project, document_type = "semantic_model",
        name = "Active semantic model", specification = sap_bi_model_manifest(app_state),
        created_by = app_state$current_user
      ), silent = TRUE)
      invisible(TRUE)
    }

    observe({
      tables <- sap_bi_table_names(app_state)
      selected_from <- input$from_table %||% sap_bi_first(tables, "")
      selected_to <- input$to_table %||% if (length(tables) >= 2L) tables[[2L]] else sap_bi_first(tables, "")
      for (id in c("from_table", "to_table", "measure_table", "hierarchy_table", "catalog_table")) {
        current <- input[[id]] %||% sap_bi_first(tables, "")
        updateSelectInput(session, id, choices = tables, selected = if (current %in% tables) current else sap_bi_first(tables, ""))
      }
      if (length(tables) > 1L && identical(selected_from, selected_to)) selected_to <- tables[[2]]
      updateSelectInput(session, "from_table", selected = selected_from)
      updateSelectInput(session, "to_table", selected = selected_to)
    })

    observeEvent(input$from_table, {
      df <- sap_bi_get_table(app_state, input$from_table)
      updateSelectInput(session, "from_key", choices = names(df) %||% character())
    }, ignoreInit = FALSE)
    observeEvent(input$to_table, {
      df <- sap_bi_get_table(app_state, input$to_table)
      updateSelectInput(session, "to_key", choices = names(df) %||% character())
    }, ignoreInit = FALSE)
    observeEvent(input$measure_table, {
      df <- sap_bi_get_table(app_state, input$measure_table)
      choices <- names(df) %||% character()
      if (input$measure_aggregation %in% c("sum", "mean", "median", "min", "max")) choices <- choices[vapply(df[choices], is.numeric, logical(1))]
      updateSelectInput(session, "measure_column", choices = c("Not required" = "", choices))
    }, ignoreInit = FALSE)
    observeEvent(input$measure_aggregation, {
      df <- sap_bi_get_table(app_state, input$measure_table)
      choices <- names(df) %||% character()
      if (input$measure_aggregation %in% c("sum", "mean", "median", "min", "max")) choices <- choices[vapply(df[choices], is.numeric, logical(1))]
      updateSelectInput(session, "measure_column", choices = c("Not required" = "", choices))
    }, ignoreInit = FALSE)
    observeEvent(input$hierarchy_table, {
      df <- sap_bi_get_table(app_state, input$hierarchy_table)
      updateSelectizeInput(session, "hierarchy_fields", choices = names(df) %||% character(), selected = character(), server = TRUE)
    }, ignoreInit = FALSE)

    validate_current <- function() {
      if (!nzchar(input$from_table %||% "") || !nzchar(input$to_table %||% "")) {
        return(list(valid = FALSE, issues = "Add at least two model tables before creating a relationship."))
      }
      if (identical(input$from_table, input$to_table)) {
        return(list(valid = FALSE, issues = "Choose two different tables."))
      }
      sap_bi_validate_relationship(
        sap_bi_get_table(app_state, input$from_table), input$from_key,
        sap_bi_get_table(app_state, input$to_table), input$to_key,
        input$cardinality
      )
    }

    observeEvent(input$validate_relationship, last_validation(validate_current()))

    observeEvent(input$add_relationship, {
      if (!require_editor()) return()
      v <- validate_current()
      last_validation(v)
      if (!isTRUE(v$valid)) {
        showNotification(paste(v$issues, collapse = " "), type = "error")
        return()
      }
      tryCatch({
        sap_bi_add_relationship(
          app_state, input$from_table, input$from_key,
          input$to_table, input$to_key, input$cardinality, input$cross_filter
        )
        persist_model()
        showNotification("Relationship created and validated.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    observeEvent(input$delete_relationship, {
      if (!require_editor()) return()
      idx <- input$relationship_registry_rows_selected
      rel <- app_state$bi$relationships
      if (!length(idx) || idx > nrow(rel)) {
        showNotification("Select a relationship first.", type = "warning")
        return()
      }
      sap_bi_remove_relationship(app_state, rel$id[[idx]])
      persist_model()
      showNotification("Relationship removed.", type = "message")
    })

    observeEvent(input$add_measure, {
      if (!require_editor()) return()
      req(nzchar(input$measure_name %||% ""), nzchar(input$measure_table %||% ""))
      tryCatch({
        sap_bi_add_measure(
          app_state, input$measure_name, input$measure_table,
          input$measure_column %||% "", input$measure_aggregation,
          input$measure_format, input$measure_description
        )
        persist_model()
        updateTextInput(session, "measure_name", value = "")
        updateTextAreaInput(session, "measure_description", value = "")
        showNotification("Reusable measure saved.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    observeEvent(input$delete_measure, {
      if (!require_editor()) return()
      idx <- input$measures_rows_selected
      model <- app_state$bi
      if (!length(idx) || idx > nrow(model$measures)) {
        showNotification("Select a measure first.", type = "warning")
        return()
      }
      model$measures <- model$measures[-idx, , drop = FALSE]
      app_state$bi <- model
      persist_model()
    })

    observeEvent(input$add_hierarchy, {
      if (!require_editor()) return()
      req(nzchar(input$hierarchy_name %||% ""), nzchar(input$hierarchy_table %||% ""), length(input$hierarchy_fields) >= 2L)
      model <- app_state$bi
      id <- sap_bi_id("hierarchy")
      model$hierarchies[[id]] <- list(
        id = id, name = sap_bi_clean_name(input$hierarchy_name, "Hierarchy"),
        table = input$hierarchy_table, fields = input$hierarchy_fields,
        created_at = as.character(Sys.time())
      )
      app_state$bi <- model
      persist_model()
      updateTextInput(session, "hierarchy_name", value = "")
      updateSelectizeInput(session, "hierarchy_fields", selected = character())
      showNotification("Drill hierarchy saved.", type = "message")
    })

    observeEvent(input$autodetect, {
      if (!require_editor()) return()
      tables <- sap_bi_table_names(app_state)
      if (length(tables) < 2L) {
        showNotification("Autodetect needs at least two tables.", type = "warning")
        return()
      }
      added <- 0L
      model <- app_state$bi
      for (i in seq_len(length(tables) - 1L)) for (j in (i + 1L):length(tables)) {
        left_name <- tables[[i]]; right_name <- tables[[j]]
        left <- sap_bi_get_table(app_state, left_name); right <- sap_bi_get_table(app_state, right_name)
        common <- intersect(names(left), names(right))
        for (key in common) {
          left_unique <- !anyDuplicated(left[[key]][!is.na(left[[key]])])
          right_unique <- !anyDuplicated(right[[key]][!is.na(right[[key]])])
          if (!left_unique && !right_unique) next
          direction <- if (right_unique) c(left_name, key, right_name, key, "many-to-one") else c(left_name, key, right_name, key, "one-to-many")
          existing <- nrow(model$relationships) && any(
            model$relationships$from_table == direction[[1]] & model$relationships$from_key == key &
              model$relationships$to_table == direction[[3]] & model$relationships$to_key == key
          )
          if (!existing) {
            try({
              sap_bi_add_relationship(app_state, direction[[1]], key, direction[[3]], key, direction[[5]], "single")
              added <- added + 1L
              model <- app_state$bi
            }, silent = TRUE)
          }
        }
      }
      persist_model()
      showNotification(if (added) paste("Created", added, "validated relationship(s).") else "No new high-confidence relationships were found.", type = if (added) "message" else "warning")
    })

    output$model_kpis <- renderUI({
      sap_bi_ensure_state(app_state)
      model <- app_state$bi
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Model tables", length(model$tables), "table-list"),
          sap_kpi("Relationships", nrow(model$relationships), "link"),
          sap_kpi("Measures", nrow(model$measures), "calculator"),
          sap_kpi("Hierarchies", length(model$hierarchies), "sitemap"))
    })

    output$model_canvas <- renderUI({
      model <- app_state$bi
      if (!length(model$tables)) return(sap_empty_state("No model tables", "Connect two or more tables to build a semantic model.", "database"))
      cards <- lapply(model$tables, function(tbl) {
        fields <- tbl$schema
        preview <- utils::head(fields, 6L)
        div(
          class = paste("sap-model-table", if (identical(tbl$name, model$active_table)) "is-active" else ""),
          div(class = "sap-model-table-head",
              div(icon(if (identical(tbl$name, model$active_table)) "star" else "table"), strong(tbl$name)),
              span(paste(format(tbl$rows, big.mark = ","), "×", tbl$cols), class = "sap-model-table-size")),
          div(class = "sap-model-fields",
              lapply(seq_len(nrow(preview)), function(i) {
                row <- preview[i, ]
                div(class = "sap-model-field",
                    icon(if (row$role == "Measure") "hashtag" else if (row$role == "Date") "calendar" else "font"),
                    span(row$field), span(row$type, class = "small"))
              }),
              if (nrow(fields) > 6L) div(class = "sap-model-more", paste("+", nrow(fields) - 6L, "more fields")))
        )
      })
      tagList(
        div(class = "sap-model-canvas", cards),
        if (nrow(model$relationships)) div(
          class = "sap-relationship-strip",
          lapply(seq_len(nrow(model$relationships)), function(i) {
            r <- model$relationships[i, ]
            div(class = "sap-relationship-chip", icon("link"),
                span(r$from_table), code(r$from_key), strong(switch(r$cardinality, `many-to-one` = "*:1", `one-to-many` = "1:*", `one-to-one` = "1:1", "*: *")),
                span(r$to_table), code(r$to_key))
          })
        )
      )
    })

    relationship_table <- reactive({
      rel <- app_state$bi$relationships
      if (!nrow(rel)) return(data.frame())
      data.frame(
        From = paste0(rel$from_table, "[", rel$from_key, "]"),
        Cardinality = rel$cardinality,
        To = paste0(rel$to_table, "[", rel$to_key, "]"),
        Filter = rel$cross_filter,
        Active = rel$active,
        stringsAsFactors = FALSE
      )
    })

    output$relationships <- renderDT({
      d <- relationship_table()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "No relationships. Use Autodetect or create one manually."), options = list(dom = "t"), rownames = FALSE))
      datatable(d, rownames = FALSE, options = list(dom = "tip", pageLength = 8, scrollX = TRUE))
    })
    output$relationship_registry <- renderDT({
      d <- relationship_table()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "No relationships have been defined."), options = list(dom = "t"), rownames = FALSE))
      datatable(d, rownames = FALSE, selection = "single", options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    })

    output$relationship_validation <- renderUI({
      v <- last_validation()
      if (is.null(v)) return(div(class = "alert alert-info", icon("circle-info"), " Validate a candidate relationship to inspect uniqueness and unmatched keys."))
      if (!isTRUE(v$valid)) return(div(class = "alert alert-danger", icon("circle-xmark"), tags$ul(class = "mb-0", lapply(v$issues, tags$li))))
      div(class = "alert alert-success",
          icon("circle-check"), strong(" Relationship is valid. "),
          paste(format(v$matches, big.mark = ","), "matching key values;", format(v$unmatched, big.mark = ","), "source rows are unmatched."))
    })

    output$measures <- renderDT({
      d <- app_state$bi$measures
      if (!nrow(d)) return(datatable(data.frame(Instruction = "Create reusable measures for consistent dashboard calculations."), options = list(dom = "t"), rownames = FALSE))
      show <- d[, c("name", "table", "aggregation", "column", "format", "description"), drop = FALSE]
      names(show) <- c("Measure", "Table", "Aggregation", "Field", "Format", "Definition")
      datatable(show, rownames = FALSE, selection = "single", options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })

    output$hierarchy_list <- renderUI({
      items <- app_state$bi$hierarchies %||% list()
      if (!length(items)) return(p(class = "text-muted mb-0", "No drill hierarchies yet."))
      tagList(lapply(items, function(x) div(
        class = "sap-hierarchy-item",
        div(icon("sitemap"), strong(x$name), span(x$table, class = "badge text-bg-light")),
        span(paste(x$fields, collapse = " → "), class = "text-muted small")
      )))
    })

    output$field_catalog <- renderDT({
      df <- sap_bi_get_table(app_state, input$catalog_table)
      d <- sap_bi_schema(df)
      if (!nrow(d)) return(datatable(data.frame(Instruction = "Select a model table."), options = list(dom = "t"), rownames = FALSE))
      names(d) <- c("Field", "Storage type", "Analytical role", "Missing", "Unique")
      datatable(d, rownames = FALSE, filter = "top", options = list(dom = "tip", pageLength = 15, scrollX = TRUE))
    })

    output$model_health <- renderUI({
      model <- app_state$bi
      tables <- names(model$tables)
      connected <- unique(c(model$relationships$from_table, model$relationships$to_table))
      disconnected <- setdiff(tables, connected)
      duplicate_measures <- anyDuplicated(tolower(model$measures$name)) > 0L
      issues <- list(
        list(ok = length(tables) > 0L, label = if (length(tables)) "Model contains data tables" else "Connect at least one table"),
        list(ok = length(tables) < 2L || nrow(model$relationships) > 0L, label = if (length(tables) < 2L || nrow(model$relationships)) "Relationship coverage available" else "Multiple tables are not related"),
        list(ok = !length(disconnected) || length(tables) == 1L, label = if (!length(disconnected) || length(tables) == 1L) "All analytical tables are connected" else paste("Disconnected:", paste(disconnected, collapse = ", "))),
        list(ok = !duplicate_measures, label = if (!duplicate_measures) "Measure names are unique" else "Duplicate measure names detected")
      )
      div(class = "sap-health-list", lapply(issues, function(x) div(
        class = paste("sap-health-row", if (x$ok) "is-ok" else "is-warning"),
        icon(if (x$ok) "circle-check" else "triangle-exclamation"), span(x$label)
      )))
    })
  })
}

message("[SAP] Semantic Model Studio v4.0.0 loaded")
