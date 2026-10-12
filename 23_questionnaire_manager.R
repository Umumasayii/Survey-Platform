# =============================================================================
# MODULE 23: QUESTIONNAIRE MANAGER
# =============================================================================

mod_questionnaire_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Questionnaire Manager",
      "Document question wording, sections, response types, routing, universes, sensitivity, and validation rules.",
      "clipboard-list"),
    uiOutput(ns("body"))
  )
}

mod_questionnaire_manager_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {

    ensure_questionnaire <- function() {
      df <- app_state$processed_data
      if (!is.data.frame(df)) return(invisible(NULL))
      q <- app_state$questionnaire
      if (!is.data.frame(q) || !all(names(df) %in% q$variable)) {
        old <- if (is.data.frame(q)) q else data.frame()
        fresh <- data.frame(
          variable = names(df), question_number = seq_along(df), short_label = names(df), question_text = "",
          section = "Unassigned", response_type = vapply(df, function(x) if (is.numeric(x)) "Numeric" else "Single choice", character(1)),
          required = FALSE, sensitive = FALSE, universe = "All respondents", notes = "", stringsAsFactors = FALSE
        )
        if (nrow(old)) {
          common <- intersect(fresh$variable, old$variable)
          for (v in common) fresh[fresh$variable == v, intersect(names(fresh), names(old))] <- old[old$variable == v, intersect(names(fresh), names(old))]
        }
        app_state$questionnaire <- fresh
      }
    }

    observeEvent(app_state$processed_data, {
      ensure_questionnaire()
      vars <- sap_safe_names(app_state)
      updateSelectInput(session, "variable", choices = vars)
      updateSelectInput(session, "route_source", choices = vars)
      updateSelectInput(session, "route_target", choices = vars)
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      navset_card_tab(
        nav_panel("Question metadata",
          layout_columns(
            col_widths = c(4, 8),
            card(class = "sap-card",
              card_header(icon("pen-to-square"), " Edit selected question"),
              card_body(
                fileInput(session$ns("instrument_file"), "Import instrument dictionary", accept = c(".csv", ".json", ".xlsx")),
                actionButton(session$ns("import_instrument"), "Import and merge", icon = icon("file-import"), class = "btn-outline-primary w-100 mb-3"),
                selectInput(session$ns("variable"), "Dataset variable", choices = sap_safe_names(app_state)),
                textInput(session$ns("question_number"), "Question number"),
                textInput(session$ns("short_label"), "Short label"),
                textAreaInput(session$ns("question_text"), "Full question wording", rows = 4),
                textInput(session$ns("section"), "Questionnaire section"),
                selectInput(session$ns("response_type"), "Response type", choices = c("Single choice", "Multiple response", "Numeric", "Open text", "Date/time", "Matrix / grid", "Ranking", "Location")),
                textAreaInput(session$ns("universe"), "Applicable universe", rows = 2, placeholder = "All respondents, women aged 15–49, employed respondents..."),
                checkboxInput(session$ns("required"), "Required question", FALSE),
                checkboxInput(session$ns("sensitive"), "Sensitive / restricted variable", FALSE),
                textAreaInput(session$ns("notes"), "Documentation notes", rows = 2),
                actionButton(session$ns("save_question"), "Save question metadata", icon = icon("floppy-disk"), class = "btn-primary w-100"),
                actionButton(session$ns("snapshot_instrument"), "Create instrument version", icon = icon("code-branch"), class = "btn-outline-secondary w-100 mt-2"),
                div(class = "d-flex gap-2 mt-2",
                    downloadButton(session$ns("download_dictionary"), "Excel dictionary", class = "btn btn-outline-secondary btn-sm flex-fill"),
                    downloadButton(session$ns("download_instrument_json"), "JSON instrument", class = "btn btn-outline-secondary btn-sm flex-fill"))
              )
            ),
            card(class = "sap-card",
                 card_header(icon("table-list"), " Questionnaire dictionary"),
                 card_body(DTOutput(session$ns("question_table"))))
          )
        ),
        nav_panel("Routing & validation",
          layout_columns(
            col_widths = c(4, 8),
            card(class = "sap-card",
              card_header(icon("code-branch"), " Add routing rule"),
              card_body(
                selectInput(session$ns("route_source"), "Source question", choices = sap_safe_names(app_state)),
                selectInput(session$ns("route_operator"), "Condition", choices = c("equals", "does not equal", "is missing", "is not missing", "greater than", "less than")),
                textInput(session$ns("route_value"), "Condition value"),
                selectInput(session$ns("route_target"), "Target question", choices = sap_safe_names(app_state)),
                selectInput(session$ns("route_action"), "Expected action", choices = c("Ask target", "Skip target", "End interview", "Require answer", "Show warning")),
                actionButton(session$ns("add_rule"), "Add routing rule", icon = icon("plus"), class = "btn-primary w-100")
              )
            ),
            card(class = "sap-card",
                 card_header(icon("list-check"), " Routing rules"),
                 card_body(DTOutput(session$ns("rules_table"))))
          )
        ),
        nav_panel("Versions",
          tagList(
            div(class = "d-flex justify-content-end mb-2",
                actionButton(session$ns("restore_version"), "Restore selected version", icon = icon("clock-rotate-left"), class = "btn-outline-primary")),
            DTOutput(session$ns("version_table"))
          )
        )
      )
    })

    observeEvent(input$variable, {
      q <- app_state$questionnaire; req(is.data.frame(q), input$variable %in% q$variable)
      row <- q[q$variable == input$variable, , drop = FALSE][1, ]
      updateTextInput(session, "question_number", value = as.character(row$question_number))
      updateTextInput(session, "short_label", value = row$short_label %||% "")
      updateTextAreaInput(session, "question_text", value = row$question_text %||% "")
      updateTextInput(session, "section", value = row$section %||% "")
      updateSelectInput(session, "response_type", selected = row$response_type %||% "Single choice")
      updateTextAreaInput(session, "universe", value = row$universe %||% "All respondents")
      updateCheckboxInput(session, "required", value = isTRUE(row$required))
      updateCheckboxInput(session, "sensitive", value = isTRUE(row$sensitive))
      updateTextAreaInput(session, "notes", value = row$notes %||% "")
    }, ignoreInit = TRUE)

    observeEvent(input$save_question, {
      q <- app_state$questionnaire; req(is.data.frame(q), input$variable %in% q$variable)
      i <- which(q$variable == input$variable)[1]
      q$question_number[i] <- input$question_number %||% ""
      q$short_label[i] <- input$short_label %||% ""
      q$question_text[i] <- input$question_text %||% ""
      q$section[i] <- input$section %||% "Unassigned"
      q$response_type[i] <- input$response_type %||% "Single choice"
      q$universe[i] <- input$universe %||% "All respondents"
      q$required[i] <- isTRUE(input$required); q$sensitive[i] <- isTRUE(input$sensitive); q$notes[i] <- input$notes %||% ""
      app_state$questionnaire <- q
      sap_frontend_record(app_state, "Questionnaire", paste("Question metadata saved:", input$variable))
      showNotification("Question metadata saved.", type = "message")
    })

    observeEvent(input$import_instrument, {
      file <- input$instrument_file; req(!is.null(file))
      imported <- tryCatch({
        ext <- tolower(tools::file_ext(file$name))
        value <- switch(ext,
          csv = utils::read.csv(file$datapath, check.names = FALSE, stringsAsFactors = FALSE),
          json = jsonlite::fromJSON(file$datapath, flatten = TRUE),
          xlsx = as.data.frame(readxl::read_excel(file$datapath), stringsAsFactors = FALSE),
          stop("Use CSV, JSON, or XLSX for an instrument dictionary.", call. = FALSE)
        )
        as.data.frame(value, stringsAsFactors = FALSE)
      }, error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(imported)) return()
      names(imported) <- janitor::make_clean_names(names(imported))
      if (!"variable" %in% names(imported)) return(showNotification("The dictionary requires a 'variable' column.", type = "error"))
      ensure_questionnaire(); current <- app_state$questionnaire
      fields <- intersect(names(current), names(imported))
      matched <- intersect(current$variable, as.character(imported$variable))
      for (variable in matched) {
        current[current$variable == variable, fields] <- imported[match(variable, imported$variable), fields, drop = FALSE]
      }
      app_state$questionnaire <- current
      sap_frontend_record(app_state, "Questionnaire", paste("Instrument dictionary merged:", length(matched), "variables"))
      showNotification(sprintf("Merged metadata for %s active variables.", length(matched)), type = "message")
    })

    observeEvent(input$snapshot_instrument, {
      q <- app_state$questionnaire; req(is.data.frame(q), nrow(q) > 0)
      versions <- app_state$questionnaire_versions %||% list()
      version <- list(id = paste0("instrument_", format(Sys.time(), "%Y%m%d%H%M%S")),
                      version = length(versions) + 1L, created_at = as.character(Sys.time()),
                      questionnaire = q, routing_rules = app_state$routing_rules %||% data.frame())
      app_state$questionnaire_versions <- c(versions, list(version))
      sap_frontend_record(app_state, "Questionnaire", paste("Instrument version", version$version, "created"))
      showNotification(paste("Instrument version", version$version, "created."), type = "message")
    })

    output$download_dictionary <- downloadHandler(
      filename = function() paste0("questionnaire-dictionary-", Sys.Date(), ".xlsx"),
      content = function(file) {
        q <- app_state$questionnaire; req(is.data.frame(q))
        if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' is required.", call. = FALSE)
        if (exists("sap_export_assert_content", mode = "function")) sap_export_assert_content(services, q, "questionnaire dictionary export")
        openxlsx::write.xlsx(list(Questions = q, Routing = app_state$routing_rules %||% data.frame()), file, overwrite = TRUE)
      }
    )
    output$download_instrument_json <- downloadHandler(
      filename = function() paste0("questionnaire-instrument-", Sys.Date(), ".json"),
      content = function(file) {
        q <- app_state$questionnaire; req(is.data.frame(q))
        if (exists("sap_export_assert_content", mode = "function")) sap_export_assert_content(services, q, "questionnaire JSON export")
        jsonlite::write_json(list(questionnaire = q, routing_rules = app_state$routing_rules %||% data.frame()), file,
                             pretty = TRUE, auto_unbox = TRUE, na = "null")
      }
    )

    output$question_table <- renderDT({
      q <- app_state$questionnaire
      if (!is.data.frame(q)) return(datatable(data.frame(), options = list(dom = "t")))
      datatable(q, options = list(pageLength = 15, scrollX = TRUE, dom = "tip"), rownames = FALSE, selection = "single")
    })

    observeEvent(input$question_table_rows_selected, {
      q <- app_state$questionnaire; idx <- input$question_table_rows_selected
      if (is.data.frame(q) && length(idx) == 1) updateSelectInput(session, "variable", selected = q$variable[idx])
    })

    observeEvent(input$add_rule, {
      req(nzchar(input$route_source %||% ""), nzchar(input$route_target %||% ""))
      rule <- data.frame(Source = input$route_source, Condition = input$route_operator, Value = input$route_value %||% "",
                         Target = input$route_target, Action = input$route_action, stringsAsFactors = FALSE)
      rules <- app_state$routing_rules
      app_state$routing_rules <- if (is.data.frame(rules)) rbind(rules, rule) else rule
      sap_frontend_record(app_state, "Questionnaire", paste("Routing rule added:", input$route_source, "→", input$route_target))
      showNotification("Routing rule added.", type = "message")
    })

    output$rules_table <- renderDT({
      rules <- app_state$routing_rules
      if (!is.data.frame(rules) || nrow(rules) == 0) return(datatable(data.frame(Instruction = "Add questionnaire routing and validation rules."), options = list(dom = "t"), rownames = FALSE))
      datatable(rules, options = list(pageLength = 12, dom = "tip", scrollX = TRUE), rownames = FALSE)
    })

    output$version_table <- renderDT({
      versions <- app_state$questionnaire_versions %||% list()
      if (!length(versions)) return(datatable(data.frame(Instruction = "Create a version before major instrument edits."), options = list(dom = "t"), rownames = FALSE))
      rows <- do.call(rbind, lapply(versions, function(x) data.frame(
        ID = x$id, Version = x$version, Created = x$created_at,
        Questions = nrow(x$questionnaire), Routing_rules = nrow(x$routing_rules), stringsAsFactors = FALSE
      )))
      datatable(rows, selection = "single", rownames = FALSE, options = list(dom = "tip", pageLength = 10))
    })
    observeEvent(input$restore_version, {
      versions <- app_state$questionnaire_versions %||% list(); idx <- input$version_table_rows_selected
      req(length(idx) == 1L, idx <= length(versions))
      app_state$questionnaire <- versions[[idx]]$questionnaire
      app_state$routing_rules <- versions[[idx]]$routing_rules
      sap_frontend_record(app_state, "Questionnaire", paste("Instrument version", versions[[idx]]$version, "restored"))
      showNotification("Instrument version restored.", type = "message")
    })
  })
}

message("[SAP] Questionnaire Manager v4.0.0 loaded")
