# =============================================================================
# MODULE 34: DATA CONNECTIONS & TABLE CATALOG
# =============================================================================

mod_data_connections_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Connect & Catalog",
      "Bring several sources into one governed workspace, inspect their schemas, and choose the active analysis table.",
      "database"
    ),
    div(
      class = "sap-studio-callout",
      icon("shield-halved"),
      div(
        strong("Credentials stay outside saved workspace documents."),
        div("Uploaded data remain in the current project; remote URLs are HTTPS-only and private-network destinations are blocked.", class = "small text-muted")
      )
    ),
    navset_card_tab(
      id = ns("connection_tabs"),
      nav_panel(
        "Files",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("copy"), " Add one or more tables"),
            card_body(
              fileInput(
                ns("files"), "Data files", multiple = TRUE,
                accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".sav", ".dta", ".sas7bdat", ".json", ".parquet")
              ),
              textInput(ns("file_prefix"), "Optional table-name prefix", placeholder = "e.g. 2026 Survey"),
              checkboxInput(ns("activate_first"), "Make the first imported table active", TRUE),
              actionButton(ns("import_files"), "Import selected files", icon = icon("arrow-up-from-bracket"), class = "btn-primary w-100")
            )
          ),
          card(
            class = "sap-card",
            card_header(icon("circle-info"), " Supported formats"),
            card_body(
              div(class = "sap-connector-grid",
                  sap_connector_tile("CSV / TSV", "Fast, portable tabular files", "file-csv", "Ready"),
                  sap_connector_tile("Excel", "XLS and XLSX workbooks", "file-excel", "Ready"),
                  sap_connector_tile("SPSS / Stata / SAS", "Label-aware research files", "flask", "Ready"),
                  sap_connector_tile("JSON", "Flattened records and API exports", "code", "Ready"),
                  sap_connector_tile("Parquet", "Columnar files when arrow is installed", "layer-group", "Optional"),
                  sap_connector_tile("Multi-file", "Load related tables in one action", "copy", "Ready"))
            )
          )
        )
      ),
      nav_panel(
        "Secure web",
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "sap-card",
            card_header(icon("globe"), " HTTPS data source"),
            card_body(
              textInput(ns("remote_url"), "Direct file URL", placeholder = "https://data.example.org/survey.csv"),
              textInput(ns("remote_name"), "Table name", placeholder = "Survey responses"),
              numericInput(ns("remote_limit"), "Maximum download (MB)", 100, min = 1, max = 500),
              actionButton(ns("import_remote"), "Validate and import", icon = icon("cloud-arrow-down"), class = "btn-primary w-100")
            )
          ),
          card(
            class = "sap-card",
            card_header(icon("lock"), " Network policy"),
            card_body(
              tags$ul(
                class = "sap-check-list mb-0",
                tags$li(icon("circle-check"), " HTTPS is required by default."),
                tags$li(icon("circle-check"), " Loopback, link-local, and private IP ranges are rejected."),
                tags$li(icon("circle-check"), " Redirect depth, connection time, and file size are bounded."),
                tags$li(icon("circle-check"), " Imported content must use a supported tabular format.")
              )
            )
          )
        )
      ),
      nav_panel(
        "PostgreSQL",
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "sap-card",
            card_header(icon("server"), " Read-only SQL connection"),
            card_body(
              layout_columns(
                col_widths = c(8, 4),
                textInput(ns("pg_host"), "Host"),
                numericInput(ns("pg_port"), "Port", 5432, min = 1, max = 65535)
              ),
              textInput(ns("pg_database"), "Database"),
              layout_columns(
                col_widths = c(6, 6),
                textInput(ns("pg_user"), "User"),
                passwordInput(ns("pg_password"), "Password")
              ),
              selectInput(ns("pg_sslmode"), "TLS mode", c("Require" = "require", "Verify CA" = "verify-ca", "Verify full" = "verify-full", "Prefer (development only)" = "prefer")),
              textAreaInput(ns("pg_query"), "SELECT query", rows = 6, placeholder = "SELECT * FROM public.survey_responses"),
              textInput(ns("pg_name"), "Table name", placeholder = "Survey responses"),
              actionButton(ns("import_pg"), "Run read-only query", icon = icon("play"), class = "btn-primary w-100")
            )
          ),
          card(
            class = "sap-card",
            card_header(icon("shield"), " Query guardrails"),
            card_body(
              p("The connector accepts a single SELECT or WITH statement and applies an outer row limit. INSERT, UPDATE, DELETE, DDL, COPY, grants, and multiple statements are rejected."),
              div(class = "alert alert-info mb-0", icon("key"), " Connection passwords are used only for this request and are never added to workspace state, logs, or connection manifests.")
            )
          )
        )
      ),
      nav_panel(
        "Table catalog",
        layout_columns(
          col_widths = c(8, 4),
          card(class = "sap-card", card_header(icon("table-list"), " Model tables"), card_body(DTOutput(ns("tables")))),
          card(
            class = "sap-card",
            card_header(icon("sliders"), " Active table"),
            card_body(
              selectInput(ns("active_table"), "Table", choices = character()),
              div(class = "d-grid gap-2",
                  actionButton(ns("set_active"), "Use for analysis", icon = icon("circle-check"), class = "btn-primary"),
                  actionButton(ns("register_current"), "Add current legacy dataset", icon = icon("plus"), class = "btn-outline-secondary"),
                  actionButton(ns("remove_table"), "Remove from model", icon = icon("trash"), class = "btn-outline-danger")),
              hr(),
              uiOutput(ns("active_summary"))
            )
          )
        ),
        layout_columns(
          col_widths = c(7, 5),
          card(class = "sap-card", card_header(icon("table"), " Data preview"), card_body(DTOutput(ns("preview")))),
          card(class = "sap-card", card_header(icon("list"), " Field profile"), card_body(DTOutput(ns("schema"))))
        )
      )
    ),
    uiOutput(ns("connection_status"))
  )
}

sap_connector_tile <- function(title, description, icon_name, status = "Ready") {
  div(
    class = "sap-connector-tile",
    div(class = "sap-connector-icon", icon(icon_name)),
    div(class = "sap-connector-copy", strong(title), span(description)),
    span(status, class = "sap-connector-status")
  )
}

mod_data_connections_server <- function(id, app_state, services = NULL, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    sap_bi_ensure_state(app_state)
    status <- reactiveVal(list(type = "info", title = "Ready", message = "Choose a source to add model tables."))
    require_editor <- function(action) {
      tryCatch({
        sap_bi_require_role(app_state, "editor", action)
        TRUE
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 7)
        FALSE
      })
    }

    persist_table <- function(table_name) {
      if (is.null(pool) || is.null(app_state$current_project) ||
          !exists("db_upsert_model_table", mode = "function")) return(invisible(FALSE))
      item <- app_state$bi$tables[[table_name]]
      tryCatch({
        db_upsert_model_table(
          pool, project_id = app_state$current_project,
          name = table_name, data = item$source_data,
          schema = item$schema, source_type = item$source_type,
          source_config = item$source_config,
          created_by = app_state$current_user
        )
        TRUE
      }, error = function(e) {
        services$log$warn(paste("Model table persistence failed:", conditionMessage(e)), "connections")
        FALSE
      })
    }

    record_source <- function(table_name, source_type, config = list()) {
      model <- app_state$bi
      model$sources[[sap_bi_id("source")]] <- list(
        table = table_name, type = source_type,
        config = sap_bi_redact_config(config), imported_at = as.character(Sys.time())
      )
      app_state$bi <- model
      invisible(TRUE)
    }

    observe({
      sap_bi_ensure_state(app_state)
      tables <- sap_bi_table_names(app_state)
      active <- app_state$bi$active_table %||% sap_bi_first(tables, "")
      updateSelectInput(session, "active_table", choices = tables, selected = active)
    })

    # The original import wizard remains useful. Register a newly imported
    # single data frame automatically so legacy and Studio workflows interoperate.
    observeEvent(app_state$processed_data, {
      df <- app_state$processed_data
      if (!isTRUE(app_state$data_imported) || !is.data.frame(df)) return()
      if (!sap_bi_has_role(app_state, "editor")) return()
      name <- sap_bi_clean_name(app_state$dataset_name %||% "Imported survey")
      model <- app_state$bi
      if (!name %in% names(model$tables)) {
        added <- sap_bi_add_table(app_state, name, df, source_type = "legacy_import", set_active = TRUE)
        record_source(added, "legacy_import", list(origin = "Import Wizard"))
        persist_table(added)
      }
    }, ignoreInit = FALSE)

    observeEvent(input$import_files, {
      if (!require_editor("connect model tables")) return()
      req(!is.null(input$files), nrow(input$files) > 0)
      files <- input$files
      cap_mb <- as.numeric(services$settings$get("max_file_size", 100) %||% 100)
      imported <- character()
      failures <- character()
      withProgress(message = "Importing model tables", value = 0, {
        for (i in seq_len(nrow(files))) {
          incProgress(1 / nrow(files), detail = files$name[[i]])
          if (files$size[[i]] > cap_mb * 1024^2) {
            failures <- c(failures, paste0(files$name[[i]], ": exceeds ", cap_mb, " MB"))
            next
          }
          result <- tryCatch({
            df <- sap_bi_read_file(files$datapath[[i]], files$name[[i]])
            stem <- tools::file_path_sans_ext(basename(files$name[[i]]))
            prefix <- trimws(input$file_prefix %||% "")
            requested <- if (nzchar(prefix)) paste(prefix, stem, sep = " — ") else stem
            table_name <- sap_bi_add_table(
              app_state, requested, df, source_type = "file",
              source_config = list(file_name = basename(files$name[[i]]), size_bytes = files$size[[i]]),
              set_active = isTRUE(input$activate_first) && !length(imported)
            )
            record_source(table_name, "file", list(file_name = basename(files$name[[i]]), size_bytes = files$size[[i]]))
            persist_table(table_name)
            table_name
          }, error = function(e) {
            failures <<- c(failures, paste0(files$name[[i]], ": ", conditionMessage(e)))
            NULL
          })
          if (!is.null(result)) imported <- c(imported, result)
        }
      })
      if (length(imported)) {
        app_state$last_action <- paste("Imported model tables:", paste(imported, collapse = ", "))
        status(list(type = if (length(failures)) "warning" else "success",
                    title = paste(length(imported), "table(s) imported"),
                    message = if (length(failures)) paste("Some files failed:", paste(failures, collapse = " | ")) else "Schemas are ready in the table catalog."))
        bslib::nav_select("connection_tabs", selected = "Table catalog", session = session)
      } else {
        status(list(type = "danger", title = "Import failed", message = paste(failures, collapse = " | ")))
      }
    })

    observeEvent(input$import_remote, {
      if (!require_editor("connect remote model tables")) return()
      req(nzchar(trimws(input$remote_url %||% "")))
      status(list(type = "info", title = "Validating source", message = "Checking the destination and download policy…"))
      result <- tryCatch({
        tmp <- sap_bi_download_remote(input$remote_url, max_mb = input$remote_limit %||% 100)
        on.exit(unlink(tmp), add = TRUE)
        display <- basename(httr::parse_url(input$remote_url)$path %||% "remote.csv")
        df <- sap_bi_read_file(tmp, display)
        requested <- input$remote_name %||% tools::file_path_sans_ext(display)
        table_name <- sap_bi_add_table(
          app_state, requested, df, source_type = "https",
          source_config = list(url = input$remote_url), set_active = TRUE
        )
        record_source(table_name, "https", list(url = input$remote_url))
        persist_table(table_name)
        table_name
      }, error = function(e) {
        status(list(type = "danger", title = "Remote import blocked", message = conditionMessage(e)))
        NULL
      })
      if (!is.null(result)) {
        status(list(type = "success", title = "Secure import complete", message = paste(result, "is now the active model table.")))
        bslib::nav_select("connection_tabs", selected = "Table catalog", session = session)
      }
    })

    observeEvent(input$import_pg, {
      if (!require_editor("connect PostgreSQL model tables")) return()
      required <- c(input$pg_host, input$pg_database, input$pg_user, input$pg_password, input$pg_query)
      req(all(nzchar(trimws(required %||% ""))))
      result <- tryCatch({
        df <- sap_bi_read_sql(
          query = input$pg_query, host = input$pg_host, port = input$pg_port,
          database = input$pg_database, user = input$pg_user,
          password = input$pg_password, sslmode = input$pg_sslmode,
          max_rows = as.integer(Sys.getenv("SAP_SOURCE_QUERY_MAX_ROWS", "500000"))
        )
        requested <- input$pg_name %||% "PostgreSQL query"
        config <- list(host = input$pg_host, port = input$pg_port,
                       database = input$pg_database, user = input$pg_user,
                       query = input$pg_query, sslmode = input$pg_sslmode)
        table_name <- sap_bi_add_table(app_state, requested, df, "postgresql", config, set_active = TRUE)
        record_source(table_name, "postgresql", config)
        persist_table(table_name)
        table_name
      }, error = function(e) {
        status(list(type = "danger", title = "Database import failed", message = conditionMessage(e)))
        NULL
      })
      updateTextInput(session, "pg_password", value = "")
      if (!is.null(result)) {
        status(list(type = "success", title = "Query imported", message = paste(result, "is ready for modelling.")))
        bslib::nav_select("connection_tabs", selected = "Table catalog", session = session)
      }
    })

    observeEvent(input$register_current, {
      if (!require_editor("register model tables")) return()
      req(is.data.frame(app_state$processed_data))
      name <- sap_bi_add_table(
        app_state, app_state$dataset_name %||% "Current dataset",
        app_state$processed_data, "legacy_import", set_active = TRUE
      )
      persist_table(name)
      status(list(type = "success", title = "Dataset registered", message = paste(name, "was added to the semantic model.")))
    })

    observeEvent(input$set_active, {
      req(input$active_table)
      sap_bi_set_active(app_state, input$active_table)
      status(list(type = "success", title = "Active table changed", message = paste(input$active_table, "now powers the classic analysis modules.")))
    })

    observeEvent(input$remove_table, {
      if (!require_editor("remove model tables")) return()
      req(input$active_table)
      showModal(modalDialog(
        title = "Remove model table?",
        p("This removes the in-memory table, its relationships, measures, and pipeline from this workspace."),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_remove"), "Remove table", class = "btn-danger"))
      ))
    })

    observeEvent(input$confirm_remove, {
      if (!require_editor("remove model tables")) {
        removeModal()
        return()
      }
      target <- isolate(input$active_table)
      sap_bi_remove_table(app_state, target)
      if (!is.null(pool) && !is.null(app_state$current_project) && exists("db_delete_model_table", mode = "function")) {
        try(db_delete_model_table(
          pool, app_state$current_project, target,
          user_id = app_state$current_user
        ), silent = TRUE)
      }
      removeModal()
      status(list(type = "warning", title = "Table removed", message = paste(target, "was removed from the model.")))
    })

    output$tables <- renderDT({
      sap_bi_ensure_state(app_state)
      model <- app_state$bi
      if (!length(model$tables)) {
        return(datatable(data.frame(Instruction = "Connect files, a secure web source, or PostgreSQL to create a model."), options = list(dom = "t"), rownames = FALSE))
      }
      rows <- do.call(rbind, lapply(model$tables, function(x) data.frame(
        Table = x$name, Rows = x$rows, Fields = x$cols,
        Source = tools::toTitleCase(x$source_type),
        Updated = format(as.POSIXct(x$updated_at), "%Y-%m-%d %H:%M"),
        Active = identical(x$name, model$active_table), stringsAsFactors = FALSE
      )))
      datatable(rows, rownames = FALSE, selection = "single",
                options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    })

    observeEvent(input$tables_rows_selected, {
      idx <- input$tables_rows_selected
      model <- app_state$bi
      if (length(idx) && idx <= length(model$tables)) {
        updateSelectInput(session, "active_table", selected = names(model$tables)[[idx]])
      }
    })

    output$preview <- renderDT({
      df <- sap_bi_get_table(app_state, input$active_table)
      if (!is.data.frame(df)) return(datatable(data.frame(Instruction = "No model table selected."), options = list(dom = "t"), rownames = FALSE))
      datatable(utils::head(df, 200), rownames = FALSE, filter = "top",
                options = list(pageLength = 10, scrollX = TRUE, deferRender = TRUE))
    })

    output$schema <- renderDT({
      df <- sap_bi_get_table(app_state, input$active_table)
      schema <- sap_bi_schema(df)
      if (!nrow(schema)) return(datatable(data.frame(Instruction = "Select a table to inspect its fields."), options = list(dom = "t"), rownames = FALSE))
      names(schema) <- c("Field", "Type", "Role", "Missing", "Unique")
      datatable(schema, rownames = FALSE, options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })

    output$active_summary <- renderUI({
      df <- sap_bi_get_table(app_state, input$active_table)
      if (!is.data.frame(df)) return(p(class = "text-muted mb-0", "No active table."))
      missing <- if (length(df)) 100 * sum(is.na(df)) / (nrow(df) * ncol(df)) else 0
      tagList(
        div(class = "sap-mini-stat-row",
            span(strong(format(nrow(df), big.mark = ",")), " rows"),
            span(strong(ncol(df)), " fields"),
            span(strong(sprintf("%.1f%%", missing)), " missing")),
        p(class = "small text-muted mt-2 mb-0", "Classic statistics modules use the active table. Studio dashboards may use any model table.")
      )
    })

    output$connection_status <- renderUI({
      s <- status()
      div(class = paste("alert mt-3", paste0("alert-", switch(s$type, danger = "danger", warning = "warning", success = "success", "info"))),
          icon(switch(s$type, danger = "circle-xmark", warning = "triangle-exclamation", success = "circle-check", "circle-info")),
          strong(paste0(" ", s$title, ". ")), s$message)
    })
  })
}

message("[SAP] Data Connections & Catalog v4.0.0 loaded")
