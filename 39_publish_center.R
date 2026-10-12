# =============================================================================
# MODULE 39: PUBLISH, COLLABORATE, REFRESH & GOVERN
# =============================================================================

mod_publish_center_ui <- function(id, connected = FALSE) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Publish",
      "Release governed dashboard versions, manage the project team, keep sources fresh, and enforce audience rules.",
      "paper-plane"
    ),
    navset_card_tab(
      nav_panel(
        "Release",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("rocket"), " Publish dashboard"),
            card_body(
              textInput(ns("publication_title"), "Release title", value = "Survey intelligence dashboard"),
              textAreaInput(ns("publication_description"), "Description", rows = 4, placeholder = "Purpose, intended audience, and interpretation notes"),
              selectInput(ns("publication_audience"), "Audience", choices = c(
                "Private draft" = "private", "Project workspace" = "workspace",
                "Organization" = "organization", "Secure embed" = "embedded"
              ), selected = "workspace"),
              checkboxInput(ns("confirm_review"), "I reviewed filters, privacy rules, and source freshness", FALSE),
              actionButton(ns("publish"), "Publish new version", icon = icon("paper-plane"), class = "btn-primary w-100"),
              downloadButton(ns("download_spec"), "Download governed specification", class = "btn btn-outline-secondary btn-sm w-100 mt-2")
            )
          ),
          div(
            uiOutput(ns("release_readiness")),
            card(class = "sap-card", card_header(icon("clock-rotate-left"), " Release history"), card_body(DTOutput(ns("publications"))))
          )
        )
      ),
      nav_panel(
        "Team",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("user-plus"), " Add project member"),
            card_body(
              if (!connected) div(class = "alert alert-info", "Team management becomes available in connected production mode."),
              textInput(ns("member_identity"), "Username or email", placeholder = "colleague@example.org"),
              selectInput(ns("member_permission"), "Permission", choices = c(
                "Editor — build and publish" = "editor",
                "Reviewer — comment and validate" = "reviewer",
                "Viewer — interact with published views" = "viewer"
              )),
              actionButton(ns("add_member"), "Add or update member", icon = icon("user-plus"), class = "btn-primary w-100")
            )
          ),
          div(
            card(class = "sap-card", card_header(icon("users"), " Workspace members"), card_body(DTOutput(ns("members")))),
            div(class = "d-flex flex-wrap justify-content-end gap-2 mt-2",
                selectInput(ns("change_permission"), NULL, choices = c("Editor" = "editor", "Reviewer" = "reviewer", "Viewer" = "viewer"), width = "180px"),
                actionButton(ns("update_member"), "Change permission", class = "btn-sm btn-outline-primary"),
                actionButton(ns("remove_member"), "Remove", icon = icon("user-minus"), class = "btn-sm btn-outline-danger"))
          )
        )
      ),
      nav_panel(
        "Refresh",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("arrows-rotate"), " Source refresh"),
            card_body(
              selectInput(ns("refresh_table"), "Model table", choices = character()),
              passwordInput(ns("refresh_password"), "Session-only database password", placeholder = "Required only for manual PostgreSQL refresh"),
              actionButton(ns("run_refresh"), "Refresh now", icon = icon("rotate"), class = "btn-primary w-100"),
              hr(),
              selectInput(ns("refresh_frequency"), "Schedule", choices = c("Manual only" = "manual", "Hourly" = "hourly", "Daily" = "daily", "Weekly" = "weekly")),
              textInput(ns("refresh_time"), "Local time (HH:MM)", value = "06:00"),
              selectInput(ns("refresh_weekday"), "Weekday", choices = c("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7)),
              textInput(ns("refresh_timezone"), "Timezone", value = "Africa/Kigali"),
              textInput(ns("credential_ref"), "Credential environment reference", placeholder = "SAP_SOURCE_CACCE_PASSWORD"),
              checkboxInput(ns("refresh_enabled"), "Schedule enabled", TRUE),
              actionButton(ns("save_schedule"), "Save schedule", icon = icon("calendar-check"), class = "btn-outline-primary w-100")
            )
          ),
          div(
            uiOutput(ns("refresh_status")),
            card(class = "sap-card", card_header(icon("calendar"), " Refresh schedules"), card_body(DTOutput(ns("schedules")))),
            card(class = "sap-card mt-3", card_header(icon("clock"), " Job history"), card_body(DTOutput(ns("refresh_runs"))))
          )
        )
      ),
      nav_panel(
        "Security",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("user-shield"), " Row-level rule"),
            card_body(
              selectInput(ns("rls_table"), "Table", choices = character()),
              selectInput(ns("rls_field"), "Field", choices = character()),
              selectizeInput(ns("rls_values"), "Allowed values", choices = character(), multiple = TRUE),
              selectInput(ns("rls_subject_type"), "Apply to", choices = c("Specific username" = "username", "Account role" = "role", "All non-admin users" = "all")),
              textInput(ns("rls_subject"), "Username or role", placeholder = "viewer"),
              actionButton(ns("add_rls"), "Add security rule", icon = icon("shield-halved"), class = "btn-primary w-100")
            )
          ),
          div(
            card(class = "sap-card", card_header(icon("lock"), " Active row-security rules"), card_body(DTOutput(ns("rls_rules")))),
            div(class = "d-flex justify-content-end mt-2", actionButton(ns("delete_rls"), "Delete selected rule", icon = icon("trash"), class = "btn-sm btn-outline-danger")),
            div(class = "alert alert-info mt-3", icon("circle-info"), " Administrators bypass application row rules. Every other matching subject receives only the listed values when using Studio visuals and governed exports.")
          )
        )
      ),
      nav_panel(
        "Review",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card",
            card_header(icon("comment-dots"), " Add review comment"),
            card_body(
              selectInput(ns("comment_publication"), "Published version", choices = character()),
              textAreaInput(ns("comment_text"), "Comment", rows = 5),
              actionButton(ns("add_comment"), "Post comment", icon = icon("paper-plane"), class = "btn-primary w-100")
            )
          ),
          card(class = "sap-card", card_header(icon("comments"), " Review thread"), card_body(DTOutput(ns("comments"))))
        )
      )
    )
  )
}

sap_next_refresh_time <- function(frequency, time = "06:00", weekday = 1L,
                                  timezone = "UTC", now = Sys.time()) {
  if (identical(frequency, "manual")) return(NULL)
  attr(now, "tzone") <- timezone
  if (identical(frequency, "hourly")) return(now + 3600)
  bits <- strsplit(time %||% "06:00", ":", fixed = TRUE)[[1]]
  hour <- suppressWarnings(as.integer(sap_bi_first(bits, NA_character_))); minute <- suppressWarnings(as.integer(if (length(bits) >= 2L) bits[[2L]] else 0))
  if (is.na(hour) || hour < 0L || hour > 23L || is.na(minute) || minute < 0L || minute > 59L) stop("Refresh time must use HH:MM.", call. = FALSE)
  today <- as.Date(now, tz = timezone)
  candidate <- as.POSIXct(sprintf("%s %02d:%02d:00", today, hour, minute), tz = timezone)
  if (identical(frequency, "daily")) {
    if (candidate <= now) candidate <- candidate + 86400
    return(candidate)
  }
  target <- as.integer(weekday)
  current <- as.POSIXlt(now, tz = timezone)$wday
  current <- if (current == 0L) 7L else current
  days <- (target - current) %% 7L
  if (days == 0L && candidate <= now) days <- 7L
  candidate + days * 86400
}

mod_publish_center_server <- function(id, app_state, services = NULL,
                                      pool = NULL, connected = FALSE) {
  moduleServer(id, function(input, output, session) {
    sap_bi_ensure_state(app_state)
    publications_data <- reactiveVal(data.frame())
    members_data <- reactiveVal(data.frame())
    schedules_data <- reactiveVal(data.frame())
    runs_data <- reactiveVal(data.frame())
    comments_data <- reactiveVal(data.frame())
    refresh_message <- reactiveVal(list(type = "info", text = "Choose a refreshable table to inspect its source."))
    can_manage_security <- reactive({
      role <- tolower(as.character((app_state$current_user_profile %||% list())$role %||% "viewer"))
      role %in% c("owner", "admin")
    })

    reload_connected <- function() {
      if (is.null(pool) || is.null(app_state$current_project) || is.null(app_state$current_user)) return(invisible(FALSE))
      pid <- app_state$current_project; uid <- app_state$current_user
      publications_data(tryCatch(db_list_dashboard_publications(pool, pid, uid), error = function(e) data.frame()))
      members_data(tryCatch(db_list_project_members(pool, pid, uid), error = function(e) data.frame()))
      schedules_data(tryCatch(db_list_refresh_schedules(pool, pid, uid), error = function(e) data.frame()))
      runs_data(tryCatch(db_list_refresh_runs(pool, pid, uid), error = function(e) data.frame()))
      invisible(TRUE)
    }

    observeEvent(list(app_state$current_project, app_state$current_user), reload_connected(), ignoreInit = FALSE)

    observe({
      tables <- sap_bi_table_names(app_state)
      for (id in c("refresh_table", "rls_table")) {
        updateSelectInput(session, id, choices = tables, selected = input[[id]] %||% app_state$bi$active_table %||% sap_bi_first(tables, ""))
      }
      pubs <- publications_data()
      if (nrow(pubs)) updateSelectInput(session, "comment_publication", choices = stats::setNames(pubs$id, paste0(pubs$title, " v", pubs$version_no)))
    })

    observeEvent(input$rls_table, {
      if (!isTRUE(can_manage_security())) {
        updateSelectInput(session, "rls_field", choices = character())
        return(invisible(NULL))
      }
      df <- sap_bi_get_table(app_state, input$rls_table, secured = FALSE)
      updateSelectInput(session, "rls_field", choices = names(df) %||% character())
    }, ignoreInit = FALSE)
    observeEvent(input$rls_field, {
      if (!isTRUE(can_manage_security())) {
        updateSelectizeInput(session, "rls_values", choices = character(),
                             selected = character(), server = TRUE)
        return(invisible(NULL))
      }
      df <- sap_bi_get_table(app_state, input$rls_table, secured = FALSE)
      values <- if (is.data.frame(df) && input$rls_field %in% names(df)) sort(unique(as.character(df[[input$rls_field]]))) else character()
      updateSelectizeInput(session, "rls_values", choices = utils::head(values[!is.na(values)], 2000L), selected = character(), server = TRUE)
    }, ignoreInit = FALSE)

    output$release_readiness <- renderUI({
      model <- app_state$bi; dash <- model$dashboard
      visuals <- length(dash$visuals %||% list())
      pages <- length(dash$pages %||% list())
      tables <- length(model$tables)
      checks <- list(
        list(ok = tables > 0L, label = paste(tables, "model table(s)")),
        list(ok = visuals > 0L, label = paste(visuals, "saved visual(s)")),
        list(ok = pages > 0L, label = paste(pages, "dashboard page(s)")),
        list(ok = isTRUE(input$confirm_review), label = if (isTRUE(input$confirm_review)) "Release review confirmed" else "Confirm release review")
      )
      card(class = "sap-card", card_header(icon("list-check"), " Release readiness"), card_body(
        div(class = "sap-health-list", lapply(checks, function(x) div(
          class = paste("sap-health-row", if (x$ok) "is-ok" else "is-warning"),
          icon(if (x$ok) "circle-check" else "triangle-exclamation"), span(x$label)
        )))
      ))
    })

    observeEvent(input$publish, {
      req(nzchar(trimws(input$publication_title %||% "")))
      if (!isTRUE(input$confirm_review)) return(showNotification("Confirm the release review before publishing.", type = "warning"))
      if (!length(app_state$bi$dashboard$visuals %||% list())) return(showNotification("Save at least one visual before publishing.", type = "warning"))
      spec <- list(
        dashboard = app_state$bi$dashboard,
        semantic_model = sap_bi_model_manifest(app_state)[c("active_table", "tables", "relationships", "measures", "hierarchies")],
        governance = list(row_security_rules = app_state$bi$security_rules %||% list(),
                          privacy_policy = app_state$governance_policy),
        published_at = as.character(Sys.time())
      )
      if (!is.null(pool) && !is.null(app_state$current_project)) {
        result <- tryCatch({
          db_publish_dashboard(pool, app_state$current_project, input$publication_title,
                               input$publication_description, input$publication_audience,
                               spec, app_state$current_user)
        }, error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8); NULL
        })
        if (is.null(result)) return()
        reload_connected()
      } else {
        model <- app_state$bi
        version <- sum(vapply(model$publications %||% list(), function(x) identical(x$title, input$publication_title), logical(1))) + 1L
        model$publications <- c(model$publications %||% list(), list(list(
          id = sap_bi_id("publication"), title = input$publication_title,
          description = input$publication_description, audience = input$publication_audience,
          version_no = version, specification = spec, published_at = as.character(Sys.time()),
          published_by = "Local workspace"
        )))
        app_state$bi <- model
        publications_data(do.call(rbind, lapply(rev(model$publications), function(x) data.frame(
          id = x$id, title = x$title, description = x$description,
          audience = x$audience, status = "published", version_no = x$version_no,
          published_at = x$published_at, published_by = x$published_by,
          stringsAsFactors = FALSE
        ))))
      }
      app_state$last_action <- paste("Dashboard published:", input$publication_title)
      showNotification("A governed dashboard version was published.", type = "message")
    })

    output$download_spec <- downloadHandler(
      filename = function() paste0("dashboard_specification_", format(Sys.Date(), "%Y%m%d"), ".json"),
      content = function(file) {
        services$privacy$assert_export_allowed("dashboard specification export")
        spec <- list(dashboard = app_state$bi$dashboard, model = sap_bi_model_manifest(app_state),
                     exported_at = as.character(Sys.time()), release = SAP_RELEASE_ID)
        jsonlite::write_json(spec, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
      }
    )

    output$publications <- renderDT({
      d <- publications_data()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "No published dashboard versions yet."), options = list(dom = "t"), rownames = FALSE))
      show <- d[, intersect(c("id", "title", "version_no", "audience", "status", "published_by", "published_at"), names(d)), drop = FALSE]
      names(show) <- tools::toTitleCase(gsub("_", " ", names(show)))
      datatable(show, rownames = FALSE, selection = "single", options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    })

    observeEvent(input$add_member, {
      if (is.null(pool) || is.null(app_state$current_project)) return(showNotification("Team management requires connected mode.", type = "warning"))
      req(nzchar(trimws(input$member_identity %||% "")))
      tryCatch({
        db_add_project_member(pool, app_state$current_project, input$member_identity,
                              input$member_permission, app_state$current_user)
        reload_connected(); updateTextInput(session, "member_identity", value = "")
        showNotification("Project membership updated.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    selected_member <- function() {
      d <- members_data(); idx <- input$members_rows_selected
      if (!length(idx) || idx > nrow(d)) NULL else d[idx, , drop = FALSE]
    }
    observeEvent(input$update_member, {
      m <- selected_member(); if (is.null(m)) return(showNotification("Select a member first.", type = "warning"))
      tryCatch({ db_update_project_member(pool, app_state$current_project, m$user_id[[1]], input$change_permission, app_state$current_user); reload_connected() },
               error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    observeEvent(input$remove_member, {
      m <- selected_member(); if (is.null(m)) return(showNotification("Select a member first.", type = "warning"))
      tryCatch({ db_remove_project_member(pool, app_state$current_project, m$user_id[[1]], app_state$current_user); reload_connected() },
               error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    output$members <- renderDT({
      d <- members_data()
      if (!nrow(d)) return(datatable(data.frame(Mode = if (connected) "Choose a connected project to manage members." else "Local workspace — one private editor"), options = list(dom = "t"), rownames = FALSE))
      show <- d[, intersect(c("user_id", "username", "full_name", "permission", "added_at"), names(d)), drop = FALSE]
      names(show) <- tools::toTitleCase(gsub("_", " ", names(show)))
      datatable(show, rownames = FALSE, selection = "single", options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })

    refresh_table_now <- function(table_name, password = "") {
      model <- app_state$bi; item <- model$tables[[table_name]]
      if (is.null(item)) stop("Unknown model table.", call. = FALSE)
      config <- item$source_config %||% list(); type <- item$source_type
      if (identical(type, "https")) {
        tmp <- sap_bi_download_remote(config$url, max_mb = services$settings$get("max_file_size", 100))
        on.exit(unlink(tmp), add = TRUE)
        display <- basename(httr::parse_url(config$url)$path %||% "remote.csv")
        data <- sap_bi_read_file(tmp, display)
      } else if (identical(type, "postgresql")) {
        if (!nzchar(password %||% "")) stop("Enter the database password for this manual refresh.", call. = FALSE)
        data <- sap_bi_read_sql(config$query, config$host, config$port, config$database,
                                config$user, password, config$sslmode %||% "require",
                                as.integer(Sys.getenv("SAP_SOURCE_QUERY_MAX_ROWS", "500000")))
      } else {
        stop("This source is a captured file. Replace it through Connect, or use HTTPS/PostgreSQL for refreshable data.", call. = FALSE)
      }
      model$tables[[table_name]]$source_data <- data
      model$tables[[table_name]]$data <- data
      model$tables[[table_name]]$rows <- nrow(data); model$tables[[table_name]]$cols <- ncol(data)
      model$tables[[table_name]]$schema <- sap_bi_schema(data); model$tables[[table_name]]$updated_at <- as.character(Sys.time())
      app_state$bi <- model
      pipeline <- sap_bi_apply_pipeline(app_state, table_name)
      if (!isTRUE(pipeline$success)) stop("Source refreshed, but the saved pipeline failed: ", pipeline$error, call. = FALSE)
      if (identical(app_state$bi$active_table, table_name)) sap_bi_sync_active(app_state)
      nrow(app_state$bi$tables[[table_name]]$data)
    }

    observeEvent(input$run_refresh, {
      req(nzchar(input$refresh_table %||% ""))
      run_id <- NULL
      if (!is.null(pool) && !is.null(app_state$current_project)) {
        run_id <- tryCatch(db_start_refresh_run(pool, app_state$current_project, input$refresh_table, "manual", app_state$current_user), error = function(e) NULL)
      }
      result <- tryCatch({
        rows <- withProgress(message = paste("Refreshing", input$refresh_table), value = .2, {
          x <- refresh_table_now(input$refresh_table, input$refresh_password); incProgress(.8); x
        })
        if (!is.null(pool) && !is.null(app_state$current_project)) {
          item <- app_state$bi$tables[[input$refresh_table]]
          try(db_upsert_model_table(pool, app_state$current_project, input$refresh_table,
                                    item$source_data, item$schema, item$source_type,
                                    item$source_config, app_state$current_user), silent = TRUE)
        }
        list(ok = TRUE, rows = rows, message = paste(format(rows, big.mark = ","), "prepared rows loaded."))
      }, error = function(e) list(ok = FALSE, rows = NULL, message = conditionMessage(e)))
      updateTextInput(session, "refresh_password", value = "")
      if (!is.null(pool) && !is.null(run_id)) try(db_finish_refresh_run(pool, run_id, if (result$ok) "succeeded" else "failed", result$rows, result$message), silent = TRUE)
      refresh_message(list(type = if (result$ok) "success" else "danger", text = result$message))
      reload_connected()
    })

    observeEvent(input$save_schedule, {
      req(nzchar(input$refresh_table %||% ""))
      item <- app_state$bi$tables[[input$refresh_table]]
      next_run <- tryCatch(sap_next_refresh_time(input$refresh_frequency, input$refresh_time,
                                                 as.integer(input$refresh_weekday), input$refresh_timezone),
                           error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      if (!identical(input$refresh_frequency, "manual") && is.null(next_run)) return()
      schedule <- list(
        id = sap_bi_id("schedule"), table_name = input$refresh_table,
        source_type = item$source_type, source_config = sap_bi_redact_config(item$source_config),
        frequency = input$refresh_frequency, scheduled_time = input$refresh_time,
        weekday = as.integer(input$refresh_weekday), timezone = input$refresh_timezone,
        credential_ref = input$credential_ref, enabled = isTRUE(input$refresh_enabled),
        next_run_at = if (is.null(next_run)) NA_character_ else as.character(next_run)
      )
      if (!is.null(pool) && !is.null(app_state$current_project)) {
        tryCatch({
          db_upsert_refresh_schedule(pool, app_state$current_project, input$refresh_table,
                                     item$source_type, item$source_config, input$refresh_frequency,
                                     input$refresh_time, as.integer(input$refresh_weekday), input$refresh_timezone,
                                     NULL, input$credential_ref, input$refresh_enabled, next_run,
                                     app_state$current_user)
          reload_connected()
        }, error = function(e) showNotification(conditionMessage(e), type = "error"))
      } else {
        model <- app_state$bi
        model$refresh_schedules <- c(Filter(function(x) !identical(x$table_name, input$refresh_table), model$refresh_schedules %||% list()), list(schedule))
        app_state$bi <- model
        schedule_rows <- lapply(model$refresh_schedules, function(x) data.frame(
          table_name = x$table_name %||% "",
          source_type = x$source_type %||% "",
          frequency = x$frequency %||% "manual",
          scheduled_time = x$scheduled_time %||% "",
          timezone = x$timezone %||% "UTC",
          enabled = isTRUE(x$enabled),
          next_run_at = x$next_run_at %||% NA_character_,
          stringsAsFactors = FALSE
        ))
        schedules_data(do.call(rbind, schedule_rows))
      }
      showNotification("Refresh schedule saved.", type = "message")
    })

    output$refresh_status <- renderUI({
      x <- refresh_message()
      div(class = paste("alert", paste0("alert-", x$type)), icon(if (x$type == "success") "circle-check" else if (x$type == "danger") "circle-xmark" else "circle-info"), " ", x$text)
    })
    output$schedules <- renderDT({
      d <- schedules_data()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "No refresh schedules have been configured."), options = list(dom = "t"), rownames = FALSE))
      show <- d[, intersect(c("table_name", "source_type", "frequency", "scheduled_time", "timezone", "enabled", "last_run_at", "next_run_at", "consecutive_failures"), names(d)), drop = FALSE]
      names(show) <- tools::toTitleCase(gsub("_", " ", names(show)))
      datatable(show, rownames = FALSE, options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    })
    output$refresh_runs <- renderDT({
      d <- runs_data()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "Refresh job history will appear here."), options = list(dom = "t"), rownames = FALSE))
      names(d) <- tools::toTitleCase(gsub("_", " ", names(d)))
      datatable(d, rownames = FALSE, options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    })

    observeEvent(input$add_rls, {
      if (!isTRUE(can_manage_security())) {
        return(showNotification("Only project owners can manage row security.", type = "error"))
      }
      req(nzchar(input$rls_table %||% ""), nzchar(input$rls_field %||% ""), length(input$rls_values))
      if (!identical(input$rls_subject_type, "all") && !nzchar(trimws(input$rls_subject %||% ""))) return(showNotification("Enter the username or role targeted by this rule.", type = "warning"))
      rule <- list(id = sap_bi_id("rls"), table = input$rls_table, field = input$rls_field,
                   values = input$rls_values, subject_type = input$rls_subject_type,
                   subject_value = if (input$rls_subject_type == "all") "*" else trimws(input$rls_subject), enabled = TRUE,
                   created_at = as.character(Sys.time()))
      if (!is.null(pool) && !is.null(app_state$current_project)) {
        result <- tryCatch(db_add_row_security_rule(pool, app_state$current_project, rule$table, rule$field,
                                                   rule$values, rule$subject_type, rule$subject_value,
                                                   app_state$current_user), error = function(e) {
          showNotification(conditionMessage(e), type = "error"); NULL
        })
        if (is.null(result)) return()
        rule$db_id <- result
      }
      model <- app_state$bi; model$security_rules <- c(model$security_rules %||% list(), list(rule)); app_state$bi <- model
      showNotification("Row-security rule enabled.", type = "message")
    })

    rls_frame <- reactive({
      rules <- app_state$bi$security_rules %||% list()
      if (!length(rules)) return(data.frame())
      do.call(rbind, lapply(rules, function(x) data.frame(
        ID = x$id, Table = x$table, Field = x$field,
        Allowed = paste(x$values, collapse = ", "), Subject = paste(x$subject_type, x$subject_value),
        Enabled = isTRUE(x$enabled), stringsAsFactors = FALSE
      )))
    })
    output$rls_rules <- renderDT({
      d <- rls_frame()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "No row-security rules. Data follow project permissions and privacy policy."), options = list(dom = "t"), rownames = FALSE))
      datatable(d, rownames = FALSE, selection = "single", options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    })
    observeEvent(input$delete_rls, {
      if (!isTRUE(can_manage_security())) {
        return(showNotification("Only project owners can manage row security.", type = "error"))
      }
      idx <- input$rls_rules_rows_selected; rules <- app_state$bi$security_rules %||% list()
      if (!length(idx) || idx > length(rules)) return(showNotification("Select a rule first.", type = "warning"))
      rule <- rules[[idx]]
      if (!is.null(pool) && !is.null(rule$db_id)) try(db_delete_row_security_rule(pool, app_state$current_project, rule$db_id, app_state$current_user), silent = TRUE)
      model <- app_state$bi; model$security_rules <- rules[-idx]; app_state$bi <- model
    })

    observeEvent(input$comment_publication, {
      if (!is.null(pool) && nzchar(input$comment_publication %||% "")) {
        comments_data(tryCatch(db_list_report_comments(pool, app_state$current_project, as.integer(input$comment_publication), app_state$current_user), error = function(e) data.frame()))
      }
    }, ignoreInit = FALSE)
    observeEvent(input$add_comment, {
      req(nzchar(trimws(input$comment_text %||% "")), nzchar(input$comment_publication %||% ""))
      if (is.null(pool)) return(showNotification("Review threads require connected mode.", type = "warning"))
      tryCatch({
        db_add_report_comment(pool, app_state$current_project, as.integer(input$comment_publication), app_state$current_user, input$comment_text)
        comments_data(db_list_report_comments(pool, app_state$current_project, as.integer(input$comment_publication), app_state$current_user))
        updateTextAreaInput(session, "comment_text", value = "")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })
    output$comments <- renderDT({
      d <- comments_data()
      if (!nrow(d)) return(datatable(data.frame(Instruction = "Select a published version to review its discussion."), options = list(dom = "t"), rownames = FALSE))
      names(d) <- tools::toTitleCase(gsub("_", " ", names(d)))
      datatable(d, rownames = FALSE, options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })
  })
}

message("[SAP] Publish Center v4.0.0 loaded")
