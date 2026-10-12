# =============================================================================
# MODULE 32: ADMIN CONTROL CENTER
# =============================================================================
# PostgreSQL-backed operational administration for authenticated administrators.
# The module exposes account/profile information and ownership/usage metadata.
# It deliberately does NOT deserialize or display raw respondent-level datasets.
# =============================================================================

.admin_fmt_bytes <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || is.na(x)) return("0 B")
  units <- c("B", "KB", "MB", "GB", "TB")
  idx <- 1L
  while (x >= 1024 && idx < length(units)) {
    x <- x / 1024
    idx <- idx + 1L
  }
  sprintf(if (idx == 1L) "%.0f %s" else "%.1f %s", x, units[[idx]])
}

.admin_empty_dt <- function(message) {
  DT::datatable(
    data.frame(Status = message, stringsAsFactors = FALSE),
    rownames = FALSE,
    options = list(dom = "t", ordering = FALSE)
  )
}

mod_admin_control_center_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("control_center"))
}

mod_admin_control_center_server <- function(id, app_state, services = NULL, pool = NULL,
                                            demo_mode = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_tick <- shiny::reactiveVal(0L)

    auth <- if (!is.null(services)) services$auth else NULL
    notify <- if (!is.null(services)) services$notify else NULL

    admin_ok <- shiny::reactive({
      shiny::invalidateLater(15000, session)
      if (!is.null(auth)) auth$version()
      !isTRUE(demo_mode) && !is.null(pool) && !is.null(auth) &&
        isTRUE(tryCatch(auth$is_authenticated(), error = function(e) FALSE)) &&
        isTRUE(tryCatch(auth$is_admin(), error = function(e) FALSE))
    })

    current_admin_id <- function() {
      if (is.null(auth)) return(NULL)
      suppressWarnings(as.integer(auth$current_user_id()))
    }

    nudge <- function(message = NULL, type = c("success", "warning", "error")) {
      refresh_tick(shiny::isolate(refresh_tick()) + 1L)
      if (is.null(message)) return(invisible(TRUE))
      type <- match.arg(type)
      if (!is.null(notify)) {
        if (identical(type, "success")) notify$success(message, "admin_control_center")
        else if (identical(type, "warning")) notify$warning(message, "admin_control_center")
        else notify$error(message, "admin_control_center")
      } else {
        shiny::showNotification(message,
          type = if (identical(type, "success")) "message" else type)
      }
      invisible(TRUE)
    }

    output$control_center <- shiny::renderUI({
      if (!is.null(auth)) auth$version()

      if (isTRUE(demo_mode)) {
        return(shiny::div(
          class = "alert alert-info",
          shiny::tags$h4(shiny::icon("database"), " Control Center requires connected mode"),
          shiny::tags$p(class = "mb-1",
            "Demo-mode accounts live only in the current Shiny session and are not written to PostgreSQL."),
          shiny::tags$p(class = "mb-0",
            "Set SAP_DEMO_MODE=false, configure PostgreSQL, restart the app, and sign in with an administrator account.")
        ))
      }

      if (is.null(pool) || is.null(auth) || !isTRUE(tryCatch(auth$is_available(), error = function(e) FALSE))) {
        return(shiny::div(
          class = "alert alert-danger",
          shiny::tags$h4(shiny::icon("triangle-exclamation"), " PostgreSQL administration unavailable"),
          shiny::tags$p(class = "mb-0",
            "The Control Center will not fall back to temporary accounts. Restore the configured PostgreSQL/authentication service first.")
        ))
      }

      if (!isTRUE(tryCatch(auth$is_authenticated(), error = function(e) FALSE))) {
        return(shiny::div(class = "alert alert-warning",
          shiny::icon("lock"), " Sign in before opening the Control Center."))
      }

      if (!isTRUE(tryCatch(auth$is_admin(), error = function(e) FALSE))) {
        return(shiny::div(
          class = "alert alert-danger",
          shiny::tags$h4(shiny::icon("shield-halved"), " Administrator access required"),
          shiny::tags$p(class = "mb-0",
            "Your account is authenticated but does not have the admin role. This page does not expose user records to normal accounts.")
        ))
      }

      shiny::tagList(
        shiny::div(
          class = "d-flex flex-wrap justify-content-between align-items-start gap-3 mb-3",
          shiny::div(
            shiny::tags$h2(class = "mb-1", shiny::icon("users-gear"), " Admin Control Center"),
            shiny::tags$p(class = "text-muted mb-0",
              "Manage PostgreSQL accounts, access state, sessions, ownership metadata, and administrative audit history.")
          ),
          shiny::actionButton(ns("refresh"), "Refresh", icon = shiny::icon("rotate"),
                              class = "btn btn-outline-primary")
        ),
        shiny::uiOutput(ns("connection_banner")),
        shiny::uiOutput(ns("overview")),
        bslib::card(
          bslib::card_header(shiny::icon("sliders"), " System controls"),
          bslib::card_body(
            shiny::tags$p(class = "text-muted", "Operational controls act on PostgreSQL-backed sessions and are recorded in the administrative audit."),
            shiny::div(class = "d-flex flex-wrap gap-2",
              shiny::actionButton(ns("revoke_all_sessions"), "Sign out other users", icon = shiny::icon("users-slash"), class = "btn btn-outline-warning"),
              shiny::actionButton(ns("cleanup_sessions"), "Clean old sessions", icon = shiny::icon("broom"), class = "btn btn-outline-secondary", title = "Delete expired/revoked session records older than 90 days")
            )
          )
        ),
        bslib::card(
          bslib::card_header(shiny::icon("users"), " Users"),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(5, shiny::textInput(ns("user_search"), "Search",
                placeholder = "Username, email, name, organization")),
              shiny::column(3, shiny::selectInput(ns("user_role"), "Role",
                choices = c("All roles" = "all", "Admin" = "admin", "User" = "user",
                            "Reviewer" = "reviewer", "Viewer" = "viewer"), selected = "all")),
              shiny::column(3, shiny::selectInput(ns("user_status"), "Status",
                choices = c("All" = "all", "Active" = "active", "Pending" = "pending", "Suspended" = "suspended", "Disabled" = "disabled", "Archived" = "archived", "Inactive" = "inactive", "Locked" = "locked"),
                selected = "all")),
              shiny::column(1, shiny::div(style = "padding-top: 31px;",
                shiny::actionButton(ns("clear_filters"), NULL, icon = shiny::icon("filter-circle-xmark"),
                                    class = "btn btn-outline-secondary", title = "Clear filters")))
            ),
            DT::DTOutput(ns("users_table"))
          )
        ),
        shiny::uiOutput(ns("selected_user_panel")),
        bslib::card(
          bslib::card_header(shiny::icon("shield"), " Administrative audit"),
          bslib::card_body(DT::DTOutput(ns("admin_audit")))
        ),
        shiny::div(
          class = "alert alert-secondary mt-3 mb-0",
          shiny::icon("eye-slash"),
          shiny::tags$strong(" Privacy boundary: "),
          "the Control Center shows dataset metadata such as project, version, rows, columns and storage size. It does not deserialize or display respondent-level records."
        )
      )
    })

    output$connection_banner <- shiny::renderUI({
      shiny::req(admin_ok())
      refresh_tick()
      info <- tryCatch(db_admin_connection_info(pool), error = function(e) NULL)
      if (is.null(info) || !nrow(info)) return(NULL)
      shiny::div(
        class = "alert alert-success py-2",
        shiny::icon("database"), " Connected to ",
        shiny::tags$strong(paste0(info$database_name[[1]], ".", info$schema_name[[1]])),
        " as PostgreSQL role ", shiny::tags$code(info$database_user[[1]]),
        ". Registered accounts are stored in ", shiny::tags$code(paste0(info$schema_name[[1]], ".users")), "."
      )
    })

    output$overview <- shiny::renderUI({
      shiny::req(admin_ok())
      refresh_tick()
      x <- tryCatch(db_admin_overview(pool), error = function(e) NULL)
      if (is.null(x) || !nrow(x)) {
        return(shiny::div(class = "alert alert-warning", "Could not load administrative metrics."))
      }
      metric <- function(icon_name, value, label) {
        shiny::div(class = "sap-metric-card",
          shiny::div(class = "sap-metric-icon", shiny::icon(icon_name)),
          shiny::div(
            shiny::div(class = "sap-metric-value", value),
            shiny::div(class = "sap-metric-label", label)
          ))
      }
      shiny::div(class = "sap-metric-grid mb-3",
        metric("users", x$total_users[[1]], "Registered users"),
        metric("signal", x$online_users[[1]], "Active users (15 min)"),
        metric("user-check", x$active_users[[1]], "Active accounts"),
        metric("key", x$active_sessions[[1]], "Active sessions"),
        metric("folder-tree", x$total_projects[[1]], "Projects"),
        metric("layer-group", x$dataset_versions[[1]], "Dataset versions"),
        metric("hard-drive", .admin_fmt_bytes(x$dataset_bytes[[1]]), "Dataset storage")
      )
    })

    users_data <- shiny::reactive({
      shiny::req(admin_ok())
      refresh_tick()
      tryCatch(
        db_admin_list_users(pool,
          search = input$user_search,
          role = input$user_role,
          status = input$user_status),
        error = function(e) {
          if (!is.null(notify)) notify$error(paste("Could not load users:", conditionMessage(e)), "admin_control_center")
          data.frame()
        }
      )
    })

    output$users_table <- DT::renderDT({
      d <- users_data()
      if (!nrow(d)) return(.admin_empty_dt("No users match the current filters."))
      show <- data.frame(
        ID = d$id,
        Username = d$username,
        Name = d$full_name,
        Email = d$email,
        Role = d$role,
        Status = ifelse(!is.na(d$account_status), d$account_status, ifelse(d$is_active == 1L, "active", "inactive")),
        Projects = d$project_count,
        Datasets = d$dataset_count,
        Online = ifelse(d$online_now == 1L, "Online", "Offline"),
        Sessions = d$active_sessions,
        Created = d$created_at,
        `Last login` = d$last_login,
        check.names = FALSE
      )
      DT::datatable(show, rownames = FALSE, selection = "single",
        options = list(pageLength = 15, scrollX = TRUE, order = list(list(11, "desc"))))
    })

    selected_user_id <- shiny::reactive({
      d <- users_data()
      idx <- input$users_table_rows_selected
      if (!length(idx) || !nrow(d) || idx[[1]] > nrow(d)) return(NULL)
      as.integer(d$id[[idx[[1]]]])
    })

    selected_user <- shiny::reactive({
      uid <- selected_user_id()
      if (is.null(uid)) return(NULL)
      db_admin_get_user(pool, uid)
    })

    output$selected_user_panel <- shiny::renderUI({
      shiny::req(admin_ok())
      u <- selected_user()
      if (is.null(u) || !nrow(u)) {
        return(shiny::div(class = "alert alert-light border",
          shiny::icon("arrow-up"), " Select a user above to inspect and manage the account."))
      }

      text_or_dash <- function(x) {
        if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) "—" else as.character(x[[1]])
      }
      active <- identical(as.integer(u$is_active[[1]]), 1L)
      locked <- !is.na(u$locked_until[[1]]) && u$locked_until[[1]] > Sys.time()

      shiny::tagList(
        bslib::card(
          bslib::card_header(shiny::icon("user-gear"), paste("Selected user:", u$username[[1]])),
          bslib::card_body(
            shiny::div(class = "row g-3",
              shiny::div(class = "col-md-4", shiny::tags$strong("Full name"), shiny::tags$div(text_or_dash(u$full_name))),
              shiny::div(class = "col-md-4", shiny::tags$strong("Email"), shiny::tags$div(text_or_dash(u$email))),
              shiny::div(class = "col-md-4", shiny::tags$strong("Organization"), shiny::tags$div(text_or_dash(u$organization))),
              shiny::div(class = "col-md-4", shiny::tags$strong("Country"), shiny::tags$div(text_or_dash(u$country))),
              shiny::div(class = "col-md-4", shiny::tags$strong("Institution"), shiny::tags$div(text_or_dash(u$institution))),
              shiny::div(class = "col-md-4", shiny::tags$strong("Position"), shiny::tags$div(text_or_dash(u$position))),
              shiny::div(class = "col-md-4", shiny::tags$strong("Account status"), shiny::tags$div(text_or_dash(u$account_status))),
              shiny::div(class = "col-md-4", shiny::tags$strong("Email verified"), shiny::tags$div(if (!is.na(u$email_verified_at[[1]])) "Yes" else "No")),
              shiny::div(class = "col-md-4", shiny::tags$strong("MFA"), shiny::tags$div(if (identical(as.integer(u$mfa_enabled[[1]]), 1L)) "Enabled" else "Off"))
            ),
            shiny::hr(),
            shiny::div(class = "d-flex flex-wrap gap-2 align-items-end",
              shiny::div(style = "min-width:180px;", shiny::selectInput(ns("selected_role"), "Account role",
                choices = c("Admin" = "admin", "User" = "user", "Reviewer" = "reviewer", "Viewer" = "viewer"),
                selected = u$role[[1]])),
              shiny::actionButton(ns("apply_role"), "Apply role", icon = shiny::icon("user-tag"), class = "btn btn-outline-primary"),
              shiny::actionButton(ns("toggle_active"), if (active) "Deactivate" else "Activate",
                icon = shiny::icon(if (active) "user-slash" else "user-check"),
                class = if (active) "btn btn-outline-danger" else "btn btn-outline-success"),
              shiny::div(style = "min-width:180px;", shiny::selectInput(ns("selected_account_status"), "Lifecycle status",
                choices = c("Pending"="pending","Active"="active","Suspended"="suspended","Disabled"="disabled","Archived"="archived"),
                selected = as.character(u$account_status[[1]] %||% if (active) "active" else "disabled"))),
              shiny::actionButton(ns("apply_account_status"), "Apply status", class = "btn btn-outline-secondary"),
              shiny::actionButton(ns("unlock_user"), "Unlock login", icon = shiny::icon("unlock"),
                class = "btn btn-outline-secondary", title = if (locked) "Clear the current login lock" else "Clear failed-login counters"),
              shiny::actionButton(ns("require_password_reset"), "Require password reset", icon = shiny::icon("key"),
                class = "btn btn-outline-warning"),
              shiny::actionButton(ns("revoke_sessions"), "Revoke sessions", icon = shiny::icon("person-circle-xmark"),
                class = "btn btn-outline-warning")
            ),
            shiny::hr(),
            shiny::div(class = "row g-2 align-items-end",
              shiny::div(class = "col-md-4", shiny::numericInput(ns("user_project_quota"), "Project quota (blank/NA = unlimited)", value = suppressWarnings(as.numeric(u$project_quota[[1]])), min = 1)),
              shiny::div(class = "col-md-4", shiny::numericInput(ns("user_storage_quota_gb"), "Storage quota (GB; blank/NA = unlimited)", value = if (is.na(u$storage_quota_bytes[[1]])) NA else round(as.numeric(u$storage_quota_bytes[[1]])/1024^3,2), min = 0.1, step = 0.5)),
              shiny::div(class = "col-md-4", shiny::actionButton(ns("apply_quota"), "Apply quotas", class = "btn btn-outline-primary w-100"))
            ),
            shiny::tags$p(class = "small text-muted mt-2 mb-0",
              "Role/status/session/quota changes are written to the PostgreSQL admin audit. Password hashes and session tokens are never displayed.")
          )
        ),
        bslib::navset_card_tab(
          id = ns("user_detail_tab"),
          bslib::nav_panel("Projects", DT::DTOutput(ns("user_projects"))),
          bslib::nav_panel("Dataset metadata", DT::DTOutput(ns("user_datasets"))),
          bslib::nav_panel("Sessions", DT::DTOutput(ns("user_sessions"))),
          bslib::nav_panel("Activity", DT::DTOutput(ns("user_activity")))
        )
      )
    })

    output$user_projects <- DT::renderDT({
      uid <- selected_user_id(); shiny::req(admin_ok(), !is.null(uid)); refresh_tick()
      d <- tryCatch(db_admin_list_user_projects(pool, uid), error = function(e) data.frame())
      if (!nrow(d)) return(.admin_empty_dt("No projects owned by this user."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$user_datasets <- DT::renderDT({
      uid <- selected_user_id(); shiny::req(admin_ok(), !is.null(uid)); refresh_tick()
      d <- tryCatch(db_admin_list_user_datasets(pool, uid), error = function(e) data.frame())
      if (!nrow(d)) return(.admin_empty_dt("No persisted dataset versions for this user."))
      d$stored_size <- vapply(d$stored_bytes, .admin_fmt_bytes, character(1))
      d$stored_bytes <- NULL
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$user_sessions <- DT::renderDT({
      uid <- selected_user_id(); shiny::req(admin_ok(), !is.null(uid)); refresh_tick()
      d <- tryCatch(db_admin_list_user_sessions(pool, uid), error = function(e) data.frame())
      if (!nrow(d)) return(.admin_empty_dt("No session records for this user."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$user_activity <- DT::renderDT({
      uid <- selected_user_id(); shiny::req(admin_ok(), !is.null(uid)); refresh_tick()
      d <- tryCatch(db_admin_list_user_activity(pool, uid), error = function(e) data.frame())
      if (!nrow(d)) return(.admin_empty_dt("No persisted activity for this user."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$admin_audit <- DT::renderDT({
      shiny::req(admin_ok()); refresh_tick()
      d <- tryCatch(db_admin_list_audit(pool), error = function(e) data.frame())
      if (!nrow(d)) return(.admin_empty_dt("No administrative changes have been recorded yet."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    shiny::observeEvent(input$refresh, nudge(), ignoreInit = TRUE)
    shiny::observeEvent(input$clear_filters, {
      shiny::updateTextInput(session, "user_search", value = "")
      shiny::updateSelectInput(session, "user_role", selected = "all")
      shiny::updateSelectInput(session, "user_status", selected = "all")
    }, ignoreInit = TRUE)

    run_admin_action <- function(fn, success_message) {
      if (!isTRUE(admin_ok())) {
        nudge("Administrator access is required.", "error")
        return(invisible(FALSE))
      }
      tryCatch({
        fn()
        nudge(success_message, "success")
        TRUE
      }, error = function(e) {
        nudge(conditionMessage(e), "error")
        FALSE
      })
    }

    shiny::observeEvent(input$apply_account_status, {
      uid <- selected_user_id(); shiny::req(!is.null(uid))
      run_admin_action(function() db_admin_set_account_status(pool, current_admin_id(), uid, input$selected_account_status), "Account lifecycle status updated.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$apply_quota, {
      uid <- selected_user_id(); shiny::req(!is.null(uid))
      pq <- suppressWarnings(as.integer(input$user_project_quota)); if (is.na(pq)) pq <- NULL
      gb <- suppressWarnings(as.numeric(input$user_storage_quota_gb)); bytes <- if (is.na(gb)) NULL else as.numeric(gb) * 1024^3
      run_admin_action(function() db_set_user_quota(pool, current_admin_id(), uid, bytes, pq), "User quotas updated.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$apply_role, {
      uid <- selected_user_id(); shiny::req(!is.null(uid))
      run_admin_action(
        function() db_admin_set_user_role(pool, current_admin_id(), uid, input$selected_role),
        "Account role updated."
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$toggle_active, {
      u <- selected_user(); shiny::req(!is.null(u), nrow(u) == 1L)
      uid <- as.integer(u$id[[1]])
      currently_active <- identical(as.integer(u$is_active[[1]]), 1L)
      action_label <- if (currently_active) "Deactivate account" else "Activate account"
      detail <- if (currently_active) {
        "Deactivation prevents new sign-ins and immediately revokes active sessions. Projects and datasets remain stored."
      } else {
        "Activation allows this account to sign in again."
      }
      shiny::showModal(shiny::modalDialog(
        title = action_label,
        shiny::tags$p(shiny::tags$strong(u$username[[1]])),
        shiny::tags$p(detail),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_toggle_active"), action_label,
            class = if (currently_active) "btn btn-danger" else "btn btn-success")
        )
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_toggle_active, {
      u <- selected_user(); shiny::req(!is.null(u), nrow(u) == 1L)
      uid <- as.integer(u$id[[1]])
      make_active <- !identical(as.integer(u$is_active[[1]]), 1L)
      shiny::removeModal()
      run_admin_action(
        function() db_admin_set_user_active(pool, current_admin_id(), uid, make_active),
        if (make_active) "Account activated." else "Account deactivated and sessions revoked."
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$unlock_user, {
      uid <- selected_user_id(); shiny::req(!is.null(uid))
      run_admin_action(
        function() db_admin_unlock_user(pool, current_admin_id(), uid),
        "Login lock cleared."
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$require_password_reset, {
      uid <- selected_user_id(); shiny::req(!is.null(uid), isTRUE(admin_ok()))
      tryCatch({
        token <- db_admin_issue_password_reset(pool, current_admin_id(), uid, 60L)
        nudge("Password reset required; active sessions revoked.", "success")
        shiny::showModal(shiny::modalDialog(
          title = "One-time password reset token",
          shiny::tags$p("A reset message was queued for the user's email when one is available. This token is shown once so an administrator can deliver it securely if SMTP is not configured."),
          shiny::tags$pre(style = "white-space:pre-wrap; word-break:break-all;", token),
          shiny::tags$p(class = "text-danger small", "Treat this token like a temporary password. It expires in 60 minutes."),
          easyClose = TRUE, footer = shiny::modalButton("Close")
        ))
      }, error = function(e) nudge(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$revoke_sessions, {
      u <- selected_user(); shiny::req(!is.null(u), nrow(u) == 1L)
      shiny::showModal(shiny::modalDialog(
        title = "Revoke active sessions",
        shiny::tags$p("Sign out all active sessions for ", shiny::tags$strong(u$username[[1]]), "?"),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_revoke_sessions"), "Revoke sessions", class = "btn btn-warning")
        )
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_revoke_sessions, {
      uid <- selected_user_id(); shiny::req(!is.null(uid))
      shiny::removeModal()
      run_admin_action(
        function() db_admin_revoke_user_sessions(pool, current_admin_id(), uid),
        "Active sessions revoked."
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$revoke_all_sessions, {
      shiny::showModal(shiny::modalDialog(
        title = "Sign out all other users?",
        shiny::tags$p("This immediately revokes every active PostgreSQL session except your own administrator session. Users can sign in again if their accounts remain active."),
        easyClose = TRUE,
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("confirm_revoke_all_sessions"), "Sign out other users", class = "btn btn-warning"))
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_revoke_all_sessions, {
      shiny::removeModal()
      run_admin_action(function() {
        n <- db_admin_revoke_all_other_sessions(pool, current_admin_id())
        if (!is.null(notify)) notify$success(paste(n, "active session(s) revoked."), "admin_control_center")
      }, "System session control completed.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cleanup_sessions, {
      run_admin_action(function() db_admin_cleanup_expired_sessions(pool, current_admin_id(), 90L),
                       "Old expired/revoked session records cleaned.")
    }, ignoreInit = TRUE)

    invisible(list(is_admin = admin_ok, refresh = refresh_tick))
  })
}
