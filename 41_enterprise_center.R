# =============================================================================
# MODULE 41: ENTERPRISE CENTER
# Organizations, teams, invitations, collaboration, notifications, security,
# recycle/version management, API/integration administration and operations.
# =============================================================================

.sap_enterprise_empty_dt <- function(message) {
  DT::datatable(data.frame(Status = message), rownames = FALSE, options = list(dom = "t"))
}

.sap_fmt_bytes <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || is.na(x)) return("—")
  units <- c("B", "KB", "MB", "GB", "TB")
  idx <- min(length(units), floor(log(max(x, 1), 1024)) + 1L)
  sprintf("%.1f %s", x / 1024^(idx - 1L), units[[idx]])
}

mod_enterprise_center_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "mb-4",
      shiny::tags$h2(shiny::icon("building-shield"), " Enterprise Center"),
      shiny::tags$p(class = "text-muted",
        "Accounts, organizations, teams, project access, collaboration, recovery, notifications, API access and operational governance in one place.")
    ),
    shiny::uiOutput(ns("connection_gate")),
    shiny::uiOutput(ns("enterprise_body"))
  )
}

mod_enterprise_center_server <- function(id, app_state, services, pool = NULL, demo_mode = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- shiny::reactiveVal(0L)
    bump <- function() tick(shiny::isolate(tick()) + 1L)
    notify <- function(msg, type = "message") {
      if (!is.null(services$notify)) services$notify$notify(msg, type = type, module = "enterprise")
      else shiny::showNotification(msg, type = type)
    }
    uid <- shiny::reactive({
      services$auth$version()
      as.integer(app_state$current_user %||% NA_integer_)
    })
    signed_in <- shiny::reactive(!is.null(pool) && !is.na(uid()) && isTRUE(services$auth$is_authenticated()))
    is_admin <- shiny::reactive(isTRUE(signed_in()) && isTRUE(services$auth$is_admin()))
    pid <- shiny::reactive({
      x <- app_state$current_project
      if (is.null(x)) NA_integer_ else as.integer(x)
    })

    output$connection_gate <- shiny::renderUI({
      if (isTRUE(demo_mode) || is.null(pool)) {
        return(shiny::div(class = "alert alert-info", shiny::icon("database"),
          " Enterprise controls require the connected PostgreSQL runtime. The local/Guest workspace remains available for analysis."))
      }
      if (!isTRUE(signed_in())) {
        return(shiny::div(class = "alert alert-warning", shiny::icon("right-to-bracket"),
          " Sign in with a persistent account to use organizations, invitations, notifications, security devices and API tokens."))
      }
      NULL
    })

    output$enterprise_body <- shiny::renderUI({
      if (!isTRUE(signed_in())) return(NULL)
      bslib::navset_card_tab(
        id = ns("enterprise_tab"),
        bslib::nav_panel("Overview & Search", value = "overview", enterprise_overview_ui(ns)),
        bslib::nav_panel("Organizations & Teams", value = "organizations", enterprise_org_ui(ns)),
        bslib::nav_panel("Project Access", value = "access", enterprise_access_ui(ns)),
        bslib::nav_panel("Collaboration", value = "collaboration", enterprise_collaboration_ui(ns)),
        bslib::nav_panel("Notifications", value = "notifications", enterprise_notifications_ui(ns)),
        bslib::nav_panel("Account Security", value = "security", enterprise_security_ui(ns)),
        bslib::nav_panel("Versions & Recycle Bin", value = "versions", enterprise_versions_ui(ns)),
        bslib::nav_panel("API & Integrations", value = "api", enterprise_api_ui(ns)),
        bslib::nav_panel("Operations", value = "operations", enterprise_operations_ui(ns))
      )
    })

    accessible_projects <- shiny::reactive({
      shiny::req(signed_in()); tick()
      tryCatch(db_list_accessible_projects(pool, uid(), include_archived = TRUE), error = function(e) data.frame())
    })
    orgs <- shiny::reactive({
      shiny::req(signed_in()); tick()
      tryCatch(db_list_user_organizations(pool, uid()), error = function(e) data.frame())
    })

    shiny::observe({
      shiny::req(signed_in()); d <- accessible_projects()
      ch <- if (nrow(d)) stats::setNames(d$id, paste0(d$name, " — ", d$permission)) else character()
      selected <- if (!is.na(pid()) && pid() %in% d$id) pid() else if (nrow(d)) d$id[[1]] else NULL
      for (id in c("access_project", "collab_project", "version_project", "manage_project", "org_project"))
        shiny::updateSelectInput(session, id, choices = ch, selected = selected)
    })
    shiny::observe({
      shiny::req(signed_in()); d <- orgs()
      ch <- if (nrow(d)) stats::setNames(d$id, paste0(d$name, " — ", d$role)) else character()
      shiny::updateSelectInput(session, "org_choice", choices = ch, selected = if (nrow(d)) d$id[[1]] else NULL)
    })

    # ---- Overview, usage and global search ---------------------------------
    output$usage_summary <- shiny::renderUI({
      shiny::req(signed_in()); tick()
      u <- tryCatch(db_user_usage(pool, uid()), error = function(e) data.frame())
      if (!nrow(u)) return(shiny::div(class = "alert alert-light border", "Usage data unavailable."))
      storage_q <- u$storage_quota_bytes[[1]]; project_q <- u$project_quota[[1]]
      shiny::div(class = "row g-3",
        shiny::div(class = "col-md-3", bslib::value_box("Projects", as.character(u$project_count[[1]]), showcase = shiny::icon("folder"))),
        shiny::div(class = "col-md-3", bslib::value_box("Stored data", .sap_fmt_bytes(u$storage_bytes[[1]]), showcase = shiny::icon("hard-drive"))),
        shiny::div(class = "col-md-3", bslib::value_box("Project quota", if (is.na(project_q)) "Unlimited" else as.character(project_q), showcase = shiny::icon("gauge"))),
        shiny::div(class = "col-md-3", bslib::value_box("Storage quota", if (is.na(storage_q)) "Unlimited" else .sap_fmt_bytes(storage_q), showcase = shiny::icon("database")))
      )
    })
    search_data <- shiny::eventReactive(input$run_global_search, {
      q <- trimws(input$global_search %||% "")
      if (!nzchar(q)) return(data.frame())
      tryCatch(db_global_search(pool, uid(), q), error = function(e) { notify(conditionMessage(e), "error"); data.frame() })
    }, ignoreInit = TRUE)
    output$global_search_results <- DT::renderDT({
      d <- search_data(); if (!nrow(d)) return(.sap_enterprise_empty_dt("Search projects, datasets, or variables."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 15, scrollX = TRUE))
    })
    shiny::observeEvent(input$open_search_result, {
      d <- search_data(); idx <- input$global_search_results_rows_selected
      if (!length(idx) || !nrow(d)) return(invisible(NULL))
      project_id <- suppressWarnings(as.integer(d$project_id[[idx[[1]]]]))
      if (!is.na(project_id)) {
        ok <- tryCatch(services$project$open(project_id), error = function(e) FALSE)
        if (isTRUE(ok)) notify("Project opened from global search.")
      }
    }, ignoreInit = TRUE)

    # ---- Organizations and teams -------------------------------------------
    output$org_members <- DT::renderDT({
      shiny::req(signed_in()); tick(); oid <- suppressWarnings(as.integer(input$org_choice))
      if (is.na(oid)) return(.sap_enterprise_empty_dt("Create or select an organization."))
      d <- tryCatch(db_list_organization_members(pool, oid, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No members."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    output$teams_table <- DT::renderDT({
      shiny::req(signed_in()); tick(); oid <- suppressWarnings(as.integer(input$org_choice))
      if (is.na(oid)) return(.sap_enterprise_empty_dt("No teams."))
      d <- tryCatch(db_list_teams(pool, oid, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No teams yet."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10))
    })
    shiny::observeEvent(input$create_org, {
      tryCatch({ db_create_organization(pool, uid(), input$new_org_name, input$new_org_description); bump(); notify("Organization created.") },
               error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$add_org_member, {
      oid <- suppressWarnings(as.integer(input$org_choice)); member <- suppressWarnings(as.integer(input$org_member_id))
      if (is.na(oid) || is.na(member)) return(notify("Select an organization and enter a valid user ID.", "warning"))
      tryCatch({ db_add_organization_member(pool, oid, uid(), member, input$org_member_role); bump(); notify("Organization member updated.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$create_team, {
      oid <- suppressWarnings(as.integer(input$org_choice)); if (is.na(oid)) return(notify("Select an organization first.", "warning"))
      tryCatch({ db_create_team(pool, oid, uid(), input$new_team_name, input$new_team_description); bump(); notify("Team created.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$assign_project_org, {
      oid <- suppressWarnings(as.integer(input$org_choice)); project_id <- suppressWarnings(as.integer(input$org_project))
      if (is.na(oid) || is.na(project_id)) return(notify("Select an organization and project.", "warning"))
      tryCatch({ db_assign_project_organization(pool, project_id, uid(), oid); bump(); notify("Project assigned to organization.") }, error=function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$assign_project_team, {
      oid <- suppressWarnings(as.integer(input$org_choice)); project_id <- suppressWarnings(as.integer(input$org_project))
      d <- tryCatch(db_list_teams(pool, oid, uid()), error=function(e) data.frame()); idx <- input$teams_table_rows_selected
      if (!length(idx) || !nrow(d) || is.na(project_id)) return(notify("Select a team and project.", "warning"))
      tryCatch({ db_assign_project_team(pool, d$id[[idx[[1]]]], project_id, uid(), input$team_project_permission); bump(); notify("Team project access updated.") }, error=function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$add_team_member, {
      d <- tryCatch(db_list_teams(pool, as.integer(input$org_choice), uid()), error = function(e) data.frame())
      idx <- input$teams_table_rows_selected; member <- suppressWarnings(as.integer(input$team_member_id))
      if (!length(idx) || !nrow(d) || is.na(member)) return(notify("Select a team and enter a valid user ID.", "warning"))
      tryCatch({ db_add_team_member(pool, d$id[[idx[[1]]]], uid(), member, input$team_member_role); bump(); notify("Team membership updated.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    # ---- Project access and invitations ------------------------------------
    output$project_members <- DT::renderDT({
      shiny::req(signed_in()); tick(); project_id <- suppressWarnings(as.integer(input$access_project))
      if (is.na(project_id)) return(.sap_enterprise_empty_dt("Select a project."))
      d <- tryCatch(db_list_project_members(pool, project_id, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No direct members."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10, scrollX = TRUE))
    })
    shiny::observeEvent(input$invite_user, {
      project_id <- suppressWarnings(as.integer(input$access_project)); if (is.na(project_id)) return(notify("Select a project.", "warning"))
      tryCatch({
        token <- db_create_project_invitation(pool, project_id, uid(), input$invite_email, input$invite_permission)
        bump(); notify("Invitation created and queued for email delivery.")
        output$invite_token_preview <- shiny::renderUI(shiny::div(class = "alert alert-secondary mt-2", shiny::tags$strong("Fallback invitation token: "), shiny::tags$code(token), shiny::tags$br(), shiny::span(class = "small", "Share this securely only if email delivery is not configured.")))
      }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$accept_invitation, {
      tryCatch({ project_id <- db_accept_project_invitation(pool, uid(), input$invitation_token); bump(); notify(paste("Invitation accepted. Project", project_id, "is now available.")) }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$set_member_permission, {
      project_id <- suppressWarnings(as.integer(input$access_project)); d <- tryCatch(db_list_project_members(pool, project_id, uid()), error = function(e) data.frame()); idx <- input$project_members_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select a project member.", "warning"))
      tryCatch({ db_set_project_member(pool, project_id, uid(), d$id[[idx[[1]]]], input$member_permission); bump(); notify("Project permission updated.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$remove_member, {
      project_id <- suppressWarnings(as.integer(input$access_project)); d <- tryCatch(db_list_project_members(pool, project_id, uid()), error = function(e) data.frame()); idx <- input$project_members_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select a project member.", "warning"))
      tryCatch({ db_remove_project_member(pool, project_id, uid(), d$id[[idx[[1]]]]); bump(); notify("Project access removed.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    # ---- Comments and review workflow --------------------------------------
    output$comments_table <- DT::renderDT({
      shiny::req(signed_in()); tick(); project_id <- suppressWarnings(as.integer(input$collab_project))
      if (is.na(project_id)) return(.sap_enterprise_empty_dt("Select a project."))
      d <- tryCatch(db_list_project_comments(pool, project_id, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No comments yet."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10, scrollX = TRUE))
    })
    output$reviews_table <- DT::renderDT({
      shiny::req(signed_in()); tick(); project_id <- suppressWarnings(as.integer(input$collab_project))
      if (is.na(project_id)) return(.sap_enterprise_empty_dt("Select a project."))
      d <- tryCatch(db_list_review_requests(pool, project_id, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No review requests."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10, scrollX = TRUE))
    })
    shiny::observeEvent(input$add_comment, {
      project_id <- suppressWarnings(as.integer(input$collab_project)); assigned <- suppressWarnings(as.integer(input$comment_assignee)); if (is.na(assigned)) assigned <- NULL
      tryCatch({ db_add_project_comment(pool, project_id, uid(), input$comment_text, input$comment_object_type, input$comment_object_key, assigned); bump(); notify("Comment added.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$resolve_comment, {
      project_id <- suppressWarnings(as.integer(input$collab_project)); d <- tryCatch(db_list_project_comments(pool, project_id, uid()), error = function(e) data.frame()); idx <- input$comments_table_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select a comment.", "warning"))
      tryCatch({ db_resolve_project_comment(pool, d$id[[idx[[1]]]], uid()); bump(); notify("Comment resolved.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$request_review, {
      project_id <- suppressWarnings(as.integer(input$collab_project)); reviewer <- suppressWarnings(as.integer(input$reviewer_user_id)); if (is.na(reviewer)) reviewer <- NULL
      tryCatch({ db_create_review_request(pool, project_id, uid(), input$review_object_type, input$review_object_key, reviewer); bump(); notify("Review requested.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$update_review, {
      project_id <- suppressWarnings(as.integer(input$collab_project)); d <- tryCatch(db_list_review_requests(pool, project_id, uid()), error = function(e) data.frame()); idx <- input$reviews_table_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select a review request.", "warning"))
      tryCatch({ db_update_review_status(pool, d$id[[idx[[1]]]], uid(), input$review_status, input$review_note); bump(); notify("Review status updated.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    # ---- Durable notification center ---------------------------------------
    output$notifications_table <- DT::renderDT({
      shiny::req(signed_in()); tick()
      d <- tryCatch(db_list_notifications(pool, uid(), unread_only = isTRUE(input$unread_only)), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No notifications."))
      keep <- intersect(c("id","notification_type","title","body","project_id","read_at","created_at"), names(d))
      DT::datatable(d[, keep, drop = FALSE], rownames = FALSE, selection = "single", options = list(pageLength = 15, scrollX = TRUE))
    })
    shiny::observeEvent(input$mark_notification_read, {
      d <- tryCatch(db_list_notifications(pool, uid(), unread_only = FALSE), error = function(e) data.frame()); idx <- input$notifications_table_rows_selected
      if (!length(idx) || !nrow(d)) return(invisible(NULL))
      db_mark_notification_read(pool, d$id[[idx[[1]]]], uid()); bump()
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$mark_all_read, { db_mark_all_notifications_read(pool, uid()); bump(); notify("Notifications marked as read.") }, ignoreInit = TRUE)

    # ---- Account security, verification, MFA and devices -------------------
    output$security_profile <- shiny::renderUI({
      shiny::req(signed_in()); tick(); u <- services$auth$current_user(); if (is.null(u)) return(NULL)
      verified <- !is.null(u$email_verified_at) && !is.na(u$email_verified_at[[1]])
      mfa <- !is.null(u$mfa_enabled) && identical(as.integer(u$mfa_enabled[[1]]), 1L)
      shiny::div(class = "row g-3",
        shiny::div(class = "col-md-4", bslib::value_box("Email", if (verified) "Verified" else "Unverified", showcase = shiny::icon(if (verified) "circle-check" else "envelope"))),
        shiny::div(class = "col-md-4", bslib::value_box("MFA", if (mfa) "Enabled" else "Off", showcase = shiny::icon("shield-halved"))),
        shiny::div(class = "col-md-4", bslib::value_box("Account", as.character(u$account_status[[1]] %||% if (u$is_active[[1]] == 1) "active" else "inactive"), showcase = shiny::icon("user-shield")))
      )
    })
    output$sessions_table <- DT::renderDT({
      shiny::req(signed_in()); tick(); d <- tryCatch(db_list_active_sessions(pool, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No device sessions."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10, scrollX = TRUE))
    })
    shiny::observeEvent(input$request_email_verification, {
      res <- services$auth$request_email_verification(); if (isTRUE(res$ok)) notify("Verification message queued.") else notify(res$error, "error")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$verify_email_token, {
      res <- services$auth$verify_email_with_token(input$email_verification_token); if (isTRUE(res$ok)) { bump(); services$auth$version(); notify("Email verified.") } else notify(res$error, "error")
    }, ignoreInit = TRUE)
    mfa_enrollment <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$begin_mfa, {
      tryCatch({
        e <- db_begin_mfa_enrollment(pool, uid()); mfa_enrollment(e)
        output$mfa_setup <- shiny::renderUI(shiny::div(class = "alert alert-info mt-2",
          shiny::tags$strong("Authenticator secret: "), shiny::tags$code(e$secret), shiny::tags$br(),
          shiny::span("Add this secret to your authenticator app as a time-based 6-digit code, then verify below."), shiny::tags$hr(),
          shiny::tags$strong("Recovery codes — store offline: "), shiny::tags$code(paste(e$recovery_codes, collapse = "  "))))
      }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$confirm_mfa, {
      tryCatch({
        if (!isTRUE(db_verify_mfa_enrollment(pool, uid(), input$mfa_confirm_code))) stop("The code did not match. Check the authenticator clock and try again.", call. = FALSE)
        mfa_enrollment(NULL); bump(); notify("Two-factor authentication enabled.")
      }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$disable_mfa, {
      tryCatch({ db_disable_mfa(pool, uid()); bump(); notify("Two-factor authentication disabled.", "warning") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$revoke_device, {
      d <- tryCatch(db_list_active_sessions(pool, uid()), error = function(e) data.frame()); idx <- input$sessions_table_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select a session/device.", "warning"))
      sid <- as.integer(d$id[[idx[[1]]]])
      if (identical(sid, services$auth$current_session_id())) return(notify("Use Sign out to end the current browser session.", "warning"))
      db_revoke_session_by_id(pool, uid(), sid); bump(); notify("Selected device session revoked.")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$revoke_other_devices, {
      tryCatch({
        db_revoke_other_user_sessions(pool, uid(), except_session_id = services$auth$current_session_id())
        bump(); notify("All other device sessions were signed out.")
      }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    # ---- Versions, folders, tags and recycle bin ---------------------------
    output$dataset_versions <- DT::renderDT({
      shiny::req(signed_in()); tick(); project_id <- suppressWarnings(as.integer(input$version_project))
      if (is.na(project_id)) return(.sap_enterprise_empty_dt("Select a project."))
      d <- tryCatch(db_list_dataset_versions(pool, project_id, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No persisted dataset versions."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })
    output$recycle_table <- DT::renderDT({
      shiny::req(signed_in()); tick(); d <- tryCatch(db_list_recycled_projects(pool, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("Recycle bin is empty."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10))
    })
    output$folders_table <- DT::renderDT({
      shiny::req(signed_in()); tick(); d <- tryCatch(db_list_folders(pool, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No project folders."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10))
    })
    shiny::observeEvent(input$create_folder, {
      tryCatch({ db_create_folder(pool, uid(), input$folder_name); bump(); notify("Folder created.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$move_to_folder, {
      project_id <- suppressWarnings(as.integer(input$manage_project)); d <- tryCatch(db_list_folders(pool, uid()), error = function(e) data.frame()); idx <- input$folders_table_rows_selected
      if (is.na(project_id) || !length(idx) || !nrow(d)) return(notify("Select a project and folder.", "warning"))
      tryCatch({ db_move_project_to_folder(pool, project_id, uid(), d$id[[idx[[1]]]]); bump(); notify("Project moved to folder.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$add_project_tag, {
      project_id <- suppressWarnings(as.integer(input$manage_project)); if (is.na(project_id)) return(notify("Select a project.", "warning"))
      tryCatch({ db_add_project_tag(pool, project_id, uid(), input$project_tag); bump(); notify("Tag added.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$restore_recycled, {
      d <- tryCatch(db_list_recycled_projects(pool, uid()), error = function(e) data.frame()); idx <- input$recycle_table_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select a recycled project.", "warning"))
      db_restore_project(pool, d$id[[idx[[1]]]], uid()); bump(); notify("Project restored.")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$purge_recycled, {
      d <- tryCatch(db_list_recycled_projects(pool, uid()), error = function(e) data.frame()); idx <- input$recycle_table_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select a recycled project.", "warning"))
      db_purge_project(pool, d$id[[idx[[1]]]], uid()); bump(); notify("Project permanently deleted.", "warning")
    }, ignoreInit = TRUE)

    # ---- API tokens and integrations ---------------------------------------
    output$api_tokens <- DT::renderDT({
      shiny::req(signed_in()); tick(); d <- tryCatch(db_list_api_tokens(pool, uid()), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No API tokens."))
      DT::datatable(d, rownames = FALSE, selection = "single", options = list(pageLength = 10, scrollX = TRUE))
    })
    shiny::observeEvent(input$create_api_token, {
      tryCatch({
        scopes <- unique(as.character(input$api_scopes %||% "read")); token <- db_create_api_token(pool, uid(), input$api_token_name, scopes, input$api_expiry_days)
        bump(); output$new_api_token <- shiny::renderUI(shiny::div(class = "alert alert-warning", shiny::tags$strong("Copy this token now — it will not be shown again:"), shiny::tags$br(), shiny::tags$code(token)))
      }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$revoke_api_token, {
      d <- tryCatch(db_list_api_tokens(pool, uid()), error = function(e) data.frame()); idx <- input$api_tokens_rows_selected
      if (!length(idx) || !nrow(d)) return(notify("Select an API token.", "warning"))
      db_revoke_api_token(pool, uid(), d$id[[idx[[1]]]]); bump(); notify("API token revoked.")
    }, ignoreInit = TRUE)
    output$sso_table <- DT::renderDT({
      if (!isTRUE(is_admin())) return(.sap_enterprise_empty_dt("Administrator access is required to configure SSO."))
      tick(); d <- tryCatch(db_list_sso_providers(pool), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No SSO providers registered."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    shiny::observeEvent(input$register_sso, {
      if (!isTRUE(is_admin())) return(notify("Administrator access is required.", "error"))
      org <- suppressWarnings(as.integer(input$sso_org_id)); if (is.na(org)) org <- NULL
      tryCatch({ db_register_sso_provider(pool, uid(), org, input$sso_type, input$sso_name, input$sso_issuer, input$sso_client_id, input$sso_secret_ref, input$sso_scopes, FALSE); bump(); notify("SSO provider registered. Enable it only after the external identity-provider callback is configured.") }, error = function(e) notify(conditionMessage(e), "error"))
    }, ignoreInit = TRUE)

    # ---- Operations and unified audit --------------------------------------
    output$audit_table <- DT::renderDT({
      shiny::req(signed_in()); tick(); project_id <- suppressWarnings(as.integer(input$ops_project)); if (is.na(project_id)) project_id <- NULL
      d <- tryCatch(db_list_audit_events(pool, uid(), project_id), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No enterprise audit events recorded yet."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })
    shiny::observe({
      d <- accessible_projects(); ch <- c("My activity" = "", if (nrow(d)) stats::setNames(d$id, d$name) else character())
      shiny::updateSelectInput(session, "ops_project", choices = ch)
    })
    output$backup_table <- DT::renderDT({
      if (!isTRUE(is_admin())) return(.sap_enterprise_empty_dt("Administrator access is required for backup history."))
      tick(); d <- tryCatch(db_list_backup_runs(pool), error = function(e) data.frame())
      if (!nrow(d)) return(.sap_enterprise_empty_dt("No backup runs recorded. Use scripts/backup_database.R or the deployment scheduler."))
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    output$health_snapshot <- shiny::renderUI({
      shiny::req(signed_in()); tick()
      health <- tryCatch(db_health_check(pool), error = function(e) NULL)
      projects <- nrow(accessible_projects())
      notes <- tryCatch(nrow(db_list_notifications(pool, uid(), unread_only = TRUE)), error = function(e) 0L)
      shiny::div(class = "row g-3",
        shiny::div(class = "col-md-3", bslib::value_box("Database", if (is.null(health)) "Unavailable" else "Healthy", showcase = shiny::icon("database"))),
        shiny::div(class = "col-md-3", bslib::value_box("Accessible projects", as.character(projects), showcase = shiny::icon("folder-open"))),
        shiny::div(class = "col-md-3", bslib::value_box("Unread notifications", as.character(notes), showcase = shiny::icon("bell"))),
        shiny::div(class = "col-md-3", bslib::value_box("Runtime", Sys.getenv("SAP_RUNTIME_PROFILE", "development"), showcase = shiny::icon("server")))
      )
    })

    # Autosave is owned centrally by app.R's debounced workspace snapshot.
    # Enterprise Center deliberately does not create a second writer.

    invisible(list(refresh = tick))
  })
}

enterprise_overview_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("usage_summary")),
    bslib::card(
      class = "mt-3",
      bslib::card_header(shiny::icon("magnifying-glass"), " Global search"),
      bslib::card_body(
        shiny::div(
          class = "row g-2 align-items-end",
          shiny::div(class = "col-md-9", shiny::textInput(ns("global_search"), "Search", placeholder = "Projects, datasets, variables...")),
          shiny::div(class = "col-md-3", shiny::actionButton(ns("run_global_search"), "Search", class = "btn btn-primary w-100"))
        ),
        DT::DTOutput(ns("global_search_results")),
        shiny::actionButton(ns("open_search_result"), "Open selected project", icon = shiny::icon("folder-open"), class = "btn btn-outline-primary mt-2")
      )
    )
  )
}

enterprise_org_ui <- function(ns) {
  shiny::tagList(
    shiny::div(
      class = "row g-3",
      shiny::div(
        class = "col-lg-5",
        bslib::card(
          bslib::card_header("Create organization"),
          bslib::card_body(
            shiny::textInput(ns("new_org_name"), "Name"),
            shiny::textAreaInput(ns("new_org_description"), "Description", rows = 2),
            shiny::actionButton(ns("create_org"), "Create organization", class = "btn btn-primary")
          )
        )
      ),
      shiny::div(
        class = "col-lg-7",
        bslib::card(
          bslib::card_header("Organization"),
          bslib::card_body(
            shiny::selectInput(ns("org_choice"), "Selected organization", choices = character()),
            DT::DTOutput(ns("org_members"))
          )
        )
      )
    ),
    shiny::div(
      class = "row g-3 mt-1",
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Add organization member"),
          bslib::card_body(
            shiny::numericInput(ns("org_member_id"), "User ID", NA, min = 1),
            shiny::selectInput(ns("org_member_role"), "Role", c("Admin" = "admin", "Manager" = "manager", "Member" = "member", "Viewer" = "viewer")),
            shiny::actionButton(ns("add_org_member"), "Add / update member", class = "btn btn-outline-primary")
          )
        )
      ),
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Teams"),
          bslib::card_body(
            shiny::textInput(ns("new_team_name"), "Team name"),
            shiny::textInput(ns("new_team_description"), "Description"),
            shiny::actionButton(ns("create_team"), "Create team", class = "btn btn-outline-primary"),
            shiny::hr(),
            DT::DTOutput(ns("teams_table")),
            shiny::numericInput(ns("team_member_id"), "User ID to add", NA, min = 1),
            shiny::selectInput(ns("team_member_role"), "Team role", c("Lead" = "lead", "Member" = "member", "Viewer" = "viewer")),
            shiny::actionButton(ns("add_team_member"), "Add to selected team", class = "btn btn-outline-secondary"),
            shiny::hr(),
            shiny::selectInput(ns("org_project"), "Project", choices = character()),
            shiny::actionButton(ns("assign_project_org"), "Assign project to organization", class = "btn btn-outline-primary"),
            shiny::selectInput(ns("team_project_permission"), "Selected team access", c("Editor" = "editor", "Reviewer" = "reviewer", "Viewer" = "viewer")),
            shiny::actionButton(ns("assign_project_team"), "Grant selected team project access", class = "btn btn-outline-secondary")
          )
        )
      )
    )
  )
}

enterprise_access_ui <- function(ns) {
  shiny::tagList(
    shiny::selectInput(ns("access_project"), "Project", choices = character()),
    shiny::div(
      class = "row g-3",
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Invite by email"),
          bslib::card_body(
            shiny::textInput(ns("invite_email"), "Email"),
            shiny::selectInput(ns("invite_permission"), "Permission", c("Editor" = "editor", "Reviewer" = "reviewer", "Viewer" = "viewer")),
            shiny::actionButton(ns("invite_user"), "Create invitation", class = "btn btn-primary"),
            shiny::uiOutput(ns("invite_token_preview"))
          )
        )
      ),
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Accept invitation"),
          bslib::card_body(
            shiny::textInput(ns("invitation_token"), "Invitation token"),
            shiny::actionButton(ns("accept_invitation"), "Accept invitation", class = "btn btn-outline-primary")
          )
        )
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Direct project members"),
      bslib::card_body(
        DT::DTOutput(ns("project_members")),
        shiny::div(
          class = "d-flex gap-2 mt-2",
          shiny::selectInput(ns("member_permission"), NULL, c("Editor" = "editor", "Reviewer" = "reviewer", "Viewer" = "viewer"), width = "180px"),
          shiny::actionButton(ns("set_member_permission"), "Change permission", class = "btn btn-outline-primary"),
          shiny::actionButton(ns("remove_member"), "Remove access", class = "btn btn-outline-danger")
        )
      )
    )
  )
}

enterprise_collaboration_ui <- function(ns) {
  shiny::tagList(
    shiny::selectInput(ns("collab_project"), "Project", choices = character()),
    shiny::div(
      class = "row g-3",
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Comments & assignments"),
          bslib::card_body(
            shiny::selectInput(ns("comment_object_type"), "Object", c("Project" = "project", "Dashboard" = "dashboard", "Dataset" = "dataset", "Report" = "report", "Chart" = "chart")),
            shiny::textInput(ns("comment_object_key"), "Object key / name (optional)"),
            shiny::textAreaInput(ns("comment_text"), "Comment", rows = 3),
            shiny::numericInput(ns("comment_assignee"), "Assign to user ID (optional)", NA, min = 1),
            shiny::actionButton(ns("add_comment"), "Add comment", class = "btn btn-primary"),
            shiny::hr(),
            DT::DTOutput(ns("comments_table")),
            shiny::actionButton(ns("resolve_comment"), "Resolve selected", class = "btn btn-outline-success mt-2")
          )
        )
      ),
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Approval workflow"),
          bslib::card_body(
            shiny::selectInput(ns("review_object_type"), "Object", c("Dashboard" = "dashboard", "Report" = "report", "Dataset" = "dataset", "Analysis" = "analysis")),
            shiny::textInput(ns("review_object_key"), "Object key / name"),
            shiny::numericInput(ns("reviewer_user_id"), "Reviewer user ID (optional)", NA, min = 1),
            shiny::actionButton(ns("request_review"), "Request review", class = "btn btn-primary"),
            shiny::hr(),
            DT::DTOutput(ns("reviews_table")),
            shiny::selectInput(ns("review_status"), "Decision", c("Request changes" = "changes_requested", "Approve" = "approved", "Publish" = "published")),
            shiny::textInput(ns("review_note"), "Decision note"),
            shiny::actionButton(ns("update_review"), "Update selected review", class = "btn btn-outline-primary")
          )
        )
      )
    )
  )
}

enterprise_notifications_ui <- function(ns) {
  shiny::tagList(
    shiny::div(
      class = "d-flex gap-3 align-items-center mb-2",
      shiny::checkboxInput(ns("unread_only"), "Unread only", FALSE),
      shiny::actionButton(ns("mark_all_read"), "Mark all read", class = "btn btn-sm btn-outline-secondary")
    ),
    DT::DTOutput(ns("notifications_table")),
    shiny::actionButton(ns("mark_notification_read"), "Mark selected read", class = "btn btn-outline-primary mt-2")
  )
}

enterprise_security_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("security_profile")),
    shiny::div(
      class = "row g-3 mt-1",
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Email verification"),
          bslib::card_body(
            shiny::actionButton(ns("request_email_verification"), "Send verification token", class = "btn btn-outline-primary"),
            shiny::textInput(ns("email_verification_token"), "Verification token"),
            shiny::actionButton(ns("verify_email_token"), "Verify email", class = "btn btn-primary")
          )
        )
      ),
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Authenticator MFA"),
          bslib::card_body(
            shiny::actionButton(ns("begin_mfa"), "Set up MFA", class = "btn btn-outline-primary"),
            shiny::uiOutput(ns("mfa_setup")),
            shiny::textInput(ns("mfa_confirm_code"), "6-digit confirmation code"),
            shiny::div(
              class = "d-flex gap-2",
              shiny::actionButton(ns("confirm_mfa"), "Enable MFA", class = "btn btn-primary"),
              shiny::actionButton(ns("disable_mfa"), "Disable MFA", class = "btn btn-outline-danger")
            )
          )
        )
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Devices & sessions"),
      bslib::card_body(
        DT::DTOutput(ns("sessions_table")),
        shiny::div(class = "d-flex gap-2 mt-2",
          shiny::actionButton(ns("revoke_device"), "Sign out selected device", class = "btn btn-outline-warning"),
          shiny::actionButton(ns("revoke_other_devices"), "Sign out all other devices", class = "btn btn-outline-danger")
        )
      )
    )
  )
}

enterprise_versions_ui <- function(ns) {
  shiny::tagList(
    shiny::selectInput(ns("version_project"), "Dataset-version project", choices = character()),
    DT::DTOutput(ns("dataset_versions")),
    shiny::hr(),
    shiny::selectInput(ns("manage_project"), "Project to organize", choices = character()),
    shiny::div(
      class = "row g-3",
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Folders & tags"),
          bslib::card_body(
            shiny::textInput(ns("folder_name"), "New folder"),
            shiny::actionButton(ns("create_folder"), "Create folder", class = "btn btn-outline-primary"),
            DT::DTOutput(ns("folders_table")),
            shiny::actionButton(ns("move_to_folder"), "Move project to selected folder", class = "btn btn-outline-secondary mt-2"),
            shiny::textInput(ns("project_tag"), "Tag"),
            shiny::actionButton(ns("add_project_tag"), "Add tag", class = "btn btn-outline-secondary")
          )
        )
      ),
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Recycle bin"),
          bslib::card_body(
            shiny::tags$p(class = "small text-muted", "Project Manager moves deleted connected projects here. Restore them or permanently purge them."),
            DT::DTOutput(ns("recycle_table")),
            shiny::div(
              class = "d-flex gap-2 mt-2",
              shiny::actionButton(ns("restore_recycled"), "Restore", class = "btn btn-outline-success"),
              shiny::actionButton(ns("purge_recycled"), "Permanently delete", class = "btn btn-outline-danger")
            )
          )
        )
      )
    )
  )
}

enterprise_api_ui <- function(ns) {
  shiny::tagList(
    shiny::div(
      class = "row g-3",
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("Personal API tokens"),
          bslib::card_body(
            shiny::textInput(ns("api_token_name"), "Token name", placeholder = "Reporting integration"),
            shiny::checkboxGroupInput(ns("api_scopes"), "Scopes", c("Read" = "read", "Write" = "write", "Export" = "export"), selected = "read"),
            shiny::numericInput(ns("api_expiry_days"), "Expires after days", 90, min = 1, max = 3650),
            shiny::actionButton(ns("create_api_token"), "Create token", class = "btn btn-primary"),
            shiny::uiOutput(ns("new_api_token")),
            DT::DTOutput(ns("api_tokens")),
            shiny::actionButton(ns("revoke_api_token"), "Revoke selected", class = "btn btn-outline-danger mt-2")
          )
        )
      ),
      shiny::div(
        class = "col-lg-6",
        bslib::card(
          bslib::card_header("SSO provider registry — administrators"),
          bslib::card_body(
            shiny::selectInput(ns("sso_type"), "Provider", c("OpenID Connect" = "oidc", "Microsoft Entra ID" = "azure_ad", "Google" = "google")),
            shiny::textInput(ns("sso_name"), "Display name"),
            shiny::numericInput(ns("sso_org_id"), "Organization ID (optional)", NA, min = 1),
            shiny::textInput(ns("sso_issuer"), "Issuer / discovery URL"),
            shiny::textInput(ns("sso_client_id"), "Client ID"),
            shiny::textInput(ns("sso_secret_ref"), "Client secret environment/reference key"),
            shiny::textInput(ns("sso_scopes"), "Scopes", "openid email profile"),
            shiny::actionButton(ns("register_sso"), "Register provider", class = "btn btn-outline-primary"),
            shiny::tags$p(class = "small text-muted mt-2", "Secrets are referenced, not stored in this screen. External provider callback/redirect configuration is deployment-specific."),
            DT::DTOutput(ns("sso_table"))
          )
        )
      )
    )
  )
}

enterprise_operations_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("health_snapshot")),
    shiny::div(
      class = "row g-3 mt-1",
      shiny::div(
        class = "col-lg-7",
        bslib::card(
          bslib::card_header("Unified activity / audit"),
          bslib::card_body(
            shiny::selectInput(ns("ops_project"), "Scope", choices = c("My activity" = "")),
            DT::DTOutput(ns("audit_table"))
          )
        )
      ),
      shiny::div(
        class = "col-lg-5",
        bslib::card(
          bslib::card_header("Backup history"),
          bslib::card_body(
            shiny::tags$p(class = "small text-muted", "Automated PostgreSQL backups are run by scripts/backup_database.R or your deployment scheduler; every run is recorded here."),
            DT::DTOutput(ns("backup_table"))
          )
        )
      )
    )
  )
}

message("[SAP] Enterprise Center v5.0 loaded")
