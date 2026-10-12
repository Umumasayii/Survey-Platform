# =============================================================================
# MODULE 14: PERSISTENT ACCOUNT — LOGIN, SIGN-UP, PROFILE, LOGOUT
# =============================================================================
# Deployment-aware authentication for Survey Analytics Platform.
#
# User journey:
#   * Connected mode offers persistent sign-in or an explicit session-only Guest workspace.
#   * Returning users sign in and the most recent saved workspace/project is restored.
#   * Explicit Demo Mode is Guest-only and does not create temporary user accounts.
#   * Backend failure never silently creates a disposable identity.
# =============================================================================

.auth_trim <- function(x) trimws(as.character(x %||% ""))

# ---- In-tool account dialog -------------------------------------------------
auth_dialog_ui <- function(id, allow_signup = TRUE, frontend_only = TRUE,
                           auth_required = FALSE, guest_available = FALSE,
                           selected_tab = "signin", show_choice_back = FALSE) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "sap-auth-dialog",
    shiny::tags$style(shiny::HTML("
      .sap-auth-dialog .sap-auth-intro {
        display: flex; gap: 14px; align-items: center;
        padding: 16px 18px; margin-bottom: 16px;
        border-radius: 16px;
        color: #fff;
        background: linear-gradient(135deg, #0b5ed7 0%, #0d6efd 58%, #4f46e5 100%);
      }
      .sap-auth-dialog .sap-auth-intro-icon {
        width: 46px; height: 46px; flex: 0 0 46px;
        display: grid; place-items: center; border-radius: 14px;
        background: rgba(255,255,255,.16); font-size: 21px;
      }
      .sap-auth-dialog .sap-auth-intro h3 { margin: 0 0 3px; font-size: 1.15rem; }
      .sap-auth-dialog .sap-auth-intro p { margin: 0; color: rgba(255,255,255,.88); font-size: .9rem; }
      .sap-auth-dialog .nav-tabs { margin-bottom: 18px; }
      .sap-auth-dialog .form-label { font-weight: 600; }
      .sap-auth-dialog .form-control { min-height: 44px; border-radius: 10px; }
      .sap-auth-dialog .btn-primary { min-height: 44px; border-radius: 10px; font-weight: 700; }
      .sap-auth-note { font-size: .83rem; color: #687386; line-height: 1.45; }
      .sap-auth-status:empty { display: none; }
      .sap-auth-status { margin-top: 12px; }
      .sap-auth-backend-warning { margin-bottom: 16px; }
    ")),
    shiny::div(
      class = "sap-auth-intro",
      shiny::div(class = "sap-auth-intro-icon", shiny::icon("user-shield")),
      shiny::div(
        shiny::tags$h3(if (isTRUE(guest_available)) "Choose how to continue" else if (isTRUE(auth_required)) "Sign in to your workspace" else "Account"),
        shiny::tags$p(if (isTRUE(guest_available))
          "Sign in for saved projects, or continue in a private session-only Guest workspace."
        else if (isTRUE(auth_required))
          "Sign in to access your protected analysis workspace."
        else "Sign in to access your personal analysis workspace.")
      )
    ),
    shiny::uiOutput(ns("backend_status")),
    if (isTRUE(show_choice_back)) {
      shiny::actionButton(
        ns("back_to_entry_choice"), "Back to access choices",
        icon = shiny::icon("arrow-left"), class = "btn btn-sm btn-outline-secondary mb-3"
      )
    },
    bslib::navset_card_tab(
      id = ns("auth_tab"),
      selected = selected_tab,
      bslib::nav_panel(
        title = "Sign in", value = "signin",
        shiny::tags$h4(class = "mb-1", "Welcome back"),
        shiny::tags$p(class = "text-muted mb-3", "Sign in to restore your saved project and continue your work."),
        shiny::textInput(ns("login_id"), "Username or email", placeholder = "name@example.org"),
        shiny::passwordInput(ns("login_pw"), "Password", placeholder = "Enter your passphrase"),
        shiny::actionButton(ns("do_login"), "Sign in", icon = shiny::icon("right-to-bracket"), class = "btn btn-primary w-100"),
        shiny::actionButton(ns("forgot_password"), "Forgot password?", icon = shiny::icon("key"), class = "btn btn-link w-100 mt-1"),
        shiny::uiOutput(ns("login_msg"), class = "sap-auth-status")
      ),
      if (isTRUE(allow_signup)) {
        bslib::nav_panel(
          title = "Create account", value = "signup",
          shiny::tags$h4(class = "mb-1", "Create your account"),
          shiny::tags$p(
            class = "text-muted mb-3",
            if (exists("SAP_POSTGRES_ENABLED", inherits = TRUE) && isTRUE(SAP_POSTGRES_ENABLED)) {
              "Your account and owner-scoped projects are stored in the configured PostgreSQL database."
            } else {
              "Your account is protected with secure password hashing in this application instance. Save or export important project files."
            }
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("reg_full"), "Full name *")),
            shiny::column(6, shiny::textInput(ns("reg_user"), "Username *", placeholder = "researcher.name"))
          ),
          shiny::textInput(ns("reg_email"), "Email *", placeholder = "name@example.org"),
          shiny::fluidRow(
            shiny::column(6, shiny::passwordInput(ns("reg_pw"), "Passphrase *")),
            shiny::column(6, shiny::passwordInput(ns("reg_pw2"), "Confirm passphrase *"))
          ),
          shiny::tags$p(
            class = "sap-auth-note",
            "Use a passphrase containing at least 15 characters."
          ),
          shiny::checkboxInput(
            ns("reg_ack"),
            "I understand the account and project-storage options for this application deployment.",
            value = FALSE
          ),
          shiny::actionButton(ns("do_register"), "Create account", icon = shiny::icon("user-plus"), class = "btn btn-primary w-100"),
          shiny::uiOutput(ns("reg_msg"), class = "sap-auth-status")
        )
      } else {
        bslib::nav_panel(
          title = "Create account", value = "signup",
          shiny::div(class = "alert alert-secondary mb-0", shiny::icon("lock"),
                     " New account creation is disabled for this deployment.")
        )
      }
    ),
    shiny::tags$p(
      class = "sap-auth-note text-center mt-3 mb-0",
      shiny::icon("person-walking-arrow-right"),
      if (isTRUE(guest_available))
        " Guest work lasts for this browser session. Signing in switches to your saved account workspace."
      else if (isTRUE(auth_required)) " Sign in to access your protected analysis workspace."
      else " You may close this dialog and continue without an account."
    )
  )
}

# ---- Mandatory connected-entry choice ----------------------------------------
auth_entry_choice_ui <- function(id, allow_signup = TRUE) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "sap-auth-entry-choice",
    shiny::tags$style(shiny::HTML("
      .sap-auth-entry-choice {
        --sap-auth-primary: #2563eb;
        --sap-auth-primary-dark: #1d4ed8;
        --sap-auth-ink: #172033;
        --sap-auth-muted: #687386;
        --sap-auth-border: #dbe4f0;
        --sap-auth-soft: #f6f9fd;
      }
      .sap-auth-entry-choice .sap-entry-hero {
        position: relative;
        overflow: hidden;
        padding: 24px 26px;
        margin-bottom: 18px;
        border: 1px solid #dbe7fb;
        border-radius: 20px;
        background:
          radial-gradient(circle at 92% 10%, rgba(37,99,235,.16), transparent 34%),
          linear-gradient(135deg, #f8fbff 0%, #eef5ff 100%);
      }
      .sap-auth-entry-choice .sap-entry-eyebrow {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        margin-bottom: 8px;
        color: var(--sap-auth-primary-dark);
        font-size: .76rem;
        font-weight: 800;
        letter-spacing: .08em;
        text-transform: uppercase;
      }
      .sap-auth-entry-choice .sap-entry-hero h3 {
        margin: 0 0 7px;
        color: var(--sap-auth-ink);
        font-size: clamp(1.35rem, 2.3vw, 1.85rem);
        font-weight: 800;
        letter-spacing: -.025em;
      }
      .sap-auth-entry-choice .sap-entry-hero p {
        max-width: 760px;
        margin: 0;
        color: var(--sap-auth-muted);
        font-size: .95rem;
        line-height: 1.55;
      }
      .sap-auth-entry-choice .sap-entry-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 16px;
        align-items: stretch;
      }
      .sap-auth-entry-choice .sap-entry-option {
        position: relative;
        min-width: 0;
        min-height: 250px;
        display: flex;
        flex-direction: column;
        padding: 20px;
        border: 1px solid var(--sap-auth-border);
        border-radius: 18px;
        background: #fff;
        box-shadow: 0 8px 24px rgba(31, 45, 61, .055);
        transition: transform .16s ease, box-shadow .16s ease, border-color .16s ease;
      }
      .sap-auth-entry-choice .sap-entry-option.sap-entry-primary {
        border-color: #b9cef8;
        box-shadow: 0 10px 28px rgba(37, 99, 235, .09);
      }
      .sap-auth-entry-choice .sap-entry-option.sap-entry-guest {
        background: linear-gradient(180deg, #fff 0%, #fbfcfe 100%);
      }
      @media (hover:hover) and (pointer:fine) {
        .sap-auth-entry-choice .sap-entry-option:hover {
          transform: translateY(-2px);
          border-color: #b8c8dc;
          box-shadow: 0 13px 30px rgba(31, 45, 61, .09);
        }
      }
      .sap-auth-entry-choice .sap-entry-card-top {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 10px;
        margin-bottom: 14px;
      }
      .sap-auth-entry-choice .sap-entry-icon {
        width: 50px;
        height: 50px;
        flex: 0 0 50px;
        display: grid;
        place-items: center;
        border-radius: 14px;
        color: var(--sap-auth-primary-dark);
        background: #edf4ff;
        font-size: 21px;
      }
      .sap-auth-entry-choice .sap-entry-option:nth-child(2) .sap-entry-icon {
        color: #6d28d9;
        background: #f3efff;
      }
      .sap-auth-entry-choice .sap-entry-guest .sap-entry-icon {
        color: #475569;
        background: #f1f5f9;
      }
      .sap-auth-entry-choice .sap-entry-badge {
        display: inline-flex;
        align-items: center;
        min-height: 26px;
        padding: 4px 9px;
        border: 1px solid #e3eaf3;
        border-radius: 999px;
        color: #58677c;
        background: #f8fafc;
        font-size: .7rem;
        font-weight: 800;
        white-space: nowrap;
      }
      .sap-auth-entry-choice .sap-entry-option h4 {
        margin: 0 0 7px;
        color: var(--sap-auth-ink);
        font-size: 1.06rem;
        font-weight: 800;
      }
      .sap-auth-entry-choice .sap-entry-option p {
        margin: 0 0 15px;
        color: var(--sap-auth-muted);
        font-size: .88rem;
        line-height: 1.5;
        flex: 1 1 auto;
      }
      .sap-auth-entry-choice .sap-entry-detail {
        display: flex;
        align-items: center;
        gap: 7px;
        margin: 0 0 14px;
        color: #526174;
        font-size: .78rem;
        font-weight: 650;
      }
      .sap-auth-entry-choice .sap-entry-detail i { color: #16a34a; }
      .sap-auth-entry-choice .btn {
        min-height: 46px;
        border-radius: 11px;
        font-weight: 800;
      }
      .sap-auth-entry-choice .sap-entry-primary .btn-primary {
        background: var(--sap-auth-primary);
        border-color: var(--sap-auth-primary);
      }
      .sap-auth-entry-choice .sap-entry-security {
        display: flex;
        align-items: flex-start;
        justify-content: center;
        gap: 8px;
        margin-top: 17px;
        padding: 11px 14px;
        border-radius: 12px;
        color: #5f6c7f;
        background: var(--sap-auth-soft);
        font-size: .79rem;
        line-height: 1.45;
        text-align: center;
      }
      .sap-auth-entry-choice .sap-entry-security i {
        margin-top: 2px;
        color: var(--sap-auth-primary-dark);
      }
      @media (max-width: 820px) {
        .sap-auth-entry-choice .sap-entry-grid { grid-template-columns: 1fr; }
        .sap-auth-entry-choice .sap-entry-option { min-height: 0; }
      }
      @media (max-width: 480px) {
        .sap-auth-entry-choice .sap-entry-hero { padding: 20px; border-radius: 16px; }
        .sap-auth-entry-choice .sap-entry-option { padding: 17px; border-radius: 15px; }
        .sap-auth-entry-choice .sap-entry-badge { display: none; }
      }
    ")),
    shiny::div(
      class = "sap-entry-hero",
      shiny::div(
        class = "sap-entry-eyebrow",
        shiny::icon("shield-halved"),
        "Workspace access"
      ),
      shiny::tags$h3("How would you like to continue?"),
      shiny::tags$p(
        "Choose the access option that fits your work. Sign in to return to an existing workspace, create an account for persistent PostgreSQL-backed projects, or continue as a temporary Guest."
      )
    ),
    shiny::uiOutput(ns("backend_status")),
    shiny::div(
      class = "sap-entry-grid",

      # 1. Sign in
      shiny::div(
        class = "sap-entry-option sap-entry-primary",
        shiny::div(
          class = "sap-entry-card-top",
          shiny::div(class = "sap-entry-icon", shiny::icon("right-to-bracket")),
          shiny::span(class = "sap-entry-badge", "Returning user")
        ),
        shiny::tags$h4("Sign in"),
        shiny::tags$p(
          "Access your existing account, saved projects, dashboards, permissions, profile, and previously stored work."
        ),
        shiny::div(
          class = "sap-entry-detail",
          shiny::icon("circle-check"),
          "Persistent account workspace"
        ),
        shiny::actionButton(
          ns("choose_signin"), "Sign in",
          icon = shiny::icon("right-to-bracket"),
          class = "btn btn-primary w-100"
        )
      ),

      # 2. Create account
      shiny::div(
        class = "sap-entry-option",
        shiny::div(
          class = "sap-entry-card-top",
          shiny::div(class = "sap-entry-icon", shiny::icon("user-plus")),
          shiny::span(class = "sap-entry-badge", "New user")
        ),
        shiny::tags$h4("Create an account"),
        shiny::tags$p(
          if (isTRUE(allow_signup))
            "Create a secure account for persistent projects, saved analyses, dashboards, and controlled collaboration."
          else
            "New account creation is currently disabled by the administrator for this deployment."
        ),
        shiny::div(
          class = "sap-entry-detail",
          shiny::icon(if (isTRUE(allow_signup)) "circle-check" else "circle-xmark"),
          if (isTRUE(allow_signup)) sap_account_storage_label() else "Registration unavailable"
        ),
        shiny::actionButton(
          ns("choose_signup"), "Create an account",
          icon = shiny::icon("user-plus"),
          class = "btn btn-outline-primary w-100",
          disabled = if (!isTRUE(allow_signup)) "disabled" else NULL
        )
      ),

      # 3. Guest
      shiny::div(
        class = "sap-entry-option sap-entry-guest",
        shiny::div(
          class = "sap-entry-card-top",
          shiny::div(class = "sap-entry-icon", shiny::icon("person-walking-arrow-right")),
          shiny::span(class = "sap-entry-badge", "No account needed")
        ),
        shiny::tags$h4("Continue as Guest"),
        shiny::tags$p(
          "Open the workspace immediately without registering. Guest work is temporary and is not restored after the browser session ends."
        ),
        shiny::div(
          class = "sap-entry-detail",
          shiny::icon("clock"),
          "Session-only workspace"
        ),
        shiny::actionButton(
          ns("continue_guest"), "Continue as Guest",
          icon = shiny::icon("arrow-right"),
          class = "btn btn-outline-secondary w-100"
        )
      )
    ),
    shiny::div(
      class = "sap-entry-security",
      shiny::icon("lock"),
      shiny::span(
        if (exists("SAP_POSTGRES_ENABLED", inherits = TRUE) && isTRUE(SAP_POSTGRES_ENABLED)) {
          "Registered accounts use the PostgreSQL-backed authentication system. Guest access remains isolated to the current session."
        } else {
          "Registered accounts use secure application-instance storage. Configure PostgreSQL when accounts must survive redeployment or be shared across multiple instances."
        }
      )
    )
  )
}

# Retained for compatibility with older app.R versions. The current connected
# deployment uses the modal as the required-authentication gate.
auth_gate_ui <- function(id, allow_signup = TRUE) {
  auth_dialog_ui(id, allow_signup = allow_signup, frontend_only = TRUE, auth_required = TRUE)
}

# ---- Compact account controls for the command bar --------------------------
auth_account_menu_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("account_menu"), inline = TRUE)
}

# ---- Legacy temporary-account compatibility guard ---------------------------
# Survey Analytics Platform deliberately disables session-only account creation.
create_frontend_auth_service <- function(app_state) {
  stop(
    "Temporary session accounts are disabled. Use connected PostgreSQL authentication, or explicit Guest-only Demo Mode.",
    call. = FALSE
  )
}

mod_auth_server <- function(id, app_state, services = NULL,
                            allow_signup = TRUE, auth_required = TRUE,
                            frontend_only = FALSE,
                            allow_guest = !tolower(Sys.getenv("SAP_ALLOW_GUEST", "true")) %in% c("false", "0", "no")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    gate_visible <- shiny::reactiveVal(FALSE)
    auth_view <- shiny::reactiveVal("choice")
    pending_mfa_user <- shiny::reactiveVal(NULL)

    connected_auth <- if (!is.null(services)) services$auth else NULL
    # Temporary/session accounts are not offered. Explicit Demo
    # Mode is Guest-only; connected mode always uses PostgreSQL-backed AuthService.
    use_frontend_auth <- FALSE
    demo_workspace <- isTRUE(frontend_only)
    auth <- connected_auth
    if (!is.null(auth) && !isTRUE(demo_workspace)) {
      req_obj <- session$request
      user_agent <- tryCatch(req_obj$HTTP_USER_AGENT, error = function(e) "")
      raw_ip <- tryCatch(req_obj$HTTP_X_FORWARDED_FOR %||% req_obj$REMOTE_ADDR %||% "", error = function(e) "")
      pseudonym_key <- Sys.getenv("SAP_PSEUDONYM_KEY", "")
      ip_hash <- if (nzchar(raw_ip) && nzchar(pseudonym_key)) digest::digest(paste0(pseudonym_key, "|", raw_ip), algo = "sha256", serialize = FALSE) else ""
      try(auth$set_request_context(user_agent, ip_hash), silent = TRUE)
    }
    guest_mode <- shiny::reactiveVal(isTRUE(demo_workspace) || !isTRUE(auth_required))

    is_authenticated <- function() {
      !is.null(auth) && isTRUE(tryCatch(auth$is_authenticated(), error = function(e) FALSE))
    }

    activate_guest <- function(message = "Continuing as Guest") {
      guest_mode(TRUE)
      gate_visible(FALSE)
      app_state$current_user <- NULL
      app_state$current_user_profile <- list(username = "guest", role = "owner")
      app_state$current_project <- NULL
      app_state$current_project_name <- "Guest Workspace"
      app_state$project_saved <- FALSE
      app_state$workspace_restored <- FALSE
      app_state$last_action <- message
      invisible(TRUE)
    }

    set_message <- function(output_id, text, ok = FALSE) {
      output[[output_id]] <- shiny::renderUI(
        shiny::div(
          class = paste("alert py-2 px-3 mb-0", if (isTRUE(ok)) "alert-success" else "alert-danger"),
          shiny::icon(if (isTRUE(ok)) "circle-check" else "triangle-exclamation"),
          " ", text
        )
      )
    }

    clear_workspace <- function() {
      app_state$raw_data <- NULL
      app_state$processed_data <- NULL
      app_state$me_indicators <- sap_me_empty_indicators()
      app_state$me_results <- sap_me_empty_results()
      app_state$me_activities <- sap_me_register_empty("activities")
      app_state$me_risks <- sap_me_register_empty("risks")
      app_state$me_grievances <- sap_me_register_empty("grievances")
      app_state$me_actions <- sap_me_register_empty("actions")
      app_state$field_visits <- sap_field_visits_empty()
      app_state$governance_export_approved <- FALSE
      app_state$workbook_sheets <- list()
      app_state$data_imported <- FALSE
      app_state$variable_types <- NULL
      app_state$variable_dictionary <- data.frame()
      app_state$current_project <- NULL
      app_state$current_project_name <- "Presentation Workspace"
      app_state$project_path <- NULL
      app_state$project_saved <- FALSE
      app_state$dataset_name <- NULL
      app_state$transformation_count <- 0L
      app_state$last_action <- "Signed out"
      app_state$analyses <- list()
      app_state$last_analysis <- NULL
      app_state$trained_models <- list()
      app_state$last_model <- NULL
      app_state$model_predictions <- NULL
      app_state$resume_route <- "home"
      app_state$resume_last_analysis <- NULL
      app_state$workspace_restored <- FALSE
      if (!is.null(services) && !is.null(services$report)) services$report$clear()
      invisible(TRUE)
    }

    open_recent_project <- function() {
      if (demo_workspace || is.null(services) || is.null(services$project)) return(invisible(FALSE))

      checkpoint <- if (!is.null(services$workspace)) {
        tryCatch(services$workspace$load(), error = function(e) NULL)
      } else NULL
      opened <- FALSE
      if (is.data.frame(checkpoint) && nrow(checkpoint) > 0L &&
          !is.null(checkpoint$project_id) && !is.na(checkpoint$project_id[[1]])) {
        opened <- isTRUE(tryCatch(services$project$open(checkpoint$project_id[[1]]), error = function(e) FALSE))
      }
      if (!opened) {
        recent <- tryCatch(services$project$list(), error = function(e) NULL)
        if (is.data.frame(recent) && nrow(recent) > 0L) {
          opened <- isTRUE(tryCatch(services$project$open(recent$id[[1]]), error = function(e) FALSE))
        }
      }
      app_state$workspace_restored <- TRUE
      invisible(opened)
    }

    output$backend_status <- shiny::renderUI({
      if (demo_workspace) {
        NULL
      } else if (is.null(auth) || !isTRUE(auth$is_available())) {
        shiny::div(
          class = paste("alert sap-auth-backend-warning", if (isTRUE(allow_guest)) "alert-warning" else "alert-danger"),
          shiny::tags$strong("Authentication backend unavailable."),
          shiny::tags$br(),
          if (isTRUE(allow_guest)) "Saved accounts are temporarily unavailable. You may continue in a session-only Guest workspace."
          else if (isTRUE(auth_required)) "This deployment requires authentication; the workspace remains locked until its secure account backend is available."
          else "Account registration and sign-in are unavailable until secure account storage is restored."
        )
      } else NULL
    })

    output$account_menu <- shiny::renderUI({
      auth$version()
      guest_active <- isTRUE(guest_mode())
      u <- auth$current_user()

      if (is.null(u)) {
        if (demo_workspace) {
          return(shiny::span(class = "small text-secondary", shiny::icon("flask"), " Demo workspace"))
        }
        return(shiny::div(
          class = "d-flex align-items-center gap-2",
          if (guest_active) shiny::span(class = "small fw-semibold", shiny::icon("user"), " Guest session"),
          shiny::actionButton(
            ns("open_auth"), "Account access",
            icon = shiny::icon("circle-user"),
            class = "btn btn-sm btn-outline-light",
            title = "Choose Sign in, Create an account, or Continue as Guest"
          )
        ))
      }

      display_name <- .auth_trim(u$full_name)
      if (!nzchar(display_name)) display_name <- .auth_trim(u$username)

      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::span(class = "small fw-semibold", shiny::icon("circle-user"), " ", display_name),
        shiny::actionButton(ns("open_profile"), NULL, icon = shiny::icon("user-gear"),
                            class = "btn btn-sm btn-outline-light", title = "Profile"),
        shiny::actionButton(ns("do_logout"), NULL, icon = shiny::icon("right-from-bracket"),
                            class = "btn btn-sm btn-outline-warning", title = "Sign out")
      )
    })

    output$auth_modal_body <- shiny::renderUI({
      view <- auth_view()
      if (identical(view, "choice") && isTRUE(allow_guest)) {
        return(auth_entry_choice_ui(id, allow_signup = allow_signup))
      }
      selected <- if (identical(view, "signup") && isTRUE(allow_signup)) "signup" else "signin"
      auth_dialog_ui(
        id, allow_signup = allow_signup, frontend_only = use_frontend_auth,
        auth_required = isTRUE(auth_required) && !isTRUE(allow_guest),
        guest_available = isTRUE(allow_guest), selected_tab = selected,
        show_choice_back = isTRUE(allow_guest)
      )
    })

    show_auth_dialog <- function(required = isTRUE(auth_required), view = NULL) {
      guest_choice <- isTRUE(allow_guest)
      if (is.null(view)) {
        view <- if (guest_choice) "choice" else "signin"
      }
      auth_view(view)
      must_authenticate <- isTRUE(required) && !guest_choice
      gate_visible(isTRUE(required) && !isTRUE(guest_mode()))
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(
          shiny::icon("circle-user"),
          if (identical(view, "choice")) " Choose how to continue"
          else if (must_authenticate) " Sign in required"
          else if (identical(view, "signup")) " Create an account"
          else " Account access"
        ),
        shiny::uiOutput(ns("auth_modal_body")),
        easyClose = !isTRUE(required) || isTRUE(guest_mode()),
        size = "l",
        footer = if (!isTRUE(required) || isTRUE(guest_mode())) shiny::modalButton("Close") else NULL
      ))
    }

    shiny::observeEvent(input$open_auth, {
      show_auth_dialog(isTRUE(auth_required))
    }, ignoreInit = TRUE)

    # A connected session starts with an explicit account-or-Guest choice when
    # Guest access is enabled. SAP_ALLOW_GUEST=false restores the strict,
    # non-dismissible authentication gate for controlled deployments.
    shiny::observe({
      if (!isTRUE(auth_required)) return(invisible(NULL))
      if (isTRUE(guest_mode())) return(invisible(NULL))
      shiny::invalidateLater(60000, session)
      valid <- is_authenticated()
      if (!valid && !isTRUE(gate_visible())) {
        # Connected sessions may expire or be revoked while the browser remains open.
        # Clear owner-scoped state before returning to the required-auth gate.
        if (!use_frontend_auth) clear_workspace()
        show_auth_dialog(TRUE)
      }
    })

    # Keep Guest authorization session-local and independent from account roles.
    # The lower priority lets the application identity observer run first, then
    # applies the Guest owner role to this isolated, non-persistent workspace.
    shiny::observe({
      guest_mode()
      auth$version()
      if (isTRUE(guest_mode()) && !is_authenticated()) {
        app_state$current_user_profile <- list(username = "guest", role = "owner")
      }
    }, priority = -100)

    shiny::observeEvent(input$choose_signin, {
      auth_view("signin")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$choose_signup, {
      if (!isTRUE(allow_signup)) return(invisible(NULL))
      auth_view("signup")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$back_to_entry_choice, {
      if (!isTRUE(allow_guest)) return(invisible(NULL))
      auth_view("choice")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$continue_guest, {
      if (!isTRUE(allow_guest)) return(invisible(NULL))
      activate_guest("Guest session started")
      shiny::removeModal()
      if (!is.null(services) && !is.null(services$notify)) {
        services$notify$success("Guest workspace opened. Sign in later if you need saved projects and cross-session persistence.")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$do_login, {
      login_id <- .auth_trim(input$login_id)
      password <- as.character(input$login_pw %||% "")
      if (!nzchar(login_id) || !nzchar(password)) {
        set_message("login_msg", "Enter your username or email and password.")
        return(invisible(NULL))
      }

      res <- auth$login(login_id, password)
      if (isTRUE(res$mfa_required)) {
        pending_mfa_user(as.integer(res$user_id))
        shiny::showModal(shiny::modalDialog(
          title = shiny::tagList(shiny::icon("shield-halved"), " Two-factor authentication"),
          shiny::tags$p("Enter the 6-digit code from your authenticator app, or one unused recovery code."),
          shiny::textInput(ns("mfa_login_code"), "Authenticator or recovery code", placeholder = "123456"),
          shiny::uiOutput(ns("mfa_login_msg")),
          easyClose = FALSE,
          footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("complete_mfa_login"), "Verify and sign in", class = "btn btn-primary"))
        ))
        return(invisible(NULL))
      }
      if (isTRUE(res$ok)) {
        guest_mode(FALSE)
        if (!use_frontend_auth) clear_workspace()
        auth$restore_user(res$user_id)
        resumed <- open_recent_project()
        gate_visible(FALSE)
        shiny::removeModal()
        if (!is.null(services) && !is.null(services$notify)) services$notify$success(
          if (isTRUE(resumed)) "Signed in. Your saved workspace was restored." else "Signed in."
        )
      } else {
        set_message("login_msg", res$error %||% "Sign-in failed.")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$complete_mfa_login, {
      uid <- pending_mfa_user()
      shiny::req(!is.null(uid))
      res <- auth$complete_mfa(uid, input$mfa_login_code)
      if (!isTRUE(res$ok)) {
        output$mfa_login_msg <- shiny::renderUI(shiny::div(class = "alert alert-danger py-2", res$error %||% "Verification failed."))
        return(invisible(NULL))
      }
      pending_mfa_user(NULL)
      guest_mode(FALSE)
      if (!use_frontend_auth) clear_workspace()
      auth$restore_user(res$user_id)
      resumed <- open_recent_project()
      gate_visible(FALSE)
      shiny::removeModal()
      if (!is.null(services) && !is.null(services$notify)) services$notify$success(if (isTRUE(resumed)) "Signed in with MFA. Your saved workspace was restored." else "Signed in with MFA.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$forgot_password, {
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("key"), " Password recovery"),
        shiny::tags$p("Request a reset token, then paste the token from your email below."),
        shiny::textInput(ns("reset_login"), "Username or email"),
        shiny::actionButton(ns("request_reset"), "Request reset", class = "btn btn-outline-primary"),
        shiny::hr(),
        shiny::textInput(ns("reset_token"), "Reset token"),
        shiny::passwordInput(ns("reset_pw"), "New passphrase"),
        shiny::passwordInput(ns("reset_pw2"), "Confirm new passphrase"),
        shiny::uiOutput(ns("reset_msg")),
        easyClose = TRUE,
        footer = shiny::tagList(shiny::modalButton("Close"), shiny::actionButton(ns("apply_reset"), "Reset password", class = "btn btn-primary"))
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$request_reset, {
      res <- auth$request_password_reset(input$reset_login)
      output$reset_msg <- shiny::renderUI(shiny::div(class = "alert alert-info py-2 mt-2", res$message %||% "If that account exists, a reset message has been queued."))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$apply_reset, {
      if (!identical(as.character(input$reset_pw %||% ""), as.character(input$reset_pw2 %||% ""))) {
        output$reset_msg <- shiny::renderUI(shiny::div(class = "alert alert-danger py-2 mt-2", "The new passphrases do not match."))
        return(invisible(NULL))
      }
      res <- auth$reset_password_with_token(input$reset_token, input$reset_pw)
      output$reset_msg <- shiny::renderUI(shiny::div(class = paste("alert py-2 mt-2", if (isTRUE(res$ok)) "alert-success" else "alert-danger"), if (isTRUE(res$ok)) "Password reset. You can now sign in." else res$error))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$do_register, {
      if (!isTRUE(allow_signup)) {
        set_message("reg_msg", "New account creation is disabled.")
        return(invisible(NULL))
      }
      if (!isTRUE(input$reg_ack)) {
        set_message("reg_msg", "Confirm the data-use acknowledgement before creating an account.")
        return(invisible(NULL))
      }
      if (!identical(as.character(input$reg_pw %||% ""), as.character(input$reg_pw2 %||% ""))) {
        set_message("reg_msg", "The passwords do not match.")
        return(invisible(NULL))
      }

      res <- auth$register(
        username = input$reg_user,
        password = input$reg_pw,
        email = input$reg_email,
        full_name = input$reg_full
      )

      if (isTRUE(res$ok)) {
        if (isTRUE(res$pending_approval)) {
          set_message(
            "reg_msg",
            "Account created successfully. An administrator must approve/activate it before you can sign in.",
            ok = TRUE
          )
          return(invisible(NULL))
        }

        # Capture whether the person has already imported or generated data as a
        # Guest. New-account creation must attach that work, not discard it.
        guest_has_data <- isTRUE(app_state$data_imported) &&
          (is.data.frame(app_state$processed_data) || is.data.frame(app_state$raw_data))
        guest_dataset_name <- app_state$dataset_name %||% "Guest dataset"

        login_res <- auth$login(input$reg_user, input$reg_pw)
        if (isTRUE(login_res$ok)) {
          guest_mode(FALSE)
          auth$restore_user(login_res$user_id)

          attached <- FALSE
          if (!use_frontend_auth && guest_has_data && !is.null(services$project) && !is.null(services$pool)) {
            project_name <- paste0("My first project — ", format(Sys.Date(), "%Y-%m-%d"))
            pid <- tryCatch(
              services$project$create(
                project_name,
                description = paste("Created from pre-account workspace:", guest_dataset_name),
                persist_current = TRUE
              ),
              error = function(e) NULL
            )
            attached <- !is.null(pid)
          }

          gate_visible(FALSE)
          shiny::removeModal()
          if (!is.null(services) && !is.null(services$notify)) {
            services$notify$success(
              if (isTRUE(res$bootstrap_admin) && !is.null(services$pool))
                "First account created as Administrator. Open Administration → Control Center to manage users."
              else if (isTRUE(res$bootstrap_admin))
                "First secure administrator account created. Your analysis workspace is ready."
              else if (attached) "Account created. Your current data was saved in a new PostgreSQL project."
              else "Account created. Your current workspace has been preserved."
            )
          }
        } else {
          set_message("reg_msg", "The account was created, but automatic sign-in failed. Use the Sign in tab.")
        }
      } else {
        set_message("reg_msg", res$error %||% "The account could not be created.")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$open_profile, {
      u <- auth$current_user()
      if (is.null(u)) return(invisible(NULL))
      val <- function(x) {
        v <- u[[x]]
        if (is.null(v) || length(v) == 0L || is.na(v[[1]])) "" else as.character(v[[1]])
      }

      shiny::showModal(shiny::modalDialog(
        title = "Your profile", easyClose = TRUE, size = "m",
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("pf_full"), "Full name", val("full_name"))),
          shiny::column(6, shiny::textInput(ns("pf_email"), "Email", val("email")))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("pf_country"), "Country", val("country"))),
          shiny::column(6, shiny::textInput(ns("pf_institution"), "Institution", val("institution")))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("pf_organization"), "Organization", val("organization"))),
          shiny::column(6, shiny::textInput(ns("pf_department"), "Department", val("department")))
        ),
        shiny::textInput(ns("pf_position"), "Position", val("position")),
        shiny::hr(),
        shiny::tags$h6("Change password"),
        shiny::passwordInput(ns("pf_oldpw"), "Current password"),
        shiny::fluidRow(
          shiny::column(6, shiny::passwordInput(ns("pf_newpw"), "New password")),
          shiny::column(6, shiny::passwordInput(ns("pf_newpw2"), "Confirm new password"))
        ),
        shiny::tags$p(
          class = "sap-auth-note",
          "Passphrases must contain at least 15 characters."
        ),
        shiny::uiOutput(ns("pf_msg")),
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          shiny::actionButton(ns("do_change_pw"), "Update password", class = "btn btn-outline-secondary"),
          shiny::actionButton(ns("do_save_profile"), "Save profile", class = "btn btn-primary")
        )
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$do_save_profile, {
      res <- auth$update_profile(list(
        full_name = input$pf_full,
        email = input$pf_email,
        country = input$pf_country,
        institution = input$pf_institution,
        organization = input$pf_organization,
        department = input$pf_department,
        position = input$pf_position
      ))
      output$pf_msg <- shiny::renderUI(shiny::div(
        class = paste("small mt-2", if (isTRUE(res$ok)) "text-success" else "text-danger"),
        if (isTRUE(res$ok)) "Profile saved." else res$error
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$do_change_pw, {
      if (!identical(as.character(input$pf_newpw %||% ""), as.character(input$pf_newpw2 %||% ""))) {
        output$pf_msg <- shiny::renderUI(shiny::div(class = "small mt-2 text-danger", "The new passwords do not match."))
        return(invisible(NULL))
      }
      res <- auth$change_password(input$pf_oldpw, input$pf_newpw)
      output$pf_msg <- shiny::renderUI(shiny::div(
        class = paste("small mt-2", if (isTRUE(res$ok)) "text-success" else "text-danger"),
        if (isTRUE(res$ok)) "Password updated." else res$error
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$do_logout, {
      # Save a lightweight resume checkpoint before removing owner-scoped state.
      if (!demo_workspace && !is.null(services) && !is.null(services$workspace)) {
        try(services$workspace$save(
          last_route = app_state$current_route %||% "home",
          last_module = app_state$current_route %||% "home",
          last_analysis = app_state$last_action,
          state = list(
            dataset_name = app_state$dataset_name,
            survey_design = app_state$survey_design,
            table_builder_spec = app_state$table_builder_spec,
            nonresponse_plan = app_state$nonresponse_plan,
            active_filter_label = app_state$active_filter_label
          )
        ), silent = TRUE)
      }
      clear_workspace()
      auth$logout()
      if (isTRUE(auth_required)) {
        # Logout returns to the mandatory access-mode chooser. Never silently
        # convert an authenticated user into a Guest session.
        guest_mode(FALSE)
        gate_visible(FALSE)
        app_state$last_action <- "Signed out — choose Guest or account access"
        if (!is.null(services) && !is.null(services$notify)) {
          services$notify$success("Signed out. Choose Guest, Sign in, or Create an account to continue.")
        }
        show_auth_dialog(TRUE, view = if (isTRUE(allow_guest)) "choice" else "signin")
      } else if (isTRUE(allow_guest)) {
        activate_guest("Signed out — Guest session started")
      } else {
        app_state$last_action <- "Signed out"
      }
    }, ignoreInit = TRUE)

    list(
      authenticated = shiny::reactive({
        auth$version()
        auth$is_authenticated()
      }),
      mode = shiny::reactive(if (demo_workspace) "demo" else if (isTRUE(guest_mode())) "guest" else "connected"),
      guest = shiny::reactive({
        auth$version()
        isTRUE(guest_mode()) || !auth$is_authenticated()
      })
    )
  })
}

message("[SAP] Auth v2.0-three-way-access-20260822 loaded")  # SAP_MODULE_BANNER

# =============================================================================
# END OF MODULE 14
# =============================================================================
