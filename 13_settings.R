# =============================================================================
# MODULE 13: SETTINGS
# =============================================================================
# A configuration page wired to the per-user backend SettingsService
# (00_services.R). Every visible control is consumed by the running app; there
# are no decorative or "coming soon" toggles.
#
# Live application:
#   - Theme changes call session$setCurrentTheme() immediately.
#   - Every write goes through services$settings$set(), so it persists and any
#     module reading services$settings$get(...) sees it next render.
#
# Signature follows the project convention: (id, app_state, services = NULL).
# =============================================================================

# ------------------------------------------------------------------ helpers --
.settings_get <- function(services, key, fallback = NULL) {
  if (!is.null(services) && !is.null(services$settings)) {
    services$settings$get(key, fallback)
  } else {
    fallback
  }
}

# =============================================================================
# UI
# =============================================================================

mod_settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "mb-4",
      h2(icon("gear"), "Settings"),
      p("Configure the platform. Changes are saved automatically.",
        class = "text-muted")
    ),

    navset_pill_list(
      widths = c(3, 9),
      well = FALSE,

      # --- GENERAL --------------------------------------------------------
      nav_panel(
        title = tagList(icon("sliders"), " General"),
        div(class = "card", div(class = "card-body",
          h5("General"),
          p("Number formatting and guarded import limits.",
            class = "text-muted small"),
          hr(),
          sliderInput(ns("decimal_places"), "Decimal places",
                      min = 0, max = 8, value = 3, step = 1),
          numericInput(ns("max_file_size"), "Max import file size (MB)",
                       value = 100, min = 1, max = 2000, step = 10)
        ))
      ),

      # --- APPEARANCE -----------------------------------------------------
      nav_panel(
        title = tagList(icon("palette"), " Appearance"),
        div(class = "card", div(class = "card-body",
          h5("Appearance"),
          p("Theme, accent, density, and type scale apply immediately.",
            class = "text-muted small"),
          hr(),
          radioButtons(ns("theme_mode"), "Theme",
                       choices = c("Light" = "light", "Dark" = "dark"),
                       inline = TRUE),
          selectInput(ns("accent_color"), "Accent colour",
                      choices = c("Classic blue" = "blue",
                                  "Signal teal" = "teal",
                                  "Nexus violet" = "violet",
                                  "Coral pulse" = "coral")),
          radioButtons(ns("interface_density"), "Interface density",
                       choices = c("Compact" = "compact",
                                   "Comfortable" = "comfortable",
                                   "Spacious" = "spacious"), inline = TRUE),
          sliderInput(ns("font_scale"), "Type scale",
                      min = 0.85, max = 1.20, value = 1, step = 0.05)
        ))
      ),

      # --- DATA IMPORT ----------------------------------------------------
      nav_panel(
        title = tagList(icon("file-import"), " Data Import"),
        div(class = "card", div(class = "card-body",
          h5("Data Import Defaults"),
          p("Defaults applied when the Import Wizard first opens.",
            class = "text-muted small"),
          hr(),
          checkboxInput(ns("imp_clean_names"),
                        "Automatically clean variable names", TRUE),
          checkboxInput(ns("imp_detect_dates"),
                        "Automatically detect date-like values", TRUE),
          checkboxInput(ns("imp_trim"),
                        "Automatically trim whitespace", TRUE),
          textInput(ns("imp_missing"),
                    "Missing-value codes (comma-separated)",
                    value = "NA, N/A, NULL, 99, 999, -99"),
          selectInput(ns("imp_encoding"), "Default encoding",
                      choices = c("UTF-8", "latin1", "Windows-1252"))
        ))
      ),

      # --- STATISTICAL ----------------------------------------------------
      nav_panel(
        title = tagList(icon("square-root-variable"), " Statistical"),
        div(class = "card", div(class = "card-body",
          h5("Statistical Defaults"),
          p("Used as defaults by correlation tests, models, tables, and power planning.",
            class = "text-muted small"),
          hr(),
          sliderInput(ns("confidence_level"), "Confidence level",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          sliderInput(ns("significance_level"), "Significance level (alpha)",
                      min = 0.001, max = 0.10, value = 0.05, step = 0.001),
          selectInput(ns("correlation_method"), "Default correlation method",
                      choices = c("Automatic (normality-aware)" = "auto",
                                  "Pearson" = "pearson",
                                  "Spearman" = "spearman",
                                  "Kendall" = "kendall"))
        ))
      ),

      # --- MACHINE LEARNING ----------------------------------------------
      nav_panel(
        title = tagList(icon("robot"), " Machine Learning"),
        div(class = "card", div(class = "card-body",
          h5("Machine Learning Defaults"),
          p("Defaults for the Machine Learning centre.",
            class = "text-muted small"),
          hr(),
          sliderInput(ns("ml_split"), "Train / test split (train fraction)",
                      min = 0.5, max = 0.9, value = 0.8, step = 0.05),
          numericInput(ns("random_seed"), "Random seed",
                       value = 123, min = 1, max = 1e6, step = 1),
          checkboxInput(ns("ml_autosave"),
                        "Automatically save trained models to the project",
                        FALSE),
          div(class = "alert alert-warning border small mt-2",
              icon("triangle-exclamation"),
              " With small datasets (~100 rows) a single split yields noisy ",
              "metrics; consider cross-validation for stable estimates.")
        ))
      ),

      # --- AI ASSISTANT ---------------------------------------------------
      nav_panel(
        title = tagList(icon("wand-magic-sparkles"), " AI Assistant"),
        div(class = "card", div(class = "card-body",
          div(class = "d-flex justify-content-between align-items-start gap-3 flex-wrap",
            div(
              h5(class = "mb-1", "Real AI Interpretation Engine"),
              p(class = "text-muted small mb-0",
                "Use Ollama locally, through a protected remote endpoint, or through Ollama Cloud. Analytical modules send structured summaries and variable labels, not respondent-level rows.")
            ),
            span(class = "badge text-bg-primary", icon("network-wired"), " Deployment-aware")
          ),
          hr(),
          fluidRow(
            column(5, selectInput(ns("ai_provider"), "Provider",
              choices = c("Ollama (local / remote / cloud)" = "ollama",
                          "Anthropic Claude" = "anthropic",
                          "Mock (testing only)" = "mock"),
              selected = "ollama")),
            column(7, selectizeInput(ns("ai_model"), "Model",
              choices = character(0), selected = NULL,
              options = list(create = TRUE,
                             placeholder = "Refresh installed models or type a model tag")))
          ),
          fluidRow(
            column(8, textInput(ns("ai_base_url"), "Ollama endpoint (Ollama only)",
              value = Sys.getenv("OLLAMA_BASE_URL", if (exists("SAP_HOSTED_RUNTIME") && SAP_HOSTED_RUNTIME) "" else "http://localhost:11434"),
              placeholder = "Local: http://localhost:11434 · Published: reachable HTTPS endpoint")),
            column(4, div(style = "margin-top:32px;",
              actionButton(ns("ai_refresh_models"), "Refresh models",
                           class = "btn-outline-secondary btn-sm w-100",
                           icon = icon("arrows-rotate"))))
          ),
          uiOutput(ns("ai_server_status")),
          fluidRow(
            column(8, passwordInput(ns("ai_api_key"), "API key / bearer token",
              placeholder = "Session-only. Prefer OLLAMA_API_KEY or ANTHROPIC_API_KEY in the environment")),
            column(4, div(style = "margin-top:32px;",
              actionButton(ns("ai_test"), "Test connection",
                           class = "btn-outline-primary btn-sm w-100",
                           icon = icon("plug"))))
          ),
          uiOutput(ns("ai_deployment_guidance")),
          div(class = "alert alert-light border small py-2",
              icon("circle-info"), " ",
              "Model discovery uses /api/tags and generation uses /api/chat. API keys are never persisted; environment variables are recommended, and keys entered here live only for this R session."),
          tags$details(
            tags$summary(class = "fw-semibold mb-3", icon("sliders"), " Advanced generation settings"),
            fluidRow(
              column(6, sliderInput(ns("ai_temperature"), "Temperature",
                min = 0, max = 1, value = 0.2, step = 0.1)),
              column(6, numericInput(ns("ai_max_tokens"), "Maximum response tokens",
                value = 1200, min = 128, max = 8192, step = 128))
            ),
            fluidRow(
              column(4, numericInput(ns("ai_timeout"), "Request timeout (seconds)",
                value = 300, min = 30, max = 1800, step = 30)),
              column(4, numericInput(ns("ai_context_window"), "Context window",
                value = 4096, min = 1024, max = 131072, step = 1024)),
              column(4, textInput(ns("ai_keep_alive"), "Keep model loaded", value = "10m"))
            )
          ),
          uiOutput(ns("ai_test_result")),
          hr(),
          div(class = "d-flex gap-2 align-items-center flex-wrap",
            actionButton(ns("ai_save"), "Save AI settings",
                         class = "btn-primary btn-sm", icon = icon("floppy-disk")),
            span(class = "small text-muted",
                 "AI output is a draft and must be reviewed before report insertion.")
          )
        ))
      ),
      nav_panel(
        title = tagList(icon("circle-info"), " System"),
        div(class = "card", div(class = "card-body",
          h5("System"),
          hr(),
          uiOutput(ns("system_info")),
          hr(),
          h6("Reset"),
          p("Restore all settings to their defaults.",
            class = "text-muted small"),
          actionButton(ns("reset_all"), "Reset all settings",
                       class = "btn-outline-danger btn-sm",
                       icon = icon("rotate-left"))
        ))
      )
    ),

    # tiny persistent "saved" indicator
    div(class = "text-muted small mt-3",
        icon("floppy-disk"), " ",
        textOutput(ns("save_status"), inline = TRUE))
  )
}

# =============================================================================
# SERVER
# =============================================================================

mod_settings_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    last_saved <- reactiveVal("Loaded.")

    # -- Initialise controls from the SettingsService (once) ----------------
    # updateXxx avoids re-rendering the UI and prevents a feedback loop with
    # the observers below (which is why we guard writes with a 'ready' flag).
    ready <- reactiveVal(FALSE)

    accent_map <- list(
      violet = c(primary = "#5b5bd6", dark = "#4444b8", rgb = "91,91,214"),
      teal   = c(primary = "#0f9f92", dark = "#0b746b", rgb = "15,159,146"),
      blue   = c(primary = "#2563eb", dark = "#1d4ed8", rgb = "37,99,235"),
      coral  = c(primary = "#e85d75", dark = "#c43f5a", rgb = "232,93,117")
    )
    apply_appearance <- function(accent, density, scale) {
      accent <- as.character(accent %||% "blue")[[1]]
      density <- as.character(density %||% "comfortable")[[1]]
      accent <- if (accent %in% names(accent_map)) accent else "blue"
      density <- if (density %in% c("compact", "comfortable", "spacious")) density else "comfortable"
      scale <- suppressWarnings(as.numeric(scale))[[1]]
      if (!is.finite(scale)) scale <- 1
      scale <- min(max(scale, .85), 1.2)
      palette <- accent_map[[accent]]
      shinyjs::runjs(sprintf(
        paste0(
          "document.documentElement.style.setProperty('--nexus-primary','%s');",
          "document.documentElement.style.setProperty('--nexus-primary-dark','%s');",
          "document.documentElement.style.setProperty('--nexus-primary-rgb','%s');",
          "document.documentElement.style.setProperty('--nexus-font-scale','%.2f');",
          "document.body.classList.remove('nexus-density-compact','nexus-density-comfortable','nexus-density-spacious');",
          "document.body.classList.add('nexus-density-%s');"
        ), palette[["primary"]], palette[["dark"]], palette[["rgb"]], scale, density
      ))
      invisible(TRUE)
    }

    settings_identity <- reactive({
      if (!is.null(services) && !is.null(services$auth)) services$auth$version()
      app_state$current_user
    })

    observeEvent(settings_identity(), {
      ready(FALSE)
      g <- function(k, d) .settings_get(services, k, d)

      updateSliderInput(session, "decimal_places",
                        value = g("decimal_places", 3))
      updateNumericInput(session, "max_file_size",
                         value = g("max_file_size", 100))
      updateRadioButtons(session, "theme_mode",
                         selected = g("theme_mode", "light"))
      updateSelectInput(session, "accent_color",
                        selected = g("accent_color", "blue"))
      updateRadioButtons(session, "interface_density",
                         selected = g("interface_density", "comfortable"))
      updateSliderInput(session, "font_scale",
                        value = g("font_scale", 1))

      updateCheckboxInput(session, "imp_clean_names",
                          value = g("imp_clean_names", TRUE))
      updateCheckboxInput(session, "imp_detect_dates",
                          value = g("imp_detect_dates", TRUE))
      updateCheckboxInput(session, "imp_trim", value = g("imp_trim", TRUE))
      updateSelectInput(session, "imp_encoding",
                        selected = g("imp_encoding", "UTF-8"))
      mc <- g("missing_value_labels", c("NA","N/A","NULL","99","999","-99"))
      updateTextInput(session, "imp_missing",
                      value = paste(mc, collapse = ", "))

      updateSliderInput(session, "confidence_level",
                        value = g("confidence_level", 0.95))
      updateSliderInput(session, "significance_level",
                        value = g("significance_level", 0.05))
      updateSelectInput(session, "correlation_method",
                        selected = g("correlation_method", "auto"))

      updateSliderInput(session, "ml_split", value = g("ml_split", 0.8))
      updateNumericInput(session, "random_seed",
                         value = g("random_seed", 123))
      updateCheckboxInput(session, "ml_autosave",
                          value = g("ml_autosave", FALSE))

      app_state$settings <- services$settings$all()
      if (exists("create_app_theme")) {
        session$setCurrentTheme(create_app_theme(g("theme_mode", "light")))
      }
      apply_appearance(g("accent_color", "blue"),
                       g("interface_density", "comfortable"),
                       g("font_scale", 1))
      ready(TRUE)
    }, ignoreInit = FALSE)

    # -- Generic persist helper --------------------------------------------
    persist <- function(key, value) {
      if (is.null(services) || is.null(services$settings)) return(invisible())
      services$settings$set(key, value)          # writes + saves to disk
      # keep the legacy app_state$settings in sync so un-migrated modules
      # that still read app_state$settings pick up the change too
      app_state$settings[[key]] <- value
      app_state$last_action <- paste("Updated setting:", key)
      last_saved(sprintf("Saved at %s", format(Sys.time(), "%H:%M:%S")))
    }

    # A write observer per control. All guarded by ready() so the initial
    # updateXxx calls above don't immediately echo back as "changes".
    obs <- function(input_id, key, transform = identity) {
      observeEvent(input[[input_id]], {
        req(ready())
        persist(key, transform(input[[input_id]]))
      }, ignoreInit = TRUE)
    }

    obs("decimal_places",     "decimal_places")
    obs("max_file_size",      "max_file_size")
    obs("imp_clean_names",    "imp_clean_names")
    obs("imp_detect_dates",   "imp_detect_dates")
    obs("imp_trim",           "imp_trim")
    obs("imp_encoding",       "imp_encoding")
    obs("confidence_level",   "confidence_level")
    obs("significance_level", "significance_level")
    obs("correlation_method", "correlation_method")
    obs("ml_split",           "ml_split")
    obs("random_seed",        "random_seed")
    obs("ml_autosave",        "ml_autosave")

    observeEvent(list(input$accent_color, input$interface_density, input$font_scale), {
      req(ready())
      persist("accent_color", input$accent_color)
      persist("interface_density", input$interface_density)
      persist("font_scale", as.numeric(input$font_scale))
      apply_appearance(input$accent_color, input$interface_density, input$font_scale)
    }, ignoreInit = TRUE)

    # missing codes: split the comma-separated text into a character vector
    observeEvent(input$imp_missing, {
      req(ready())
      codes <- trimws(strsplit(input$imp_missing, ",", fixed = TRUE)[[1]])
      codes <- codes[nzchar(codes)]
      persist("missing_value_labels", codes)
    }, ignoreInit = TRUE)

    # -- Theme: apply live -------------------------------------------------
    observeEvent(input$theme_mode, {
      req(ready())
      persist("theme_mode", input$theme_mode)
      # create_app_theme() is defined in global; swap the running theme
      if (exists("create_app_theme")) {
        session$setCurrentTheme(create_app_theme(input$theme_mode))
      }
      if (!is.null(services) && !is.null(services$notify)) {
        services$notify$success(
          sprintf("Theme set to %s", input$theme_mode), module = "settings")
      }
    }, ignoreInit = TRUE)

    # -- Reset -------------------------------------------------------------
    observeEvent(input$reset_all, {
      showModal(modalDialog(
        title = "Reset all settings?",
        "This restores every setting to its default value.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("reset_confirm"), "Reset", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$reset_confirm, {
      ready(FALSE)
      if (!is.null(services) && !is.null(services$settings)) {
        services$settings$reset()          # clears overrides, saves
        app_state$settings <- services$settings$all()
      }
      defaults <- if (exists("DEFAULT_SETTINGS")) DEFAULT_SETTINGS else list()
      updateSliderInput(session, "decimal_places", value = defaults$decimal_places %||% 3)
      updateNumericInput(session, "max_file_size", value = defaults$max_file_size %||% 100)
      updateRadioButtons(session, "theme_mode", selected = defaults$theme_mode %||% "light")
      updateSelectInput(session, "accent_color", selected = defaults$accent_color %||% "blue")
      updateRadioButtons(session, "interface_density", selected = defaults$interface_density %||% "comfortable")
      updateSliderInput(session, "font_scale", value = defaults$font_scale %||% 1)
      updateCheckboxInput(session, "imp_clean_names", value = defaults$imp_clean_names %||% TRUE)
      updateCheckboxInput(session, "imp_detect_dates", value = defaults$imp_detect_dates %||% TRUE)
      updateCheckboxInput(session, "imp_trim", value = defaults$imp_trim %||% TRUE)
      updateSelectInput(session, "imp_encoding", selected = defaults$imp_encoding %||% "UTF-8")
      updateTextInput(session, "imp_missing", value = paste(defaults$missing_value_labels %||% character(), collapse = ", "))
      updateSliderInput(session, "confidence_level", value = defaults$confidence_level %||% .95)
      updateSliderInput(session, "significance_level", value = defaults$significance_level %||% .05)
      updateSelectInput(session, "correlation_method", selected = defaults$correlation_method %||% "auto")
      updateSliderInput(session, "ml_split", value = defaults$ml_split %||% .8)
      updateNumericInput(session, "random_seed", value = defaults$random_seed %||% 123L)
      updateCheckboxInput(session, "ml_autosave", value = defaults$ml_autosave %||% FALSE)
      updateSelectInput(session, "ai_provider", selected = defaults$ai_provider %||% "ollama")
      default_model <- as.character(defaults$ai_model %||% "")
      updateSelectizeInput(session, "ai_model",
                           choices = if (nzchar(default_model)) default_model else character(0),
                           selected = if (nzchar(default_model)) default_model else NULL,
                           server = TRUE)
      updateTextInput(session, "ai_base_url", value = defaults$ai_base_url %||% "")
      updateTextInput(session, "ai_api_key", value = "")
      if (!is.null(services) && !is.null(services$ai)) services$ai$clear_session_api_key()
      updateSliderInput(session, "ai_temperature", value = defaults$ai_temperature %||% .2)
      updateNumericInput(session, "ai_max_tokens", value = defaults$ai_max_tokens %||% 1200L)
      updateNumericInput(session, "ai_timeout", value = defaults$ai_timeout %||% 300L)
      updateNumericInput(session, "ai_context_window", value = defaults$ai_context_window %||% 4096L)
      updateTextInput(session, "ai_keep_alive", value = defaults$ai_keep_alive %||% "10m")
      app_state$last_action <- "Reset settings to defaults"
      if (exists("create_app_theme")) {
        session$setCurrentTheme(create_app_theme(DEFAULT_SETTINGS$theme_mode))
      }
      apply_appearance(DEFAULT_SETTINGS$accent_color,
                       DEFAULT_SETTINGS$interface_density,
                       DEFAULT_SETTINGS$font_scale)
      removeModal()
      session$onFlushed(function() ready(TRUE), once = TRUE)
      last_saved("Reset to defaults and applied immediately.")
      if (!is.null(services) && !is.null(services$notify)) {
        services$notify$warning("All settings reset to defaults",
                                module = "settings")
      }
    })

    # -- System info -------------------------------------------------------
    output$system_info <- renderUI({
      db_ok <- !is.null(services) && !is.null(services$data)
      tagList(
        tags$dl(class = "row mb-0",
          tags$dt(class = "col-sm-4", "Application"),
          tags$dd(class = "col-sm-8", if (exists("SAP_PRESENTATION_VERSION")) SAP_PRESENTATION_VERSION else "Survey Analytics"),
          tags$dt(class = "col-sm-4", "R version"),
          tags$dd(class = "col-sm-8", R.version.string),
          tags$dt(class = "col-sm-4", "Backend services"),
          tags$dd(class = "col-sm-8",
                  if (!is.null(services)) "Active" else "Not wired"),
          tags$dt(class = "col-sm-4", "Settings store"),
          tags$dd(class = "col-sm-8", if (!is.null(app_state$current_user))
            "PostgreSQL per-user preferences" else "Session preferences (set SAP_SETTINGS_FILE for an optional local file)")
        )
      )
    })

    # ---- AI Assistant settings ------------------------------------------
    ai_status <- reactiveVal(NULL)

    observeEvent(settings_identity(), {
      if (is.null(services) || is.null(services$settings)) return(invisible(NULL))
      s <- services$settings
      cfg <- tryCatch(services$ai$config(), error = function(e) NULL)
      provider <- cfg$provider %||% s$get("ai_provider", "ollama")
      model <- as.character(cfg$model %||% s$get("ai_model", "") %||% "")
      updateSelectInput(session, "ai_provider", selected = provider)
      updateSelectizeInput(session, "ai_model",
        choices = if (nzchar(model)) model else character(0),
        selected = if (nzchar(model)) model else NULL,
        server = TRUE)
      updateTextInput(session, "ai_base_url",
                      value = cfg$base_url %||% s$get("ai_base_url", ""))
      updateSliderInput(session, "ai_temperature",
                        value = as.numeric(s$get("ai_temperature", 0.2)))
      updateNumericInput(session, "ai_max_tokens",
                         value = as.integer(s$get("ai_max_tokens", 1200)))
      updateNumericInput(session, "ai_timeout",
                         value = as.integer(s$get("ai_timeout", 300)))
      updateNumericInput(session, "ai_context_window",
                         value = as.integer(s$get("ai_context_window", 4096)))
      updateTextInput(session, "ai_keep_alive",
                      value = s$get("ai_keep_alive", "10m"))
    }, ignoreInit = FALSE)

    save_ai_from_inputs <- function() {
      if (is.null(services) || is.null(services$ai)) return(FALSE)
      services$ai$save_config(
        provider = input$ai_provider,
        api_key = input$ai_api_key,
        model = input$ai_model %||% "",
        base_url = input$ai_base_url,
        temperature = input$ai_temperature,
        max_tokens = input$ai_max_tokens,
        timeout = input$ai_timeout,
        num_ctx = input$ai_context_window,
        keep_alive = input$ai_keep_alive
      )
      app_state$settings <- services$settings$all()
      TRUE
    }

    refresh_ollama_models <- function(show_notice = FALSE) {
      if (is.null(services) || is.null(services$ai)) return(invisible(NULL))
      status <- services$ai$available_models(input$ai_base_url)
      ai_status(status)
      app_state$ai_connection_status <- list(
        state = if (isTRUE(status$ok) && length(status$models %||% character(0))) "ready" else if (isTRUE(status$ok)) "warning" else "error",
        message = status$error %||% sprintf("%s model(s) available", length(status$models %||% character(0)))
      )
      if (!isTRUE(status$ok)) {
        if (show_notice) showNotification(status$error, type = "error", duration = 8)
        return(invisible(status))
      }

      models <- status$models %||% character(0)
      current <- trimws(as.character(input$ai_model %||% ""))
      selected <- if (nzchar(current) && current %in% models) {
        current
      } else if (length(models)) models[[1]] else NULL

      updateSelectizeInput(session, "ai_model",
                           choices = models, selected = selected, server = TRUE)
      if (!is.null(selected) && nzchar(selected)) {
        services$ai$save_config(model = selected, base_url = status$base_url)
      }
      if (show_notice) {
        if (length(models)) {
          showNotification(sprintf("Found %d Ollama model%s.",
                                   length(models), if (length(models) == 1L) "" else "s"),
                           type = "message")
        } else {
          showNotification("The Ollama endpoint is reachable, but no models are available.",
                           type = "warning", duration = 8)
        }
      }
      invisible(status)
    }

    output$ai_deployment_guidance <- renderUI({
      provider <- as.character(input$ai_provider %||% "ollama")
      if (identical(provider, "mock")) {
        return(div(class = "alert alert-warning small py-2",
          icon("flask"), " Mock mode never contacts a model and is only for interface testing."))
      }
      if (identical(provider, "anthropic")) {
        return(div(class = "alert alert-info small py-2",
          icon("cloud"), " Anthropic uses the API key and model fields. The Ollama endpoint field is ignored."))
      }

      hosted <- exists("SAP_HOSTED_RUNTIME") && isTRUE(SAP_HOSTED_RUNTIME)
      base <- trimws(as.character(input$ai_base_url %||% ""))
      loopback <- grepl("^https?://(localhost|127\\.0\\.0\\.1)(:|/|$)", base, ignore.case = TRUE)
      if (hosted && (loopback || !nzchar(base))) {
        return(div(class = "alert alert-danger small py-2",
          icon("triangle-exclamation"), " ",
          "Published apps cannot use the Ollama service running on your personal computer through localhost. Set OLLAMA_BASE_URL to a reachable protected endpoint or use https://ollama.com/api with OLLAMA_API_KEY."))
      }
      if (hosted) {
        return(div(class = "alert alert-info small py-2",
          icon("cloud"), " Published runtime: the Ollama endpoint must be reachable from the hosting service. Prefer HTTPS and authentication."))
      }
      div(class = "alert alert-success small py-2",
          icon("computer"), " Local runtime: http://localhost:11434 can use the Ollama service installed on this computer.")
    })

    output$ai_server_status <- renderUI({
      provider <- as.character(input$ai_provider %||% "ollama")
      if (!identical(provider, "ollama")) {
        return(div(class = "small text-muted mb-3",
                   icon("circle-info"), " ",
                   if (identical(provider, "mock"))
                     "Mock mode is ready for interface testing."
                   else "Use Test connection to verify the selected cloud provider."))
      }
      status <- ai_status()
      if (is.null(status)) {
        return(div(class = "small text-muted mb-3",
                   icon("server"), " Refresh models to check the configured Ollama endpoint."))
      }
      if (!isTRUE(status$ok)) {
        return(div(class = "alert alert-warning py-2 small",
                   icon("triangle-exclamation"), " ", status$error))
      }
      model_count <- length(status$models %||% character(0))
      div(class = sprintf("alert %s py-2 small",
                          if (model_count) "alert-success" else "alert-warning"),
          icon(if (model_count) "circle-check" else "circle-exclamation"), " ",
          sprintf("Ollama %s is reachable at %s. %d model%s detected.",
                  status$version %||% "unknown", status$base_url,
                  model_count, if (model_count == 1L) "" else "s"))
    })

    # Paint the Settings page first, then perform a short local health check.
    session$onFlushed(function() {
      if (!is.null(services) && !is.null(services$ai) &&
          identical(services$ai$current_provider(), "ollama")) {
        try(refresh_ollama_models(FALSE), silent = TRUE)
      }
    }, once = TRUE)

    observeEvent(input$ai_provider, {
      provider <- as.character(input$ai_provider %||% "ollama")
      if (identical(provider, "ollama")) {
        refresh_ollama_models(FALSE)
      } else {
        ai_status(NULL)
        default_model <- if (identical(provider, "anthropic"))
          trimws(Sys.getenv("ANTHROPIC_MODEL", "")) else "mock-1"
        updateSelectizeInput(
          session, "ai_model",
          choices = if (nzchar(default_model)) default_model else character(0),
          selected = if (nzchar(default_model)) default_model else NULL,
          server = TRUE
        )
      }
    }, ignoreInit = TRUE)

    observeEvent(input$ai_refresh_models, {
      if (!identical(input$ai_provider, "ollama")) {
        showNotification("Select Ollama before refreshing models.",
                         type = "warning")
        return(invisible(NULL))
      }
      services$ai$save_config(base_url = input$ai_base_url, api_key = input$ai_api_key)
      refresh_ollama_models(TRUE)
    })

    observeEvent(input$ai_save, {
      if (!save_ai_from_inputs()) {
        showNotification("AI service unavailable.", type = "error")
        return(invisible(NULL))
      }
      updateTextInput(session, "ai_api_key", value = "")
      app_state$last_action <- "Saved AI settings"
      showNotification("AI settings saved.", type = "message")
    })

    observeEvent(input$ai_test, {
      if (is.null(services) || is.null(services$ai)) return(invisible(NULL))
      save_ai_from_inputs()
      updateTextInput(session, "ai_api_key", value = "")
      res <- services$ai$test_connection()
      app_state$ai_connection_status <- list(
        state = if (isTRUE(res$ok)) "ready" else "error",
        message = res$message %||% "AI connection test completed"
      )
      if (!is.null(res$models) || !is.null(res$version) || !isTRUE(res$ok)) {
        ai_status(list(
          ok = isTRUE(res$ok),
          base_url = input$ai_base_url,
          version = res$version %||% "unknown",
          models = res$models %||% character(0),
          error = if (isTRUE(res$ok)) NULL else res$message
        ))
      }
      if (isTRUE(res$ok) && nzchar(res$model %||% "")) {
        models <- res$models %||% res$model
        updateSelectizeInput(session, "ai_model",
                             choices = models, selected = res$model, server = TRUE)
      }
      output$ai_test_result <- renderUI({
        div(class = sprintf("alert %s mt-2 mb-0",
                            if (isTRUE(res$ok)) "alert-success" else "alert-danger"),
            icon(if (isTRUE(res$ok)) "circle-check" else "circle-xmark"), " ",
            res$message)
      })
    })
    output$save_status <- renderText(last_saved())
  })
}

message("[SAP] Settings v2.7-20260801 loaded")  # SAP_MODULE_BANNER
