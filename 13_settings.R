# =============================================================================
# MODULE 13: SETTINGS
# =============================================================================
# A configuration page wired to the backend SettingsService (00_services.R).
# The module NEVER stores settings itself — it reads/writes through
# services$settings (get/set/all/reset), which persists to sap_settings.rds.
#
# Scope discipline:
#   - Controls exist for settings that are actually consumed somewhere in the
#     app today (theme, decimals, confidence/significance, import defaults,
#     ML split, seed, correlation method).
#   - Sections the spec lists but that aren't wired yet (backup, security,
#     full reporting template designer) are shown as clearly-labelled
#     "planned" placeholders rather than fake toggles that do nothing.
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
          p("Number formatting and application defaults.",
            class = "text-muted small"),
          hr(),
          sliderInput(ns("decimal_places"), "Decimal places",
                      min = 0, max = 8, value = 3, step = 1),
          selectInput(ns("date_format"), "Preferred date format",
                      choices = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
                                  "%d-%m-%Y", "%Y/%m/%d")),
          numericInput(ns("max_file_size"), "Max import file size (MB)",
                       value = 100, min = 1, max = 2000, step = 10)
        ))
      ),

      # --- APPEARANCE -----------------------------------------------------
      nav_panel(
        title = tagList(icon("palette"), " Appearance"),
        div(class = "card", div(class = "card-body",
          h5("Appearance"),
          p("Theme is applied immediately across the app.",
            class = "text-muted small"),
          hr(),
          radioButtons(ns("theme_mode"), "Theme",
                       choices = c("Light" = "light", "Dark" = "dark"),
                       inline = TRUE),
          selectInput(ns("chart_palette"), "Default chart palette",
                      choices = c("Viridis" = "viridis",
                                  "Blues" = "blues",
                                  "Set2 (qualitative)" = "set2",
                                  "Okabe-Ito (colorblind-safe)" = "okabe")),
          div(class = "alert alert-light border small mt-2",
              icon("circle-info"),
              " Accent colour, density, and font-size controls are planned; ",
              "theme + palette are live now.")
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
          checkboxInput(ns("imp_detect_types"),
                        "Automatically detect variable types", TRUE),
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
          p("Used as defaults by tests, descriptives, and reports.",
            class = "text-muted small"),
          hr(),
          sliderInput(ns("confidence_level"), "Confidence level",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          sliderInput(ns("significance_level"), "Significance level (alpha)",
                      min = 0.001, max = 0.10, value = 0.05, step = 0.001),
          selectInput(ns("correlation_method"), "Default correlation method",
                      choices = c("Pearson" = "pearson",
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

      # --- REPORTING ------------------------------------------------------
      nav_panel(
        title = tagList(icon("file-lines"), " Reporting"),
        div(class = "card", div(class = "card-body",
          h5("Reporting Preferences"),
          p("Default sections included in generated reports.",
            class = "text-muted small"),
          hr(),
          checkboxGroupInput(ns("report_sections"),
            "Include by default:",
            choices = c(
              "Cover page"            = "cover",
              "Dataset overview"      = "overview",
              "Data quality summary"  = "quality",
              "Descriptive statistics"= "descriptives",
              "Frequency tables"      = "frequencies",
              "Charts"                = "charts",
              "Statistical tests"     = "tests",
              "Machine learning"      = "ml",
              "Generated R code"      = "code",
              "Session information"   = "session"
            ),
            selected = c("cover", "overview", "descriptives", "charts")
          ),
          textInput(ns("report_author"), "Default report author", ""),
          textInput(ns("report_org"), "Organisation / institution", "")
        ))
      ),

      # --- SYSTEM ---------------------------------------------------------
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

    observeEvent(TRUE, once = TRUE, {
      g <- function(k, d) .settings_get(services, k, d)

      updateSliderInput(session, "decimal_places",
                        value = g("decimal_places", 3))
      updateSelectInput(session, "date_format",
                        selected = g("date_format",
                                     g("date_formats", "%Y-%m-%d")[1]))
      updateNumericInput(session, "max_file_size",
                         value = g("max_file_size", 100))
      updateRadioButtons(session, "theme_mode",
                         selected = g("theme_mode", "light"))
      updateSelectInput(session, "chart_palette",
                        selected = g("chart_palette", "viridis"))

      updateCheckboxInput(session, "imp_clean_names",
                          value = g("imp_clean_names", TRUE))
      updateCheckboxInput(session, "imp_detect_types",
                          value = g("imp_detect_types", TRUE))
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
                        selected = g("correlation_method", "pearson"))

      updateSliderInput(session, "ml_split", value = g("ml_split", 0.8))
      updateNumericInput(session, "random_seed",
                         value = g("random_seed", 123))
      updateCheckboxInput(session, "ml_autosave",
                          value = g("ml_autosave", FALSE))

      updateCheckboxGroupInput(session, "report_sections",
        selected = g("report_sections",
                     c("cover","overview","descriptives","charts")))
      updateTextInput(session, "report_author", value = g("report_author", ""))
      updateTextInput(session, "report_org", value = g("report_org", ""))

      ready(TRUE)
    })

    # -- Generic persist helper --------------------------------------------
    persist <- function(key, value) {
      if (is.null(services) || is.null(services$settings)) return(invisible())
      services$settings$set(key, value)          # writes + saves to disk
      # keep the legacy app_state$settings in sync so un-migrated modules
      # that still read app_state$settings pick up the change too
      app_state$settings[[key]] <- value
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
    obs("date_format",        "date_format")
    obs("max_file_size",      "max_file_size")
    obs("chart_palette",      "chart_palette")
    obs("imp_clean_names",    "imp_clean_names")
    obs("imp_detect_types",   "imp_detect_types")
    obs("imp_trim",           "imp_trim")
    obs("imp_encoding",       "imp_encoding")
    obs("confidence_level",   "confidence_level")
    obs("significance_level", "significance_level")
    obs("correlation_method", "correlation_method")
    obs("ml_split",           "ml_split")
    obs("random_seed",        "random_seed")
    obs("ml_autosave",        "ml_autosave")
    obs("report_sections",    "report_sections")
    obs("report_author",      "report_author")
    obs("report_org",         "report_org")

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
      if (!is.null(services) && !is.null(services$settings)) {
        services$settings$reset()          # clears overrides, saves
      }
      removeModal()
      last_saved("Reset to defaults. Reopen Settings to see values.")
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
          tags$dd(class = "col-sm-8", "Survey Analytics Platform v2.2"),
          tags$dt(class = "col-sm-4", "R version"),
          tags$dd(class = "col-sm-8", R.version.string),
          tags$dt(class = "col-sm-4", "Backend services"),
          tags$dd(class = "col-sm-8",
                  if (!is.null(services)) "Active" else "Not wired"),
          tags$dt(class = "col-sm-4", "Settings store"),
          tags$dd(class = "col-sm-8", "sap_settings.rds")
        )
      )
    })

    output$save_status <- renderText(last_saved())
  })
}

message("[SAP] Settings v2.2-20260706 loaded")  # SAP_MODULE_BANNER
