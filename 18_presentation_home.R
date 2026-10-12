# =============================================================================
# MODULE 18: PRESENTATION HOME
# =============================================================================
# Compact landing page focused on three clear starts:
#   1. import the user's own data
#   2. test the application with a bundled survey
#   3. open learning or personal tools
# =============================================================================

mod_presentation_home_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "sap-home-hero mb-4",
      div(
        class = "sap-home-copy",
        span(class = "sap-home-eyebrow", "Survey Analytics Workspace"),
        h1("Start with data. Finish with a decision."),
        p(
          "Import a survey, test the workflow with an example, or explore the learning tools.",
          class = "sap-home-lead"
        ),
        div(
          class = "d-flex gap-2 flex-wrap mt-3",
          actionButton(
            ns("go_import"), "Use My Data",
            icon = icon("upload"), class = "btn btn-light btn-lg"
          ),
          actionButton(
            ns("choose_demo"), "Test Using Existing Data",
            icon = icon("vial-circle-check"), class = "btn btn-outline-light btn-lg"
          )
        )
      ),
      div(
        class = "sap-home-route",
        div(class = "sap-home-route-icon", icon("route")),
        div(
          span("Recommended path", class = "small text-uppercase opacity-75"),
          strong("Load → Review → Analyse → Report"),
          p("A clear five-minute route for first-time users.", class = "mb-0 small opacity-75")
        )
      )
    ),

    uiOutput(ns("survey_overview")),

    div(
      class = "sap-home-action-grid",
      div(
        class = "sap-home-action-card",
        div(class = "sap-home-action-icon", icon("flask")),
        div(
          h5("Explore sample data"),
          p("Browse every dataset shipped with base R or generate synthetic survey data."),
          actionButton(ns("go_data_lab"), "Open Data Lab", icon = icon("arrow-right"),
                       class = "btn btn-sm btn-outline-primary")
        )
      ),
      div(
        class = "sap-home-action-card",
        div(class = "sap-home-action-icon", icon("heart-pulse")),
        div(
          h5("Track personal health"),
          p("Record adult BMI, blood-pressure readings, sleep, steps and basic trends."),
          actionButton(ns("go_health_tracker"), "Open Health Tracker", icon = icon("arrow-right"),
                       class = "btn btn-sm btn-outline-primary")
        )
      ),
      div(
        class = "sap-home-action-card",
        div(class = "sap-home-action-icon", icon("wand-magic-sparkles")),
        div(
          h5("Continue confidently"),
          p("Use guided checks, reproducible outputs and AI-assisted interpretation."),
          actionButton(ns("go_analysis"), "Open Analysis", icon = icon("arrow-right"),
                       class = "btn btn-sm btn-outline-primary")
        )
      )
    ),

    uiOutput(ns("next_actions"))
  )
}

mod_presentation_home_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    household_loaded <- reactiveVal(0L)
    health_loaded <- reactiveVal(0L)
    likert_loaded <- reactiveVal(0L)

    current_df <- reactive({
      if (!isTRUE(app_state$data_imported) || !is.data.frame(app_state$processed_data)) {
        return(NULL)
      }
      app_state$processed_data
    })

    load_demo <- function(df, dataset_name) {
      withProgress(
        message = sprintf("Loading %s", dataset_name),
        detail = "Preparing variable types and quality indicators",
        value = 0.4,
        {
          variable_types <- tryCatch(detect_variable_types(df), error = function(e) NULL)
          app_state$dataset_name <- dataset_name
          if (!is.null(services) && !is.null(services$data)) {
            if (!is.null(services$project) && !is.null(app_state$current_user) &&
                is.null(services$project$current_id())) {
              services$project$create(
                name = paste(dataset_name, "analysis"),
                description = "Auto-created from a built-in demonstration dataset",
                persist_current = FALSE
              )
            }
            services$data$set(
              df, kind = "raw", module = "presentation_home",
              description = sprintf("Stored original demo dataset: %s", dataset_name),
              variable_types = variable_types, name = dataset_name
            )
            services$data$set(
              df, kind = "processed", module = "presentation_home",
              description = sprintf("Loaded demo dataset: %s", dataset_name),
              variable_types = variable_types, name = dataset_name
            )
          } else {
            app_state$raw_data <- df
            app_state$processed_data <- df
            app_state$data_imported <- TRUE
            app_state$variable_types <- variable_types
          }

          if (is.null(app_state$current_project))
            app_state$current_project_name <- "Example Workspace"
          app_state$quality_score <- tryCatch(calculate_quality_score(df)$score,
                                              error = function(e) NA_real_)
          app_state$transformation_count <- 0L
          app_state$last_action <- sprintf("Loaded %s", dataset_name)
          incProgress(0.6)
        }
      )

      showNotification(
        sprintf("%s loaded: %s rows × %s variables",
                dataset_name, format(nrow(df), big.mark = ","), ncol(df)),
        type = "message", duration = 5
      )
    }

    observeEvent(input$choose_demo, {
      showModal(modalDialog(
        title = tagList(icon("vial-circle-check"), "Test using existing data"),
        size = "m", easyClose = TRUE,
        p("Choose one realistic survey dataset. It will become the active workspace dataset."),
        radioButtons(
          session$ns("demo_choice"), NULL,
          choices = c(
            "Household survey — demographics, income, services and satisfaction" = "household",
            "Health survey — blood pressure, BMI, history and adherence" = "health"
          ),
          selected = "household"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_demo"), "Load selected dataset",
                       icon = icon("play"), class = "btn btn-primary")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_demo, {
      choice <- input$demo_choice %||% "household"
      removeModal()
      if (identical(choice, "health")) {
        load_demo(generate_health_survey(300), "Health Survey Demo")
        health_loaded(health_loaded() + 1L)
      } else {
        load_demo(generate_household_survey(500), "Household Survey Demo")
        household_loaded(household_loaded() + 1L)
      }
    }, ignoreInit = TRUE, priority = 100)

    output$survey_overview <- renderUI({
      df <- current_df()
      if (is.null(df)) return(NULL)

      total_cells <- nrow(df) * ncol(df)
      missing_cells <- sum(is.na(df))
      completion <- if (total_cells == 0) 100 else 100 * (1 - missing_cells / total_cells)
      quality <- app_state$quality_score
      if (is.null(quality) || length(quality) == 0 || is.na(quality)) {
        quality <- tryCatch(calculate_quality_score(df)$score, error = function(e) NA_real_)
      }

      div(
        class = "sap-home-active mb-4",
        div(
          class = "sap-home-active-copy",
          span(icon("circle-check"), " Active dataset", class = "sap-home-active-label"),
          strong(app_state$dataset_name %||% "Active dataset"),
          span(sprintf("%s rows · %s variables · %.1f%% complete",
                       format(nrow(df), big.mark = ","), ncol(df), completion),
               class = "text-muted small")
        ),
        div(
          class = "sap-home-active-actions",
          actionButton(session$ns("go_explorer"), "Explore", icon = icon("magnifying-glass"),
                       class = "btn btn-sm btn-primary"),
          actionButton(session$ns("go_cleaning"), "Clean", icon = icon("broom"),
                       class = "btn btn-sm btn-outline-primary"),
          actionButton(session$ns("go_report"), "Report", icon = icon("file-lines"),
                       class = "btn btn-sm btn-outline-primary")
        )
      )
    })

    output$next_actions <- renderUI({
      if (is.null(current_df())) return(NULL)

      missing_vars <- sum(vapply(current_df(), function(x) any(is.na(x)), logical(1)))
      duplicate_rows <- sum(duplicated(current_df()))
      div(
        class = "sap-home-note mt-4",
        div(
          icon("shield-halved"),
          div(
            strong("Quick quality check"),
            p(sprintf("%s variables contain missing values and %s duplicate rows were detected.",
                      missing_vars, duplicate_rows), class = "mb-0 small text-muted")
          )
        ),
        actionButton(session$ns("review_quality"), "Review quality",
                     icon = icon("arrow-right"), class = "btn btn-sm btn-outline-primary")
      )
    })

    list(
      go_import = reactive(input$go_import),
      next_import = reactive(NULL),
      go_explorer = reactive(input$go_explorer),
      go_cleaning = reactive(input$go_cleaning),
      review_quality = reactive(input$review_quality),
      go_analysis = reactive(input$go_analysis),
      go_report = reactive(input$go_report),
      go_data_lab = reactive(input$go_data_lab),
      go_health_tracker = reactive(input$go_health_tracker),
      household_loaded = household_loaded,
      health_loaded = health_loaded,
      likert_loaded = likert_loaded
    )
  })
}

message("[SAP] Presentation Home v1.1-20260731 loaded")  # SAP_MODULE_BANNER
