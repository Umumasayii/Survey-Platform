# =============================================================================
# MODULE 40: ADVANCED BI STUDIO HOME
# =============================================================================

mod_studio_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "sap-nexus-hero",
      div(
        class = "sap-nexus-hero-copy",
        div(class = "sap-hero-eyebrow", span(class = "sap-live-pulse"), "ADVANCED BI STUDIO · SURVEY INTELLIGENCE"),
        h1("Connected BI, built for serious survey work."),
        p("Prepare complex field data, model trusted relationships, build interactive stories, and publish governed evidence while keeping the familiar survey-analysis workspace intact."),
        div(class = "sap-hero-actions",
            actionButton(ns("start_connect"), "Connect data", icon = icon("arrow-right"), class = "btn-light btn-lg"),
            actionButton(ns("open_dashboard"), "Open dashboard", icon = icon("gauge-high"), class = "btn-outline-light btn-lg"))
      ),
      div(
        class = "sap-hero-orbit",
        div(class = "sap-orbit-ring ring-one"),
        div(class = "sap-orbit-ring ring-two"),
        div(class = "sap-orbit-center", icon("chart-simple")),
        div(class = "sap-orbit-node node-a", icon("database")),
        div(class = "sap-orbit-node node-b", icon("diagram-project")),
        div(class = "sap-orbit-node node-c", icon("wand-magic-sparkles")),
        div(class = "sap-orbit-node node-d", icon("shield-halved"))
      )
    ),
    uiOutput(ns("workspace_metrics")),
    div(
      class = "sap-home-section-head",
      div(h2("Your analytics journey"), p("A familiar BI workflow, with survey science built into every stage.")),
      span("6 connected stages", class = "sap-section-badge")
    ),
    div(
      class = "sap-journey-grid",
      sap_journey_card(ns("journey_connect"), "01", "Connect", "Files, HTTPS, PostgreSQL and several related tables.", "database", "Bring data in"),
      sap_journey_card(ns("journey_prepare"), "02", "Prepare", "Replayable cleaning, joins, pivots and safe calculations.", "wand-magic-sparkles", "Shape data"),
      sap_journey_card(ns("journey_model"), "03", "Model", "Relationships, measures, field roles and drill hierarchies.", "diagram-project", "Build meaning"),
      sap_journey_card(ns("journey_explore"), "04", "Explore", "Interactive visuals with reusable measures and filters.", "chart-column", "Find insight"),
      sap_journey_card(ns("journey_dashboard"), "05", "Dashboard", "Pages, cross-filtering, drillthrough and bookmarks.", "gauge-high", "Tell the story"),
      sap_journey_card(ns("journey_publish"), "06", "Publish", "Versions, reviewers, refresh schedules and row security.", "paper-plane", "Share safely")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        class = "sap-card sap-home-panel",
        card_header(icon("lightbulb"), " Workspace intelligence"),
        card_body(uiOutput(ns("workspace_intelligence")))
      ),
      card(
        class = "sap-card sap-home-panel",
        card_header(icon("microscope"), " Specialist advantage"),
        card_body(
          div(class = "sap-specialist-list",
              sap_specialist_item("Survey design", "Weights, strata, clusters and domain estimates", "scale-balanced"),
              sap_specialist_item("Questionnaires & scales", "Metadata, routing, reliability and constructs", "clipboard-list"),
              sap_specialist_item("Inference & modelling", "Tests, diagnostics, prediction and effect sizes", "square-root-variable"),
              sap_specialist_item("Privacy-aware AI", "Structured summaries, review gates and local models", "brain")),
          actionButton(ns("open_advanced"), "Open advanced survey workspace", icon = icon("arrow-right"), class = "btn-outline-primary w-100 mt-3")
        )
      )
    ),
    div(
      class = "sap-home-footer-cta",
      div(icon("circle-play"), div(strong("New workspace?"), span("Start with a guided import or load a practice dataset in Data Lab."))),
      div(class = "d-flex gap-2 flex-wrap",
          actionButton(ns("guided_import"), "Guided import", class = "btn-primary"),
          actionButton(ns("open_lab"), "Open Data Lab", class = "btn-outline-secondary"))
    )
  )
}

sap_journey_card <- function(id, number, title, description, icon_name, action) {
  actionButton(
    id,
    div(
      class = "sap-journey-card-inner",
      div(class = "sap-journey-card-top", span(number, class = "sap-journey-number"), div(class = "sap-journey-icon", icon(icon_name))),
      div(class = "sap-journey-copy", h3(title), p(description)),
      div(class = "sap-journey-action", span(action), icon("arrow-up-right-from-square"))
    ),
    class = "sap-journey-card"
  )
}

sap_specialist_item <- function(title, description, icon_name) {
  div(class = "sap-specialist-item", div(class = "sap-specialist-icon", icon(icon_name)),
      div(strong(title), span(description)))
}

mod_studio_home_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    sap_bi_ensure_state(app_state)

    output$workspace_metrics <- renderUI({
      model <- app_state$bi
      df <- sap_bi_get_table(app_state, model$active_table)
      rows <- if (is.data.frame(df)) nrow(df) else 0L
      fields <- if (is.data.frame(df)) ncol(df) else 0L
      quality <- app_state$quality_score
      quality_text <- if (is.null(quality) || is.na(quality)) "Not scored" else paste0(round(quality), "/100")
      div(class = "sap-home-metrics",
          div(class = "sap-home-metric", div(class = "sap-home-metric-icon", icon("table-list")), div(span("Model tables"), strong(length(model$tables)), span(if (length(model$tables)) "Connected workspace" else "Connect your first source", class = "small"))),
          div(class = "sap-home-metric", div(class = "sap-home-metric-icon", icon("users")), div(span("Active rows"), strong(format(rows, big.mark = ",")), span(paste(fields, "fields"), class = "small"))),
          div(class = "sap-home-metric", div(class = "sap-home-metric-icon", icon("chart-column")), div(span("Saved visuals"), strong(length(model$dashboard$visuals %||% list())), span(paste(length(model$dashboard$pages %||% list()), "dashboard pages"), class = "small"))),
          div(class = "sap-home-metric", div(class = "sap-home-metric-icon", icon("shield-heart")), div(span("Data quality"), strong(quality_text), span(if (isTRUE(app_state$data_imported)) "Active table assessment" else "Waiting for data", class = "small"))))
    })

    output$workspace_intelligence <- renderUI({
      model <- app_state$bi
      if (!length(model$tables)) return(div(
        class = "sap-intelligence-empty",
        div(class = "sap-intelligence-illustration", icon("database")),
        h3("A clean canvas"),
        p("Connect survey responses and supporting lookup tables. BI Studio will profile fields and guide the next stage."),
        actionButton(session$ns("empty_connect"), "Connect first table", icon = icon("arrow-right"), class = "btn-primary")
      ))
      df <- sap_bi_get_table(app_state, model$active_table)
      missing <- if (is.data.frame(df) && nrow(df) * ncol(df) > 0) 100 * sum(is.na(df)) / (nrow(df) * ncol(df)) else 0
      steps <- length(model$pipelines[[model$active_table]] %||% list())
      insights <- c(
        paste0("Active table: ", model$active_table, " with ", format(nrow(df), big.mark = ","), " rows."),
        paste0(sprintf("%.1f%%", missing), " of cells are missing; review missingness before publication."),
        paste0(steps, " repeatable preparation step(s) are attached to the active table."),
        paste0(nrow(model$relationships), " relationship(s) and ", nrow(model$measures), " reusable measure(s) define the semantic layer.")
      )
      div(class = "sap-intelligence-feed", lapply(seq_along(insights), function(i) div(
        class = "sap-intelligence-row", span(sprintf("%02d", i), class = "sap-intelligence-index"),
        span(insights[[i]]), icon(if (i == 2L && missing > 10) "triangle-exclamation" else "circle-check")
      )))
    })

    list(
      connect = reactive(sum(c(input$start_connect, input$journey_connect, input$empty_connect), na.rm = TRUE)),
      prepare = reactive(input$journey_prepare),
      model = reactive(input$journey_model),
      explore = reactive(input$journey_explore),
      dashboard = reactive(sum(c(input$open_dashboard, input$journey_dashboard), na.rm = TRUE)),
      publish = reactive(input$journey_publish),
      advanced = reactive(input$open_advanced),
      import = reactive(input$guided_import),
      lab = reactive(input$open_lab)
    )
  })
}

message("[SAP] Advanced BI Studio Home v4.2.0 loaded")
