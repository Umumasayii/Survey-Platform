# Guided workspace: task-oriented entry point without hiding existing modules.

mod_guided_workflow_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_professional_header(
      "Guided Workspace",
      "Start with the decision you need to make; the platform recommends the safest next workflow.",
      "route",
      "Role-aware navigation"
    ),
    layout_columns(
      card(
        card_header("Your working mode"),
        selectInput(ns("mode"), "Profile", SAP_USER_MODES, selected = "MEL Officer"),
        selectInput(
          ns("goal"), "Primary task",
          c(
            "Monitor project performance" = "target",
            "Clean and validate data" = "quality",
            "Describe a survey" = "describe",
            "Compare groups" = "compare",
            "Understand change over time" = "change",
            "Map intervention coverage" = "where",
            "Build a predictive model" = "predict",
            "Prepare a programme report" = "report"
          )
        ),
        actionButton(
          ns("recommend"), "Recommend workflow",
          icon = icon("wand-magic-sparkles"), class = "btn-primary"
        )
      ),
      card(
        card_header("Recommended pathway"),
        uiOutput(ns("pathway"))
      ),
      col_widths = c(4, 8)
    ),
    card(
      card_header("Launch checklist"),
      div(
        class = "sap-launch-checklist",
        p(strong("1. Define the question"), " — decision, indicator or hypothesis."),
        p(strong("2. Validate the data"), " — quality, consent, design and missingness."),
        p(strong("3. Use the appropriate method"), " — assumptions, weights and uncertainty."),
        p(strong("4. Interpret responsibly"), " — effect size, limitations and disaggregation."),
        p(strong("5. Publish evidence"), " — verification, privacy review and reproducible export.")
      )
    )
  )
}

mod_guided_workflow_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    rec <- reactiveVal(sap_literacy_recommendation("target", "count"))

    observeEvent(input$recommend, {
      app_state$user_mode <- input$mode
      g <- input$goal
      r <- if (identical(g, "quality")) {
        list(
          route = "cleaning", title = "Data quality workflow",
          method = "Data Cleaning → Missing & Nonresponse → Privacy",
          why = "Validate the dataset before analysis."
        )
      } else if (identical(g, "report")) {
        list(
          route = "program_reports", title = "Programme reporting",
          method = "MEL → verification → Programme Reports",
          why = "Reports should be generated from governed results."
        )
      } else {
        sap_literacy_recommendation(g, "count")
      }
      rec(r)
    })

    output$pathway <- renderUI({
      r <- rec()
      div(
        class = "sap-guided-path",
        h3(r$title),
        p(r$why),
        div(class = "sap-guided-step", icon("circle-check"), r$method),
        p(class = "text-muted", "All existing specialist tools remain available from the main navigation.")
      )
    })
  })
}
