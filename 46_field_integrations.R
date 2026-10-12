# Governed field-data connectors and field-monitoring register.

sap_integration_json_request <- function(url, token = "", timeout_seconds = 20) {
  url <- trimws(as.character(url)[[1]])
  if (!grepl("^https://", url, ignore.case = TRUE)) {
    stop("Field-data connectors require HTTPS endpoints.", call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Install the recommended 'httr' package to use remote field connectors.", call. = FALSE)
  }
  headers <- if (nzchar(token)) httr::add_headers(Authorization = token) else NULL
  resp <- httr::GET(url, headers, httr::timeout(timeout_seconds))
  httr::stop_for_status(resp)
  httr::content(resp, as = "parsed", type = "application/json")
}

sap_field_visits_empty <- function() {
  data.frame(
    date = character(), site = character(), observation = character(),
    follow_up = character(), recorded_by = character(),
    stringsAsFactors = FALSE
  )
}

mod_field_integrations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_professional_header(
      "Field Integrations",
      "Bring approved HTTPS field systems into a controlled staging area and capture monitoring visits.",
      "satellite-dish", "Secure connectors"
    ),
    layout_columns(
      card(
        card_header("HTTPS API connector"),
        textInput(ns("url"), "JSON endpoint", placeholder = "https://example.org/api/submissions"),
        passwordInput(ns("token"), "Authorization header (optional)"),
        actionButton(ns("test"), "Validate connector", icon = icon("shield")),
        verbatimTextOutput(ns("status"))
      ),
      card(
        card_header("Field visit note"),
        textInput(ns("site"), "Site / location"),
        dateInput(ns("visit_date"), "Visit date"),
        textAreaInput(ns("observation"), "Observation", rows = 4),
        textInput(ns("followup"), "Follow-up action"),
        actionButton(ns("add_visit"), "Add monitoring note", class = "btn-primary"),
        DT::DTOutput(ns("visits"))
      ),
      col_widths = c(5, 7)
    )
  )
}

mod_field_integrations_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    # One-time state repair must be isolated because app_state is reactiveValues.
    shiny::isolate({
      if (is.null(app_state$field_visits) ||
          !is.data.frame(app_state$field_visits) ||
          !all(c("date", "site", "observation", "follow_up", "recorded_by") %in% names(app_state$field_visits))) {
        app_state$field_visits <- sap_field_visits_empty()
      }
    })

    status <- reactiveVal("Not checked")

    observeEvent(input$test, {
      status(tryCatch({
        sap_integration_json_request(input$url, input$token)
        "HTTPS connector responded successfully."
      }, error = function(e) paste("Validation:", conditionMessage(e))))
    })

    output$status <- renderText(status())
    output$visits <- DT::renderDT(
      DT::datatable(app_state$field_visits, options = list(scrollX = TRUE), rownames = FALSE)
    )

    observeEvent(input$add_visit, {
      req(nzchar(trimws(input$site)), nzchar(trimws(input$observation)))
      app_state$field_visits <- rbind(
        app_state$field_visits,
        data.frame(
          date = as.character(input$visit_date), site = input$site,
          observation = input$observation, follow_up = input$followup,
          recorded_by = app_state$current_user_profile$username %||% "user",
          stringsAsFactors = FALSE
        )
      )
      app_state$last_action <- "Added field monitoring note"
    })
  })
}
