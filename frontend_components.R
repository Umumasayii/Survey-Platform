# =============================================================================
# SURVEY ANALYTICS PLATFORM — FRONTEND COMPONENTS
# =============================================================================
# Shared UI helpers used by survey-specialist and Studio modules.
# =============================================================================

sap_module_header <- function(title, subtitle, icon_name = "layer-group",
                              badge = "Survey Analytics") {
  div(
    class = "sap-module-hero mb-4",
    div(
      class = "sap-module-icon",
      icon(icon_name)
    ),
    div(
      class = "flex-grow-1",
      div(class = "d-flex flex-wrap align-items-center gap-2",
          h2(class = "mb-1", title),
          span(class = "badge rounded-pill text-bg-primary", badge)),
      p(class = "text-muted mb-0", subtitle)
    )
  )
}

sap_frontend_notice <- function(text = "This workspace records its specification in the active project checkpoint.") {
  div(
    class = "alert alert-light border d-flex gap-2 align-items-start sap-backend-notice",
    icon("circle-info", class = "mt-1 text-primary"),
    div(strong("Implementation note"), div(class = "small text-muted", text))
  )
}

sap_no_data <- function(message = "Load a survey dataset to activate this workspace.") {
  div(
    class = "sap-empty-state",
    div(class = "sap-empty-icon", icon("database")),
    h5("No active survey dataset"),
    p(class = "text-muted mb-0", message)
  )
}

sap_kpi <- function(label, value, icon_name = "circle-info", note = NULL) {
  div(
    class = "sap-kpi",
    div(class = "sap-kpi-icon", icon(icon_name)),
    div(
      div(class = "sap-kpi-value", value),
      div(class = "sap-kpi-label", label),
      if (!is.null(note)) div(class = "sap-kpi-note", note)
    )
  )
}

sap_status_pill <- function(text, status = c("neutral", "success", "warning", "danger")) {
  status <- match.arg(status)
  span(class = paste("sap-status-pill", paste0("sap-status-", status)), text)
}

sap_safe_names <- function(app_state) {
  df <- app_state$processed_data
  if (!is.data.frame(df)) character(0) else names(df)
}

sap_numeric_names <- function(app_state) {
  df <- app_state$processed_data
  if (!is.data.frame(df)) return(character(0))
  names(df)[vapply(df, is.numeric, logical(1))]
}

sap_character_names <- function(app_state) {
  df <- app_state$processed_data
  if (!is.data.frame(df)) return(character(0))
  names(df)[vapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
}

sap_fmt <- function(x, digits = 1L) {
  if (length(x) == 0L || is.null(x) || all(is.na(x))) return("—")
  format(round(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE)
}

sap_frontend_record <- function(app_state, module, action, details = NULL) {
  entry <- list(
    timestamp = Sys.time(),
    module = module,
    action = action,
    details = details
  )
  history <- app_state$frontend_activity %||% list()
  app_state$frontend_activity <- c(history, list(entry))
  app_state$last_action <- action
  invisible(entry)
}


# Return a valid empty Plotly widget instead of raising a Shiny validation error.
# This is compatible with older Plotly/Shiny combinations used on hosted services.
sap_empty_plotly <- function(message = "No data are available for this chart.") {
  fig <- plotly::plot_ly(
    x = numeric(0), y = numeric(0),
    type = "scatter", mode = "markers",
    hoverinfo = "skip", showlegend = FALSE
  )
  fig <- plotly::layout(
    fig,
    xaxis = list(visible = FALSE, fixedrange = TRUE),
    yaxis = list(visible = FALSE, fixedrange = TRUE),
    annotations = list(list(
      text = as.character(message)[1],
      x = 0.5, y = 0.5, xref = "paper", yref = "paper",
      showarrow = FALSE, align = "center"
    )),
    margin = list(l = 20, r = 20, t = 20, b = 20)
  )
  plotly::config(fig, displaylogo = FALSE, responsive = TRUE)
}


# Convert a ggplot to Plotly without letting one incompatible aesthetic crash
# the Shiny output. The original ggplot can still be retained for report export.
sap_ggplotly <- function(p, ..., empty_message = "This chart could not be rendered for the selected data.") {
  tryCatch(
    plotly::ggplotly(p, ...),
    error = function(e) {
      message("[SAP] Plotly conversion warning: ", conditionMessage(e))
      sap_empty_plotly(empty_message)
    }
  )
}

message("[SAP] Frontend components v1.2-20260801 loaded")
