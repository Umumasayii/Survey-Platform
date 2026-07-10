# =============================================================================
# MODULE 4: DASHBOARD
# =============================================================================
#
# Professional dashboard module featuring:
# 1. KPI Cards (Dataset size, missing %, quality score)
# 2. Data Overview (Variable breakdown, types, distributions)
# 3. Recent Analyses (Summary of last 5 analyses)
# 4. Quick Actions (Navigate to key modules)
# 5. Quality Metrics (Missing values, duplicates, outliers)
# 6. Session Summary (Current session info)
#
# Advanced R patterns:
# - Reactive event logging
# - Dynamic metric calculations
# - Professional card layouts
# - Real-time data updates
#
# =============================================================================

# =============================================================================
# UTILITY FUNCTIONS - DASHBOARD
# =============================================================================

#' Create KPI card
#'
#' @param title Card title
#' @param value Main value
#' @param subtitle Subtitle text
#' @param icon Icon name
#' @param color Color theme (primary, success, danger, warning, info)
#'
#' @return HTML/Bootstrap card
create_kpi_card <- function(title, value, subtitle = "", icon = "chart-bar", color = "primary") {
  div(
    class = paste0("card border-", color, " mb-3"),
    style = "border-left: 4px solid;",
    div(
      class = "card-body p-4",
      div(
        class = "d-flex align-items-center",
        div(
          class = paste0("flex-shrink-0 p-3 rounded-circle bg-", color, " text-white"),
          icon(icon, class = "fa-2x")
        ),
        div(
          class = "flex-grow-1 ms-3",
          h6(title, class = "card-title text-muted mb-1"),
          h3(value, class = paste0("card-text text-", color, " mb-0"))
        )
      ),
      if (subtitle != "") {
        p(subtitle, class = "small text-muted mt-2 mb-0")
      }
    )
  )
}

#' Calculate dataset quality metrics
#'
#' @param df Data frame
#'
#' @return List with quality metrics
calculate_dashboard_metrics <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      n_rows = 0,
      n_cols = 0,
      n_numeric = 0,
      n_categorical = 0,
      missing_pct = 0,
      duplicate_rows = 0,
      quality_score = 0
    ))
  }
  
  # Count variables by type
  numeric_cols <- sum(sapply(df, is.numeric))
  categorical_cols <- sum(sapply(df, function(x) is.factor(x) || is.character(x)))
  
  # Missing values
  total_cells <- nrow(df) * ncol(df)
  missing_cells <- sum(is.na(df))
  missing_pct <- round((missing_cells / total_cells) * 100, 1)
  
  # Duplicate rows
  duplicate_rows <- sum(duplicated(df))
  
  # Quality score (0-100)
  # Based on: completeness, uniqueness, validity
  completeness <- 100 - missing_pct
  uniqueness <- max(0, 100 - (duplicate_rows / nrow(df) * 100))
  quality_score <- round((completeness + uniqueness) / 2, 1)
  
  list(
    n_rows = nrow(df),
    n_cols = ncol(df),
    n_numeric = numeric_cols,
    n_categorical = categorical_cols,
    missing_pct = missing_pct,
    duplicate_rows = duplicate_rows,
    quality_score = quality_score,
    completeness = completeness,
    uniqueness = uniqueness
  )
}

#' Get variable type summary
#'
#' @param df Data frame
#'
#' @return Data frame with type counts
get_variable_summary <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(type = character(), count = integer()))
  }
  
  types <- sapply(df, function(x) {
    if (is.numeric(x)) return("Numeric")
    if (is.logical(x)) return("Logical")
    if (is.factor(x)) return("Factor")
    if (is.character(x)) return("Character")
    "Other"
  })
  
  as.data.frame(table(types)) %>%
    rename(type = types, count = Freq) %>%
    mutate(type = as.character(type)) %>%
    arrange(desc(count))
}

#' Create analysis session log entry
#'
#' @param module Module name
#' @param action Action performed
#'
#' @return List with log entry
create_log_entry <- function(module, action) {
  list(
    timestamp = Sys.time(),
    module = module,
    action = action
  )
}

#' Format large numbers with commas
#'
#' @param x Number
#'
#' @return Formatted string
format_number <- function(x) {
  format(x, big.mark = ",", trim = TRUE)
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("gauge"), "Dashboard"),
      p("Overview of your data and recent analyses", class = "text-muted")
    ),
    
    # No data message
    uiOutput(ns("no_data_message")),
    
    # Dashboard content
    uiOutput(ns("dashboard_content"))
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_dashboard_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    # Data access routes through services$data (backend single source of truth)
    # when wired, with an app_state fallback for standalone use.
    data_exists <- reactive({
      if (!is.null(services) && !is.null(services$data)) {
        return(isTRUE(services$data$is_loaded()) &&
               !is.null(services$data$get("processed")) &&
               nrow(services$data$get("processed")) > 0)
      }
      req(app_state$data_imported)
      !is.null(app_state$processed_data) && nrow(app_state$processed_data) > 0
    })
    
    df_data <- reactive({
      req(data_exists())
      if (!is.null(services) && !is.null(services$data)) {
        return(services$data$get("processed"))
      }
      app_state$processed_data
    })
    
    metrics <- reactive({
      req(data_exists())
      calculate_dashboard_metrics(df_data())
    })
    
    var_summary <- reactive({
      req(data_exists())
      get_variable_summary(df_data())
    })
    
    # =========================================================================
    # NO DATA MESSAGE
    # =========================================================================
    
    output$no_data_message <- renderUI({
      if (!data_exists()) {
        empty_state(
          icon = "gauge",
          title = "No data loaded",
          message = "Import data using Data > Import to see your dashboard"
        )
      }
    })
    
    # =========================================================================
    # DASHBOARD CONTENT
    # =========================================================================
    
    output$dashboard_content <- renderUI({
      if (!data_exists()) return(NULL)
      
      m <- metrics()
      
      tagList(
        # ===================================================================
        # ROW 1: KPI CARDS
        # ===================================================================
        div(
          class = "row mb-4",
          div(class = "col-md-3",
            create_kpi_card(
              title = "Observations",
              value = format_number(m$n_rows),
              subtitle = "Total rows in dataset",
              icon = "table",
              color = "primary"
            )
          ),
          div(class = "col-md-3",
            create_kpi_card(
              title = "Variables",
              value = m$n_cols,
              subtitle = sprintf("%d numeric, %d categorical", 
                               m$n_numeric, m$n_categorical),
              icon = "columns",
              color = "info"
            )
          ),
          div(class = "col-md-3",
            create_kpi_card(
              title = "Missing Data",
              value = paste0(m$missing_pct, "%"),
              subtitle = sprintf("%d missing values total", 
                               sum(is.na(df_data()))),
              icon = "exclamation-triangle",
              color = if (m$missing_pct > 5) "warning" else "success"
            )
          ),
          div(class = "col-md-3",
            create_kpi_card(
              title = "Data Quality",
              value = paste0(m$quality_score, "/100"),
              subtitle = "Overall quality score",
              icon = "check-circle",
              color = if (m$quality_score >= 80) "success" else if (m$quality_score >= 60) "warning" else "danger"
            )
          )
        ),
        
        # ===================================================================
        # ROW 2: QUALITY METRICS & VARIABLES
        # ===================================================================
        div(
          class = "row mb-4",
          
          # Quality Breakdown
          div(class = "col-md-6",
            div(
              class = "card",
              div(class = "card-header", h5("Quality Metrics")),
              div(class = "card-body",
                div(
                  class = "mb-3",
                  div(
                    class = "d-flex justify-content-between mb-1",
                    span("Completeness"),
                    strong(sprintf("%.1f%%", m$completeness))
                  ),
                  div(
                    class = "progress",
                    div(
                      class = "progress-bar bg-success",
                      style = sprintf("width: %.0f%%", m$completeness),
                      role = "progressbar"
                    )
                  )
                ),
                div(
                  class = "mb-3",
                  div(
                    class = "d-flex justify-content-between mb-1",
                    span("Uniqueness"),
                    strong(sprintf("%.1f%%", m$uniqueness))
                  ),
                  div(
                    class = "progress",
                    div(
                      class = "progress-bar bg-info",
                      style = sprintf("width: %.0f%%", m$uniqueness),
                      role = "progressbar"
                    )
                  )
                ),
                div(
                  class = "alert alert-sm alert-info mt-3 p-2",
                  h6("Issues Found:", class = "mb-2"),
                  tags$ul(
                    if (m$missing_pct > 0) tags$li(sprintf("%.1f%% missing values", m$missing_pct)),
                    if (m$duplicate_rows > 0) tags$li(sprintf("%d duplicate rows", m$duplicate_rows)),
                    if (m$missing_pct == 0 && m$duplicate_rows == 0) tags$li("No issues detected")
                  )
                )
              )
            )
          ),
          
          # Variable Types
          div(class = "col-md-6",
            div(
              class = "card",
              div(class = "card-header", h5("Variable Types")),
              div(class = "card-body",
                div(
                  class = "row text-center",
                  div(class = "col-6 mb-3",
                    div(class = "display-4 text-primary mb-1", 
                        m$n_numeric),
                    p("Numeric Variables", class = "small text-muted")
                  ),
                  div(class = "col-6 mb-3",
                    div(class = "display-4 text-info mb-1", 
                        m$n_categorical),
                    p("Categorical Variables", class = "small text-muted")
                  )
                ),
                div(class = "text-muted small",
                  p("Use Data > Explorer to analyze individual variables", 
                    class = "mb-0")
                )
              )
            )
          )
        ),
        
        # ===================================================================
        # ROW 3: QUICK ACTIONS
        # ===================================================================
        div(
          class = "row mb-4",
          div(class = "col-12",
            div(
              class = "card",
              div(class = "card-header", h5("Quick Actions")),
              div(class = "card-body",
                div(
                  class = "row",
                  div(class = "col-md-4 mb-3 mb-md-0",
                    div(
                      class = "text-center p-3 border rounded cursor-pointer hover-shadow",
                      icon("broom", class = "fa-2x text-warning mb-2"),
                      h6("Clean Data"),
                      p("Remove duplicates & impute", class = "small text-muted")
                    )
                  ),
                  div(class = "col-md-4 mb-3 mb-md-0",
                    div(
                      class = "text-center p-3 border rounded cursor-pointer hover-shadow",
                      icon("chart-line", class = "fa-2x text-primary mb-2"),
                      h6("Describe"),
                      p("View descriptive statistics", class = "small text-muted")
                    )
                  ),
                  div(class = "col-md-4 mb-3 mb-md-0",
                    div(
                      class = "text-center p-3 border rounded cursor-pointer hover-shadow",
                      icon("calculator", class = "fa-2x text-success mb-2"),
                      h6("Analyze"),
                      p("Run statistical tests", class = "small text-muted")
                    )
                  )
                )
              )
            )
          )
        ),
        
        # ===================================================================
        # ROW 4: SESSION INFO
        # ===================================================================
        div(
          class = "row",
          div(class = "col-12",
            div(
              class = "card bg-light",
              div(class = "card-header", h5("Session Summary")),
              div(class = "card-body",
                div(
                  class = "row",
                  div(class = "col-md-4",
                    p(class = "small",
                      strong("Dataset:"), br(),
                      sprintf("%d rows × %d columns", m$n_rows, m$n_cols), br(),
                      class = "text-muted"
                    )
                  ),
                  div(class = "col-md-4",
                    p(class = "small",
                      strong("Last Updated:"), br(),
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), br(),
                      class = "text-muted"
                    )
                  ),
                  div(class = "col-md-4",
                    p(class = "small",
                      strong("Memory Usage:"), br(),
                      sprintf("~%.2f MB", 
                             object.size(df_data()) / 1024^2), br(),
                      class = "text-muted"
                    )
                  )
                )
              )
            )
          )
        )
      )
    })
    
  })
}

# =============================================================================
# END OF MODULE 4: DASHBOARD
# =============================================================================

message("[SAP] Dashboard v2.2-20260706 loaded")  # SAP_MODULE_BANNER
