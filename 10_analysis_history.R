# =============================================================================
# MODULE 10: ANALYSIS HISTORY
# =============================================================================
# Complete action tracking:
# - Record every important operation
# - Undo/redo support
# - Reproducible analysis workflow
# - Action timestamps and metadata
# - Generated R code for each action
# 
# Architecture: Immutable history stack with action serialization
# =============================================================================

# =============================================================================
# ACTION CLASSES & UTILITIES
# =============================================================================

#' Create history entry
#' @param action_type Type of action (import, clean, analyze, etc.)
#' @param module Module name
#' @param description User-friendly description
#' @param timestamp Action timestamp
#' @param r_code R code that performed the action
#' @param data_before State before action
#' @param data_after State after action
#' @param reversible Can action be undone?
#' @return List with action structure
create_action <- function(action_type, module, description, 
                         r_code = "", data_before = NULL, data_after = NULL,
                         reversible = TRUE) {
  list(
    id = paste0("action_", format(Sys.time(), "%Y%m%d_%H%M%S_"), 
                sample(10000, 1)),
    type = action_type,
    module = module,
    description = description,
    timestamp = Sys.time(),
    r_code = r_code,
    data_before = data_before,
    data_after = data_after,
    reversible = reversible,
    metadata = list()
  )
}

#' History manager class
#' @return R6 object managing action history
# State fields intentionally included in reversible snapshots. UI-only values,
# service objects, credentials, and connection handles are excluded.
SAP_HISTORY_STATE_FIELDS <- c(
  "raw_data", "processed_data", "data_imported", "variable_types",
  "variable_dictionary", "settings", "import_log", "current_project",
  "current_project_name", "project_path", "project_saved", "dataset_name",
  "quality_score", "transformation_count", "last_action", "trained_models",
  "last_model", "model_predictions", "analyses", "last_analysis",
  "survey_design", "questionnaire", "routing_rules", "scale_definitions",
  "nonresponse_plan", "table_builder_spec", "advanced_model_queue",
  "text_codebook", "geospatial_spec", "longitudinal_spec",
  "privacy_register", "governance_policy", "report_frontend_blocks",
  "frontend_activity", "active_filter_label", "health_log"
)

sap_history_clone <- function(x) {
  # Deep-copy ordinary workspaces; avoid an expensive serialization pass for
  # very large datasets and rely on R's copy-on-modify semantics instead.
  size_bytes <- suppressWarnings(as.numeric(utils::object.size(x)))
  if (is.finite(size_bytes) && size_bytes > 100 * 1024^2) return(x)
  tryCatch(unserialize(serialize(x, NULL)), error = function(e) x)
}

sap_capture_app_state <- function(app_state, fields = SAP_HISTORY_STATE_FIELDS,
                                  isolate_read = FALSE) {
  read_values <- function() shiny::reactiveValuesToList(app_state, all.names = TRUE)
  values <- if (isTRUE(isolate_read)) shiny::isolate(read_values()) else read_values()
  fields <- intersect(fields, names(values))
  sap_history_clone(values[fields])
}

sap_restore_app_state <- function(app_state, snapshot) {
  if (is.null(snapshot) || !is.list(snapshot)) return(invisible(FALSE))
  for (name in intersect(names(snapshot), SAP_HISTORY_STATE_FIELDS)) {
    app_state[[name]] <- sap_history_clone(snapshot[[name]])
  }
  invisible(TRUE)
}

sap_history_module_from_action <- function(description) {
  text <- tolower(as.character(description %||% "workspace")[1])
  if (grepl("import|dataset|data lab", text)) return("Data")
  if (grepl("clean|transform|recode", text)) return("Data Cleaning")
  if (grepl("health", text)) return("Personal Health")
  if (grepl("survey design|weight", text)) return("Survey Design")
  if (grepl("question|routing", text)) return("Questionnaire")
  if (grepl("scale|composite", text)) return("Scale Analysis")
  if (grepl("privacy|governance", text)) return("Privacy")
  if (grepl("model", text)) return("Models")
  if (grepl("report", text)) return("Report")
  if (grepl("setting|ai", text)) return("Settings")
  "Workspace"
}

HistoryManager <- R6::R6Class(
  "HistoryManager",
  public = list(
    actions = list(),
    current_index = 0L,
    current_snapshot = NULL,
    version = NULL,
    max_actions = 20L,

    initialize = function(max_actions = 20L) {
      self$actions <- list()
      self$current_index <- 0L
      self$current_snapshot <- NULL
      self$max_actions <- max(1L, as.integer(max_actions)[1])
      self$version <- shiny::reactiveVal(0L)
    },

    touch = function() {
      self$version(shiny::isolate(self$version()) + 1L)
      invisible(self)
    },

    seed_state = function(state) {
      self$current_snapshot <- sap_history_clone(state)
      invisible(self)
    },

    add_action = function(action) {
      if (self$current_index < length(self$actions)) {
        self$actions <- if (self$current_index == 0L) list() else
          self$actions[seq_len(self$current_index)]
      }
      self$actions[[length(self$actions) + 1L]] <- action
      if (length(self$actions) > self$max_actions) {
        keep <- seq.int(length(self$actions) - self$max_actions + 1L,
                        length(self$actions))
        self$actions <- self$actions[keep]
      }
      self$current_index <- length(self$actions)
      self$current_snapshot <- sap_history_clone(action$data_after)
      self$touch()
      invisible(self)
    },

    record_state = function(action_type, module, description, state, r_code = "") {
      state <- sap_history_clone(state)
      if (is.null(self$current_snapshot)) {
        self$seed_state(state)
        return(invisible(FALSE))
      }
      if (identical(state, self$current_snapshot)) return(invisible(FALSE))
      action <- create_action(
        action_type = action_type, module = module, description = description,
        r_code = r_code, data_before = self$current_snapshot,
        data_after = state, reversible = TRUE
      )
      self$add_action(action)
      invisible(TRUE)
    },

    get_current = function() {
      if (self$current_index == 0L) return(NULL)
      self$actions[[self$current_index]]
    },

    can_undo = function() self$current_index > 0L,
    can_redo = function() self$current_index < length(self$actions),

    undo = function() {
      if (!self$can_undo()) return(NULL)
      action <- self$actions[[self$current_index]]
      self$current_index <- self$current_index - 1L
      self$current_snapshot <- sap_history_clone(action$data_before)
      self$touch()
      action
    },

    redo = function() {
      if (!self$can_redo()) return(NULL)
      next_index <- self$current_index + 1L
      action <- self$actions[[next_index]]
      self$current_index <- next_index
      self$current_snapshot <- sap_history_clone(action$data_after)
      self$touch()
      action
    },

    get_summary = function() {
      if (length(self$actions) == 0L) return(data.frame())
      data.frame(
        id = vapply(self$actions, function(a) a$id, character(1)),
        timestamp = as.POSIXct(vapply(self$actions, function(a) as.numeric(a$timestamp), numeric(1)),
                               origin = "1970-01-01", tz = "UTC"),
        type = vapply(self$actions, function(a) a$type, character(1)),
        module = vapply(self$actions, function(a) a$module, character(1)),
        description = vapply(self$actions, function(a) a$description, character(1)),
        reversible = vapply(self$actions, function(a) isTRUE(a$reversible), logical(1)),
        applied = seq_along(self$actions) <= self$current_index,
        stringsAsFactors = FALSE
      )
    },

    get_r_code = function() {
      if (length(self$actions) == 0L) return("")
      code_lines <- c(
        "# Auto-generated R code from Survey Analytics Platform",
        "# Only actions that supplied reproducible R code are included.",
        "", "library(dplyr)", "library(tidyr)", "library(ggplot2)", ""
      )
      applied <- if (self$current_index == 0L) list() else
        self$actions[seq_len(self$current_index)]
      for (action in applied) {
        if (nzchar(action$r_code %||% "")) {
          code_lines <- c(code_lines, sprintf("# %s", action$description),
                          action$r_code, "")
        }
      }
      paste(code_lines, collapse = "\n")
    },

    export_markdown = function() {
      summary <- self$get_summary()
      if (nrow(summary) == 0L) return("# Analysis History\n\nNo actions recorded yet.")
      lines <- c(
        "# Analysis History", "",
        sprintf("Total actions: %d", length(self$actions)),
        sprintf("Currently applied: %d", self$current_index),
        "", "## Action Timeline", ""
      )
      for (i in seq_len(nrow(summary))) {
        row <- summary[i, ]
        lines <- c(lines, sprintf("### %d. %s", i, row$description),
          sprintf("- **Time**: %s", format(row$timestamp, "%Y-%m-%d %H:%M:%S")),
          sprintf("- **Module**: %s", row$module),
          sprintf("- **Type**: %s", row$type),
          sprintf("- **Applied**: %s", if (row$applied) "Yes" else "No (redo available)"),
          "")
      }
      paste(lines, collapse = "\n")
    }
  )
)

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_analysis_history_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header with undo/redo buttons
    div(
      class = "d-flex justify-content-between align-items-center mb-4",
      h2(icon("history"), "Analysis History"),
      div(
        class = "btn-group",
        actionButton(ns("btn_undo"), "Undo", 
                    class = "btn-sm btn-outline-secondary",
                    icon = icon("arrow-left")),
        actionButton(ns("btn_redo"), "Redo",
                    class = "btn-sm btn-outline-secondary",
                    icon = icon("arrow-right"))
      )
    ),
    
    # Tabs
    navset_tab(
      # Timeline Tab
      nav_panel(
        "Timeline",
        div(
          class = "card",
          div(class = "card-body",
            div(class = "table-responsive",
              tableOutput(ns("history_timeline"))
            ),
            br(),
            div(class = "btn-group w-100",
              downloadButton(ns("export_markdown"), "Export as Markdown",
                           class = "btn-sm")
            )
          )
        )
      ),
      
      # Generated Code Tab
      nav_panel(
        "R Code",
        div(
          class = "card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h5("Reproducible R Code"),
              actionButton(ns("btn_copy_code"), "Copy to Clipboard",
                         class = "btn-sm btn-outline-primary")
            )
          ),
          div(class = "card-body",
            div(
              style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px; 
                       max-height: 500px; overflow-y: auto; font-family: monospace;",
              verbatimTextOutput(ns("generated_code"))
            )
          )
        )
      ),
      
      # Statistics Tab
      nav_panel(
        "Statistics",
        div(
          class = "row",
          div(class = "col-md-4",
            div(class = "card",
              div(class = "card-body text-center",
                div(class = "display-4 text-primary mb-2", 
                    textOutput(ns("total_actions"))),
                p("Total Actions", class = "text-muted mb-0")
              )
            )
          ),
          div(class = "col-md-4",
            div(class = "card",
              div(class = "card-body text-center",
                div(class = "display-4 text-success mb-2",
                    textOutput(ns("reversible_actions"))),
                p("Available Undo Steps", class = "text-muted mb-0")
              )
            )
          ),
          div(class = "col-md-4",
            div(class = "card",
              div(class = "card-body text-center",
                div(class = "display-4 text-info mb-2",
                    textOutput(ns("analysis_time"))),
                p("Analysis Duration", class = "text-muted mb-0")
              )
            )
          )
        ),
        br(),
        div(
          class = "card",
          div(class = "card-header", h5("Actions by Module")),
          div(class = "card-body",
            plotOutput(ns("actions_by_module"))
          )
        )
      )
    )
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_analysis_history_server <- function(id, app_state, history_manager = NULL,
                                        services = NULL) {
  moduleServer(id, function(input, output, session) {
    if (is.null(history_manager)) {
      history_manager <- HistoryManager$new()
      history_manager$seed_state(sap_capture_app_state(app_state, isolate_read = TRUE))
    }

    observeEvent(input$btn_undo, {
      action <- history_manager$undo()
      if (is.null(action)) {
        showNotification("Nothing is available to undo.", type = "warning")
        return(invisible(NULL))
      }
      sap_restore_app_state(app_state, action$data_before)
      showNotification(sprintf("Undid: %s", action$description), type = "message")
    })

    observeEvent(input$btn_redo, {
      action <- history_manager$redo()
      if (is.null(action)) {
        showNotification("Nothing is available to redo.", type = "warning")
        return(invisible(NULL))
      }
      sap_restore_app_state(app_state, action$data_after)
      showNotification(sprintf("Redid: %s", action$description), type = "message")
    })

    output$history_timeline <- renderTable({
      history_manager$version()
      summary <- history_manager$get_summary()
      if (nrow(summary) == 0L) return(data.frame(Message = "No reversible actions recorded yet"))
      summary$timestamp <- format(summary$timestamp, "%Y-%m-%d %H:%M:%S")
      summary$reversible <- ifelse(summary$reversible, "Yes", "No")
      summary$applied <- ifelse(summary$applied, "Applied", "Redo available")
      summary[, c("timestamp", "module", "description", "applied", "reversible")]
    })

    output$generated_code <- renderText({
      history_manager$version()
      history_manager$get_r_code()
    })

    observeEvent(input$btn_copy_code, {
      code <- history_manager$get_r_code()
      shinyjs::runjs(sprintf(
        "var code = %s; navigator.clipboard.writeText(code).then(() => { alert('R code copied to clipboard!'); });",
        jsonlite::toJSON(code, auto_unbox = TRUE)
      ))
    })

    output$export_markdown <- downloadHandler(
      filename = function() paste0("analysis_history_", format(Sys.Date(), "%Y%m%d"), ".md"),
      content = function(file) {
        if (!is.null(services) && !is.null(services$privacy)) {
          services$privacy$assert_content_export_allowed(
            history_manager$export_markdown(), "analysis history export"
          )
        }
        writeLines(history_manager$export_markdown(), file)
      }
    )

    output$total_actions <- renderText({
      history_manager$version()
      length(history_manager$actions)
    })

    output$reversible_actions <- renderText({
      history_manager$version()
      as.character(history_manager$current_index)
    })

    output$analysis_time <- renderText({
      history_manager$version()
      acts <- history_manager$actions
      if (length(acts) < 2L) return("N/A")
      times <- vapply(acts, function(a) as.numeric(a$timestamp), numeric(1))
      sprintf("%.1f hours", (max(times) - min(times)) / 3600)
    })

    output$actions_by_module <- renderPlot({
      history_manager$version()
      summary <- history_manager$get_summary()
      if (!nrow(summary)) return(NULL)
      module_counts <- table(summary$module)
      barplot(module_counts, main = "Actions by Module", xlab = "Module",
              ylab = "Count", col = "#0d6efd")
    })

    history_manager
  })
}

# =============================================================================
# END OF MODULE 10
# =============================================================================

message("[SAP] Analysis History v2.3-20260801 loaded")  # SAP_MODULE_BANNER
