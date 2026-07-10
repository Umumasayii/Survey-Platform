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
HistoryManager <- R6::R6Class(
  "HistoryManager",
  public = list(
    actions = list(),
    current_index = 0,
    
    initialize = function() {
      self$actions <- list()
      self$current_index <- 0
    },
    
    #' Add action to history
    add_action = function(action) {
      # Remove redo stack if new action is taken
      if (self$current_index < length(self$actions)) {
        self$actions <- self$actions[1:self$current_index]
      }
      
      self$actions[[length(self$actions) + 1]] <- action
      self$current_index <- length(self$actions)
      invisible(self)
    },
    
    #' Get current state
    get_current = function() {
      if (self$current_index == 0) return(NULL)
      self$actions[[self$current_index]]
    },
    
    #' Can undo?
    can_undo = function() {
      self$current_index > 1
    },
    
    #' Can redo?
    can_redo = function() {
      self$current_index < length(self$actions)
    },
    
    #' Undo last action
    undo = function() {
      if (self$can_undo()) {
        self$current_index <- self$current_index - 1
        return(self$get_current())
      }
      NULL
    },
    
    #' Redo last action
    redo = function() {
      if (self$can_redo()) {
        self$current_index <- self$current_index + 1
        return(self$get_current())
      }
      NULL
    },
    
    #' Get history summary
    get_summary = function() {
      if (length(self$actions) == 0) {
        return(data.frame())
      }
      
      data.frame(
        id = sapply(self$actions, function(a) a$id),
        timestamp = sapply(self$actions, function(a) a$timestamp),
        type = sapply(self$actions, function(a) a$type),
        module = sapply(self$actions, function(a) a$module),
        description = sapply(self$actions, function(a) a$description),
        reversible = sapply(self$actions, function(a) a$reversible),
        stringsAsFactors = FALSE
      )
    },
    
    #' Get R code for all actions
    get_r_code = function() {
      if (length(self$actions) == 0) {
        return("")
      }
      
      code_lines <- c(
        "# Auto-generated R code from Survey Analytics Platform",
        "# This code reproduces all analyses performed",
        "",
        "library(dplyr)",
        "library(tidyr)",
        "library(ggplot2)",
        ""
      )
      
      for (action in self$actions) {
        if (action$r_code != "") {
          code_lines <- c(
            code_lines,
            sprintf("# %s", action$description),
            action$r_code,
            ""
          )
        }
      }
      
      paste(code_lines, collapse = "\n")
    },
    
    #' Export history as markdown
    export_markdown = function() {
      summary <- self$get_summary()
      
      if (nrow(summary) == 0) {
        return("# Analysis History\n\nNo actions recorded yet.")
      }
      
      lines <- c(
        "# Analysis History",
        "",
        sprintf("Total actions: %d", length(self$actions)),
        sprintf("Time span: %s to %s", 
               format(min(summary$timestamp), "%Y-%m-%d %H:%M:%S"),
               format(max(summary$timestamp), "%Y-%m-%d %H:%M:%S")),
        "",
        "## Action Timeline",
        ""
      )
      
      for (i in 1:nrow(summary)) {
        s <- summary[i, ]
        lines <- c(
          lines,
          sprintf("### %d. %s", i, s$description),
          sprintf("- **Time**: %s", format(s$timestamp, "%Y-%m-%d %H:%M:%S")),
          sprintf("- **Module**: %s", s$module),
          sprintf("- **Type**: %s", s$type),
          sprintf("- **Reversible**: %s", ifelse(s$reversible, "Yes", "No")),
          ""
        )
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
                p("Can Undo", class = "text-muted mb-0")
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

mod_analysis_history_server <- function(id, app_state, history_manager = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Initialize history manager if not provided
    if (is.null(history_manager)) {
      history_manager <- HistoryManager$new()
    }
    
    # Undo button
    observeEvent(input$btn_undo, {
      if (history_manager$can_undo()) {
        action <- history_manager$undo()
        showNotification(
          sprintf("Undo: %s", action$description),
          type = "message"
        )
        # Would restore app_state from action$data_before here
      }
    })
    
    # Redo button
    observeEvent(input$btn_redo, {
      if (history_manager$can_redo()) {
        action <- history_manager$redo()
        showNotification(
          sprintf("Redo: %s", action$description),
          type = "message"
        )
      }
    })
    
    # Timeline output
    output$history_timeline <- renderTable({
      summary <- history_manager$get_summary()
      if (nrow(summary) == 0) {
        return(data.frame("Message" = "No actions recorded yet"))
      }
      
      summary$timestamp <- format(summary$timestamp, "%Y-%m-%d %H:%M:%S")
      summary$reversible <- ifelse(summary$reversible, "Yes", "No")
      summary[, c("timestamp", "module", "description", "reversible")]
    })
    
    # Generated R code
    output$generated_code <- renderText({
      history_manager$get_r_code()
    })
    
    # Copy code to clipboard
    observeEvent(input$btn_copy_code, {
      code <- history_manager$get_r_code()
      
      # Use JavaScript to copy (requires shinyjs)
      shinyjs::runjs(sprintf(
        "
        var code = %s;
        navigator.clipboard.writeText(code).then(() => {
          alert('R code copied to clipboard!');
        });
        ",
        jsonlite::toJSON(code)
      ))
    })
    
    # Export markdown
    output$export_markdown <- downloadHandler(
      filename = function() {
        paste0("analysis_history_", format(Sys.Date(), "%Y%m%d"), ".md")
      },
      content = function(file) {
        markdown <- history_manager$export_markdown()
        writeLines(markdown, file)
      }
    )
    
    # Statistics
    output$total_actions <- renderText({
      length(history_manager$actions)
    })

    output$reversible_actions <- renderText({
      acts <- history_manager$actions
      if (length(acts) == 0) return("0")
      # vapply guarantees a logical vector, so sum() never receives a list
      as.character(sum(vapply(acts, function(a) isTRUE(a$reversible), logical(1))))
    })

    output$analysis_time <- renderText({
      acts <- history_manager$actions
      if (length(acts) < 2) {
        "N/A"
      } else {
        # timestamps are POSIXct; unlist drops class, so compute on numeric seconds
        times <- vapply(acts, function(a) as.numeric(a$timestamp), numeric(1))
        duration_hours <- (max(times) - min(times)) / 3600
        sprintf("%.1f hours", duration_hours)
      }
    })
    
    output$actions_by_module <- renderPlot({
      summary <- history_manager$get_summary()
      if (nrow(summary) == 0) return(NULL)
      
      module_counts <- table(summary$module)
      barplot(module_counts, main = "Actions by Module",
             xlab = "Module", ylab = "Count", col = "#0d6efd")
    })
    
    # Return history manager for external use
    history_manager
  })
}

# =============================================================================
# END OF MODULE 10
# =============================================================================

message("[SAP] Analysis History v2.2-20260706 loaded")  # SAP_MODULE_BANNER
