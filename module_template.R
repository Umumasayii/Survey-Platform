# =============================================================================
# MODULE TEMPLATE
# =============================================================================
# This file serves as a template for creating new Shiny modules.
# 
# Module structure:
# 1. UI function: mod_[name]_ui() - returns UI elements
# 2. Server function: mod_[name]_server() - contains reactive logic
#
# NAMING CONVENTION:
# - File name: [module_name].R
# - UI function: mod_[module_name]_ui(id, ...)
# - Server function: mod_[module_name]_server(id, app_state, ...)
# - Usage in app.R:
#   - mod_[module_name]_ui("unique_id")
#   - mod_[module_name]_server("unique_id", app_state)
# =============================================================================

# =============================================================================
# EXAMPLE MODULE: Data Display
# =============================================================================
# This example module displays a reactive table with data

#' Data Display Module UI
#'
#' Creates the UI for displaying data in a table with options for filtering,
#' sorting, and pagination.
#'
#' @param id Module namespace id
#' @param title Optional title for the section
#' @param show_options Logical. Show filtering/sorting options?
#'
#' @return UI elements for the module
#'
#' @examples
#' # In app.R UI section:
#' mod_data_display_ui("my_data_table", title = "Survey Responses")
#'
#' # In app.R server section:
#' mod_data_display_server("my_data_table", app_state)
#'
mod_data_display_ui <- function(id, title = NULL, show_options = TRUE) {
  ns <- NS(id)
  
  tagList(
    # Title section
    if (!is.null(title)) {
      div(
        class = "mb-4",
        h3(title)
      )
    },
    
    # Options section
    if (show_options) {
      div(
        class = "card mb-3",
        div(
          class = "card-body",
          div(
            class = "row",
            div(
              class = "col-md-6",
              numericInput(
                ns("rows_to_show"),
                "Rows per page:",
                value = 10,
                min = 5,
                max = 100
              )
            ),
            div(
              class = "col-md-6",
              textInput(
                ns("search_term"),
                "Search:",
                placeholder = "Search all columns..."
              )
            )
          )
        )
      )
    },
    
    # Data table section
    div(
      class = "card",
      div(
        class = "card-body",
        # Loading spinner
        withSpinner(
          DTOutput(ns("data_table")),
          type = 8,
          color = "#0d6efd"
        )
      )
    ),
    
    # Summary section
    div(
      class = "mt-3 small text-muted",
      textOutput(ns("table_summary"))
    )
  )
}

#' Data Display Module Server
#'
#' Handles the reactive logic for displaying and filtering data
#'
#' @param id Module namespace id
#' @param app_state Reactive list containing application state
#' @param data Optional reactive data frame. If NULL, uses app_state$processed_data
#'
#' @return None (modifies reactives in place)
#'
mod_data_display_server <- function(id, app_state, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Determine which data to use
    data_to_display <- reactive({
      if (!is.null(data)) {
        return(data())
      } else if (!is.null(app_state$processed_data)) {
        return(app_state$processed_data)
      } else {
        return(NULL)
      }
    })
    
    # Filter data based on search term
    filtered_data <- reactive({
      df <- data_to_display()
      
      if (is.null(df)) return(NULL)
      
      search <- input$search_term
      
      if (search == "" || is.null(search)) {
        return(df)
      }
      
      # Apply search across all columns
      mask <- apply(df, 1, function(row) {
        any(grepl(search, as.character(row), ignore.case = TRUE))
      })
      
      df[mask, ]
    })
    
    # Render data table
    output$data_table <- renderDT({
      df <- filtered_data()
      
      if (is.null(df) || nrow(df) == 0) {
        return(datatable(
          data.frame(),
          options = list(dom = 't'),
          class = "stripe hover"
        ))
      }
      
      datatable(
        df,
        options = list(
          pageLength = input$rows_to_show,
          lengthChange = FALSE,
          searching = FALSE,
          ordering = TRUE,
          dom = "ltp",
          columnDefs = list(
            list(className = "dt-right", targets = "_all")
          )
        ),
        class = "stripe hover",
        rownames = FALSE
      )
    })
    
    # Render summary text
    output$table_summary <- renderText({
      df <- filtered_data()
      
      if (is.null(df)) {
        return("No data to display")
      }
      
      rows_shown <- nrow(df)
      total_cols <- ncol(df)
      
      sprintf(
        "Showing %d rows × %d columns",
        rows_shown,
        total_cols
      )
    })
    
    # Return reactive expressions if needed by other modules
    return(reactive(filtered_data()))
  })
}

# =============================================================================
# BEST PRACTICES FOR MODULE DEVELOPMENT
# =============================================================================

# 1. NAMESPACE MANAGEMENT
#    - Always use NS(id) to create namespace
#    - Prefix all IDs with ns("id_name")
#    - This prevents ID conflicts between modules

# 2. REACTIVE PROGRAMMING
#    - Use reactive({}) for computed values
#    - Use observeEvent({}) for side effects (downloads, notifications, etc.)
#    - Use eventReactive({}) for computations triggered by buttons
#    - Avoid observe() - prefer reactive() or observeEvent()

# 3. ERROR HANDLING
#    - Always validate inputs before processing
#    - Use req() to ensure data exists before rendering
#    - Use tryCatch() for risky operations
#    - Display user-friendly error messages, never raw R errors

# 4. PERFORMANCE
#    - Use reactive() and req() to avoid unnecessary computations
#    - For heavy computations, consider using future or async
#    - Cache results when possible

# 5. TESTING
#    - Each module should be testable independently
#    - Consider using testServer() for unit testing

# 6. DOCUMENTATION
#    - Document all function parameters with @param
#    - Provide @examples for module usage
#    - Include comments for complex logic

# =============================================================================
# COMMON MODULE PATTERNS
# =============================================================================

# Pattern 1: Conditional Content Display
conditional_display <- function(condition, content, empty_message = "No data") {
  if (condition) {
    content
  } else {
    div(
      class = "alert alert-info",
      icon("circle-info"),
      empty_message
    )
  }
}

# Pattern 2: Input Validation
validate_numeric_input <- function(value, min_val = NULL, max_val = NULL) {
  if (!is.numeric(value)) {
    return(list(valid = FALSE, message = "Input must be numeric"))
  }
  
  if (!is.null(min_val) && value < min_val) {
    return(list(valid = FALSE, message = sprintf("Value must be >= %d", min_val)))
  }
  
  if (!is.null(max_val) && value > max_val) {
    return(list(valid = FALSE, message = sprintf("Value must be <= %d", max_val)))
  }
  
  return(list(valid = TRUE, message = ""))
}

# Pattern 3: Downloading Files
create_download_handler <- function(filename, content_func, extension = "csv") {
  downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", filename), "_", 
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".", extension)
    },
    content = content_func
  )
}

# Pattern 4: Progress Indicators
show_progress <- function(message = "Processing...") {
  withProgress(
    message = message,
    value = 0,
    {
      # Long-running computation here
      for (i in 1:10) {
        Sys.sleep(0.1)
        incProgress(0.1)
      }
    }
  )
}

# =============================================================================
# TEMPLATE STRUCTURE FOR NEW MODULES
# =============================================================================

#' Template for new module
#'
#' INSTRUCTIONS:
#' 1. Copy this section
#' 2. Replace [MODULE_NAME] with your module name (e.g., data_cleaning)
#' 3. Replace [MODULE_DESCRIPTION] with description
#' 4. Implement UI and server functions
#' 5. Save as modules/[module_name].R
#' 6. Source in app.R: source("modules/[module_name].R")
#' 7. Add UI call: mod_[module_name]_ui("[module_name]")
#' 8. Add server call: mod_[module_name]_server("[module_name]", app_state)

# mod_[MODULE_NAME]_ui <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     # Your UI components here
#   )
# }
#
# mod_[MODULE_NAME]_server <- function(id, app_state) {
#   moduleServer(id, function(input, output, session) {
#     
#     ns <- session$ns
#     
#     # Validate data availability
#     req(app_state$data_imported)
#     
#     # Your server logic here
#     
#   })
# }

# =============================================================================
print("✓ Module template loaded")
# =============================================================================
