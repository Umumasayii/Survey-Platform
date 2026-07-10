# =============================================================================
# MODULE 9: PROJECT MANAGER
# =============================================================================
# Professional project workspace:
# - Create/open/save projects
# - Restore all state (data, analyses, charts, models)
# - Project metadata & history
# - Recent projects
# 
# Architecture: Projects stored as RDS with serialized app_state
# =============================================================================

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Project structure
#' @return List with project schema
get_project_schema <- function() {
  list(
    version = "1.0",
    created = Sys.time(),
    modified = Sys.time(),
    name = "",
    description = "",
    raw_data = NULL,
    processed_data = NULL,
    variable_types = NULL,
    data_dictionary = list(),
    derived_variables = list(),
    analyses = list(),
    visualizations = list(),
    models = list(),
    predictions = list(),
    reports = list(),
    settings = list(),
    history = data.frame(),
    dashboard_layout = list()
  )
}

#' Create new project
#' @param name Project name
#' @param description Project description
#' @return Project object
create_project <- function(name, description = "") {
  project <- get_project_schema()
  project$name <- name
  project$description <- description
  project$created <- Sys.time()
  project$modified <- Sys.time()
  project
}

#' Save project to disk
#' @param project Project object
#' @param path File path
#' @return Invisibly TRUE
save_project <- function(project, path) {
  project$modified <- Sys.time()
  
  tryCatch({
    saveRDS(project, file = path, compress = "xz")
    return(invisible(TRUE))
  }, error = function(e) {
    warning("Failed to save project: ", e$message)
    return(invisible(FALSE))
  })
}

#' Load project from disk
#' @param path File path
#' @return Project object or NULL
load_project <- function(path) {
  tryCatch({
    project <- readRDS(path)
    
    # Validate schema
    schema <- get_project_schema()
    missing_fields <- setdiff(names(schema), names(project))
    
    if (length(missing_fields) > 0) {
      for (field in missing_fields) {
        project[[field]] <- schema[[field]]
      }
    }
    
    return(project)
  }, error = function(e) {
    warning("Failed to load project: ", e$message)
    return(NULL)
  })
}

#' Get list of recent projects
#' @param project_dir Directory containing projects
#' @param n Number of recent projects
#' @return Data frame with project metadata
get_recent_projects <- function(project_dir = "projects", n = 10) {
  if (!dir.exists(project_dir)) {
    return(data.frame())
  }
  
  files <- list.files(project_dir, pattern = "\\.sap$", full.names = TRUE)
  
  if (length(files) == 0) {
    return(data.frame())
  }
  
  # Get file info
  file_info <- file.info(files)
  file_info$path <- rownames(file_info)
  
  # Sort by modification time
  file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]
  file_info <- file_info[1:min(n, nrow(file_info)), ]
  
  # Try to load project names
  projects <- data.frame(
    name = basename(file_info$path),
    path = file_info$path,
    modified = file_info$mtime,
    size_mb = round(file_info$size / 1024^2, 2),
    row.names = NULL
  )
  
  # Remove .sap extension from name
  projects$name <- sub("\\.sap$", "", projects$name)
  
  projects
}

#' Add entry to project history
#' @param history Current history data frame
#' @param action Action description
#' @param module Module name
#' @param details Additional details (optional)
#' @return Updated history data frame
add_history_entry <- function(history, action, module, details = "") {
  entry <- data.frame(
    timestamp = Sys.time(),
    module = module,
    action = action,
    details = details,
    stringsAsFactors = FALSE
  )
  
  if (nrow(history) == 0) {
    return(entry)
  } else {
    return(rbind(history, entry))
  }
}

#' Project validator
#' @param project Project object
#' @return TRUE if valid, character vector of errors otherwise
validate_project <- function(project) {
  errors <- character()
  
  if (is.null(project$name) || project$name == "") {
    errors <- c(errors, "Project name is required")
  }
  
  if (!is.data.frame(project$history)) {
    errors <- c(errors, "Project history is corrupted")
  }
  
  if (length(errors) > 0) {
    return(errors)
  } else {
    return(TRUE)
  }
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_project_manager_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("folder"), "Project Manager"),
      p("Create, open, and manage your analysis projects", class = "text-muted")
    ),
    
    # Tabs
    navset_tab(
      # New Project Tab
      nav_panel(
        "New Project",
        div(
          class = "row",
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-body",
                h5("Create New Project"),
                textInput(ns("new_project_name"), "Project Name:",
                         placeholder = "e.g., Q1 Sales Survey"),
                textAreaInput(ns("new_project_desc"), "Description (optional):",
                            rows = 4,
                            placeholder = "Enter project description..."),
                div(class = "d-grid gap-2",
                  actionButton(ns("btn_create"), "Create Project",
                             class = "btn-primary")
                ),
                hr(),
                div(class = "alert alert-info alert-sm",
                  "A new project will be created with the current dataset.",
                  br(),
                  "Existing data will not be lost."
                )
              )
            )
          )
        )
      ),
      
      # Open Project Tab
      nav_panel(
        "Open Project",
        div(
          class = "card",
          div(class = "card-header",
            h5("Recent Projects")
          ),
          div(class = "card-body",
            div(
              class = "table-responsive",
              tableOutput(ns("recent_projects_table"))
            ),
            br(),
            div(
              class = "form-group",
              tags$label("Or browse for a project file:"),
              shinyFilesButton(ns("file_browser"), "Browse Files",
                             title = "Select project (.sap file)",
                             multiple = FALSE)
            )
          )
        )
      ),
      
      # Save Project Tab
      nav_panel(
        "Save Project",
        div(
          class = "row",
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-body",
                h5("Save Current Project"),
                textInput(ns("save_project_name"), "Project Name:"),
                div(class = "d-grid gap-2 mt-3",
                  actionButton(ns("btn_save"), "Save Project",
                             class = "btn-success"),
                  actionButton(ns("btn_save_as"), "Save As...",
                             class = "btn-secondary")
                ),
                hr(),
                uiOutput(ns("project_info"))
              )
            )
          )
        )
      ),
      
      # Project History Tab
      nav_panel(
        "Project History",
        div(
          class = "card",
          div(class = "card-header",
            h5("Analysis History")
          ),
          div(class = "card-body",
            div(class = "table-responsive",
              tableOutput(ns("history_table"))
            )
          )
        )
      )
    )
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_project_manager_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive values
    current_project <- reactiveValues(
      data = NULL,
      loaded = FALSE,
      path = NULL,
      name = "Untitled Project"
    )
    
    # Create new project
    observeEvent(input$btn_create, {
      req(input$new_project_name != "")
      
      project <- create_project(
        name = input$new_project_name,
        description = input$new_project_desc
      )
      
      # Add current data if available
      if (app_state$data_imported) {
        project$processed_data <- app_state$processed_data
        project$variable_types <- app_state$variable_types
      }
      
      current_project$data <- project
      current_project$loaded <- TRUE
      current_project$name <- input$new_project_name
      
      showNotification(
        sprintf("Project '%s' created", input$new_project_name),
        type = "message"
      )
    })
    
    # Save project
    observeEvent(input$btn_save, {
      req(current_project$loaded)
      req(input$save_project_name != "")
      
      # Create projects directory if needed
      if (!dir.exists("projects")) {
        dir.create("projects")
      }
      
      project_name <- input$save_project_name
      project_path <- file.path("projects", paste0(project_name, ".sap"))
      
      # Update project with current state
      project <- current_project$data
      project$processed_data <- app_state$processed_data
      project$settings <- app_state$settings
      project$modified <- Sys.time()
      
      success <- save_project(project, project_path)
      
      if (success) {
        current_project$path <- project_path
        showNotification(
          sprintf("Project saved to %s", project_path),
          type = "message"
        )
      } else {
        showNotification(
          "Failed to save project",
          type = "error"
        )
      }
    })
    
    # Display recent projects
    output$recent_projects_table <- renderTable({
      projects <- get_recent_projects()
      
      if (nrow(projects) == 0) {
        return(data.frame(
          "No projects found" = "Create a new project or open an existing one",
          check.names = FALSE
        ))
      }
      
      projects[, c("name", "modified", "size_mb")]
    })
    
    # Display project info
    output$project_info <- renderUI({
      if (!current_project$loaded) {
        return(div(
          class = "alert alert-info",
          "No project loaded"
        ))
      }
      
      project <- current_project$data
      
      div(
        p(strong("Project Name: "), project$name),
        p(strong("Created: "), format(project$created, "%Y-%m-%d %H:%M:%S")),
        p(strong("Modified: "), format(project$modified, "%Y-%m-%d %H:%M:%S")),
        if (!is.null(project$processed_data)) {
          p(strong("Data: "), nrow(project$processed_data), 
            " rows × ", ncol(project$processed_data), " columns")
        }
      )
    })
    
    # Display history
    output$history_table <- renderTable({
      if (!current_project$loaded) {
        return(data.frame())
      }
      
      history <- current_project$data$history
      
      if (nrow(history) == 0) {
        return(data.frame(
          "No history" = "Actions will be recorded here",
          check.names = FALSE
        ))
      }
      
      history[order(history$timestamp, decreasing = TRUE), ]
    })
    
    # Return reactive project state
    reactive({
      current_project$data
    })
  })
}

# =============================================================================
# END OF MODULE 9
# =============================================================================

message("[SAP] Project Manager v2.2-20260706 loaded")  # SAP_MODULE_BANNER
