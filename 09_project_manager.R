# =============================================================================
# MODULE 9: PROJECT MANAGER
# =============================================================================
# Dual-mode workspace manager:
#   - Frontend/demo mode: local .sap project files (legacy compatibility)
#   - Connected mode: PostgreSQL projects shared through owner/editor/reviewer/viewer permissions
# =============================================================================

get_project_schema <- function() {
  list(
    version = "1.0", created = Sys.time(), modified = Sys.time(),
    name = "", description = "", raw_data = NULL, processed_data = NULL,
    variable_types = NULL, data_dictionary = list(), derived_variables = list(),
    analyses = list(), visualizations = list(), models = list(), predictions = list(),
    reports = list(), settings = list(), history = data.frame(),
    dashboard_layout = list()
  )
}

create_project <- function(name, description = "") {
  project <- get_project_schema()
  project$name <- name
  project$description <- description
  project$created <- Sys.time()
  project$modified <- Sys.time()
  project
}

save_project <- function(project, path) {
  project$modified <- Sys.time()
  tryCatch({
    saveRDS(project, file = path, compress = "xz")
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to save project: ", e$message)
    invisible(FALSE)
  })
}

load_project <- function(path) {
  tryCatch({
    project <- readRDS(path)
    schema <- get_project_schema()
    for (field in setdiff(names(schema), names(project))) project[[field]] <- schema[[field]]
    project
  }, error = function(e) {
    warning("Failed to load project: ", e$message)
    NULL
  })
}

get_recent_projects <- function(project_dir = "projects", n = 10) {
  if (!dir.exists(project_dir)) return(data.frame())
  files <- list.files(project_dir, pattern = "\\.sap$", full.names = TRUE)
  if (!length(files)) return(data.frame())
  info <- file.info(files)
  info$path <- rownames(info)
  info <- info[order(info$mtime, decreasing = TRUE), , drop = FALSE]
  info <- head(info, n)
  data.frame(
    name = sub("\\.sap$", "", basename(info$path)),
    path = info$path,
    modified = info$mtime,
    size_mb = round(info$size / 1024^2, 2),
    row.names = NULL
  )
}

.sap_project_connected_ui <- function(ns) {
  tagList(
    uiOutput(ns("connection_notice")),
    div(class = "row g-3",
      div(class = "col-xl-4",
        div(class = "card h-100",
          div(class = "card-body",
            h5(icon("folder-open"), " Current workspace"),
            uiOutput(ns("current_project_card")),
            actionButton(ns("persist_now"), "Save a dataset version now",
                         icon = icon("floppy-disk"), class = "btn btn-outline-primary w-100")
          )
        )
      ),
      div(class = "col-xl-8",
        div(class = "card h-100",
          div(class = "card-body",
            h5(icon("folder-plus"), " Create a project"),
            fluidRow(
              column(5, textInput(ns("new_project_name"), "Project name",
                                  placeholder = "e.g., Household Survey 2026")),
              column(7, textInput(ns("new_project_desc"), "Description",
                                  placeholder = "Purpose, wave, population, or client"))
            ),
            actionButton(ns("btn_create_connected"), "Create and open project",
                         icon = icon("plus"), class = "btn btn-primary")
          )
        )
      )
    ),
    div(class = "card mt-3",
      div(class = "card-header d-flex justify-content-between align-items-center",
        h5(class = "mb-0", icon("database"), " My & shared PostgreSQL projects"),
        actionButton(ns("refresh_projects"), "Refresh", icon = icon("rotate"),
                     class = "btn btn-sm btn-outline-secondary")
      ),
      div(class = "card-body",
        fluidRow(
          column(6, selectInput(ns("project_choice"), "Select project", choices = character(0))),
          column(6,
            div(class = "d-flex flex-wrap gap-2 mt-4",
              actionButton(ns("open_connected"), "Open", icon = icon("folder-open"),
                           class = "btn btn-primary"),
              actionButton(ns("favourite_connected"), "Favourite", icon = icon("star"),
                           class = "btn btn-outline-warning"),
              actionButton(ns("archive_connected"), "Archive", icon = icon("box-archive"),
                           class = "btn btn-outline-secondary"),
              actionButton(ns("delete_connected"), "Delete", icon = icon("trash"),
                           class = "btn btn-outline-danger")
            )
          )
        ),
        div(class = "table-responsive", tableOutput(ns("connected_projects_table")))
      )
    ),
    div(class = "card mt-3",
      div(class = "card-body",
        h5(icon("pen-to-square"), " Rename current project"),
        fluidRow(
          column(5, textInput(ns("rename_project_name"), "New name")),
          column(5, textInput(ns("rename_project_desc"), "Updated description")),
          column(2, actionButton(ns("rename_connected"), "Update",
                                 class = "btn btn-outline-primary mt-4 w-100"))
        )
      )
    )
  )
}

.sap_project_local_ui <- function(ns) {
  navset_tab(
    nav_panel(
      "New Project",
      div(class = "row",
        div(class = "col-md-6",
          div(class = "card", div(class = "card-body",
            h5("Create New Project"),
            textInput(ns("new_project_name_local"), "Project Name:",
                      placeholder = "e.g., Q1 Sales Survey"),
            textAreaInput(ns("new_project_desc_local"), "Description (optional):", rows = 4),
            actionButton(ns("btn_create_local"), "Create Project", class = "btn-primary"),
            hr(), div(class = "alert alert-info alert-sm",
                      "Local project files are intended for desktop/demo mode.")
          ))
        )
      )
    ),
    nav_panel(
      "Open Project",
      div(class = "card", div(class = "card-body",
        h5("Recent Projects"),
        tableOutput(ns("recent_projects_table")),
        shinyFilesButton(ns("file_browser"), "Browse Files",
                         title = "Select project (.sap file)", multiple = FALSE)
      ))
    ),
    nav_panel(
      "Save Project",
      div(class = "row", div(class = "col-md-6",
        div(class = "card", div(class = "card-body",
          h5("Save Current Project"),
          textInput(ns("save_project_name"), "Project Name:"),
          actionButton(ns("btn_save"), "Save Project", class = "btn-success"),
          hr(), uiOutput(ns("project_info"))
        ))
      ))
    )
  )
}

mod_project_manager_ui <- function(id, connected = FALSE) {
  ns <- NS(id)
  tagList(
    div(class = "mb-4",
      h2(icon("folder"), "Project Manager"),
      p(if (isTRUE(connected))
          "Create, share, and reopen secure PostgreSQL workspaces. Owner/editor/reviewer/viewer permissions control collaboration."
        else "Create, open, and manage local analysis project files.",
        class = "text-muted")
    ),
    if (isTRUE(connected)) .sap_project_connected_ui(ns) else .sap_project_local_ui(ns)
  )
}

.sap_connected_project_server <- function(input, output, session, app_state, services) {
  ns <- session$ns
  refresh_tick <- reactiveVal(0L)

  authenticated <- reactive({
    services$auth$version()
    isTRUE(services$auth$is_authenticated())
  })

  projects <- reactive({
    refresh_tick()
    services$project$version()
    services$auth$version()
    if (!authenticated()) return(data.frame())
    services$project$list(include_archived = FALSE)
  })

  observe({
    dat <- projects()
    choices <- if (is.data.frame(dat) && nrow(dat)) {
      stats::setNames(as.character(dat$id), paste0(dat$name,
        ifelse(dat$is_favourite == 1, " ★", "")))
    } else character(0)
    updateSelectInput(session, "project_choice", choices = choices,
                      selected = if (length(choices)) unname(choices[[1]]) else character(0))
  })

  output$connection_notice <- renderUI({
    if (!authenticated()) {
      div(class = "alert alert-info",
          icon("circle-info"),
          strong(" Guest workspace: "),
          "You may analyse data now. Sign in or create an account from the top bar before creating a durable project.")
    } else {
      div(class = "alert alert-success py-2",
          icon("database"), " PostgreSQL persistence is active for your account.")
    }
  })

  output$current_project_card <- renderUI({
    services$project$version()
    pid <- app_state$current_project
    if (is.null(pid)) {
      return(div(class = "text-muted py-3",
                 icon("folder-minus"), " No database project is open."))
    }
    p <- services$project$current()
    if (is.null(p)) return(div(class = "alert alert-warning", "The current project is unavailable."))
    tagList(
      h4(as.character(p$name[[1]])),
      p(class = "text-muted", as.character(p$description[[1]] %||% "")),
      p(class = "mb-1", strong("Project ID: "), as.character(p$id[[1]])),
      p(class = "mb-1", strong("Modified: "), format(p$modified_at[[1]], "%Y-%m-%d %H:%M")),
      if (isTRUE(app_state$data_imported) && is.data.frame(app_state$processed_data))
        p(class = "mb-0", strong("Active data: "),
          nrow(app_state$processed_data), " rows × ", ncol(app_state$processed_data), " variables")
    )
  })

  output$connected_projects_table <- renderTable({
    dat <- projects()
    if (!is.data.frame(dat) || !nrow(dat)) {
      return(data.frame(Status = if (authenticated()) "No projects yet" else "Sign in to view projects",
                        check.names = FALSE))
    }
    data.frame(
      ID = dat$id,
      Project = dat$name,
      Owner = if ("owner_username" %in% names(dat)) dat$owner_username else dat$owner_id,
      Access = if ("permission" %in% names(dat)) tools::toTitleCase(dat$permission) else "Owner",
      Favourite = ifelse(dat$is_favourite == 1, "Yes", ""),
      Modified = format(dat$modified_at, "%Y-%m-%d %H:%M"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  observeEvent(input$refresh_projects, refresh_tick(refresh_tick() + 1L), ignoreInit = TRUE)

  observeEvent(input$btn_create_connected, {
    if (!authenticated()) {
      services$notify$warning("Sign in or create an account before creating a durable project.")
      return()
    }
    name <- trimws(input$new_project_name %||% "")
    if (!nzchar(name)) {
      services$notify$warning("Enter a project name.")
      return()
    }
    pid <- services$project$create(name, input$new_project_desc %||% "", persist_current = TRUE)
    if (!is.null(pid)) {
      updateTextInput(session, "new_project_name", value = "")
      updateTextInput(session, "new_project_desc", value = "")
      refresh_tick(refresh_tick() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$open_connected, {
    req(input$project_choice)
    services$project$open(as.integer(input$project_choice))
  }, ignoreInit = TRUE)

  observeEvent(input$favourite_connected, {
    req(input$project_choice)
    id <- as.integer(input$project_choice)
    dat <- projects()
    row <- dat[dat$id == id, , drop = FALSE]
    value <- !(nrow(row) && row$is_favourite[[1]] == 1)
    if (services$project$favourite(id, value)) refresh_tick(refresh_tick() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$archive_connected, {
    req(input$project_choice)
    if (services$project$archive(as.integer(input$project_choice), TRUE)) {
      services$notify$success("Project archived.")
      refresh_tick(refresh_tick() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$delete_connected, {
    req(input$project_choice)
    id <- as.integer(input$project_choice)
    dat <- projects(); row <- dat[dat$id == id, , drop = FALSE]
    label <- if (nrow(row)) row$name[[1]] else paste("project", id)
    showModal(modalDialog(
      title = "Move project to Recycle Bin?",
      p("The project will be hidden from normal project lists and can be restored from Enterprise Center. Permanent purge remains an explicit second action."),
      p(strong(label)),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirm_delete_connected"), "Move to Recycle Bin",
                     class = "btn btn-danger")
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_delete_connected, {
    req(input$project_choice)
    id <- as.integer(input$project_choice)
    ok <- if (!is.null(pool) && !is.null(app_state$current_user)) {
      tryCatch({ db_soft_delete_project(pool, id, app_state$current_user); TRUE }, error = function(e) { services$notify$error(conditionMessage(e)); FALSE })
    } else {
      services$project$delete(id)
    }
    if (isTRUE(ok)) {
      if (identical(as.integer(app_state$current_project %||% NA_integer_), id)) {
        app_state$current_project <- NULL
        app_state$current_project_name <- "Presentation Workspace"
      }
      removeModal()
      services$notify$success("Project moved to Recycle Bin.")
      refresh_tick(refresh_tick() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$rename_connected, {
    pid <- app_state$current_project
    if (is.null(pid)) {
      services$notify$warning("Open a project first.")
      return()
    }
    name <- trimws(input$rename_project_name %||% "")
    desc <- trimws(input$rename_project_desc %||% "")
    if (!nzchar(name) && !nzchar(desc)) {
      services$notify$warning("Enter a new name or description.")
      return()
    }
    ok <- services$project$rename(pid,
      name = if (nzchar(name)) name else NULL,
      description = if (nzchar(desc)) desc else NULL)
    if (ok) {
      services$notify$success("Project updated.")
      refresh_tick(refresh_tick() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$persist_now, {
    if (is.null(app_state$current_project)) {
      services$notify$warning("Create or open a project first.")
      return()
    }
    df <- app_state$processed_data
    if (!is.data.frame(df)) df <- app_state$raw_data
    if (!is.data.frame(df)) {
      services$notify$warning("No active dataset is available to save.")
      return()
    }
    services$data$set(df, kind = "processed", module = "project_manager",
                      description = "manual project snapshot",
                      variable_types = app_state$variable_types)
    services$notify$success("Dataset version saved.")
  }, ignoreInit = TRUE)
}

.sap_local_project_server <- function(input, output, session, app_state) {
  current_project <- reactiveValues(data = NULL, loaded = FALSE, path = NULL,
                                    name = "Untitled Project")

  observeEvent(input$btn_create_local, {
    req(nzchar(input$new_project_name_local))
    project <- create_project(input$new_project_name_local, input$new_project_desc_local)
    if (isTRUE(app_state$data_imported)) {
      project$processed_data <- app_state$processed_data
      project$variable_types <- app_state$variable_types
    }
    current_project$data <- project
    current_project$loaded <- TRUE
    current_project$name <- input$new_project_name_local
    showNotification(sprintf("Project '%s' created", input$new_project_name_local), type = "message")
  })

  observeEvent(input$btn_save, {
    req(current_project$loaded, nzchar(input$save_project_name))
    if (!dir.exists("projects")) dir.create("projects")
    path <- file.path("projects", paste0(input$save_project_name, ".sap"))
    project <- current_project$data
    project$processed_data <- app_state$processed_data
    project$settings <- app_state$settings
    project$modified <- Sys.time()
    if (save_project(project, path)) {
      current_project$path <- path
      showNotification(sprintf("Project saved to %s", path), type = "message")
    } else showNotification("Failed to save project", type = "error")
  })

  output$recent_projects_table <- renderTable({
    x <- get_recent_projects()
    if (!nrow(x)) return(data.frame("No projects found" = "Create a new project", check.names = FALSE))
    x[, c("name", "modified", "size_mb"), drop = FALSE]
  })

  output$project_info <- renderUI({
    if (!current_project$loaded) return(div(class = "alert alert-info", "No project loaded"))
    p <- current_project$data
    tagList(
      p(strong("Project Name: "), p$name),
      p(strong("Created: "), format(p$created, "%Y-%m-%d %H:%M:%S")),
      if (is.data.frame(p$processed_data))
        p(strong("Data: "), nrow(p$processed_data), " rows × ", ncol(p$processed_data), " columns")
    )
  })
}

mod_project_manager_server <- function(id, app_state, services = NULL, connected = FALSE, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    if (isTRUE(connected) && !is.null(services) && !is.null(services$project)) {
      .sap_connected_project_server(input, output, session, app_state, services)
    } else {
      .sap_local_project_server(input, output, session, app_state)
    }
  })
}

message("[SAP] Project Manager v3.0-postgresql-20260801 loaded")
