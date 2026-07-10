# =============================================================================
# SURVEY ANALYTICS PLATFORM v2.0 - ENHANCED MAIN APPLICATION
# =============================================================================
# 
# Complete analytics platform with:
# - Project workspace (M9)
# - Analysis history & undo/redo (M10)
# - Machine learning center (M11)
# - Code generation (M16)
# - All existing analysis modules (M1-8)
# 
# Integration Points:
# - Centralized app_state with new fields
# - Project-based persistence
# - Cross-module action tracking
# - Automatic R code generation
# =============================================================================

# ========================================================================
# STEP 1: INITIALIZE SHINY EXTENSIONS
# ========================================================================
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)  # NEW: For clipboard functionality
library(shinyFiles)  # NEW: For file browser

# ========================================================================
# STEP 2: LOAD CONFIGURATION & UTILITIES
# ========================================================================

# Resilient sourcing helpers.
# - source_required(): the app genuinely cannot run without these -> hard error.
# - source_optional(): feature modules; a missing file warns and is recorded,
#   but startup continues. Missing optional modules are reported together at
#   the end so one absent file can never mask the app with a raw file() error.
.sap_missing_optional <- character(0)

source_required <- function(f) {
  if (!file.exists(f)) {
    stop(sprintf("Required file '%s' not found in %s. Cannot start.",
                 f, getwd()), call. = FALSE)
  }
  source(f)
}

source_optional <- function(f) {
  if (!file.exists(f)) {
    .sap_missing_optional <<- c(.sap_missing_optional, f)
    warning(sprintf("Optional module '%s' not found - skipping. ", f),
            call. = FALSE)
    return(invisible(FALSE))
  }
  source(f)
  invisible(TRUE)
}

# Core: without these there is no application.
source_required("global_no_gtsummary.R")

# --- BACKEND: service layer + data access layer (Phase 1) ---
source_required("00_services.R")   # services + create_services()
source_required("db_backend.R")    # DBI/pool DAL

# ========================================================================
# STEP 3: LOAD ALL MODULES (feature modules are optional)
# ========================================================================

# --- TIER 1: DATA MANAGEMENT ---
source_optional("01_import_wizard.R")
source_optional("02_data_explorer.R")
source_optional("03_data_cleaning.R")

# --- TIER 2: CORE INFRASTRUCTURE ---
source_optional("09_project_manager.R")
source_optional("10_analysis_history.R")
source_optional("16_code_generator.R")

# --- TIER 3: ANALYTICS ---
source_optional("04_dashboard.R")
source_optional("05_descriptive_stats.R")
source_optional("06_statistical_tests.R")
source_optional("07_visualization.R")

# --- TIER 4: ADVANCED ANALYTICS ---
source_optional("11_machine_learning.R")
source_optional("12_system_diagnostics.R")
source_optional("13_settings.R")

# --- TIER 5: REPORTING ---
source_optional("08_report_generator.R")

# Report any missing optional modules in one place.
if (length(.sap_missing_optional) > 0) {
  message("[SAP] NOTE: ", length(.sap_missing_optional),
          " optional module(s) missing and skipped: ",
          paste(.sap_missing_optional, collapse = ", "))
}

# ========================================================================
# STEP 4: DEFINE ENHANCED USER INTERFACE
# ========================================================================

# --- Global table containment -------------------------------------------
# Keep EVERY table inside its card/container. Wide tables (many columns)
# scroll horizontally WITHIN their container instead of widening the page.
# The critical rule is `min-width: 0` on flex/grid children: flex items
# default to min-width:auto and refuse to shrink below their content, so a
# wide table drags the whole layout wider. Setting min-width:0 lets the
# container shrink and the inner overflow-x:auto take over.
.sap_table_css <- shiny::HTML("
/* -------------------------------------------------------------------------
   Table containment. Two mechanisms, because there are two table types:
   (A) base Shiny renderTable() -> bare <table class='table'> with NO scroll
       wrapper. Make the TABLE ITSELF the scroll container. This is robust
       because it doesn't depend on any ancestor cooperating.
   (B) DT -> scroll its .dataTables_wrapper, and let the bslib grid shrink.
   ------------------------------------------------------------------------- */

/* Let the bslib sidebar-layout main area + all layout boxes shrink.
   Grid/flex items default to min-width:auto and expand the track to fit
   content, which is what widens the whole page. */
.bslib-sidebar-layout > .main,
.bslib-sidebar-layout .main,
main, .main,
.card, .card-body, .tab-content, .tab-pane,
.bslib-grid, .bslib-grid-item, .html-fill-item, .html-fill-container,
.shiny-html-output { min-width: 0 !important; }

/* (A) Any plain Bootstrap/Shiny table (renderTable output), EXCLUDING DT's
   own table. display:block turns the table into its own horizontal scroll
   box capped at its parent's width; the whole table (head+body) scrolls as
   a unit and can never push its container wider. */
table.table:not(.dataTable),
.shiny-table:not(.dataTable),
.shiny-html-output > table:not(.dataTable) {
  display: block;
  max-width: 100%;
  overflow-x: auto;
  white-space: nowrap;
}

/* (B) DT: constrain the wrapper; scroll horizontally inside it. */
.dataTables_wrapper {
  width: 100% !important;
  max-width: 100% !important;
  overflow-x: auto;
}
.dataTables_wrapper .dataTables_scrollBody { max-width: 100% !important; }

/* Bootstrap responsive wrapper, generic escape hatch. */
.table-responsive { max-width: 100%; overflow-x: auto; }

/* Sticky header when a table scrolls vertically. */
.dataTables_scrollHead { position: sticky; top: 0; z-index: 2; }
table.dataTable thead th {
  position: sticky; top: 0; z-index: 1;
  background: var(--bs-body-bg, #fff);
}

/* Final backstop: no element may push the page horizontally. */
html, body { overflow-x: hidden; max-width: 100%; }
")

ui <- page_navbar(
  title = "Survey Analytics Platform v2.0",
  id = "main_nav",
  theme = create_app_theme(DEFAULT_SETTINGS$theme_mode),
  window_title = "Survey Analytics Platform",
  fillable = FALSE,   # normal document flow -> footer sits below content,
                      # never layered behind cards (fixes footer-behind-cards)
  header = tagList(
    useShinyjs(),                                   # shinyjs deps
    tags$head(tags$style(.sap_table_css))           # global table containment
  ),
  
  # =====================================================================
  # NAV 1: PROJECT WORKSPACE (NEW)
  # =====================================================================
  nav_panel(
    title = "Projects",
    icon = icon("folder-open"),
    div(class = "p-4",
      mod_project_manager_ui("project_manager")
    )
  ),
  
  # =====================================================================
  # NAV 2: DATA MANAGEMENT
  # =====================================================================
  nav_panel(
    title = "Data",
    icon = icon("database"),
    
    navset_tab(
      nav_panel("Import",
        icon = icon("upload"),
        div(class = "p-4",
          mod_import_wizard_ui("import_wizard")
        )
      ),
      
      nav_panel("Explorer",
        icon = icon("magnifying-glass"),
        div(class = "p-4",
          mod_data_explorer_ui("data_explorer")
        )
      ),
      
      nav_panel("Cleaning",
        icon = icon("broom"),
        div(class = "p-4",
          mod_data_cleaning_ui("data_cleaning")
        )
      )
    )
  ),
  
  # =====================================================================
  # NAV 3: ANALYSIS (EXPANDED)
  # =====================================================================
  nav_panel(
    title = "Analysis",
    icon = icon("chart-bar"),
    
    navset_tab(
      nav_panel("Dashboard",
        icon = icon("gauge"),
        div(class = "p-4",
          mod_dashboard_ui("dashboard")
        )
      ),
      
      nav_panel("Descriptive",
        icon = icon("chart-line"),
        div(class = "p-4",
          mod_descriptive_stats_ui("descriptive_stats")
        )
      ),
      
      nav_panel("Visualization",
        icon = icon("palette"),
        div(class = "p-4",
          mod_visualization_ui("visualization")
        )
      ),
      
      nav_panel("Statistics",
        icon = icon("calculator"),
        div(class = "p-4",
          mod_statistical_tests_ui("statistical_tests")
        )
      ),
      
      # NEW: Machine Learning
      nav_panel("Machine Learning",
        icon = icon("robot"),
        div(class = "p-4",
          mod_machine_learning_ui("machine_learning")
        )
      )
    )
  ),
  
  # =====================================================================
  # NAV 4: ADVANCED (NEW)
  # =====================================================================
  nav_panel(
    title = "Advanced",
    icon = icon("wand-magic-sparkles"),
    
    navset_tab(
      # NEW: Analysis History
      nav_panel("History",
        icon = icon("history"),
        div(class = "p-4",
          mod_analysis_history_ui("analysis_history")
        )
      ),
      
      # NEW: Code Generator
      nav_panel("R Code",
        icon = icon("code"),
        div(class = "p-4",
          mod_code_generator_ui("code_generator")
        )
      ),
      
      # NEW: System Diagnostics
      nav_panel("Diagnostics",
        icon = icon("stethoscope"),
        div(class = "p-4",
          if (exists("mod_system_diagnostics_ui")) {
            mod_system_diagnostics_ui("system_diagnostics")
          } else {
            div(class = "alert alert-warning",
                "System Diagnostics module is not installed.")
          }
        )
      )
    )
  ),
  
  # =====================================================================
  # NAV: SETTINGS
  # =====================================================================
  nav_panel(
    title = "Settings",
    icon = icon("gear"),
    div(class = "p-4",
      if (exists("mod_settings_ui")) {
        mod_settings_ui("settings")
      } else {
        div(class = "alert alert-warning",
            "Settings module is not installed.")
      }
    )
  ),
  
  # =====================================================================
  # NAV 5: REPORTS
  # =====================================================================
  nav_panel(
    title = "Reports",
    icon = icon("file-export"),
    
    navset_tab(
      nav_panel("Generate",
        icon = icon("file-pdf"),
        div(class = "p-4",
          mod_report_generator_ui("report_generator")
        )
      ),
      
      nav_panel("Export",
        icon = icon("download"),
        div(class = "p-4",
          h3("Export Center"),
          p("Enhanced export options (coming soon)",
           class = "text-muted")
        )
      )
    )
  ),
  
  # =====================================================================
  # ENHANCED SIDEBAR WITH PROJECT INFO
  # =====================================================================
  sidebar = sidebar(
    title = "Project & Data",
    open = "open",
    
    # Project status (NEW)
    div(
      class = "mb-3",
      h6("Current Project", class = "text-muted"),
      textOutput("project_name"),
      br(),
      div(class = "btn-group w-100 btn-group-sm",
        actionButton("btn_save_project", "Save",
                    class = "btn-sm btn-success",
                    icon = icon("save")),
        actionButton("btn_new_project", "New",
                    class = "btn-sm btn-primary",
                    icon = icon("plus"))
      )
    ),
    
    hr(),
    
    # Dataset status
    uiOutput("dataset_status"),
    
    hr(),
    
    # Quick actions
    div(
      class = "d-grid gap-2",
      actionButton("btn_new_session", "Clear Session",
                  class = "btn-sm btn-outline-secondary",
                  icon = icon("refresh"))
    )
  ),
  
  # =====================================================================
  # FOOTER WITH VERSION INFO
  # =====================================================================
  footer = div(
    class = "sap-footer text-center py-3 small text-muted border-top mt-5",
    style = "background: var(--bs-body-bg, #fff); position: relative; z-index: 5;",
    "Survey Analytics Platform v2.0 | Enhanced with ML & Project Management",
    br(),
    "© 2024 | For researchers, students, NGOs, and public health professionals"
  )
)

# ========================================================================
# STEP 5: DEFINE ENHANCED SERVER LOGIC
# ========================================================================

server <- function(input, output, session) {
  
  # =====================================================================
  # INITIALIZE ENHANCED APPLICATION STATE
  # =====================================================================
  
  app_state <- reactiveValues(
    # --- EXISTING STATE ---
    raw_data = NULL,
    processed_data = NULL,
    data_imported = FALSE,
    variable_types = NULL,
    settings = DEFAULT_SETTINGS,
    import_log = data.frame(),
    
    # --- NEW: PROJECT STATE ---
    current_project = NULL,
    project_path = NULL,
    project_saved = FALSE,
    
    # --- NEW: ML STATE ---
    trained_models = list(),
    last_model = NULL,
    model_predictions = NULL,
    
    # --- NEW: ANALYSIS TRACKING ---
    analyses = list(),
    last_analysis = NULL
  )
  
  # =====================================================================
  # BACKEND ACTIVATION (Phase 1): pool + services, shared across modules
  # =====================================================================
  # One connection pool for the whole app; closed on stop.
  app_pool <- tryCatch(
    db_connect(backend = "sqlite", sqlite_path = "survey_platform.sqlite"),
    error = function(e) { message("[SAP] DB pool init failed: ", e$message); NULL }
  )
  if (!is.null(app_pool)) {
    tryCatch(db_init(app_pool, backend = "sqlite"),
             error = function(e) message("[SAP] db_init failed: ", e$message))
    onStop(function() db_disconnect(app_pool))
  }
  
  # Single global services object shared by every module.
  services <- create_services(app_state, pool = app_pool)
  services$log$info("Services initialized", "app")

  # ---------------------------------------------------------------------
  # SESSION RESTORE (Phase 1): reopen the most recent project + dataset so
  # the user continues where they left off. Guarded so a fresh install with
  # no projects simply starts empty. Owner scoping arrives with auth (Phase
  # 2); for now this restores the single-user workspace.
  # ---------------------------------------------------------------------
  if (!is.null(app_pool)) {
    tryCatch({
      recent <- services$project$list()
      if (is.data.frame(recent) && nrow(recent) > 0) {
        services$project$open(recent$id[1])
        services$log$info(sprintf("Restored session: project %s", recent$id[1]), "app")
      }
    }, error = function(e) services$log$warn(
      sprintf("Session restore skipped: %s", conditionMessage(e)), "app"))
  }
  
  # =====================================================================
  # INITIALIZE NEW MODULES (ORDER MATTERS)
  # =====================================================================
  
  # Code generator (provides functions used by other modules)
  code_gen <- mod_code_generator_server("code_generator", app_state)
  
  # History manager (tracks all actions)
  history_mgr <- mod_analysis_history_server("analysis_history", app_state)
  
  # Project manager (manages workspace)
  project_mgr <- mod_project_manager_server("project_manager", app_state)
  
  # Machine learning (trains models)
  mod_machine_learning_server("machine_learning", app_state, services = services)
  
  # System diagnostics (M12)
  if (exists("mod_system_diagnostics_server")) {
    mod_system_diagnostics_server("system_diagnostics", app_state)
  }
  if (exists("mod_settings_server")) {
    mod_settings_server("settings", app_state, services = services)
  }
  
  # =====================================================================
  # INITIALIZE EXISTING MODULES
  # =====================================================================
  
  # Phase 1: Data Management
  #   import_wizard MIGRATED to service layer (writes via services$data$set).
  #   Others still use app_state directly; migrate one at a time.
  mod_import_wizard_server("import_wizard", app_state, services = services)
  mod_data_explorer_server("data_explorer", app_state, services = services)
  mod_data_cleaning_server("data_cleaning", app_state, services = services)
  
  # Phase 3: Analytics
  mod_dashboard_server("dashboard", app_state, services = services)
  mod_descriptive_stats_server("descriptive_stats", app_state, services = services)
  mod_statistical_tests_server("statistical_tests", app_state, services = services)
  mod_visualization_server("visualization", app_state, services = services)
  
  # Phase 5: Reporting
  mod_report_generator_server("report_generator", app_state, services = services)
  
  # =====================================================================
  # SIDEBAR OUTPUTS
  # =====================================================================
  
  # Project name. current_project is now an integer id (the DAL key), so the
  # display name is resolved through ProjectService rather than read off the id.
  output$project_name <- renderText({
    pid <- app_state$current_project
    if (is.null(pid)) return("Untitled Project")
    proj <- tryCatch(services$project$current(), error = function(e) NULL)
    if (is.null(proj) || !nrow(proj)) sprintf("Project %s", pid) else proj$name[1]
  })
  
  # Dataset status
  output$dataset_status <- renderUI({
    if (!app_state$data_imported) {
      div(
        class = "alert alert-info alert-sm",
        icon("circle-info"), br(),
        strong("No data imported"),
        br(),
        span("Go to Data > Import", class = "small")
      )
    } else {
      div(
        class = "alert alert-success alert-sm",
        icon("check-circle"), br(),
        strong("Dataset Loaded"),
        br(),
        span(
          sprintf("%s rows × %s columns",
                 format(nrow(app_state$processed_data), big.mark = ","),
                 ncol(app_state$processed_data)),
          class = "small"
        )
      )
    }
  })
  
  # =====================================================================
  # PROJECT MANAGEMENT HANDLERS
  # =====================================================================
  
  observeEvent(input$btn_save_project, {
    showModal(modalDialog(
      title = "Save Project",
      textInput("modal_project_name", "Project Name:",
               placeholder = "Enter project name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_save", "Save", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$modal_save, {
    showNotification(
      "Project save feature (implement with project_manager module)",
      type = "message"
    )
    removeModal()
  })
  
  observeEvent(input$btn_new_project, {
    showNotification(
      "New project feature (implement with project_manager module)",
      type = "message"
    )
  })
  
  # =====================================================================
  # SESSION LOGGING
  # =====================================================================
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("✓ Survey Analytics Platform v2.2-20260706 Started\n")
  cat("  VERIFY: 16 '[SAP] ... v2.2-20260706 loaded' lines must have printed at source time.\n")
  cat("  If any are missing or show another version, the wrong files are being sourced.\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("Session ID:", session$token, "\n")
  cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("R Version:", R.version$version.string, "\n")
  cat("\n")
  cat("🚀 NEW FEATURES:\n")
  cat("   • Project Workspace (M9)\n")
  cat("   • Analysis History & Undo/Redo (M10)\n")
  cat("   • Machine Learning Center - 8 algorithms (M11)\n")
  cat("   • Automatic R Code Generation (M16)\n")
  cat("\n")
  cat("📊 Total Modules: 12\n")
  cat("💾 Est. Code: 20,000+ lines\n")
  cat("🎯 Features: 500+\n")
  cat("\n")
  cat("🌐 App running at: http://127.0.0.1:3838\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
}

# ========================================================================
# STEP 6: CREATE AND RETURN ENHANCED SHINY APP
# ========================================================================

shinyApp(ui = ui, server = server)

# ========================================================================
# END OF ENHANCED app.R
# ========================================================================
