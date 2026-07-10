# =============================================================================
# MODULE 3: DATA CLEANING
# =============================================================================
# 
# Complete data cleaning and transformation module with:
# 1. Data Quality Assessment (0-100 score, issues, recommendations)
# 2. Missing Values Analysis & Imputation (6+ methods)
# 3. Duplicate Detection (rows, columns, IDs)
# 4. Outlier Detection & Handling (IQR, Z-score, Mahalanobis)
# 5. Variable Transformations (log, sqrt, standardize, normalize)
# 6. Variable Recoding & Renaming
# 7. Cleaning History & Undo/Redo
#
# Dependencies: plotly, DT, dplyr, ggplot2, tidyr
#
# =============================================================================

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_data_cleaning_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("broom"), "Data Cleaning"),
      p("Clean, transform, and validate your data with professional tools", 
        class = "text-muted")
    ),
    
    # Tab structure
    navset_tab(
      id = ns("cleaning_tabs"),
      
      # TAB 1: QUALITY ASSESSMENT
      nav_panel(
        "Quality Assessment",
        icon = icon("gauge"),
        div(
          class = "p-4",
          
          # Quality score gauge
          div(
            class = "row mb-4",
            div(
              class = "col-md-6",
              div(
                class = "card",
                div(
                  class = "card-body",
                  h4("Data Quality Score"),
                  plotly::plotlyOutput(ns("quality_gauge"), height = "250px")
                )
              )
            ),
            
            # Quality metrics
            div(
              class = "col-md-6",
              div(
                class = "row",
                lapply(1:4, function(i) {
                  div(
                    class = "col-md-6 mb-3",
                    div(
                      class = "card",
                      div(
                        class = "card-body",
                        uiOutput(ns(paste0("quality_metric_", i)))
                      )
                    )
                  )
                })
              )
            )
          ),
          
          hr(),
          
          # Issues and recommendations
          div(
            class = "row",
            div(
              class = "col-md-6",
              div(
                class = "card",
                div(
                  class = "card-header bg-warning",
                  h5("Issues Detected")
                ),
                div(
                  class = "card-body",
                  style = "max-height: 300px; overflow-y: auto;",
                  uiOutput(ns("quality_issues"))
                )
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "card",
                div(
                  class = "card-header bg-info text-white",
                  h5("Recommendations")
                ),
                div(
                  class = "card-body",
                  style = "max-height: 300px; overflow-y: auto;",
                  uiOutput(ns("quality_recommendations"))
                )
              )
            )
          )
        )
      ),
      
      # TAB 2: MISSING VALUES
      nav_panel(
        "Missing Values",
        icon = icon("triangle-exclamation"),
        div(
          class = "p-4",
          
          # Missing value summary
          div(
            class = "row mb-4",
            lapply(1:3, function(i) {
              div(
                class = "col-md-4",
                div(
                  class = "card",
                  div(
                    class = "card-body",
                    uiOutput(ns(paste0("missing_summary_", i)))
                  )
                )
              )
            })
          ),
          
          hr(),
          
          # Missing value heatmap
          div(
            class = "card mb-4",
            div(
              class = "card-header",
              h5("Missing Values Pattern")
            ),
            div(
              class = "card-body",
              plotly::plotlyOutput(ns("missing_heatmap"), height = "300px")
            )
          ),
          
          # Imputation options
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Impute Missing Values"),
              icon("hand-holding-water", class = "ms-2")
            ),
            div(
              class = "card-body",
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  selectInput(ns("impute_var"), "Select Variable:",
                             choices = "", multiple = FALSE),
                  selectInput(ns("impute_method"), "Imputation Method:",
                             choices = c(
                               "Mean (numeric)" = "mean",
                               "Median (numeric)" = "median",
                               "Mode (any type)" = "mode",
                               "Zero" = "zero",
                               "Forward Fill" = "forward_fill",
                               "Backward Fill" = "backward_fill"
                             ))
                ),
                div(
                  class = "col-md-6",
                  div(
                    class = "alert alert-info",
                    icon("circle-info"),
                    strong("Preview:"),
                    br(),
                    textOutput(ns("impute_preview"))
                  )
                )
              ),
              div(
                class = "d-grid gap-2 mt-3",
                actionButton(ns("btn_impute"), "Apply Imputation", 
                           class = "btn btn-primary btn-lg")
              )
            )
          ),

          # Remove (delete) missing values
          div(
            class = "card mt-4",
            div(
              class = "card-header",
              h5("Remove Missing Values"),
              icon("trash", class = "ms-2")
            ),
            div(
              class = "card-body",
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  radioButtons(ns("remove_na_mode"), "What to remove:",
                    choices = c(
                      "Rows with ANY missing value"                 = "rows_any",
                      "Rows missing in a specific variable"         = "rows_var",
                      "Columns exceeding a missing % threshold"     = "cols_thresh"
                    )),
                  conditionalPanel(
                    condition = "input.remove_na_mode == 'rows_var'", ns = ns,
                    selectInput(ns("remove_na_var"), "Variable:", choices = "")
                  ),
                  conditionalPanel(
                    condition = "input.remove_na_mode == 'cols_thresh'", ns = ns,
                    sliderInput(ns("remove_na_thresh"),
                                "Drop columns with more than (% missing):",
                                min = 0, max = 100, value = 50, step = 5)
                  )
                ),
                div(
                  class = "col-md-6",
                  div(
                    class = "alert alert-warning",
                    icon("triangle-exclamation"),
                    strong(" Preview:"), br(),
                    textOutput(ns("remove_na_preview"))
                  )
                )
              ),
              div(
                class = "d-grid gap-2 mt-3",
                actionButton(ns("btn_remove_na"), "Apply Removal",
                             class = "btn btn-danger btn-lg")
              )
            )
          )
        )
      ),
      
      # TAB 3: DUPLICATES
      nav_panel(
        "Duplicates",
        icon = icon("copy"),
        div(
          class = "p-4",
          
          # Duplicate summary
          div(
            class = "row mb-4",
            lapply(1:3, function(i) {
              div(
                class = "col-md-4",
                div(
                  class = "card",
                  div(
                    class = "card-body",
                    uiOutput(ns(paste0("dup_summary_", i)))
                  )
                )
              )
            })
          ),
          
          hr(),
          
          # Duplicate detection options
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Detect & Remove Duplicates")
            ),
            div(
              class = "card-body",
              div(
                class = "row",
                div(
                  class = "col-md-4",
                  radioButtons(ns("dup_type"), "Duplicate Type:",
                             choices = c(
                               "Entire Rows" = "rows",
                               "Column Values (by ID)" = "ids",
                               "Entire Columns" = "columns"
                             ),
                             selected = "rows")
                ),
                div(
                  class = "col-md-4",
                  conditionalPanel(
                    condition = "input.dup_type == 'ids'",
                    ns = ns,
                    selectInput(ns("dup_id_column"), "ID Column:",
                               choices = "")
                  ),
                  div(
                    class = "alert alert-info mt-3",
                    span("Checking for duplicate rows...")
                  )
                ),
                div(
                  class = "col-md-4",
                  div(
                    class = "d-grid gap-2",
                    actionButton(ns("btn_remove_dups"), "Remove Duplicates", 
                               class = "btn btn-danger btn-lg")
                  )
                )
              )
            )
          ),
          
          hr(),
          
          # Duplicate details table
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Duplicate Details")
            ),
            div(
              class = "card-body",
              div(
                class = "table-responsive",
                DT::dataTableOutput(ns("dup_details_table"))
              )
            )
          )
        )
      ),
      
      # TAB 4: OUTLIERS
      nav_panel(
        "Outliers",
        icon = icon("chart-line"),
        div(
          class = "p-4",
          
          # Outlier summary
          div(
            class = "row mb-4",
            div(
              class = "col-md-3",
              div(
                class = "card",
                div(
                  class = "card-body",
                  h6("Total Outliers"),
                  h2(textOutput(ns("total_outliers")))
                )
              )
            ),
            div(
              class = "col-md-3",
              div(
                class = "card",
                div(
                  class = "card-body",
                  h6("Variables Affected"),
                  h2(textOutput(ns("vars_with_outliers")))
                )
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "card",
                div(
                  class = "card-body",
                  selectInput(ns("outlier_var"), "Select Variable (numeric only):",
                             choices = "")
                )
              )
            )
          ),
          
          # Outlier visualization
          div(
            class = "card mb-4",
            div(
              class = "card-header",
              h5("Distribution with Outliers")
            ),
            div(
              class = "card-body",
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  plotly::plotlyOutput(ns("outlier_boxplot"), height = "300px")
                ),
                div(
                  class = "col-md-6",
                  plotly::plotlyOutput(ns("outlier_histogram"), height = "300px")
                )
              )
            )
          ),
          
          # Outlier handling
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Handle Outliers")
            ),
            div(
              class = "card-body",
              div(
                class = "row",
                div(
                  class = "col-md-4",
                  selectInput(ns("outlier_method"), "Detection Method:",
                             choices = c(
                               "Interquartile Range" = "iqr",
                               "Z-Score" = "zscore",
                               "Mahalanobis Distance" = "mahalanobis"
                             ))
                ),
                div(
                  class = "col-md-4",
                  selectInput(ns("outlier_action"), "Action:",
                             choices = c(
                               "Remove" = "remove",
                               "Winsorize" = "winsorize",
                               "Replace with NA" = "replace"
                             ))
                ),
                div(
                  class = "col-md-4",
                  div(
                    class = "d-grid gap-2",
                    actionButton(ns("btn_handle_outliers"), "Apply", 
                               class = "btn btn-warning btn-lg")
                  )
                )
              )
            )
          )
        )
      ),
      
      # TAB 5: TRANSFORMATIONS
      nav_panel(
        "Transformations",
        icon = icon("wand-magic-sparkles"),
        div(
          class = "p-4",
          
          # Variable selection
          div(
            class = "row mb-4",
            div(
              class = "col-md-12",
              selectInput(ns("transform_var"), "Select Numeric Variable:",
                         choices = "", multiple = FALSE)
            )
          ),
          
          # Transformation preview
          div(
            class = "row mb-4",
            div(
              class = "col-md-6",
              div(
                class = "card",
                div(
                  class = "card-header",
                  h5("Original Distribution")
                ),
                div(
                  class = "card-body",
                  plotly::plotlyOutput(ns("transform_original"), height = "250px")
                )
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "card",
                div(
                  class = "card-header",
                  h5("Transformed Preview")
                ),
                div(
                  class = "card-body",
                  plotly::plotlyOutput(ns("transform_preview"), height = "250px")
                )
              )
            )
          ),
          
          # Transformation options
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Apply Transformation")
            ),
            div(
              class = "card-body",
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  selectInput(ns("transformation"), "Transformation Type:",
                             choices = c(
                               "Log (log10)" = "log",
                               "Log + 1 (handles zeros)" = "log1p",
                               "Square Root" = "sqrt",
                               "Square" = "square",
                               "Reciprocal (1/x)" = "reciprocal",
                               "Standardize (z-score)" = "standardize",
                               "Normalize (0-1)" = "normalize",
                               "Center (x - mean)" = "center",
                               "Rank" = "rank"
                             ))
                ),
                div(
                  class = "col-md-6",
                  div(
                    class = "alert alert-info",
                    span("ℹ️ Transformation creates a new column"),
                    br(),
                    span("Original data is preserved")
                  )
                )
              ),
              div(
                class = "d-grid gap-2 mt-3",
                actionButton(ns("btn_transform"), "Apply Transformation", 
                           class = "btn btn-primary btn-lg")
              )
            )
          )
        )
      ),
      
      # TAB 6: RECODING & RENAMING
      nav_panel(
        "Recoding",
        icon = icon("quote-left"),
        div(
          class = "p-4",
          
          navset_tab(
            # Subtab: Variable Renaming
            nav_panel(
              "Rename Variables",
              div(
                class = "p-4",
                
                div(
                  class = "card mb-4",
                  div(
                    class = "card-header",
                    h5("Bulk Rename")
                  ),
                  div(
                    class = "card-body",
                    div(
                      class = "row",
                      div(
                        class = "col-md-4",
                        radioButtons(ns("rename_type"), "Rename Style:",
                                   choices = c(
                                     "Manual Mapping" = "manual",
                                     "Add Prefix" = "prefix",
                                     "Add Suffix" = "suffix",
                                     "To Lowercase" = "lower"
                                   ))
                      ),
                      div(
                        class = "col-md-4",
                        conditionalPanel(
                          condition = "input.rename_type == 'prefix'",
                          ns = ns,
                          textInput(ns("rename_prefix"), "Prefix:")
                        ),
                        conditionalPanel(
                          condition = "input.rename_type == 'suffix'",
                          ns = ns,
                          textInput(ns("rename_suffix"), "Suffix:")
                        )
                      ),
                      div(
                        class = "col-md-4",
                        div(
                          class = "d-grid gap-2",
                          actionButton(ns("btn_rename"), "Apply Renaming", 
                                     class = "btn btn-primary btn-lg")
                        )
                      )
                    )
                  )
                ),
                
                div(
                  class = "card",
                  div(
                    class = "card-header",
                    h5("Column Name Preview")
                  ),
                  div(
                    class = "card-body",
                    # Manual mapping: one text box per variable so the user can
                    # type any new name they want.
                    conditionalPanel(
                      condition = "input.rename_type == 'manual'", ns = ns,
                      div(
                        class = "alert alert-info small",
                        icon("circle-info"),
                        " Type a new name for any variable. Blank = keep current name."
                      ),
                      # Fixed-height, scrollable box so a long variable list does
                      # not stretch the page.
                      div(
                        style = "max-height: 320px; overflow-y: auto;
                                 border: 1px solid #dee2e6; border-radius: .375rem;
                                 padding: .75rem; background: var(--bs-body-bg, #fff);",
                        uiOutput(ns("manual_rename_inputs"))
                      ),
                      hr()
                    ),
                    div(
                      class = "table-responsive",
                      DT::dataTableOutput(ns("rename_preview"))
                    )
                  )
                )
              )
            ),
            
            # Subtab: Value Recoding
            nav_panel(
              "Recode Values",
              div(
                class = "p-4",
                div(
                  class = "card",
                  div(
                    class = "card-header",
                    h5("Recode Variable Values")
                  ),
                  div(
                    class = "card-body",
                    div(
                      class = "row mb-4",
                      div(
                        class = "col-md-6",
                        selectInput(ns("recode_var"), "Select Variable:",
                                   choices = "")
                      ),
                      div(
                        class = "col-md-6",
                        div(
                          class = "alert alert-info",
                          span("Show unique values for this variable")
                        )
                      )
                    ),
                    
                    div(
                      class = "row",
                      div(
                        class = "col-md-6",
                        div(
                          class = "card",
                          div(
                            class = "card-header",
                            h6("Current Values")
                          ),
                          div(
                            class = "card-body",
                            style = "max-height: 300px; overflow-y: auto;",
                            uiOutput(ns("recode_values_list"))
                          )
                        )
                      ),
                      div(
                        class = "col-md-6",
                        div(
                          class = "card",
                          div(
                            class = "card-header",
                            h6("Recode Mapping")
                          ),
                          div(
                            class = "card-body",
                            textAreaInput(ns("recode_mapping"), 
                                        "Old Value = New Value (one per line):",
                                        rows = 8,
                                        placeholder = "Yes = 1\nNo = 0\nUnknown = NA")
                          )
                        )
                      )
                    ),
                    
                    div(
                      class = "d-grid gap-2 mt-3",
                      actionButton(ns("btn_recode"), "Apply Recoding", 
                                 class = "btn btn-primary btn-lg")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # TAB 7: CLEANING HISTORY
      nav_panel(
        "History",
        icon = icon("history"),
        div(
          class = "p-4",
          
          # History summary
          div(
            class = "row mb-4",
            div(
              class = "col-md-4",
              div(
                class = "card",
                div(
                  class = "card-body",
                  h6("Operations Performed"),
                  h2(textOutput(ns("history_count")))
                )
              )
            ),
            div(
              class = "col-md-4",
              div(
                class = "card",
                div(
                  class = "card-body",
                  h6("Rows in Original"),
                  h2(textOutput(ns("history_original_rows")))
                )
              )
            ),
            div(
              class = "col-md-4",
              div(
                class = "card",
                div(
                  class = "card-body",
                  h6("Rows in Current"),
                  h2(textOutput(ns("history_current_rows")))
                )
              )
            )
          ),
          
          # Action buttons
          div(
            class = "row mb-4",
            div(
              class = "col-md-4",
              div(
                class = "d-grid gap-2",
                actionButton(ns("btn_undo"), "↶ Undo Last", 
                           class = "btn btn-warning btn-lg",
                           icon = icon("arrow-left"))
              )
            ),
            div(
              class = "col-md-4",
              div(
                class = "d-grid gap-2",
                actionButton(ns("btn_redo"), "Redo ↷", 
                           class = "btn btn-warning btn-lg",
                           icon = icon("arrow-right"))
              )
            ),
            div(
              class = "col-md-4",
              div(
                class = "d-grid gap-2",
                actionButton(ns("btn_reset"), "⟲ Reset to Original", 
                           class = "btn btn-danger btn-lg")
              )
            )
          ),
          
          hr(),
          
          # Cleaning history log
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Cleaning Operations Log")
            ),
            div(
              class = "card-body",
              style = "max-height: 400px; overflow-y: auto;",
              uiOutput(ns("history_log")),
              div(class = "mt-2 text-end", report_button(ns, "add_cleaning_history", "Add cleaning log to Report"))
            )
          ),

          # Download the cleaned dataset for future use
          div(
            class = "card mt-3",
            div(
              class = "card-header",
              h5("Export Cleaned Dataset")
            ),
            div(
              class = "card-body",
              p("Download the current (cleaned) dataset to reuse later.",
                class = "text-muted"),
              div(
                class = "d-flex gap-2 flex-wrap",
                downloadButton(ns("download_clean_csv"), "Download CSV",
                               class = "btn btn-success"),
                downloadButton(ns("download_clean_xlsx"), "Download Excel",
                               class = "btn btn-outline-success"),
                downloadButton(ns("download_clean_rds"), "Download RDS",
                               class = "btn btn-outline-secondary")
              )
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

mod_data_cleaning_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =========================================================================
    # REACTIVE STATE FOR CLEANING MODULE
    # =========================================================================
    
    cleaning_state <- reactiveValues(
      current_data = NULL,
      original_data = NULL,
      history = list(),
      history_index = 0,
      variable_types = NULL,
      quality_score = NULL
    )
    
    # =========================================================================
    # INITIALIZE DATA & SYNC WITH APP STATE
    # =========================================================================
    
    # Initialize from app_state once data is imported. observeEvent with an
    # explicit trigger (not a plain observe) so nothing runs until
    # data_imported/processed_data actually change.
    shiny::observeEvent(
      list(app_state$data_imported, app_state$processed_data),
      {
        req(app_state$data_imported)

        # Initialize data if not already done
        if (is.null(cleaning_state$current_data)) {
          cleaning_state$original_data  <- app_state$raw_data
          cleaning_state$current_data   <- app_state$processed_data
          cleaning_state$variable_types <- app_state$variable_types
        }
      },
      ignoreInit = FALSE
    )
    
    # Sync processed data back to the backend whenever cleaning_state changes.
    # Migrated to the service layer: DatasetService is the single write path for
    # the active dataset (it bumps the data version, invalidates cache, logs,
    # and persists via the DAL). Falls back to a direct app_state write when
    # services are not wired, so the module still works standalone.
    observeEvent(cleaning_state$current_data, {
      req(app_state$data_imported)
      new_df <- cleaning_state$current_data
      req(!is.null(new_df))

      if (!is.null(services)) {
        # Skip no-op writes (e.g. the initial population that just mirrors the
        # dataset we read in) so we don't bump the version / re-persist for free.
        current <- services$data$get("processed")
        if (!identical(current, new_df)) {
          services$data$set(
            new_df,
            kind        = "processed",
            module      = "data_cleaning",
            description = "cleaning operation applied"
          )
        }
      } else {
        app_state$processed_data <- new_df
      }
    })
    
    # =========================================================================
    # VALIDATE DATA AVAILABILITY
    # =========================================================================
    
    validate_data_available <- reactive({
      req(app_state$data_imported)
      req(!is.null(cleaning_state$current_data))
      cleaning_state$current_data
    })
    
    # =========================================================================
    # TAB 1: QUALITY ASSESSMENT
    # =========================================================================
    
    quality_assessment <- reactive({
      df <- validate_data_available()
      req(df)
      calculate_quality_score(df)
    })
    
    # Quality gauge
    output$quality_gauge <- plotly::renderPlotly({
      score <- quality_assessment()$score
      
      plotly::plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = score,
        title = list(text = "Quality Score"),
        domain = list(x = c(0, 1), y = c(0, 1)),
        gauge = list(
          axis = list(range = list(0, 100)),
          bar = list(color = if (score >= 70) "#28a745" 
                           else if (score >= 40) "#ffc107" 
                           else "#dc3545"),
          steps = list(
            list(range = list(0, 40), color = "#f8d7da"),
            list(range = list(40, 70), color = "#fff3cd"),
            list(range = list(70, 100), color = "#d4edda")
          ),
          threshold = list(
            line = list(color = "red", width = 4),
            thickness = 0.75,
            value = 70
          )
        )
      )
    })
    
    # Quality metrics
    output$quality_metric_1 <- renderUI({
      score <- quality_assessment()
      div(
        h6("Missing Values", class = "text-muted"),
        h3(sprintf("%.1f%%", score$missing_pct), style = "color: #dc3545;"),
        span("Requires attention if > 10%")
      )
    })
    
    output$quality_metric_2 <- renderUI({
      score <- quality_assessment()
      div(
        h6("Duplicate Rows", class = "text-muted"),
        h3(score$duplicate_rows, style = "color: #fd7e14;"),
        span("Should be removed")
      )
    })
    
    output$quality_metric_3 <- renderUI({
      score <- quality_assessment()
      div(
        h6("Outliers", class = "text-muted"),
        h3(score$outlier_count, style = "color: #ffc107;"),
        span("Review and handle")
      )
    })
    
    output$quality_metric_4 <- renderUI({
      score <- quality_assessment()
      div(
        h6("Empty Columns", class = "text-muted"),
        h3(score$empty_columns, style = "color: #6c757d;"),
        span("Should be removed")
      )
    })
    
    # Quality issues
    output$quality_issues <- renderUI({
      score <- quality_assessment()
      
      if (length(score$issues) == 0) {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          "No major issues detected!"
        )
      } else {
        lapply(score$issues, function(issue) {
          div(
            class = "alert alert-warning alert-sm mb-2",
            icon("exclamation-triangle"),
            issue
          )
        })
      }
    })
    
    # Quality recommendations
    output$quality_recommendations <- renderUI({
      score <- quality_assessment()
      
      if (length(score$recommendations) == 0) {
        div(
          class = "alert alert-info",
          icon("circle-info"),
          "Data looks good! Continue with analysis."
        )
      } else {
        lapply(score$recommendations, function(rec) {
          div(
            class = "alert alert-info alert-sm mb-2",
            icon("lightbulb"),
            rec
          )
        })
      }
    })
    
    # =========================================================================
    # TAB 2: MISSING VALUES
    # =========================================================================
    
    # Update missing value variable selector
    observeEvent(validate_data_available(), {
      df <- validate_data_available()
      var_names <- colnames(df)
      
      updateSelectInput(session, "impute_var", choices = var_names)
      # For removal-by-variable, only offer variables that actually have NAs
      vars_with_na <- var_names[colSums(is.na(df)) > 0]
      updateSelectInput(session, "remove_na_var",
                        choices = if (length(vars_with_na)) vars_with_na else var_names)
    })
    
    # Missing value summaries
    output$missing_summary_1 <- renderUI({
      df <- validate_data_available()
      total_missing <- sum(is.na(df))
      div(
        h6("Total Missing", class = "text-muted"),
        h2(total_missing, style = "color: #dc3545;")
      )
    })
    
    output$missing_summary_2 <- renderUI({
      df <- validate_data_available()
      vars_with_missing <- sum(colSums(is.na(df)) > 0)
      div(
        h6("Variables with Missing", class = "text-muted"),
        h2(vars_with_missing, style = "color: #fd7e14;")
      )
    })
    
    output$missing_summary_3 <- renderUI({
      df <- validate_data_available()
      complete_cases <- sum(complete.cases(df))
      div(
        h6("Complete Cases", class = "text-muted"),
        h2(complete_cases, style = "color: #28a745;")
      )
    })
    
    # Missing value heatmap
    output$missing_heatmap <- plotly::renderPlotly({
      df <- validate_data_available()
      
      if (sum(is.na(df)) == 0) {
        plotly::plot_ly(
          type = "scatter",
          x = c(1),
          y = c(1),
          mode = "text",
          text = "No missing values detected",
          textposition = "middle center"
        )
      } else {
        # Create binary missing matrix (sample if large)
        n_rows <- min(nrow(df), 200)
        missing_matrix <- is.na(df[1:n_rows, ])
        
        plotly::plot_ly(
          z = missing_matrix * 1,
          x = colnames(df),
          y = 1:n_rows,
          type = "heatmap",
          colorscale = list(c(0, "#ffffff"), c(1, "#dc3545")),
          showscale = FALSE,
          hoverinfo = "x+y"
        ) %>%
          plotly::layout(
            title = "Missing Value Pattern (first 200 rows)",
            xaxis = list(title = "Variables"),
            yaxis = list(title = "Observations")
          )
      }
    })
    
    # Impute preview
    output$impute_preview <- renderText({
      df <- validate_data_available()
      
      var <- input$impute_var
      method <- input$impute_method
      
      if (is.null(var) || var == "" || !(var %in% colnames(df))) {
        return("Select a variable first")
      }
      
      col <- df[[var]]
      missing_count <- sum(is.na(col))
      
      if (missing_count == 0) {
        return(sprintf("Variable '%s' has no missing values", var))
      }
      
      method_name <- switch(method,
        "mean" = "Mean imputation",
        "median" = "Median imputation",
        "mode" = "Mode imputation",
        "zero" = "Zero imputation",
        "forward_fill" = "Forward fill",
        "backward_fill" = "Backward fill",
        "Unknown method"
      )
      
      sprintf("Will impute %d missing values\nusing %s", missing_count, method_name)
    })
    
    # Apply imputation
    observeEvent(input$btn_impute, {
      df <- validate_data_available()
      req(input$impute_var)
      req(input$impute_method)
      
      var <- input$impute_var
      method <- input$impute_method
      
      if (!(var %in% colnames(df))) {
        showNotification("Variable not found", type = "error")
        return()
      }
      
      if (sum(is.na(df[[var]])) == 0) {
        showNotification("No missing values to impute", type = "warning")
        return()
      }
      
      # Apply imputation
      df[[var]] <- impute_missing(df[[var]], method = method)
      
      # Add to history and update state
      cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
        operation = "impute",
        data = cleaning_state$current_data,
        description = sprintf("Imputed %s using %s", var, method)
      )
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data <- df
      
      showNotification(sprintf("✓ Imputed '%s' using %s", var, method), type = "message")
    })

    # -- Download cleaned dataset (CSV / Excel / RDS) ------------------------
    .clean_stamp <- function(ext) {
      paste0("cleaned_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
    }

    output$download_clean_csv <- downloadHandler(
      filename = function() .clean_stamp(".csv"),
      content  = function(file) {
        utils::write.csv(cleaning_state$current_data, file, row.names = FALSE)
      }
    )

    output$download_clean_xlsx <- downloadHandler(
      filename = function() .clean_stamp(".xlsx"),
      content  = function(file) {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(cleaning_state$current_data, file)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(cleaning_state$current_data, file)
        } else {
          # Fall back to CSV content so the download never fails outright
          utils::write.csv(cleaning_state$current_data, file, row.names = FALSE)
          showNotification(
            "Install 'openxlsx' or 'writexl' for Excel export; wrote CSV instead.",
            type = "warning")
        }
      }
    )

    output$download_clean_rds <- downloadHandler(
      filename = function() .clean_stamp(".rds"),
      content  = function(file) {
        saveRDS(cleaning_state$current_data, file)
      }
    )

    # -- Remove missing values (delete rows/cols) ---------------------------
    output$remove_na_preview <- renderText({
      df <- cleaning_state$current_data
      if (is.null(df)) return("No data loaded.")
      mode <- input$remove_na_mode %||% "rows_any"

      if (mode == "rows_any") {
        keep <- stats::complete.cases(df)
        sprintf("Will remove %s of %s rows (rows with any missing value); %s remain.",
                format(sum(!keep), big.mark = ","),
                format(nrow(df), big.mark = ","),
                format(sum(keep), big.mark = ","))
      } else if (mode == "rows_var") {
        v <- input$remove_na_var
        if (is.null(v) || !(v %in% names(df))) return("Select a variable.")
        nrm <- sum(is.na(df[[v]]))
        sprintf("Will remove %s row(s) where '%s' is missing; %s remain.",
                format(nrm, big.mark = ","), v,
                format(nrow(df) - nrm, big.mark = ","))
      } else {
        thr <- (input$remove_na_thresh %||% 50) / 100
        pct <- colSums(is.na(df)) / max(1, nrow(df))
        drop <- names(df)[pct > thr]
        if (length(drop) == 0) {
          sprintf("No columns exceed %.0f%% missing; nothing would be dropped.",
                  thr * 100)
        } else {
          sprintf("Will drop %d column(s): %s",
                  length(drop), paste(drop, collapse = ", "))
        }
      }
    })

    observeEvent(input$btn_remove_na, {
      df <- validate_data_available()
      mode <- input$remove_na_mode %||% "rows_any"
      before_r <- nrow(df); before_c <- ncol(df)

      if (mode == "rows_any") {
        df <- df[stats::complete.cases(df), , drop = FALSE]
        desc <- sprintf("Removed rows with any missing value (%d -> %d rows)",
                        before_r, nrow(df))
      } else if (mode == "rows_var") {
        v <- input$remove_na_var
        if (is.null(v) || !(v %in% names(df))) {
          showNotification("Select a valid variable", type = "warning"); return()
        }
        df <- df[!is.na(df[[v]]), , drop = FALSE]
        desc <- sprintf("Removed rows missing in '%s' (%d -> %d rows)",
                        v, before_r, nrow(df))
      } else {
        thr <- (input$remove_na_thresh %||% 50) / 100
        pct <- colSums(is.na(df)) / max(1, nrow(df))
        keep <- names(df)[pct <= thr]
        if (length(keep) == 0) {
          showNotification("That threshold would drop all columns", type = "error")
          return()
        }
        df <- df[, keep, drop = FALSE]
        desc <- sprintf("Dropped columns > %.0f%% missing (%d -> %d cols)",
                        thr * 100, before_c, ncol(df))
      }

      cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
        operation   = "remove_na",
        data        = cleaning_state$current_data,
        description = desc
      )
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data <- df
      showNotification(paste0("\u2713 ", desc), type = "message")
    })
    
    # =========================================================================
    # TAB 3: DUPLICATES
    # =========================================================================
    
    # Update ID column selector
    observeEvent(validate_data_available(), {
      df <- validate_data_available()
      updateSelectInput(session, "dup_id_column", choices = colnames(df))
    })
    
    # Duplicate summaries
    output$dup_summary_1 <- renderUI({
      df <- validate_data_available()
      dup_count <- sum(duplicated(df))
      div(
        h6("Duplicate Rows", class = "text-muted"),
        h2(dup_count, style = "color: #dc3545;")
      )
    })
    
    output$dup_summary_2 <- renderUI({
      df <- validate_data_available()
      dup_cols <- sum(duplicated(as.list(df)))
      div(
        h6("Duplicate Columns", class = "text-muted"),
        h2(dup_cols, style = "color: #fd7e14;")
      )
    })
    
    output$dup_summary_3 <- renderUI({
      df <- validate_data_available()
      total_rows <- nrow(df)
      div(
        h6("Total Rows", class = "text-muted"),
        h2(total_rows, style = "color: #0d6efd;")
      )
    })
    
    # Duplicate details table
    output$dup_details_table <- DT::renderDataTable({
      df <- validate_data_available()
      
      dup_mask <- duplicated(df, fromLast = FALSE) | duplicated(df, fromLast = TRUE)
      dup_data <- df[dup_mask, ]
      
      if (nrow(dup_data) == 0) {
        return(DT::datatable(
          data.frame(),
          options = list(dom = "t"),
          class = "stripe hover"
        ))
      }
      
      DT::datatable(
        head(dup_data, 100),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE
        ),
        class = "stripe hover",
        rownames = FALSE
      )
    })
    
    # Remove duplicates
    observeEvent(input$btn_remove_dups, {
      df <- validate_data_available()
      dup_type <- input$dup_type
      
      if (dup_type == "rows") {
        result <- detect_duplicates(df, type = "rows")
      } else if (dup_type == "columns") {
        result <- detect_duplicates(df, type = "columns")
      } else if (dup_type == "ids") {
        id_col <- input$dup_id_column
        if (is.null(id_col) || id_col == "") {
          showNotification("Please select an ID column", type = "error")
          return()
        }
        result <- detect_duplicates(df, type = "ids", id_column = id_col)
      }
      
      if (result$removed_count == 0) {
        showNotification("No duplicates found to remove", type = "warning")
      } else {
        # Add to history and update
        cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
          operation = "remove_duplicates",
          data = cleaning_state$current_data,
          description = result$message
        )
        cleaning_state$history_index <- length(cleaning_state$history)
        cleaning_state$current_data <- result$data
        
        showNotification(result$message, type = "message")
      }
    })
    
    # =========================================================================
    # TAB 4: OUTLIERS
    # =========================================================================
    
    # Update numeric variable selector for outliers
    observeEvent(validate_data_available(), {
      df <- validate_data_available()
      numeric_cols <- colnames(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "outlier_var", choices = numeric_cols)
    })
    
    # Total outliers
    output$total_outliers <- renderText({
      df <- validate_data_available()
      numeric_cols <- colnames(df)[sapply(df, is.numeric)]
      
      total <- 0
      for (col in numeric_cols) {
        outliers <- detect_outliers(df[[col]], method = "iqr")
        total <- total + sum(outliers, na.rm = TRUE)
      }
      total
    })
    
    # Variables with outliers
    output$vars_with_outliers <- renderText({
      df <- validate_data_available()
      numeric_cols <- colnames(df)[sapply(df, is.numeric)]
      
      count <- 0
      for (col in numeric_cols) {
        outliers <- detect_outliers(df[[col]], method = "iqr")
        if (sum(outliers, na.rm = TRUE) > 0) {
          count <- count + 1
        }
      }
      count
    })
    
    # Outlier visualizations
    output$outlier_boxplot <- plotly::renderPlotly({
      df <- validate_data_available()
      req(input$outlier_var)
      
      var <- input$outlier_var
      if (!(var %in% colnames(df)) || !is.numeric(df[[var]])) {
        return(plotly::plot_ly())
      }
      
      plotly::plot_ly(
        y = df[[var]],
        type = "box",
        name = var,
        boxmean = "sd"
      ) %>%
        plotly::layout(
          title = paste0("Box Plot: ", var),
          yaxis = list(title = var),
          showlegend = FALSE
        )
    })
    
    output$outlier_histogram <- plotly::renderPlotly({
      df <- validate_data_available()
      req(input$outlier_var)
      
      var <- input$outlier_var
      if (!(var %in% colnames(df)) || !is.numeric(df[[var]])) {
        return(plotly::plot_ly())
      }
      
      plotly::plot_ly(
        x = df[[var]],
        type = "histogram",
        nbinsx = 30,
        name = var,
        marker = list(color = "#0d6efd")
      ) %>%
        plotly::layout(
          title = paste0("Distribution: ", var),
          xaxis = list(title = var),
          yaxis = list(title = "Frequency"),
          showlegend = FALSE
        )
    })
    
    # Handle outliers
    observeEvent(input$btn_handle_outliers, {
      df <- validate_data_available()
      req(input$outlier_var)
      
      var <- input$outlier_var
      method <- input$outlier_method
      action <- input$outlier_action
      
      if (!(var %in% colnames(df)) || !is.numeric(df[[var]])) {
        showNotification("Invalid variable selected", type = "error")
        return()
      }
      
      outliers <- sum(detect_outliers(df[[var]], method = method), na.rm = TRUE)
      
      if (outliers == 0) {
        showNotification("No outliers detected with selected method", type = "warning")
        return()
      }
      
      # Handle outliers
      if (action == "remove") {
        df <- df[!detect_outliers(df[[var]], method = method), ]
        msg <- sprintf("Removed %d outlier rows from '%s'", outliers, var)
      } else if (action == "winsorize") {
        df[[var]] <- handle_outliers(df[[var]], method = method, action = "winsorize")
        msg <- sprintf("Winsorized %d outliers in '%s'", outliers, var)
      } else if (action == "replace") {
        df[[var]] <- handle_outliers(df[[var]], method = method, action = "replace")
        msg <- sprintf("Replaced %d outliers with NA in '%s'", outliers, var)
      }
      
      # Update history
      cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
        operation = "handle_outliers",
        data = cleaning_state$current_data,
        description = msg
      )
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data <- df
      
      showNotification(msg, type = "message")
    })
    
    # =========================================================================
    # TAB 5: TRANSFORMATIONS
    # =========================================================================
    
    # Update numeric variable selector for transformations
    observeEvent(validate_data_available(), {
      df <- validate_data_available()
      numeric_cols <- colnames(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "transform_var", choices = numeric_cols)
    })
    
    # Original distribution
    output$transform_original <- plotly::renderPlotly({
      df <- validate_data_available()
      req(input$transform_var)
      
      var <- input$transform_var
      if (!(var %in% colnames(df)) || !is.numeric(df[[var]])) {
        return(plotly::plot_ly())
      }
      
      plotly::plot_ly(
        x = df[[var]],
        type = "histogram",
        nbinsx = 30,
        marker = list(color = "#0d6efd")
      ) %>%
        plotly::layout(
          title = "Original",
          xaxis = list(title = var),
          yaxis = list(title = "Frequency"),
          showlegend = FALSE
        )
    })
    
    # Transformed preview
    output$transform_preview <- plotly::renderPlotly({
      df <- validate_data_available()
      req(input$transform_var)
      req(input$transformation)
      
      var <- input$transform_var
      trans <- input$transformation
      
      if (!(var %in% colnames(df)) || !is.numeric(df[[var]])) {
        return(plotly::plot_ly())
      }
      
      transformed <- tryCatch({
        transform_variable(df[[var]], transformation = trans)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(transformed)) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "Transformation not applicable",
            annotations = list(
              text = "This transformation cannot be applied to this variable",
              x = 0.5, y = 0.5,
              showarrow = FALSE
            )
          ))
      }
      
      transformed <- transformed[!is.na(transformed)]
      
      plotly::plot_ly(
        x = transformed,
        type = "histogram",
        nbinsx = 30,
        marker = list(color = "#28a745")
      ) %>%
        plotly::layout(
          title = "Transformed",
          xaxis = list(title = paste0(var, " (", trans, ")")),
          yaxis = list(title = "Frequency"),
          showlegend = FALSE
        )
    })
    
    # Apply transformation
    observeEvent(input$btn_transform, {
      df <- validate_data_available()
      req(input$transform_var)
      req(input$transformation)
      
      var <- input$transform_var
      trans <- input$transformation
      
      if (!(var %in% colnames(df)) || !is.numeric(df[[var]])) {
        showNotification("Invalid variable selected", type = "error")
        return()
      }
      
      # Apply transformation to new column
      new_col_name <- paste0(var, "_", trans)
      
      df[[new_col_name]] <- tryCatch({
        transform_variable(df[[var]], transformation = trans)
      }, error = function(e) {
        showNotification(sprintf("Error: %s", e$message), type = "error")
        NULL
      })
      
      if (is.null(df[[new_col_name]])) {
        return()
      }
      
      # Update history
      cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
        operation = "transform",
        data = cleaning_state$current_data,
        description = sprintf("Created '%s' from '%s' using %s transformation", 
                             new_col_name, var, trans)
      )
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data <- df
      
      showNotification(sprintf("✓ Created new column '%s'", new_col_name), 
                      type = "message")
    })
    
    # =========================================================================
    # TAB 6: RECODING & RENAMING
    # =========================================================================
    
    # Rename preview
    # Manual rename: render one text input per current column name.
    output$manual_rename_inputs <- renderUI({
      df <- validate_data_available()
      cols <- colnames(df)
      tagList(
        lapply(seq_along(cols), function(i) {
          div(
            class = "row mb-2 align-items-center",
            div(class = "col-md-5",
                tags$code(cols[i])),
            div(class = "col-md-7",
                textInput(ns(paste0("manual_name_", i)), NULL,
                          value = "", placeholder = cols[i]))
          )
        })
      )
    })

    # Compute the proposed new names given the selected rename style.
    proposed_names <- reactive({
      df <- validate_data_available()
      cols <- colnames(df)
      rename_type <- input$rename_type %||% "manual"

      if (rename_type == "prefix") {
        paste0(input$rename_prefix %||% "", cols)
      } else if (rename_type == "suffix") {
        paste0(cols, input$rename_suffix %||% "")
      } else if (rename_type == "lower") {
        tolower(cols)
      } else {
        # manual: use typed value where provided, else keep current
        vapply(seq_along(cols), function(i) {
          v <- input[[paste0("manual_name_", i)]]
          if (is.null(v) || !nzchar(trimws(v))) cols[i] else trimws(v)
        }, character(1))
      }
    })

    output$rename_preview <- DT::renderDataTable({
      df <- validate_data_available()
      preview <- data.frame(
        "Current Name" = colnames(df),
        "New Name"     = proposed_names(),
        check.names = FALSE
      )
      DT::datatable(
        preview,
        options = list(
          dom = "t",
          scrollX = TRUE,
          scrollY = "300px",       # fixed-height scroll box, header stays put
          paging  = FALSE
        ),
        class = "stripe hover",
        rownames = FALSE
      )
    })
    
    # Apply renaming
    observeEvent(input$btn_rename, {
      df <- validate_data_available()
      
      rename_type <- input$rename_type
      
      if (rename_type == "prefix") {
        prefix <- input$rename_prefix
        if (is.null(prefix) || prefix == "") {
          showNotification("Please enter a prefix", type = "warning")
          return()
        }
        df <- rename_columns(df, prefix = prefix)
      } else if (rename_type == "suffix") {
        suffix <- input$rename_suffix
        if (is.null(suffix) || suffix == "") {
          showNotification("Please enter a suffix", type = "warning")
          return()
        }
        df <- rename_columns(df, suffix = suffix)
      } else if (rename_type == "lower") {
        df <- rename_columns(df, to_lower = TRUE)
      } else {
        # manual: apply the typed names
        new_names <- proposed_names()
        if (any(duplicated(new_names))) {
          showNotification(
            paste("Duplicate names not allowed:",
                  paste(unique(new_names[duplicated(new_names)]), collapse = ", ")),
            type = "error")
          return()
        }
        if (any(!nzchar(new_names))) {
          showNotification("Variable names cannot be empty", type = "error")
          return()
        }
        colnames(df) <- new_names
      }
      
      # Update history
      cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
        operation = "rename",
        data = cleaning_state$current_data,
        description = sprintf("Applied %s renaming to columns", rename_type)
      )
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data <- df
      
      showNotification("✓ Columns renamed successfully", type = "message")
    })
    
    # Recode variable selector
    observeEvent(validate_data_available(), {
      df <- validate_data_available()
      updateSelectInput(session, "recode_var", choices = colnames(df))
    })
    
    # Show unique values for recoding
    output$recode_values_list <- renderUI({
      df <- validate_data_available()
      req(input$recode_var)
      
      var <- input$recode_var
      if (!(var %in% colnames(df))) {
        return(div("Variable not found"))
      }
      
      unique_vals <- unique(df[[var]][!is.na(df[[var]])])
      
      if (length(unique_vals) > 20) {
        unique_vals <- unique_vals[1:20]
        append_text <- "... (showing first 20)"
      } else {
        append_text <- ""
      }
      
      tags$ul(
        lapply(unique_vals, function(val) {
          tags$li(as.character(val))
        }),
        if (append_text != "") tags$li(tags$em(append_text))
      )
    })
    
    # Apply recoding
    observeEvent(input$btn_recode, {
      df <- validate_data_available()
      req(input$recode_var)
      req(input$recode_mapping)
      
      var <- input$recode_var
      mapping_text <- input$recode_mapping
      
      if (!(var %in% colnames(df))) {
        showNotification("Variable not found", type = "error")
        return()
      }
      
      # Parse mapping
      mapping_lines <- strsplit(mapping_text, "\n")[[1]]
      mapping <- list()
      
      for (line in mapping_lines) {
        line <- trimws(line)
        if (line == "") next
        
        parts <- strsplit(line, "=")[[1]]
        if (length(parts) != 2) next
        
        old_val <- trimws(parts[1])
        new_val <- trimws(parts[2])
        
        # Handle special values
        if (old_val == "NA") {
          old_val <- NA_character_
        }
        if (new_val == "NA") {
          new_val <- NA_character_
        } else if (new_val == "NULL") {
          new_val <- NULL
        }
        
        mapping[[old_val]] <- new_val
      }
      
      if (length(mapping) == 0) {
        showNotification("No valid mappings found", type = "warning")
        return()
      }
      
      # Apply recoding
      df[[var]] <- recode_variable(df[[var]], mapping)
      
      # Update history
      cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
        operation = "recode",
        data = cleaning_state$current_data,
        description = sprintf("Recoded '%s' with %d mappings", var, length(mapping))
      )
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data <- df
      
      showNotification(sprintf("✓ Recoded '%s' with %d mappings", var, length(mapping)), 
                      type = "message")
    })
    
    # =========================================================================
    # TAB 7: CLEANING HISTORY
    # =========================================================================
    
    output$history_count <- renderText({
      length(cleaning_state$history)
    })
    
    output$history_original_rows <- renderText({
      if (is.null(cleaning_state$original_data)) {
        "0"
      } else {
        format(nrow(cleaning_state$original_data), big.mark = ",")
      }
    })
    
    output$history_current_rows <- renderText({
      df <- validate_data_available()
      format(nrow(df), big.mark = ",")
    })
    
    # History log
    # Cleaning audit trail as a report-ready table. This is the reproducibility
    # record: every transform applied, in order.
    cleaning_history_df <- reactive({
      req(length(cleaning_state$history) > 0)
      h <- cleaning_state$history
      data.frame(
        Step        = seq_along(h),
        Operation   = vapply(h, function(op) as.character(op$operation %||% ""),
                             character(1)),
        Description = vapply(h, function(op) as.character(op$description %||% ""),
                             character(1)),
        stringsAsFactors = FALSE
      )
    })

    output$history_log <- renderUI({
      if (length(cleaning_state$history) == 0) {
        return(div(
          class = "alert alert-info",
          icon("circle-info"),
          "No operations performed yet"
        ))
      }
      
      lapply(rev(seq_along(cleaning_state$history)), function(i) {
        op <- cleaning_state$history[[i]]
        is_current <- i == cleaning_state$history_index
        
        div(
          class = if (is_current) "alert alert-success" else "alert alert-light",
          style = "margin-bottom: 0.5rem;",
          div(
            strong("Step ", i),
            if (is_current) span(" (current)", class = "badge bg-success ms-2")
          ),
          div(class = "small text-muted", op$description)
        )
      })
    })
    
    # Undo operation
    observeEvent(input$btn_undo, {
      if (cleaning_state$history_index > 1) {
        cleaning_state$history_index <- cleaning_state$history_index - 1
        op <- cleaning_state$history[[cleaning_state$history_index]]
        cleaning_state$current_data <- op$data
        showNotification("↶ Undid operation", type = "message")
      } else {
        showNotification("No operations to undo", type = "warning")
      }
    })
    
    # Redo operation
    observeEvent(input$btn_redo, {
      if (cleaning_state$history_index < length(cleaning_state$history)) {
        cleaning_state$history_index <- cleaning_state$history_index + 1
        op <- cleaning_state$history[[cleaning_state$history_index]]
        cleaning_state$current_data <- op$data
        showNotification("Redo ↷", type = "message")
      } else {
        showNotification("No operations to redo", type = "warning")
      }
    })
    
    # Reset to original
    observeEvent(input$btn_reset, {
      shinyjs::runjs('if (confirm("Reset to original data? This cannot be undone.")) {
        Shiny.onInputChange("confirm_reset", true);
      }')
    })
    
    observeEvent(input$confirm_reset, {
      if (input$confirm_reset) {
        cleaning_state$current_data <- cleaning_state$original_data
        cleaning_state$history <- list()
        cleaning_state$history_index <- 0
        showNotification("✓ Reset to original data", type = "message")
      }
    })
    # =========================================================================
    # ADD TO REPORT
    # =========================================================================

    register_report_button(
      input, services, "add_cleaning_history", kind = "table",
      title   = "Data cleaning log",
      payload = function() tryCatch(cleaning_history_df(), error = function(e) NULL),
      code    = function() {
        h <- cleaning_state$history
        if (!length(h)) return("")
        paste(vapply(h, function(op) sprintf("# %s", op$description %||% ""),
                     character(1)), collapse = "\n")
      })

  })
}

# =============================================================================
# END OF MODULE 3: DATA CLEANING
# =============================================================================

message("[SAP] Data Cleaning v2.5-20260708 loaded")  # SAP_MODULE_BANNER
