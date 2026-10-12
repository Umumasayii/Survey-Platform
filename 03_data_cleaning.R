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
  
  div(
    class = "sap-data-cleaning-page",
    # Header
    sap_module_header(
      "Data Cleaning",
      "Profile data quality, clean one or many columns visually, compare changes, and keep every transformation reversible.",
      icon_name = "sliders",
      badge = "Data preparation"
    ),
    
    # Tab structure
    navset_tab(
      id = ns("cleaning_tabs"),

      # NEW v5.2: visual, non-coding cleaning path. The expert tabs below
      # remain unchanged and available for experienced analysts.
      sap_guided_cleaning_ui(ns),
      
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
          ),

          # ===== HEADER MANAGEMENT =====
          div(class = "card mt-4",
            div(class = "card-header", h5(tagList(icon("heading"), " Column Headers"))),
            div(class = "card-body",
              p(class = "text-muted small",
                "If data imported without headers (columns look like V1, V2...), promote the first row to headers or rename columns yourself."),
              div(class = "row g-2 align-items-end",
                div(class = "col-md-5",
                  selectInput(ns("header_action"), "Action",
                    choices = c("Promote first data row to headers" = "promote",
                                "Set / edit column names manually" = "manual"))),
                div(class = "col-md-7",
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'manual'", ns("header_action")),
                    textInput(ns("header_manual"), "New names (comma-separated, in order)",
                              placeholder = "id, region, age, income", width = "100%")))
              ),
              uiOutput(ns("header_preview")),
              div(class = "d-flex gap-2 mt-2",
                  actionButton(ns("btn_header_preview"), "Preview",
                               class = "btn-outline-secondary btn-sm", icon = icon("eye")),
                  actionButton(ns("btn_header_apply"), "Apply headers",
                               class = "btn-primary btn-sm", icon = icon("check")))
            )
          ),

          # ===== CREATE VARIABLE - MATH =====
          div(class = "card mt-4",
            div(class = "card-header", h5(tagList(icon("calculator"), " Create Variable - Math"))),
            div(class = "card-body",
              p(class = "text-muted small", "Build a new column from two variables (or a variable and a constant)."),
              div(class = "row g-2",
                div(class = "col-md-3", selectInput(ns("math_a"), "Variable A", choices = "")),
                div(class = "col-md-2", selectInput(ns("math_op"), "Operation",
                      choices = c("A + B" = "add", "A - B" = "subtract", "A x B" = "multiply",
                                  "A / B" = "divide", "A mod B" = "modulus", "A ^ B" = "power",
                                  "pct of total (A)" = "pct"))),
                div(class = "col-md-3", selectInput(ns("math_b_kind"), "B is",
                      choices = c("another variable" = "var", "a constant" = "const"))),
                div(class = "col-md-4",
                    conditionalPanel(condition = sprintf("input['%s'] == 'var'", ns("math_b_kind")),
                      selectInput(ns("math_b_var"), "Variable B", choices = "")),
                    conditionalPanel(condition = sprintf("input['%s'] == 'const'", ns("math_b_kind")),
                      numericInput(ns("math_b_const"), "Constant", value = 1)))
              ),
              textInput(ns("math_name"), "New column name", placeholder = "e.g. income_per_child"),
              uiOutput(ns("math_preview")),
              div(class = "d-flex gap-2 mt-2",
                  actionButton(ns("btn_math_preview"), "Preview",
                               class = "btn-outline-secondary btn-sm", icon = icon("eye")),
                  actionButton(ns("btn_math_apply"), "Create variable",
                               class = "btn-primary btn-sm", icon = icon("plus")))
            )
          ),

          # ===== CATEGORIZE =====
          div(class = "card mt-4",
            div(class = "card-header", h5(tagList(icon("layer-group"), " Categorize - Bin a numeric variable"))),
            div(class = "card-body",
              p(class = "text-muted small", "Turn a continuous variable into groups (e.g. age -> age bands)."),
              div(class = "row g-2",
                div(class = "col-md-4", selectInput(ns("cat_var"), "Numeric variable", choices = "")),
                div(class = "col-md-4", selectInput(ns("cat_method"), "Method",
                      choices = c("Equal-width intervals" = "width",
                                  "Equal-frequency (quantiles)" = "freq",
                                  "Manual breakpoints" = "manual"))),
                div(class = "col-md-4",
                    conditionalPanel(condition = sprintf("input['%s'] != 'manual'", ns("cat_method")),
                      numericInput(ns("cat_n"), "Number of groups", value = 4, min = 2, max = 20)),
                    conditionalPanel(condition = sprintf("input['%s'] == 'manual'", ns("cat_method")),
                      textInput(ns("cat_breaks"), "Breakpoints (comma-separated)",
                                placeholder = "0, 18, 35, 65, 120")))
              ),
              textInput(ns("cat_name"), "New column name", placeholder = "e.g. age_group"),
              uiOutput(ns("cat_preview")),
              div(class = "d-flex gap-2 mt-2",
                  actionButton(ns("btn_cat_preview"), "Preview",
                               class = "btn-outline-secondary btn-sm", icon = icon("eye")),
                  actionButton(ns("btn_cat_apply"), "Create variable",
                               class = "btn-primary btn-sm", icon = icon("plus")))
            )
          ),

          # ===== CONDITIONAL (IF/ELSE) =====
          div(class = "card mt-4 mb-2",
            div(class = "card-header", h5(tagList(icon("code-branch"), " Create Variable - Condition (IF/ELSE)"))),
            div(class = "card-body",
              p(class = "text-muted small",
                "If a condition is true assign one value, otherwise another. E.g. if age >= 18 then Adult else Minor."),
              div(class = "row g-2 align-items-end",
                div(class = "col-md-3", selectInput(ns("cond_var"), "If variable", choices = "")),
                div(class = "col-md-2", selectInput(ns("cond_op"), "Operator",
                      choices = c("greater or equal" = "ge", "greater" = "gt", "less or equal" = "le",
                                  "less" = "lt", "equal" = "eq", "not equal" = "ne"))),
                div(class = "col-md-2", textInput(ns("cond_value"), "Value", placeholder = "18")),
                div(class = "col-md-2", textInput(ns("cond_then"), "Then", placeholder = "Adult")),
                div(class = "col-md-3", textInput(ns("cond_else"), "Else", placeholder = "Minor"))
              ),
              textInput(ns("cond_name"), "New column name", placeholder = "e.g. adult_status"),
              uiOutput(ns("cond_preview")),
              div(class = "d-flex gap-2 mt-2",
                  actionButton(ns("btn_cond_preview"), "Preview",
                               class = "btn-outline-secondary btn-sm", icon = icon("eye")),
                  actionButton(ns("btn_cond_apply"), "Create variable",
                               class = "btn-primary btn-sm", icon = icon("plus")))
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
              div(class = "mt-2 d-flex justify-content-end gap-2 align-items-center",
                  ai_assistant_ui(ns("ai_cleaning")),
                  report_button(ns, "add_cleaning_history", "Add cleaning log to Report"))
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

    # Publish a concise cleaning status to the application shell. The cleaning
    # module keeps its detailed undo history private; the shell only needs the
    # count and latest human-readable action for presentation context.
    observeEvent(cleaning_state$history_index, {
      app_state$transformation_count <- length(cleaning_state$history)
      idx <- cleaning_state$history_index
      if (idx > 0L && idx <= length(cleaning_state$history)) {
        latest <- cleaning_state$history[[idx]]
        if (!is.null(latest$description) && nzchar(latest$description)) {
          app_state$last_action <- latest$description
        }
      }
    }, ignoreInit = TRUE)
    
    # =========================================================================
    # INITIALIZE DATA & SYNC WITH APP STATE
    # =========================================================================
    
    # Initialize from app_state once data is imported. observeEvent with an
    # explicit trigger (not a plain observe) so nothing runs until
    # data_imported/processed_data actually change.
    shiny::observeEvent(
      list(app_state$data_imported, app_state$processed_data),
      {
        if (!isTRUE(app_state$data_imported) ||
            !is.data.frame(app_state$processed_data)) {
          cleaning_state$current_data <- NULL
          cleaning_state$original_data <- NULL
          cleaning_state$history <- list()
          cleaning_state$history_index <- 0L
          cleaning_state$variable_types <- NULL
          return(invisible(NULL))
        }

        incoming <- app_state$processed_data
        is_new_source <- is.null(cleaning_state$current_data) ||
          !identical(incoming, cleaning_state$current_data)

        # A new import/demo dataset starts a fresh cleaning history. A cleaning
        # operation does not reset itself because the service writes back the
        # exact same data frame currently held by cleaning_state.
        if (is_new_source) {
          cleaning_state$original_data <- app_state$raw_data
          cleaning_state$current_data <- incoming
          cleaning_state$variable_types <- app_state$variable_types
          cleaning_state$history <- list()
          cleaning_state$history_index <- 0L
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

    # Wire the Visual Cleaning Workbench into the same current_data/history state
    # used by the existing expert tools. This keeps both cleaning paths fully
    # interoperable and prevents a second, conflicting data model.
    sap_guided_cleaning_server(
      input = input, output = output, session = session, ns = ns,
      app_state = app_state, services = services, cleaning_state = cleaning_state,
      validate_data_available = validate_data_available
    )
    
    # =========================================================================
    # TAB 1: QUALITY ASSESSMENT
    # =========================================================================
    
    quality_assessment <- reactive({
      df <- validate_data_available()
      req(df)
      calculate_quality_score(df)
    })

    observeEvent(quality_assessment()$score, {
      app_state$quality_score <- quality_assessment()$score
    }, ignoreInit = FALSE)
    
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
        safe <- if (!is.null(services) && !is.null(services$privacy)) services$privacy$prepare_export(cleaning_state$current_data, "cleaned CSV export") else cleaning_state$current_data
        utils::write.csv(safe, file, row.names = FALSE)
      }
    )

    output$download_clean_xlsx <- downloadHandler(
      filename = function() .clean_stamp(".xlsx"),
      content  = function(file) {
        safe <- if (!is.null(services) && !is.null(services$privacy)) services$privacy$prepare_export(cleaning_state$current_data, "cleaned Excel export") else cleaning_state$current_data
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(safe, file)
        } else if (sap_optional_available("writexl")) {
          sap_optional_call("writexl", "write_xlsx", safe, file, feature = "Excel workbook export")
        } else {
          # Fall back to CSV content so the download never fails outright
          utils::write.csv(safe, file, row.names = FALSE)
          showNotification(
            "Install 'openxlsx' or 'writexl' for Excel export; wrote CSV instead.",
            type = "warning")
        }
      }
    )

    output$download_clean_rds <- downloadHandler(
      filename = function() .clean_stamp(".rds"),
      content  = function(file) {
        safe <- if (!is.null(services) && !is.null(services$privacy)) services$privacy$prepare_export(cleaning_state$current_data, "cleaned RDS export") else cleaning_state$current_data
        saveRDS(safe, file)
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
        return(sap_empty_plotly("Choose a valid numeric variable to display this chart."))
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
        return(sap_empty_plotly("Choose a valid numeric variable to display this chart."))
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
      all_cols_tx <- colnames(df)
      updateSelectInput(session, "math_a",    choices = numeric_cols)
      updateSelectInput(session, "math_b_var", choices = numeric_cols)
      updateSelectInput(session, "cat_var",   choices = numeric_cols)
      updateSelectInput(session, "cond_var",  choices = all_cols_tx)
    })
    
    # Original distribution
    output$transform_original <- plotly::renderPlotly({
      df <- validate_data_available()
      req(input$transform_var)
      
      var <- input$transform_var
      if (!(var %in% colnames(df)) || !is.numeric(df[[var]])) {
        return(sap_empty_plotly("Choose a valid numeric variable to display this chart."))
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
        return(sap_empty_plotly("Choose a valid numeric variable to display this chart."))
      }
      
      transformed <- tryCatch({
        transform_variable(df[[var]], transformation = trans)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(transformed)) {
        return(sap_empty_plotly("This transformation cannot be applied to the selected variable."))
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
    # TRANSFORMATIONS (extended): headers, math, categorize, conditional.
    # Each has Preview + Apply. Apply commits through the SAME history +
    # current_data pattern as the existing transforms, so undo and the
    # auto-propagation to services$data keep working.
    # =========================================================================

    .commit_tx <- function(new_df, operation, description) {
      cleaning_state$history[[length(cleaning_state$history) + 1]] <- list(
        operation = operation, data = cleaning_state$current_data,
        description = description)
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data  <- new_df
      showNotification(sprintf("✓ %s", description), type = "message")
    }

    .tx_preview_ui <- function(df, new_vals, note, src_cols) {
      keep <- utils::head(seq_len(nrow(df)), 8)
      tab  <- df[keep, intersect(src_cols, names(df)), drop = FALSE]
      tab[["-> new"]] <- new_vals[keep]
      div(class = "mt-2",
        div(class = "small text-muted mb-1", note),
        tags$div(class = "table-responsive",
          tags$table(class = "table table-sm table-striped mb-0",
            tags$thead(tags$tr(lapply(names(tab), function(h) tags$th(h)))),
            tags$tbody(lapply(seq_len(nrow(tab)), function(i)
              tags$tr(lapply(tab[i, ], function(cell)
                tags$td(format(cell, digits = 4)))))))))
    }

    .compute_math_tx <- function(df) {
      a <- input$math_a; op <- input$math_op
      if (is.null(a) || !a %in% names(df)) return(list(ok = FALSE, error = "Pick variable A."))
      va <- df[[a]]
      if (identical(input$math_b_kind, "const")) {
        vb <- input$math_b_const %||% NA; b_label <- as.character(vb)
      } else {
        b <- input$math_b_var
        if (is.null(b) || !b %in% names(df)) return(list(ok = FALSE, error = "Pick variable B."))
        vb <- df[[b]]; b_label <- b
      }
      res <- tryCatch(switch(op, add = va + vb, subtract = va - vb, multiply = va * vb,
        divide = va / vb, modulus = va %% vb, power = va ^ vb,
        pct = va / sum(va, na.rm = TRUE) * 100, va + vb), error = function(e) NULL)
      if (is.null(res)) return(list(ok = FALSE, error = "Computation failed."))
      list(ok = TRUE, value = res,
           note = sprintf("%s %s %s", a, op, if (identical(op, "pct")) "" else b_label))
    }

    .compute_cat_tx <- function(df) {
      v <- input$cat_var
      if (is.null(v) || !v %in% names(df) || !is.numeric(df[[v]]))
        return(list(ok = FALSE, error = "Pick a numeric variable."))
      x <- df[[v]]; method <- input$cat_method; breaks <- NULL
      if (identical(method, "manual")) {
        breaks <- suppressWarnings(as.numeric(trimws(strsplit(input$cat_breaks %||% "", ",")[[1]])))
        breaks <- breaks[!is.na(breaks)]
        if (length(breaks) < 2) return(list(ok = FALSE, error = "Enter at least 2 breakpoints."))
      } else {
        n <- input$cat_n %||% 4
        if (is.na(n) || n < 2) return(list(ok = FALSE, error = "Groups must be >= 2."))
        if (identical(method, "width")) {
          breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
        } else {
          breaks <- stats::quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
        }
        breaks <- unique(breaks)
        if (length(breaks) < 2) return(list(ok = FALSE, error = "Not enough distinct values."))
      }
      res <- tryCatch(cut(x, breaks = breaks, include.lowest = TRUE), error = function(e) NULL)
      if (is.null(res)) return(list(ok = FALSE, error = "Binning failed."))
      list(ok = TRUE, value = as.character(res),
           note = sprintf("%d groups from %s", length(breaks) - 1, v))
    }

    .compute_cond_tx <- function(df) {
      v <- input$cond_var
      if (is.null(v) || !v %in% names(df)) return(list(ok = FALSE, error = "Pick a variable."))
      x <- df[[v]]; raw <- input$cond_value %||% ""
      num_val <- suppressWarnings(as.numeric(raw))
      use_num <- is.numeric(x) && !is.na(num_val)
      cmp <- if (use_num) x else as.character(x)
      rhs <- if (use_num) num_val else raw
      cond <- tryCatch(switch(input$cond_op, ge = cmp >= rhs, gt = cmp > rhs,
        le = cmp <= rhs, lt = cmp < rhs, eq = cmp == rhs, ne = cmp != rhs,
        cmp == rhs), error = function(e) NULL)
      if (is.null(cond)) return(list(ok = FALSE, error = "Condition failed."))
      res <- ifelse(is.na(cond), NA, ifelse(cond, input$cond_then %||% "TRUE",
                                            input$cond_else %||% "FALSE"))
      list(ok = TRUE, value = res, note = sprintf("if %s %s %s", v, input$cond_op, raw))
    }

    .header_result_tx <- function(df) {
      if (identical(input$header_action, "promote")) {
        if (nrow(df) < 1) return(list(ok = FALSE, error = "No rows to promote."))
        new_names <- as.character(unlist(df[1, ], use.names = FALSE))
        new_names <- make.unique(make.names(trimws(new_names)))
        out <- df[-1, , drop = FALSE]; names(out) <- new_names; rownames(out) <- NULL
        out[] <- lapply(out, function(col) {
          num <- suppressWarnings(as.numeric(col))
          if (!all(is.na(num)) && sum(is.na(num)) == sum(is.na(col))) num else col
        })
        list(ok = TRUE, df = out,
             note = sprintf("Promoted row 1 to headers; %d columns", ncol(out)))
      } else {
        nm <- trimws(strsplit(input$header_manual %||% "", ",")[[1]]); nm <- nm[nzchar(nm)]
        if (length(nm) != ncol(df))
          return(list(ok = FALSE, error = sprintf("Provide exactly %d names (got %d).", ncol(df), length(nm))))
        out <- df; names(out) <- make.unique(make.names(nm))
        list(ok = TRUE, df = out, note = sprintf("Renamed %d columns", ncol(out)))
      }
    }

    # ---- HEADERS ----
    observeEvent(input$btn_header_preview, {
      df <- validate_data_available(); r <- .header_result_tx(df)
      output$header_preview <- renderUI({
        if (!isTRUE(r$ok)) return(div(class = "alert alert-warning mt-2", r$error))
        old <- names(df); new <- names(r$df)
        div(class = "mt-2", div(class = "small text-muted mb-1", r$note),
          tags$table(class = "table table-sm mb-0",
            tags$thead(tags$tr(tags$th("Old"), tags$th("New"))),
            tags$tbody(lapply(seq_along(new), function(i)
              tags$tr(tags$td(tags$code(if (i <= length(old)) old[i] else "")),
                      tags$td(tags$code(new[i])))))))
      })
    })
    observeEvent(input$btn_header_apply, {
      df <- validate_data_available(); r <- .header_result_tx(df)
      if (!isTRUE(r$ok)) { showNotification(r$error, type = "error"); return() }
      .commit_tx(r$df, "headers", r$note); output$header_preview <- renderUI(NULL)
    })

    # ---- MATH ----
    observeEvent(input$btn_math_preview, {
      df <- validate_data_available(); r <- .compute_math_tx(df)
      output$math_preview <- renderUI({
        if (!isTRUE(r$ok)) return(div(class = "alert alert-warning mt-2", r$error))
        src <- c(input$math_a, if (identical(input$math_b_kind, "var")) input$math_b_var)
        .tx_preview_ui(df, r$value, r$note, src)
      })
    })
    observeEvent(input$btn_math_apply, {
      df <- validate_data_available(); r <- .compute_math_tx(df)
      if (!isTRUE(r$ok)) { showNotification(r$error, type = "error"); return() }
      nm <- trimws(input$math_name %||% ""); if (!nzchar(nm)) nm <- paste0("new_", input$math_op)
      nm <- make.unique(c(names(df), make.names(nm)))[length(names(df)) + 1]
      df[[nm]] <- r$value
      .commit_tx(df, "math", sprintf("Created %s (%s)", nm, r$note))
      output$math_preview <- renderUI(NULL)
    })

    # ---- CATEGORIZE ----
    observeEvent(input$btn_cat_preview, {
      df <- validate_data_available(); r <- .compute_cat_tx(df)
      output$cat_preview <- renderUI({
        if (!isTRUE(r$ok)) return(div(class = "alert alert-warning mt-2", r$error))
        .tx_preview_ui(df, r$value, r$note, input$cat_var)
      })
    })
    observeEvent(input$btn_cat_apply, {
      df <- validate_data_available(); r <- .compute_cat_tx(df)
      if (!isTRUE(r$ok)) { showNotification(r$error, type = "error"); return() }
      nm <- trimws(input$cat_name %||% ""); if (!nzchar(nm)) nm <- paste0(input$cat_var, "_group")
      nm <- make.unique(c(names(df), make.names(nm)))[length(names(df)) + 1]
      df[[nm]] <- r$value
      .commit_tx(df, "categorize", sprintf("Created %s (%s)", nm, r$note))
      output$cat_preview <- renderUI(NULL)
    })

    # ---- CONDITIONAL ----
    observeEvent(input$btn_cond_preview, {
      df <- validate_data_available(); r <- .compute_cond_tx(df)
      output$cond_preview <- renderUI({
        if (!isTRUE(r$ok)) return(div(class = "alert alert-warning mt-2", r$error))
        .tx_preview_ui(df, r$value, r$note, input$cond_var)
      })
    })
    observeEvent(input$btn_cond_apply, {
      df <- validate_data_available(); r <- .compute_cond_tx(df)
      if (!isTRUE(r$ok)) { showNotification(r$error, type = "error"); return() }
      nm <- trimws(input$cond_name %||% ""); if (!nzchar(nm)) nm <- paste0(input$cond_var, "_cond")
      nm <- make.unique(c(names(df), make.names(nm)))[length(names(df)) + 1]
      df[[nm]] <- r$value
      .commit_tx(df, "conditional", sprintf("Created %s (%s)", nm, r$note))
      output$cond_preview <- renderUI(NULL)
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
    # AI INTERPRETATION (Phase B: Data Cleaning)
    # Anchor: the cleaning log. Payload is the structured step/operation table —
    # useful for a data-quality narrative of what was done to the data.
    # =========================================================================
    mod_ai_assistant_server(
      "ai_cleaning", services,
      payload = function() {
        df <- tryCatch(cleaning_history_df(), error = function(e) NULL)
        if (is.null(df) || nrow(df) == 0) return(NULL)
        list(
          analysis_type = "Data cleaning log / data quality steps",
          results = df,
          metadata = list(n_steps = nrow(df))
        )
      },
      default_style = "Research Report")

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

message("[SAP] Data Cleaning v5.1.0 with Guided Studio loaded")  # SAP_MODULE_BANNER
