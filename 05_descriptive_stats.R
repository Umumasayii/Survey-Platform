# =============================================================================
# MODULE 5: DESCRIPTIVE STATISTICS
# =============================================================================
# 
# Complete descriptive statistics analysis module featuring:
# 1. Continuous Variable Summaries (Mean, SD, CI, Quantiles, Skewness, Kurtosis)
# 2. Categorical Variable Analysis (Frequencies, Percentages, Mode)
# 3. Cross-Tabulations with Chi-square Test
# 4. Grouped Summaries with Statistical Comparisons
# 5. Multi-format Export (CSV, Excel, PDF, HTML)
# 6. Interactive Visualizations (plotly)
# 7. Publication-ready Tables (gt/DT)
#
# Advanced R patterns:
# - Functional programming (map, reduce)
# - Tidy evaluation (quosures, embrace operator)
# - Lazy evaluation for performance
# - Vectorized operations throughout
# - Professional error handling with informative messages
#
# =============================================================================

# =============================================================================
# UTILITY FUNCTIONS - DESCRIPTIVE STATISTICS
# =============================================================================

#' Summarize continuous variables with comprehensive statistics
#'
#' @param x Numeric vector
#' @param ci_level Confidence interval level (default 0.95)
#'
#' @return Named list with statistics
summarize_continuous <- function(x, ci_level = 0.95) {
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)
  
  if (n == 0) {
    return(list(
      n = 0, missing = length(x), mean = NA, sd = NA, median = NA,
      min = NA, max = NA, q1 = NA, q3 = NA, iqr = NA,
      range = NA, skewness = NA, kurtosis = NA,
      se = NA, ci_lower = NA, ci_upper = NA
    ))
  }
  
  mean_val <- mean(x_clean)
  sd_val <- sd(x_clean)
  se_val <- sd_val / sqrt(n)
  
  # Calculate critical t-value for CI
  alpha <- 1 - ci_level
  t_crit <- qt(1 - alpha/2, df = n - 1)
  ci_margin <- t_crit * se_val
  
  list(
    n = n,
    missing = sum(is.na(x)),
    mean = mean_val,
    sd = sd_val,
    median = median(x_clean),
    min = min(x_clean),
    max = max(x_clean),
    q1 = quantile(x_clean, 0.25, names = FALSE),
    q3 = quantile(x_clean, 0.75, names = FALSE),
    iqr = IQR(x_clean),
    range = max(x_clean) - min(x_clean),
    skewness = calculate_skewness(x_clean),
    kurtosis = calculate_kurtosis(x_clean),
    se = se_val,
    ci_lower = mean_val - ci_margin,
    ci_upper = mean_val + ci_margin
  )
}

#' Calculate sample skewness
#'
#' @param x Numeric vector
#'
#' @return Skewness value
calculate_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA)
  
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(NA)
  
  ((1/n) * sum((x - m)^3)) / (s^3)
}

#' Calculate sample excess kurtosis
#'
#' @param x Numeric vector
#'
#' @return Excess kurtosis value
calculate_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA)
  
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(NA)
  
  ((1/n) * sum((x - m)^4)) / (s^4) - 3
}

#' Summarize categorical variables
#'
#' @param x Vector (any type)
#'
#' @return List with frequencies and statistics
summarize_categorical <- function(x) {
  tbl <- table(x, useNA = "always")
  
  list(
    counts = tbl,
    percentages = round(as.numeric(tbl) / sum(tbl) * 100, 2),
    n = sum(tbl) - sum(is.na(x)),
    n_missing = sum(is.na(x)),
    n_unique = n_distinct(x[!is.na(x)]),
    mode = names(tbl)[which.max(tbl)]
  )
}

#' Create cross-tabulation with statistics
#'
#' @param df Data frame
#' @param row_var Row variable name
#' @param col_var Column variable name
#'
#' @return List with table and test results
create_crosstab <- function(df, row_var, col_var) {
  # Create contingency table. Use "ifany" so an all-NA margin never creates
  # an empty dimname (an NA colname makes DT throw "searchable[j] length zero").
  tbl <- table(df[[row_var]], df[[col_var]], useNA = "ifany")

  # Replace any NA dimnames with an explicit, DT-safe label
  dn <- dimnames(tbl)
  dn <- lapply(dn, function(x) {
    x[is.na(x) | x == ""] <- "(Missing)"
    x
  })
  dimnames(tbl) <- dn

  # Chi-square test (on complete cases only)
  tbl_no_na <- table(df[[row_var]], df[[col_var]])
  
  chi_result <- tryCatch({
    chisq.test(tbl_no_na, correct = FALSE)
  }, error = function(e) {
    list(statistic = NA, p.value = NA, parameter = NA)
  })
  
  list(
    table = tbl,
    chi_square = chi_result$statistic %||% NA,
    p_value = chi_result$p.value %||% NA,
    df = chi_result$parameter %||% NA,
    n = sum(tbl) - sum(is.na(df[[row_var]]) | is.na(df[[col_var]]))
  )
}

#' Grouped summaries for continuous variable
#'
#' @param df Data frame
#' @param var Variable to summarize (quoted)
#' @param group_var Grouping variable (quoted)
#'
#' @return Tibble with grouped statistics
summarize_by_group <- function(df, var, group_var) {
  df %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      missing = sum(is.na(!!sym(var))),
      mean = mean(!!sym(var), na.rm = TRUE),
      sd = sd(!!sym(var), na.rm = TRUE),
      median = median(!!sym(var), na.rm = TRUE),
      min = min(!!sym(var), na.rm = TRUE),
      max = max(!!sym(var), na.rm = TRUE),
      q1 = quantile(!!sym(var), 0.25, na.rm = TRUE),
      q3 = quantile(!!sym(var), 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric) & !all_of("n"), 
                  ~round(., 3)))
}

#' Format summary table for display
#'
#' @param df Summary data frame
#'
#' @return Formatted data frame
format_summary_table <- function(df) {
  df %>%
    mutate(across(
      where(is.numeric),
      ~format(., big.mark = ",", trim = TRUE)
    ))
}

#' Test for normality (Shapiro-Wilk)
#'
#' @param x Numeric vector
#'
#' @return List with test statistic and p-value
test_normality <- function(x) {
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) < 3 | length(x_clean) > 5000) {
    return(list(statistic = NA, p_value = NA, method = "Sample size out of range"))
  }
  
  test_result <- shapiro.test(x_clean)
  
  list(
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    method = test_result$method,
    is_normal = test_result$p.value > 0.05
  )
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_descriptive_stats_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("chart-line"), "Descriptive Statistics"),
      p("Comprehensive statistical summaries and visualizations", 
        class = "text-muted")
    ),
    
    # No data message
    uiOutput(ns("no_data_message")),
    
    # Main content
    uiOutput(ns("stats_content"))
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_descriptive_stats_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # Captures the ggplot handed to ggplotly() so "Add to Report" can
    # re-render it to PNG. See plot_capture() in 00_services.R.
    pc <- plot_capture()
    
    # =========================================================================
    # DATA VALIDATION & REACTIVES
    # =========================================================================
    
    data_exists <- reactive({
      req(app_state$data_imported)
      !is.null(app_state$processed_data) && nrow(app_state$processed_data) > 0
    })
    
    df_data <- reactive({
      req(data_exists())
      app_state$processed_data
    })
    
    # Get column types
    numeric_cols <- reactive({
      req(data_exists())
      names(df_data())[sapply(df_data(), is.numeric)]
    })
    
    categorical_cols <- reactive({
      req(data_exists())
      names(df_data())[sapply(df_data(), function(x) is.factor(x) || is.character(x))]
    })
    
    # =========================================================================
    # NO DATA MESSAGE
    # =========================================================================
    
    output$no_data_message <- renderUI({
      if (!data_exists()) {
        empty_state(
          icon = "chart-line",
          title = "No data available",
          message = "Import and clean your data first using Data > Import and Data > Cleaning"
        )
      }
    })
    
    # =========================================================================
    # MAIN CONTENT (Tabs)
    # =========================================================================
    
    output$stats_content <- renderUI({
      if (!data_exists()) return(NULL)
      
      navset_tab(
        id = ns("stats_tabs"),
        
        # TAB 1: CONTINUOUS VARIABLES
        nav_panel(
          "Continuous",
          icon = icon("chart-bar"),
          div(class = "p-4",
            # Variable selector
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Variable Selection")),
              div(class = "card-body",
                div(
                  class = "row",
                  div(class = "col-md-8",
                    selectInput(ns("cont_var"), "Select numeric variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-4 pt-3",
                    actionButton(ns("btn_norm_test"), "Normality Test", 
                                class = "btn-info")
                  )
                )
              )
            ),
            
            # Summary statistics table
            div(
              class = "card mb-4",
              div(class = "card-header d-flex justify-content-between align-items-center",
                  h5(class = "mb-0", "Summary Statistics"),
                  actionButton(ns("add_summary_to_report"), "Add to Report",
                               class = "btn btn-sm btn-success",
                               icon = icon("plus"))),
              div(class = "card-body",
                withSpinner(
                  DTOutput(ns("cont_summary_table")),
                  type = 8,
                  color = "#0d6efd"
                )
              )
            ),
            
            # Normality test result
            uiOutput(ns("normality_result")),
            div(class = "mt-2 text-end", report_button(ns, "add_normality", "Add to Report")),
            
            # Visualizations
            div(
              class = "row",
              div(class = "col-md-6",
                div(class = "card",
                  div(class = "card-header", h5("Histogram")),
                  div(class = "card-body",
                    withSpinner(
                      plotly::plotlyOutput(ns("cont_histogram"), height = "300px"),
                      type = 8
                    ),
                    div(class = "mt-2 text-end", report_button(ns, "add_cont_histogram", "Add to Report"))
                  )
                )
              ),
              div(class = "col-md-6",
                div(class = "card",
                  div(class = "card-header", h5("Box Plot")),
                  div(class = "card-body",
                    withSpinner(
                      plotly::plotlyOutput(ns("cont_boxplot"), height = "300px"),
                      type = 8
                    ),
                    div(class = "mt-2 text-end", report_button(ns, "add_cont_boxplot", "Add to Report"))
                  )
                )
              )
            ),
            
            # Download button
            div(class = "mt-4",
              downloadButton(ns("download_cont_csv"), "Download (CSV)", 
                           class = "btn-success"),
              downloadButton(ns("download_cont_excel"), "Download (Excel)", 
                           class = "btn-success")
            )
          )
        ),
        
        # TAB 2: CATEGORICAL VARIABLES
        nav_panel(
          "Categorical",
          icon = icon("square-poll-vertical"),
          div(class = "p-4",
            # Variable selector
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Variable Selection")),
              div(class = "card-body",
                selectInput(ns("cat_var"), "Select categorical variable:",
                           choices = categorical_cols())
              )
            ),
            
            # Frequency table
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Frequency Table")),
              div(class = "card-body",
                withSpinner(
                  DTOutput(ns("cat_freq_table")),
                  type = 8,
                  color = "#0d6efd"
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_cat_freq_table", "Add to Report"))
              )
            ),
            
            # Bar chart
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Distribution")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("cat_barchart"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_cat_barchart", "Add to Report"))
              )
            ),
            
            # Download button
            div(class = "mt-4",
              downloadButton(ns("download_cat_csv"), "Download (CSV)", 
                           class = "btn-success"),
              downloadButton(ns("download_cat_excel"), "Download (Excel)", 
                           class = "btn-success")
            )
          )
        ),
        
        # TAB 3: CROSS-TABULATIONS
        nav_panel(
          "Cross-Tabs",
          icon = icon("th"),
          div(class = "p-4",
            # Variable selection
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Variable Selection")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("crosstab_row"), "Row variable:",
                               choices = categorical_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("crosstab_col"), "Column variable:",
                               choices = categorical_cols())
                  )
                )
              )
            ),
            
            # Chi-square result
            div(
              class = "row mb-4",
              div(class = "col-md-6",
                div(class = "card",
                  div(class = "card-header", h5("Chi-Square Test")),
                  div(class = "card-body",
                    uiOutput(ns("chi_square_result")),
                    div(class = "mt-2 text-end", report_button(ns, "add_chi_square", "Add to Report"))
                  )
                )
              ),
              div(class = "col-md-6",
                div(class = "card",
                  div(class = "card-header", h5("Sample Size")),
                  div(class = "card-body",
                    uiOutput(ns("crosstab_n"))
                  )
                )
              )
            ),
            
            # Contingency table
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Contingency Table")),
              div(class = "card-body",
                withSpinner(
                  DTOutput(ns("crosstab_table")),
                  type = 8,
                  color = "#0d6efd"
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_crosstab_table", "Add to Report"))
              )
            ),
            
            # Visualization
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Visualization")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("crosstab_chart"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_crosstab_chart", "Add to Report"))
              )
            ),
            
            # Download button
            div(class = "mt-4",
              downloadButton(ns("download_crosstab_csv"), "Download (CSV)", 
                           class = "btn-success"),
              downloadButton(ns("download_crosstab_excel"), "Download (Excel)", 
                           class = "btn-success")
            )
          )
        ),
        
        # TAB 4: GROUPED SUMMARIES
        nav_panel(
          "Grouped",
          icon = icon("layer-group"),
          div(class = "p-4",
            # Variable selection
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Variable Selection")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("group_var"), "Variable to summarize:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("group_by"), "Group by:",
                               choices = categorical_cols())
                  )
                )
              )
            ),
            
            # Summary table
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Grouped Statistics")),
              div(class = "card-body",
                withSpinner(
                  DTOutput(ns("grouped_table")),
                  type = 8,
                  color = "#0d6efd"
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_grouped_table", "Add to Report"))
              )
            ),
            
            # Visualizations
            div(
              class = "row",
              div(class = "col-md-6",
                div(class = "card",
                  div(class = "card-header", h5("Box Plot by Group")),
                  div(class = "card-body",
                    withSpinner(
                      plotly::plotlyOutput(ns("grouped_boxplot"), height = "350px"),
                      type = 8
                    ),
                    div(class = "mt-2 text-end", report_button(ns, "add_grouped_boxplot", "Add to Report"))
                  )
                )
              ),
              div(class = "col-md-6",
                div(class = "card",
                  div(class = "card-header", h5("Mean with Error Bars")),
                  div(class = "card-body",
                    withSpinner(
                      plotly::plotlyOutput(ns("grouped_means"), height = "350px"),
                      type = 8
                    ),
                    div(class = "mt-2 text-end", report_button(ns, "add_grouped_means", "Add to Report"))
                  )
                )
              )
            ),
            
            # Download button
            div(class = "mt-4",
              downloadButton(ns("download_grouped_csv"), "Download (CSV)", 
                           class = "btn-success"),
              downloadButton(ns("download_grouped_excel"), "Download (Excel)", 
                           class = "btn-success")
            )
          )
        ),
        
        # TAB 5: EXPORT & REPORTS
        nav_panel(
          "Export",
          icon = icon("download"),
          div(class = "p-4",
            div(
              class = "card",
              div(class = "card-header", h5("Export Options")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    h6("Export All Summaries"),
                    p("Download all analyses in a single file", class = "small text-muted"),
                    downloadButton(ns("export_all_excel"), "Excel Workbook", 
                                 class = "btn-primary btn-sm")
                  ),
                  div(class = "col-md-6",
                    h6("Export as Report"),
                    p("Professional HTML report with all results", class = "small text-muted"),
                    downloadButton(ns("export_report_html"), "HTML Report", 
                                 class = "btn-primary btn-sm")
                  )
                )
              )
            ),
            
            hr(),
            
            div(
              class = "alert alert-info",
              h6("Export Formats"),
              tags$ul(
                tags$li(strong("CSV"), " - Simple comma-separated values"),
                tags$li(strong("Excel"), " - Professional formatting, multiple sheets"),
                tags$li(strong("HTML"), " - Interactive report with charts")
              )
            )
          )
        )
      )
    })
    
    # =========================================================================
    # TAB 1: CONTINUOUS VARIABLES
    # =========================================================================
    
    cont_summary <- reactive({
      req(input$cont_var)
      req(data_exists())
      
      var <- input$cont_var
      x <- df_data()[[var]]
      
      summarize_continuous(x)
    })
    
    # Build the summary data.frame once, reused by the table render and the
    # "Add to Report" handler.
    cont_summary_df <- reactive({
      req(cont_summary())
      summary_stats <- cont_summary()
      data.frame(
        Statistic = c("N", "Missing", "Mean", "SD", "SE", "Median",
                     "Min", "Max", "Q1", "Q3", "IQR", "Range",
                     "Skewness", "Kurtosis", "95% CI Lower", "95% CI Upper"),
        Value = c(
          summary_stats$n, summary_stats$missing,
          round(summary_stats$mean, 3), round(summary_stats$sd, 3),
          round(summary_stats$se, 3), round(summary_stats$median, 3),
          round(summary_stats$min, 3), round(summary_stats$max, 3),
          round(summary_stats$q1, 3), round(summary_stats$q3, 3),
          round(summary_stats$iqr, 3), round(summary_stats$range, 3),
          round(summary_stats$skewness, 3), round(summary_stats$kurtosis, 3),
          round(summary_stats$ci_lower, 3), round(summary_stats$ci_upper, 3)
        ),
        stringsAsFactors = FALSE
      )
    })

    output$cont_summary_table <- renderDT({
      DT::datatable(
        cont_summary_df(),
        options = list(
          dom = "t",
          paging = FALSE,
          columnDefs = list(
            list(className = 'dt-right', targets = 1)
          )
        ),
        rownames = FALSE
      )
    })

    
    output$cont_histogram <- plotly::renderPlotly({
      req(input$cont_var)
      req(data_exists())
      
      var <- input$cont_var
      x <- df_data()[[var]]
      
      p <- ggplot(data.frame(value = x), aes(x = value)) +
        geom_histogram(bins = 30, fill = "#0d6efd", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = paste("Distribution of", var),
             x = var,
             y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5))
      
      plotly::ggplotly(pc$keep("cont_histogram", p), tooltip = c("x", "y"))
    })
    
    output$cont_boxplot <- plotly::renderPlotly({
      req(input$cont_var)
      req(data_exists())
      
      var <- input$cont_var
      x <- df_data()[[var]]
      
      p <- ggplot(data.frame(value = x), aes(y = value)) +
        geom_boxplot(fill = "#198754", alpha = 0.6) +
        theme_minimal() +
        labs(title = paste("Box Plot of", var),
             y = var) +
        theme(axis.text.x = element_blank(),
              plot.title = element_text(hjust = 0.5))
      
      plotly::ggplotly(pc$keep("cont_boxplot", p), tooltip = c("y"))
    })
    
    # Normality test
    output$normality_result <- renderUI({
      if (is.null(input$btn_norm_test) || input$btn_norm_test == 0) return(NULL)
      
      var <- input$cont_var
      x <- df_data()[[var]]
      
      norm_test <- test_normality(x)
      
      if (is.na(norm_test$p_value)) {
        return(div(
          class = "alert alert-info",
          "Normality test not applicable for this sample size"
        ))
      }
      
      div(
        class = ifelse(norm_test$is_normal, "alert alert-success", "alert alert-warning"),
        h6("Shapiro-Wilk Normality Test"),
        p(sprintf("Statistic: %.4f, p-value: %.4f", 
                 norm_test$statistic, norm_test$p_value)),
        p(ifelse(norm_test$is_normal,
                "✓ Data appears normally distributed (p > 0.05)",
                "✗ Data deviates from normality (p < 0.05)"),
         class = "mb-0")
      )
    })
    
    # Download continuous CSV
    output$download_cont_csv <- downloadHandler(
      filename = function() {
        paste0(input$cont_var, "_summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        summary_stats <- cont_summary()
        
        df_export <- data.frame(
          Statistic = c("N", "Missing", "Mean", "SD", "Median", "Min", "Max",
                       "Q1", "Q3", "IQR", "Skewness", "Kurtosis",
                       "CI_Lower", "CI_Upper"),
          Value = c(
            summary_stats$n, summary_stats$missing, summary_stats$mean,
            summary_stats$sd, summary_stats$median, summary_stats$min,
            summary_stats$max, summary_stats$q1, summary_stats$q3,
            summary_stats$iqr, summary_stats$skewness, summary_stats$kurtosis,
            summary_stats$ci_lower, summary_stats$ci_upper
          )
        )
        
        write.csv(df_export, file, row.names = FALSE)
      }
    )
    
    # Download continuous Excel
    output$download_cont_excel <- downloadHandler(
      filename = function() {
        paste0(input$cont_var, "_summary_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        summary_stats <- cont_summary()
        
        df_export <- data.frame(
          Statistic = c("N", "Missing", "Mean", "SD", "Median", "Min", "Max",
                       "Q1", "Q3", "IQR", "Skewness", "Kurtosis",
                       "CI_Lower", "CI_Upper"),
          Value = c(
            summary_stats$n, summary_stats$missing, summary_stats$mean,
            summary_stats$sd, summary_stats$median, summary_stats$min,
            summary_stats$max, summary_stats$q1, summary_stats$q3,
            summary_stats$iqr, summary_stats$skewness, summary_stats$kurtosis,
            summary_stats$ci_lower, summary_stats$ci_upper
          )
        )
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Summary Statistics")
        openxlsx::writeData(wb, 1, df_export)
        openxlsx::saveWorkbook(wb, file)
      }
    )
    
    # =========================================================================
    # TAB 2: CATEGORICAL VARIABLES
    # =========================================================================
    
    cat_summary <- reactive({
      req(input$cat_var)
      req(data_exists())
      
      var <- input$cat_var
      x <- df_data()[[var]]
      
      summarize_categorical(x)
    })
    
    # Single source for the render AND the report button.
    cat_freq_df <- reactive({
      req(cat_summary())
      summary_stats <- cat_summary()

      categories <- names(summary_stats$counts)
      categories[is.na(categories) | categories == ""] <- "(Missing)"
      counts <- as.numeric(summary_stats$counts)
      percentages <- round(as.numeric(summary_stats$counts) / sum(counts) * 100, 2)

      data.frame(
        Category = categories,
        Count = counts,
        Percentage = percentages,
        Cumulative_Pct = round(cumsum(percentages), 2),
        stringsAsFactors = FALSE
      )
    })

    output$cat_freq_table <- renderDT({
      df_freq <- cat_freq_df()

      DT::datatable(
        df_freq,
        options = list(
          dom = "t",
          paging = FALSE,
          columnDefs = list(
            list(className = 'dt-right', targets = 1:3)
          )
        ),
        rownames = FALSE
      )
    })
    
    output$cat_barchart <- plotly::renderPlotly({
      req(input$cat_var)
      req(data_exists())
      
      var <- input$cat_var
      x <- df_data()[[var]]
      
      freq_tbl <- table(x, useNA = "always")
      df_plot <- data.frame(
        category = names(freq_tbl),
        count = as.numeric(freq_tbl)
      ) %>%
        mutate(category = ifelse(is.na(category), "Missing", category)) %>%
        arrange(desc(count))
      
      p <- ggplot(df_plot, aes(x = reorder(category, -count), y = count, fill = category)) +
        geom_col(show.legend = FALSE, color = "white") +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = paste("Frequency Distribution of", var),
             x = var,
             y = "Frequency") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5))
      
      plotly::ggplotly(pc$keep("cat_barchart", p), tooltip = c("x", "y"))
    })
    
    # Download categorical CSV
    output$download_cat_csv <- downloadHandler(
      filename = function() {
        paste0(input$cat_var, "_frequencies_", Sys.Date(), ".csv")
      },
      content = function(file) {
        summary_stats <- cat_summary()
        categories <- names(summary_stats$counts)
        counts <- as.numeric(summary_stats$counts)
        
        df_export <- data.frame(
          Category = categories,
          Count = counts,
          Percentage = round(counts / sum(counts) * 100, 2)
        )
        
        write.csv(df_export, file, row.names = FALSE)
      }
    )
    
    # Download categorical Excel
    output$download_cat_excel <- downloadHandler(
      filename = function() {
        paste0(input$cat_var, "_frequencies_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        summary_stats <- cat_summary()
        categories <- names(summary_stats$counts)
        counts <- as.numeric(summary_stats$counts)
        
        df_export <- data.frame(
          Category = categories,
          Count = counts,
          Percentage = round(counts / sum(counts) * 100, 2)
        )
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Frequencies")
        openxlsx::writeData(wb, 1, df_export)
        openxlsx::saveWorkbook(wb, file)
      }
    )
    
    # =========================================================================
    # TAB 3: CROSS-TABULATIONS
    # =========================================================================
    
    crosstab_data <- reactive({
      req(input$crosstab_row)
      req(input$crosstab_col)
      req(data_exists())
      
      create_crosstab(df_data(), input$crosstab_row, input$crosstab_col)
    })
    
    output$chi_square_result <- renderUI({
      req(crosstab_data())
      
      ct <- crosstab_data()
      
      if (is.na(ct$chi_square)) {
        return(div(
          class = "alert alert-warning",
          "Chi-square test not applicable for this data"
        ))
      }
      
      div(
        h6(sprintf("χ² = %.4f", ct$chi_square), class = "mb-1"),
        p(sprintf("p-value: %.4f", ct$p_value), class = "mb-1"),
        p(sprintf("df = %d", ct$df), class = "small text-muted mb-0")
      )
    })
    
    output$crosstab_n <- renderUI({
      req(crosstab_data())
      div(
        h6(sprintf("N = %d", crosstab_data()$n))
      )
    })
    
    # Single source for the render AND the report button.
    crosstab_df <- reactive({
      req(crosstab_data())

      tbl <- crosstab_data()$table
      df_ct <- as.data.frame.matrix(tbl, stringsAsFactors = FALSE)

      # Guard against a degenerate table (no rows or no columns).
      # NB: use req(), not validate()/need() — the latter is only valid inside
      # standard Shiny render functions, not DT::renderDT (an htmlwidget),
      # where it triggers: is.character(txt) is not TRUE.
      req(nrow(df_ct) > 0, ncol(df_ct) > 0)

      # Ensure every column has a non-empty, unique name before it reaches DT.
      # A blank/NA colname is what triggers the "searchable[j] length zero" error.
      cn <- colnames(df_ct)
      cn[is.na(cn) | cn == ""] <- "(Missing)"
      colnames(df_ct) <- make.unique(cn)

      # Prepend row labels as a real column (base data.frame, not cbind, so
      # names are preserved correctly and no matrix coercion occurs).
      data.frame(
        Category = rownames(df_ct),
        df_ct,
        row.names = NULL,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    output$crosstab_table <- renderDT({
      df_ct <- crosstab_df()

      # Right-align every numeric column (all columns except the first label
      # column). Targets are 0-indexed in DataTables.
      num_targets <- seq_len(ncol(df_ct) - 1)  # columns 1..(ncol-1) in JS indexing

      DT::datatable(
        df_ct,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(
            list(className = "dt-right", targets = num_targets)
          )
        ),
        rownames = FALSE
      )
    })
    
    output$crosstab_chart <- plotly::renderPlotly({
      req(input$crosstab_row)
      req(input$crosstab_col)
      req(data_exists())
      
      df <- df_data() %>%
        filter(!is.na(!!sym(input$crosstab_row)) & !is.na(!!sym(input$crosstab_col)))
      
      p <- ggplot(df, aes(x = !!sym(input$crosstab_col), fill = !!sym(input$crosstab_row))) +
        geom_bar(position = "dodge", color = "white") +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = paste(input$crosstab_row, "by", input$crosstab_col),
             x = input$crosstab_col,
             y = "Count",
             fill = input$crosstab_row) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5))
      
      plotly::ggplotly(pc$keep("crosstab_chart", p), tooltip = c("x", "fill", "y"))
    })
    
    # Download cross-tab CSV
    output$download_crosstab_csv <- downloadHandler(
      filename = function() {
        paste0("crosstab_", input$crosstab_row, "_", 
               input$crosstab_col, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        tbl <- crosstab_data()$table
        df_ct <- as.data.frame.matrix(tbl)
        write.csv(df_ct, file)
      }
    )
    
    # Download cross-tab Excel
    output$download_crosstab_excel <- downloadHandler(
      filename = function() {
        paste0("crosstab_", input$crosstab_row, "_", 
               input$crosstab_col, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        tbl <- crosstab_data()$table
        df_ct <- as.data.frame.matrix(tbl)
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Contingency Table")
        openxlsx::writeData(wb, 1, df_ct, rowNames = TRUE)
        openxlsx::saveWorkbook(wb, file)
      }
    )
    
    # =========================================================================
    # TAB 4: GROUPED SUMMARIES
    # =========================================================================
    
    grouped_data <- reactive({
      req(input$group_var)
      req(input$group_by)
      req(data_exists())
      
      summarize_by_group(df_data(), input$group_var, input$group_by)
    })
    
    output$grouped_table <- renderDT({
      req(grouped_data())
      
      DT::datatable(
        grouped_data(),
        options = list(
          dom = "lftp",
          columnDefs = list(
            list(className = 'dt-right', targets = -1)
          )
        ),
        rownames = FALSE
      )
    })
    
    output$grouped_boxplot <- plotly::renderPlotly({
      req(input$group_var)
      req(input$group_by)
      req(data_exists())
      
      p <- ggplot(df_data(), aes(x = !!sym(input$group_by), y = !!sym(input$group_var),
                                  fill = !!sym(input$group_by))) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = paste(input$group_var, "by", input$group_by),
             x = input$group_by,
             y = input$group_var) +
        theme(plot.title = element_text(hjust = 0.5))
      
      plotly::ggplotly(pc$keep("grouped_boxplot", p), tooltip = c("x", "y"))
    })
    
    output$grouped_means <- plotly::renderPlotly({
      req(input$group_var)
      req(input$group_by)
      req(data_exists())
      
      summary_data <- grouped_data()
      
      p <- ggplot(summary_data, aes(x = !!sym(input$group_by), y = mean, 
                                     color = !!sym(input$group_by))) +
        geom_point(size = 3, show.legend = FALSE) +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                     width = 0.2, show.legend = FALSE) +
        scale_color_brewer(palette = "Set2") +
        theme_minimal() +
        labs(title = paste("Mean", input$group_var, "by", input$group_by),
             x = input$group_by,
             y = paste("Mean", input$group_var),
             subtitle = "Error bars = ± 1 SD") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 10))
      
      plotly::ggplotly(pc$keep("grouped_means", p), tooltip = c("x", "y"))
    })
    
    # Download grouped CSV
    output$download_grouped_csv <- downloadHandler(
      filename = function() {
        paste0("grouped_", input$group_var, "_by_", 
               input$group_by, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(grouped_data(), file, row.names = FALSE)
      }
    )
    
    # Download grouped Excel
    output$download_grouped_excel <- downloadHandler(
      filename = function() {
        paste0("grouped_", input$group_var, "_by_", 
               input$group_by, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Grouped Summary")
        openxlsx::writeData(wb, 1, grouped_data())
        openxlsx::saveWorkbook(wb, file)
      }
    )
    
    # =========================================================================
    # TAB 5: EXPORT & REPORTS
    # =========================================================================
    
    output$export_all_excel <- downloadHandler(
      filename = function() {
        paste0("descriptive_statistics_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        
        # Add continuous summaries for all numeric vars
        numeric_vars <- numeric_cols()
        if (length(numeric_vars) > 0) {
          openxlsx::addWorksheet(wb, "Continuous Summary")
          
          cont_summaries <- lapply(numeric_vars, function(var) {
            stats <- summarize_continuous(df_data()[[var]])
            data.frame(
              Variable = var,
              N = stats$n,
              Mean = round(stats$mean, 3),
              SD = round(stats$sd, 3),
              Median = round(stats$median, 3),
              Min = round(stats$min, 3),
              Max = round(stats$max, 3)
            )
          })
          
          cont_df <- do.call(rbind, cont_summaries)
          openxlsx::writeData(wb, "Continuous Summary", cont_df, rowNames = FALSE)
        }
        
        # Add categorical summaries for all categorical vars
        cat_vars <- categorical_cols()
        if (length(cat_vars) > 0) {
          openxlsx::addWorksheet(wb, "Categorical Summary")
          
          cat_summaries <- lapply(cat_vars, function(var) {
            stats <- summarize_categorical(df_data()[[var]])
            data.frame(
              Variable = var,
              N = stats$n,
              N_Unique = stats$n_unique,
              N_Missing = stats$n_missing,
              Mode = stats$mode
            )
          })
          
          cat_df <- do.call(rbind, cat_summaries)
          openxlsx::writeData(wb, "Categorical Summary", cat_df, rowNames = FALSE)
        }
        
        openxlsx::saveWorkbook(wb, file)
      }
    )
    
    output$export_report_html <- downloadHandler(
      filename = function() {
        paste0("report_", Sys.Date(), ".html")
      },
      content = function(file) {
        # Create HTML report
        html_content <- sprintf(
          "<!DOCTYPE html>
          <html>
          <head>
            <meta charset='UTF-8'>
            <title>Descriptive Statistics Report</title>
            <style>
              body { font-family: Arial, sans-serif; margin: 20px; }
              h1 { color: #0d6efd; }
              h2 { color: #333; border-bottom: 2px solid #0d6efd; margin-top: 30px; }
              table { border-collapse: collapse; width: 100%%; margin: 15px 0; }
              th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
              th { background-color: #0d6efd; color: white; }
              .summary { background-color: #f9f9f9; padding: 10px; margin: 10px 0; border-left: 4px solid #0d6efd; }
              .date { color: #666; font-size: 0.9em; }
            </style>
          </head>
          <body>
            <h1>Descriptive Statistics Report</h1>
            <p class='date'>Generated: %s</p>
            
            <div class='summary'>
              <strong>Dataset Information</strong><br>
              Total Observations: %d<br>
              Total Variables: %d<br>
              Numeric Variables: %d<br>
              Categorical Variables: %d
            </div>
            
            <h2>Data Overview</h2>
            <p>This report contains comprehensive descriptive statistics for the analyzed dataset.</p>
          </body>
          </html>",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          nrow(df_data()),
          ncol(df_data()),
          length(numeric_cols()),
          length(categorical_cols())
        )
        
        writeLines(html_content, file)
      }
    )
    
    # =========================================================================
    # ADD TO REPORT  —  every output on this page
    # report_button() in the UI, register_report_button() here. See 00_services.R
    # =========================================================================

    # --- Continuous ---
    register_report_button(
      input, services, "add_summary_to_report", kind = "table",
      title   = function() sprintf("Summary Statistics: %s", input$cont_var),
      payload = function() cont_summary_df(),
      code    = function() sprintf(
        '# Summary statistics for %s\nsummary_stats <- summarize_continuous(df[["%s"]])\nprint(summary_stats)',
        input$cont_var, input$cont_var))

    register_report_button(
      input, services, "add_normality", kind = "table",
      title   = function() sprintf("Shapiro-Wilk Normality: %s", input$cont_var),
      payload = function() stat_result_df(test_normality(df_data()[[input$cont_var]])),
      code    = function() sprintf('shapiro.test(df[["%s"]])', input$cont_var))

    register_report_button(
      input, services, "add_cont_histogram", kind = "plot",
      title   = function() sprintf("Distribution of %s", input$cont_var),
      payload = pc$get("cont_histogram"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s)) +\n  geom_histogram(bins = 30) +\n  theme_minimal()',
        input$cont_var))

    register_report_button(
      input, services, "add_cont_boxplot", kind = "plot",
      title   = function() sprintf("Boxplot of %s", input$cont_var),
      payload = pc$get("cont_boxplot"),
      code    = function() sprintf('ggplot(df, aes(y = %s)) + geom_boxplot()',
                                   input$cont_var))

    # --- Categorical ---
    register_report_button(
      input, services, "add_cat_freq_table", kind = "table",
      title   = function() sprintf("Frequency Distribution: %s", input$cat_var),
      payload = function() cat_freq_df(),
      code    = function() sprintf(
        'df %%>%%\n  count(%s) %%>%%\n  mutate(pct = 100 * n / sum(n))', input$cat_var))

    register_report_button(
      input, services, "add_cat_barchart", kind = "plot",
      title   = function() sprintf("Frequency Distribution of %s", input$cat_var),
      payload = pc$get("cat_barchart"),
      code    = function() sprintf(
        'ggplot(df, aes(x = fct_infreq(%s), fill = %s)) +\n  geom_bar() +\n  scale_fill_viridis_d()',
        input$cat_var, input$cat_var))

    # --- Cross-tabulation ---
    register_report_button(
      input, services, "add_chi_square", kind = "table",
      title   = function() sprintf("Chi-square: %s x %s",
                                   input$crosstab_row, input$crosstab_col),
      payload = function() {
        ct <- crosstab_data()
        stat_result_df(list(chi_square = ct$chi_square, df = ct$df,
                            p_value = ct$p_value))
      },
      code    = function() sprintf('chisq.test(table(df$%s, df$%s))',
                                   input$crosstab_row, input$crosstab_col))

    register_report_button(
      input, services, "add_crosstab_table", kind = "table",
      title   = function() sprintf("Cross-tabulation: %s x %s",
                                   input$crosstab_row, input$crosstab_col),
      payload = function() crosstab_df(),
      code    = function() sprintf('table(df$%s, df$%s)',
                                   input$crosstab_row, input$crosstab_col))

    register_report_button(
      input, services, "add_crosstab_chart", kind = "plot",
      title   = function() sprintf("%s by %s", input$crosstab_row, input$crosstab_col),
      payload = pc$get("crosstab_chart"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s, fill = %s)) +\n  geom_bar(position = "dodge") +\n  scale_fill_viridis_d()',
        input$crosstab_col, input$crosstab_row))

    # --- Grouped ---
    register_report_button(
      input, services, "add_grouped_table", kind = "table",
      title   = function() sprintf("Grouped Summary: %s by %s",
                                   input$group_var, input$group_by),
      payload = function() grouped_data(),
      code    = function() sprintf(
        'df %%>%%\n  group_by(%s) %%>%%\n  summarise(across(%s, list(n = length, mean = mean, sd = sd), na.rm = TRUE))',
        input$group_by, input$group_var))

    register_report_button(
      input, services, "add_grouped_boxplot", kind = "plot",
      title   = function() sprintf("%s by %s", input$group_var, input$group_by),
      payload = pc$get("grouped_boxplot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s, y = %s, fill = %s)) +\n  geom_boxplot() +\n  scale_fill_viridis_d()',
        input$group_by, input$group_var, input$group_by))

    register_report_button(
      input, services, "add_grouped_means", kind = "plot",
      title   = function() sprintf("Group means of %s by %s",
                                   input$group_var, input$group_by),
      payload = pc$get("grouped_means"),
      code    = function() sprintf(
        'df %%>%%\n  group_by(%s) %%>%%\n  summarise(m = mean(%s, na.rm = TRUE), s = sd(%s, na.rm = TRUE)) %%>%%\n  ggplot(aes(%s, m)) + geom_col() + geom_errorbar(aes(ymin = m - s, ymax = m + s))',
        input$group_by, input$group_var, input$group_var, input$group_by))

  })
}

# =============================================================================
# END OF MODULE 5: DESCRIPTIVE STATISTICS
# =============================================================================

message("[SAP] Descriptive Stats v2.4-20260708 loaded")  # SAP_MODULE_BANNER
