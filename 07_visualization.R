# =============================================================================
# MODULE 7: VISUALIZATION
# =============================================================================
#
# Advanced visualization module featuring:
# 1. Distribution Plots (histograms, density, violin, ridgeline)
# 2. Comparison Plots (box plots, violin plots, strip plots)
# 3. Correlation Plots (scatter, correlation matrix heatmaps)
# 4. Multi-panel Displays (faceted plots, small multiples)
# 5. Publication-ready Styling (themes, color palettes, annotations)
# 6. Interactive Features (hover info, dynamic sizing)
# 7. Export Capabilities (PNG, PDF, SVG, high-resolution)
# 8. Customization (colors, themes, layouts)
#
# Advanced R patterns:
# - Advanced ggplot2 techniques
# - Functional programming for plot generation
# - Custom themes and aesthetics
# - Dynamic plot sizing
# - High-resolution export
#
# =============================================================================

# =============================================================================
# UTILITY FUNCTIONS - VISUALIZATION
# =============================================================================

#' Create a professional ggplot2 theme
#'
#' @param base_size Base font size
#' @param palette Color palette name
#'
#' @return ggplot2 theme object
theme_professional <- function(base_size = 12, palette = "Set2") {
  theme_minimal(base_size = base_size) +
  theme(
    plot.title = element_text(size = base_size + 3, face = "bold", 
                              hjust = 0.5, margin = ggplot2::margin(b = 10)),
    plot.subtitle = element_text(size = base_size, hjust = 0.5, 
                                 color = "#666666", margin = ggplot2::margin(b = 8)),
    axis.title = element_text(size = base_size, face = "bold"),
    axis.text = element_text(size = base_size - 1),
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = base_size, face = "bold"),
    legend.text = element_text(size = base_size - 1),
    strip.text = element_text(size = base_size, face = "bold")
  )
}

#' Create distribution plot (histogram + density)
#'
#' @param df Data frame
#' @param var Variable name
#' @param bins Number of bins
#'
#' @return ggplot2 object
plot_distribution <- function(df, var, bins = 30) {
  df_clean <- df %>% filter(!is.na(!!sym(var)))
  
  ggplot(df_clean, aes(x = !!sym(var))) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, 
                   fill = "#0d6efd", alpha = 0.6, color = "white") +
    geom_density(color = "#dc3545", linewidth = 1, fill = "#dc354520", alpha = 0.3) +
    theme_professional() +
    labs(title = paste("Distribution of", var),
         subtitle = sprintf("N = %d, Mean = %.2f, SD = %.2f", 
                           nrow(df_clean), mean(df_clean[[var]], na.rm = TRUE),
                           sd(df_clean[[var]], na.rm = TRUE)),
         x = var,
         y = "Density")
}

#' Create violin plot with data points
#'
#' @param df Data frame
#' @param var Variable name
#' @param group_var Grouping variable
#'
#' @return ggplot2 object
#' Grouped summary statistics of a numeric variable by a grouping variable.
#'
#' @return Data frame: one row per group with N, Missing, Mean, Median, SD,
#'   Min, Max, Sum (all computed on non-missing values).
#' Grouped summary statistics.
#'
#' @param stats character vector of column names to KEEP (from
#'   Missing/Mean/Median/SD/Min/Max/Sum). `Group` and `N` are always kept as
#'   anchors. NULL or empty means keep everything (back-compat).
grouped_summary <- function(df, var, group_var, stats = NULL) {
  out <- df %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      N       = sum(!is.na(.data[[var]])),
      Missing = sum(is.na(.data[[var]])),
      Mean    = mean(.data[[var]],   na.rm = TRUE),
      Median  = stats::median(.data[[var]], na.rm = TRUE),
      SD      = stats::sd(.data[[var]], na.rm = TRUE),
      Min     = suppressWarnings(min(.data[[var]], na.rm = TRUE)),
      Max     = suppressWarnings(max(.data[[var]], na.rm = TRUE)),
      Sum     = sum(.data[[var]],    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(Group = 1) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3)))

  # Non-finite Min/Max/Mean arise for all-NA groups; show as NA not -Inf/Inf.
  out <- out %>%
    dplyr::mutate(dplyr::across(
      where(is.numeric), ~ifelse(is.finite(.), ., NA_real_)))

  if (!is.null(stats) && length(stats)) {
    keep <- c("Group", "N", intersect(
      c("Missing", "Mean", "Median", "SD", "Min", "Max", "Sum"), stats))
    out <- out[, keep, drop = FALSE]
  }
  out
}

plot_violin <- function(df, var, group_var) {
  df_clean <- df %>%
    filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))

  # A violin/density needs >= 2 points per group; drop singleton groups here
  # (rather than letting ggplot warn once per dropped group). The grouped
  # summary table still reports every group.
  df_clean <- df_clean %>%
    group_by(!!sym(group_var)) %>%
    filter(dplyr::n() >= 2) %>%
    ungroup()
  
  ggplot(df_clean, aes(x = !!sym(group_var), y = !!sym(var),
                       fill = !!sym(group_var))) +
    geom_violin(alpha = 0.6, show.legend = FALSE) +
    geom_jitter(width = 0.1, alpha = 0.3, size = 2, show.legend = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", alpha = 0.8, 
                 show.legend = FALSE) +
    scale_fill_viridis_d() +
    theme_professional() +
    labs(title = paste(var, "by", group_var),
         subtitle = sprintf("Violin plot with individual points (N = %d)", nrow(df_clean)),
         x = group_var,
         y = var)
}

#' Create scatter plot with regression line
#'
#' @param df Data frame
#' @param x X variable name
#' @param y Y variable name
#'
#' @return ggplot2 object
plot_scatter_regression <- function(df, x, y) {
  df_clean <- df %>%
    filter(!is.na(!!sym(x)) & !is.na(!!sym(y)))
  
  ggplot(df_clean, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point(alpha = 0.5, color = "#0d6efd", size = 2.5) +
    geom_smooth(method = "lm", se = TRUE, color = "#dc3545", 
               fill = "#dc354520", linewidth = 1) +
    theme_professional() +
    labs(title = paste("Relationship between", x, "and", y),
         subtitle = sprintf("Linear fit with 95%% CI (N = %d)", nrow(df_clean)),
         x = x,
         y = y)
}

#' Create correlation heatmap
#'
#' @param df Data frame
#' @param vars Variable names
#'
#' @return ggplot2 object
plot_correlation_heatmap <- function(df, vars) {
  if (length(vars) < 2) {
    return(NULL)
  }
  
  # Calculate correlations
  df_numeric <- df %>%
    select(all_of(vars)) %>%
    filter(if_all(everything(), ~!is.na(.)))
  
  corr_matrix <- cor(df_numeric, use = "complete.obs")
  
  # Reshape for ggplot
  corr_df <- as.data.frame(corr_matrix)
  corr_df$var1 <- rownames(corr_df)
  rownames(corr_df) <- NULL
  
  corr_long <- corr_df %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation")
  
  ggplot(corr_long, aes(x = var2, y = var1, fill = correlation)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", correlation)), 
             color = "white", size = 4, fontface = "bold") +
    scale_fill_gradient2(low = "#0d6efd", mid = "white", high = "#dc3545",
                         limits = c(-1, 1)) +
    theme_professional() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Matrix",
         subtitle = sprintf("Pearson correlations (N = %d)", nrow(df_numeric)),
         x = "",
         y = "",
         fill = "Correlation")
}

#' Create faceted distribution plot
#'
#' @param df Data frame
#' @param var Variable name
#' @param group_var Grouping variable
#'
#' @return ggplot2 object
plot_distributions_faceted <- function(df, var, group_var) {
  df_clean <- df %>%
    filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))

  # density needs >= 2 points per group; drop singletons to avoid per-group warnings
  df_clean <- df_clean %>%
    group_by(!!sym(group_var)) %>%
    filter(dplyr::n() >= 2) %>%
    ungroup()
  
  ggplot(df_clean, aes(x = !!sym(var), fill = !!sym(group_var))) +
    geom_histogram(bins = 30, alpha = 0.7, color = "white", show.legend = FALSE) +
    facet_wrap(as.formula(paste0("~", group_var))) +
    scale_fill_viridis_d() +
    theme_professional() +
    theme(strip.text = element_text(size = 11, face = "bold")) +
    labs(title = paste("Distribution of", var, "by", group_var),
         subtitle = sprintf("Faceted histograms (N = %d)", nrow(df_clean)),
         x = var,
         y = "Count")
}

#' Create Q-Q plot
#'
#' @param x Numeric vector
#' @param var_name Variable name
#'
#' @return ggplot2 object
plot_qq <- function(x, var_name) {
  x_clean <- x[!is.na(x)]
  
  theoretical <- qqnorm(x_clean, plot.it = FALSE)
  qq_df <- data.frame(theoretical = theoretical$x, sample = theoretical$y)
  
  ggplot(qq_df, aes(x = theoretical, y = sample)) +
    geom_point(alpha = 0.6, color = "#0d6efd", size = 2.5) +
    geom_abline(intercept = 0, slope = 1, color = "#dc3545", 
               linetype = "dashed", linewidth = 1) +
    theme_professional() +
    labs(title = paste("Q-Q Plot:", var_name),
         subtitle = "Points should lie on the dashed line for normality",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
}

#' Create residual plot
#'
#' @param fit lm object
#'
#' @return ggplot2 object
plot_residuals <- function(fit) {
  # Explicitly use stats functions to avoid conflicts
  residuals_df <- data.frame(
    fitted = stats::fitted(fit),
    residuals = stats::residuals(fit),
    standardized = stats::rstandard(fit)
  )
  
  ggplot(residuals_df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.6, color = "#0d6efd", size = 2.5) +
    geom_hline(yintercept = 0, color = "#dc3545", 
              linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "#198754", 
               linewidth = 1) +
    theme_professional() +
    labs(title = "Residual Plot",
         subtitle = "Should show random scatter around 0",
         x = "Fitted Values",
         y = "Residuals")
}

#' Create scale-location plot
#'
#' @param fit lm object
#'
#' @return ggplot2 object
plot_scale_location <- function(fit) {
  residuals_df <- data.frame(
    fitted = stats::fitted(fit),
    sqrt_std_residuals = sqrt(abs(stats::rstandard(fit)))
  )
  
  ggplot(residuals_df, aes(x = fitted, y = sqrt_std_residuals)) +
    geom_point(alpha = 0.6, color = "#0d6efd", size = 2.5) +
    geom_smooth(method = "loess", se = FALSE, color = "#dc3545", 
               linewidth = 1) +
    theme_professional() +
    labs(title = "Scale-Location Plot",
         subtitle = "Check for homogeneity of variance",
         x = "Fitted Values",
         y = "√|Standardized Residuals|")
}

#' Create multi-panel diagnostic plot
#'
#' @param fit lm object
#'
#' @return Combined plot
plot_diagnostics <- function(fit) {
  p1 <- plot_residuals(fit)
  p2 <- plot_scale_location(fit)
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_visualization_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("palette"), "Visualization"),
      p("Advanced statistical graphics and exploratory plots", 
        class = "text-muted")
    ),
    
    # No data message
    uiOutput(ns("no_data_message")),
    
    # Main content
    uiOutput(ns("viz_content"))
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_visualization_server <- function(id, app_state, services = NULL) {
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
    
    numeric_cols <- reactive({
      req(data_exists())
      df <- df_data()
      is_num <- vapply(df, function(x) is.numeric(x) && is.null(dim(x)),
                       logical(1))
      names(df)[is_num]
    })
    
    categorical_cols <- reactive({
      req(data_exists())
      df <- df_data()
      is_cat <- vapply(df, function(x) (is.factor(x) || is.character(x)) &&
                         is.null(dim(x)), logical(1))
      names(df)[is_cat]
    })
    
    # =========================================================================
    # NO DATA MESSAGE
    # =========================================================================
    
    output$no_data_message <- renderUI({
      if (!data_exists()) {
        empty_state(
          icon = "palette",
          title = "No data available",
          message = "Import and clean your data first using Data > Import and Data > Cleaning"
        )
      }
    })
    
    # =========================================================================
    # MAIN CONTENT (Tabs)
    # =========================================================================
    
    output$viz_content <- renderUI({
      if (!data_exists()) return(NULL)
      
      tryCatch({
      
      navset_tab(
        id = ns("viz_tabs"),
        
        # TAB 1: DISTRIBUTIONS
        nav_panel(
          "Distributions",
          icon = icon("chart-column"),
          div(class = "p-4",
            # Variable selection
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Select Variable")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-8",
                    selectInput(ns("dist_var"), "Numeric variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-4 pt-3",
                    sliderInput(ns("dist_bins"), "Bins:", 
                               min = 10, max = 100, value = 30)
                  )
                )
              )
            ),
            
            # Main distribution plot
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Distribution with Density")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("dist_plot"), height = "450px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_dist_plot", "Add to Report"))
              )
            ),
            
            # Export
            div(
              downloadButton(ns("download_dist_png"), "Download (PNG)", 
                           class = "btn-success btn-sm"),
              downloadButton(ns("download_dist_pdf"), "Download (PDF)", 
                           class = "btn-success btn-sm")
            )
          )
        ),
        
        # TAB 2: COMPARISONS
        nav_panel(
          "Comparisons",
          icon = icon("code-compare"),
          div(class = "p-4",
            # Variable selection
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Select Variables")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-4",
                    selectInput(ns("comp_var"), "Numeric variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-4",
                    selectInput(ns("comp_group"), "Grouping variable:",
                               choices = categorical_cols())
                  ),
                  div(class = "col-md-4",
                    selectInput(ns("comp_stat"), "Statistic (for chart):",
                               choices = c(
                                 "Mean"    = "mean",
                                 "Median"  = "median",
                                 "Count (n)" = "n",
                                 "Sum"     = "sum",
                                 "Std. Dev." = "sd",
                                 "Min"     = "min",
                                 "Max"     = "max"
                               ),
                               selected = "mean")
                  )
                ),
                # Which statistics appear in the summary table. Group and N are
                # always shown; everything else is opt-in so the table stays
                # focused on what the user asked for.
                div(class = "row mt-2",
                  div(class = "col-12",
                    checkboxGroupInput(
                      ns("comp_table_stats"),
                      "Show in table (Group and N always shown):",
                      choices = c("Missing", "Mean", "Median", "SD",
                                  "Min", "Max", "Sum"),
                      selected = c("Mean", "Median", "SD"),
                      inline = TRUE
                    )
                  )
                )
              )
            ),

            # Grouped summary: full table of statistics per group + bar chart
            # of the selected statistic. Answers "group by X, compute stat of Y".
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Grouped Summary Statistics")),
              div(class = "card-body",
                div(class = "table-responsive mb-3",
                    DT::DTOutput(ns("comp_summary_table")),
                    div(class = "mt-2 text-end", report_button(ns, "add_comp_summary_table", "Add to Report"))
                ),
                withSpinner(
                  plotly::plotlyOutput(ns("comp_stat_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_comp_stat_plot", "Add to Report"))
              )
            ),
            
            # Violin plot
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Violin Plot with Data Points")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("violin_plot"), height = "450px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_violin_plot", "Add to Report"))
              )
            ),
            
            # Faceted distributions
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Faceted Distributions")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("facet_plot"), height = "450px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_facet_plot", "Add to Report"))
              )
            ),
            
            # Export
            div(
              downloadButton(ns("download_comp_png"), "Download (PNG)", 
                           class = "btn-success btn-sm")
            )
          )
        ),
        
        # TAB 3: CORRELATIONS
        nav_panel(
          "Correlations",
          icon = icon("diagram-project"),
          div(class = "p-4",
            # For scatter plot
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Scatter Plot")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("scatter_x"), "X variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("scatter_y"), "Y variable:",
                               choices = numeric_cols())
                  )
                )
              )
            ),
            
            # Scatter plot
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Scatter Plot with Regression")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("scatter_plot"), height = "450px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_scatter_plot", "Add to Report"))
              )
            ),
            
            # Correlation heatmap
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Correlation Matrix Heatmap")),
              div(class = "card-body",
                p("Showing all numeric variables", class = "small text-muted"),
                withSpinner(
                  plotly::plotlyOutput(ns("corr_heatmap"), height = "500px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_corr_heatmap", "Add to Report"))
              )
            ),
            
            # Export
            div(
              downloadButton(ns("download_corr_png"), "Download (PNG)", 
                           class = "btn-success btn-sm")
            )
          )
        ),
        
        # TAB 4: DIAGNOSTICS
        nav_panel(
          "Diagnostics",
          icon = icon("magnifying-glass"),
          div(class = "p-4",
            # Q-Q Plot
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Normality: Q-Q Plot")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-8",
                    selectInput(ns("qq_var"), "Variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-4", "")
                ),
                withSpinner(
                  plotly::plotlyOutput(ns("qq_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_qq_plot", "Add to Report"))
              )
            ),
            
            # Residual diagnostics
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Regression Diagnostics")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("diag_outcome"), "Outcome variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("diag_predictor"), "Predictor variable:",
                               choices = numeric_cols())
                  )
                ),
                withSpinner(
                  plotly::plotlyOutput(ns("residual_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_residual_plot", "Add to Report"))
              )
            ),
            
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Homogeneity of Variance")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("scale_location_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_scale_location_plot", "Add to Report"))
              )
            )
          )
        ),
        
        # TAB 5: EXPORT
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
                    h6("All Visualizations"),
                    p("Create a PDF report with all plots", class = "small text-muted"),
                    downloadButton(ns("export_all_pdf"), "PDF Report", 
                                 class = "btn-primary btn-sm")
                  ),
                  div(class = "col-md-6",
                    h6("High-Resolution"),
                    p("Export as high-res PNG (300 dpi)", class = "small text-muted"),
                    downloadButton(ns("export_hires_png"), "High-Res PNG", 
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
                tags$li(strong("PNG"), " - Raster format, good for presentations"),
                tags$li(strong("PDF"), " - Vector format, good for publication"),
                tags$li(strong("SVG"), " - Vector format, editable in Illustrator")
              )
            )
          )
        )
      )
      
      }, error = function(e) {
        div(
          class = "alert alert-danger",
          tags$strong("Visualization failed to build: "),
          tags$code(conditionMessage(e))
        )
      })
    })
    
    # =========================================================================
    # TAB 1: DISTRIBUTIONS
    # =========================================================================
    
    output$dist_plot <- plotly::renderPlotly({
      req(input$dist_var)
      req(data_exists())
      
      var <- input$dist_var
      bins <- input$dist_bins
      
      p <- plot_distribution(df_data(), var, bins = bins)
      
      plotly::ggplotly(pc$keep("dist_plot", p), tooltip = c("x", "y"))
    })
    
    output$download_dist_png <- downloadHandler(
      filename = function() {
        paste0(input$dist_var, "_distribution_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- plot_distribution(df_data(), input$dist_var, 
                              bins = input$dist_bins)
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    output$download_dist_pdf <- downloadHandler(
      filename = function() {
        paste0(input$dist_var, "_distribution_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- plot_distribution(df_data(), input$dist_var, 
                              bins = input$dist_bins)
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    # =========================================================================
    # TAB 2: COMPARISONS
    # =========================================================================
    
    # Grouped summary table (all stats per group)
    comp_summary_df <- reactive({
      req(input$comp_var, input$comp_group)
      df <- df_data(); req(df)
      req(input$comp_var %in% names(df), input$comp_group %in% names(df))
      grouped_summary(df, input$comp_var, input$comp_group,
                      stats = input$comp_table_stats)
    })

    output$comp_summary_table <- DT::renderDT({
      # comp_summary_df() already filters to the chosen statistics.
      summ <- comp_summary_df()
      DT::datatable(
        summ,
        rownames = FALSE,
        options  = list(dom = "t", scrollX = TRUE, pageLength = -1)
      )
    })

    # Bar chart of the SELECTED statistic by group
    output$comp_stat_plot <- plotly::renderPlotly({
      req(input$comp_var, input$comp_group, input$comp_stat)
      df <- df_data(); req(df)
      req(input$comp_var %in% names(df), input$comp_group %in% names(df))

      summ <- grouped_summary(df, input$comp_var, input$comp_group)
      stat_col <- switch(input$comp_stat,
        mean = "Mean", median = "Median", n = "N", sum = "Sum",
        sd = "SD", min = "Min", max = "Max", "Mean")

      summ <- summ[order(summ[[stat_col]], decreasing = TRUE), ]
      summ$Group <- factor(summ$Group, levels = summ$Group)

      # Native plot_ly cannot be rasterized without kaleido/webshot2, so mirror
      # it as a ggplot purely for the report PNG path. Display stays plotly.
      pc$keep("comp_stat_plot",
        ggplot2::ggplot(summ, ggplot2::aes(x = Group, y = .data[[stat_col]])) +
          ggplot2::geom_col(fill = "#0d6efd") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = 0)) +
          ggplot2::labs(
            title = sprintf("%s of %s by %s", stat_col, input$comp_var, input$comp_group),
            x = input$comp_group,
            y = sprintf("%s of %s", stat_col, input$comp_var)))

      plotly::plot_ly(
        summ, x = ~Group, y = summ[[stat_col]], type = "bar",
        marker = list(color = "#0d6efd"),
        hovertemplate = paste0("%{x}<br>", stat_col, ": %{y}<extra></extra>")
      ) %>%
        plotly::layout(
          title = sprintf("%s of %s by %s",
                          stat_col, input$comp_var, input$comp_group),
          xaxis = list(title = input$comp_group, tickangle = -45),
          yaxis = list(title = sprintf("%s of %s", stat_col, input$comp_var))
        )
    })

    output$violin_plot <- plotly::renderPlotly({
      req(input$comp_var)
      req(input$comp_group)
      req(data_exists())
      
      p <- plot_violin(df_data(), input$comp_var, input$comp_group)
      
      plotly::ggplotly(pc$keep("violin_plot", p), tooltip = c("x", "y"))
    })
    
    output$facet_plot <- plotly::renderPlotly({
      req(input$comp_var)
      req(input$comp_group)
      req(data_exists())
      
      p <- plot_distributions_faceted(df_data(), input$comp_var, input$comp_group)
      
      plotly::ggplotly(pc$keep("facet_plot", p), tooltip = c("x", "y"))
    })
    
    output$download_comp_png <- downloadHandler(
      filename = function() {
        paste0(input$comp_var, "_comparison_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- plot_violin(df_data(), input$comp_var, input$comp_group)
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    # =========================================================================
    # TAB 3: CORRELATIONS
    # =========================================================================
    
    output$scatter_plot <- plotly::renderPlotly({
      req(input$scatter_x)
      req(input$scatter_y)
      req(data_exists())
      
      p <- plot_scatter_regression(df_data(), input$scatter_x, input$scatter_y)
      
      plotly::ggplotly(pc$keep("scatter_plot", p), tooltip = c("x", "y"))
    })
    
    output$corr_heatmap <- plotly::renderPlotly({
      req(data_exists())
      
      p <- plot_correlation_heatmap(df_data(), numeric_cols())
      
      if (is.null(p)) {
        return(plotly::plot_ly() %>%
          plotly::add_text(x = 0.5, y = 0.5,
                          text = "Need at least 2 numeric variables"))
      }
      
      plotly::ggplotly(pc$keep("corr_heatmap", p), tooltip = c("x", "y", "fill"))
    })
    
    output$download_corr_png <- downloadHandler(
      filename = function() {
        paste0("correlation_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- plot_scatter_regression(df_data(), input$scatter_x, input$scatter_y)
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )
    
    # =========================================================================
    # TAB 4: DIAGNOSTICS
    # =========================================================================
    
    output$qq_plot <- plotly::renderPlotly({
      req(input$qq_var)
      req(data_exists())
      
      var <- input$qq_var
      x <- df_data()[[var]]
      
      p <- plot_qq(x, var)
      
      plotly::ggplotly(pc$keep("qq_plot", p), tooltip = c("x", "y"))
    })
    
    output$residual_plot <- plotly::renderPlotly({
      req(input$diag_outcome)
      req(input$diag_predictor)
      req(data_exists())
      
      outcome <- input$diag_outcome
      predictor <- input$diag_predictor
      
      fit <- lm(as.formula(paste0(outcome, "~", predictor)), 
               data = df_data())
      
      p <- plot_residuals(fit)
      
      plotly::ggplotly(pc$keep("residual_plot", p), tooltip = c("x", "y"))
    })
    
    output$scale_location_plot <- plotly::renderPlotly({
      req(input$diag_outcome)
      req(input$diag_predictor)
      req(data_exists())
      
      outcome <- input$diag_outcome
      predictor <- input$diag_predictor
      
      fit <- lm(as.formula(paste0(outcome, "~", predictor)), 
               data = df_data())
      
      p <- plot_scale_location(fit)
      
      plotly::ggplotly(pc$keep("scale_location_plot", p), tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # TAB 5: EXPORT
    # =========================================================================
    
    output$export_all_pdf <- downloadHandler(
      filename = function() {
        paste0("visualization_report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # Create multi-page PDF with all plots
        pdf(file, width = 10, height = 6)
        
        # Plot 1: Distribution
        if (!is.null(input$dist_var)) {
          print(plot_distribution(df_data(), input$dist_var))
        }
        
        # Plot 2: Violin plot
        if (!is.null(input$comp_var) && !is.null(input$comp_group)) {
          print(plot_violin(df_data(), input$comp_var, input$comp_group))
        }
        
        # Plot 3: Scatter plot
        if (!is.null(input$scatter_x) && !is.null(input$scatter_y)) {
          print(plot_scatter_regression(df_data(), input$scatter_x, input$scatter_y))
        }
        
        dev.off()
      }
    )
    
    output$export_hires_png <- downloadHandler(
      filename = function() {
        paste0("visualization_hires_", Sys.Date(), ".png")
      },
      content = function(file) {
        if (!is.null(input$dist_var)) {
          p <- plot_distribution(df_data(), input$dist_var)
          ggsave(file, p, width = 12, height = 8, dpi = 300)
        }
      }
    )

    # =========================================================================
    # ADD TO REPORT
    # Every visible output on this page can be pushed into services$report.
    # Two lines each: report_button() in the UI, register_report_button() here.
    # =========================================================================

    register_report_button(
      input, services, "add_dist_plot", kind = "plot",
      title   = function() paste("Distribution of", input$dist_var),
      payload = pc$get("dist_plot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s)) +\n  geom_histogram(bins = %s) +\n  theme_minimal()',
        input$dist_var, input$dist_bins))

    register_report_button(
      input, services, "add_comp_summary_table", kind = "table",
      title   = function() sprintf("Summary of %s by %s", input$comp_var, input$comp_group),
      payload = function() comp_summary_df(),
      code    = function() sprintf(
        'df %%>%%\n  group_by(%s) %%>%%\n  summarise(across(%s, list(mean = mean, sd = sd, n = length), na.rm = TRUE))',
        input$comp_group, input$comp_var))

    register_report_button(
      input, services, "add_comp_stat_plot", kind = "plot",
      title   = function() sprintf("%s of %s by %s",
                                   input$comp_stat, input$comp_var, input$comp_group),
      payload = pc$get("comp_stat_plot"),
      code    = function() sprintf(
        'df %%>%%\n  group_by(%s) %%>%%\n  summarise(stat = %s(%s, na.rm = TRUE)) %%>%%\n  ggplot(aes(x = reorder(%s, -stat), y = stat)) + geom_col()',
        input$comp_group, input$comp_stat, input$comp_var, input$comp_group))

    register_report_button(
      input, services, "add_violin_plot", kind = "plot",
      title   = function() sprintf("Violin: %s by %s", input$comp_var, input$comp_group),
      payload = pc$get("violin_plot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s, y = %s, fill = %s)) +\n  geom_violin() + geom_boxplot(width = 0.1)',
        input$comp_group, input$comp_var, input$comp_group))

    register_report_button(
      input, services, "add_facet_plot", kind = "plot",
      title   = function() sprintf("Faceted distribution: %s by %s",
                                   input$comp_var, input$comp_group),
      payload = pc$get("facet_plot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s)) +\n  geom_histogram() +\n  facet_wrap(~ %s)',
        input$comp_var, input$comp_group))

    register_report_button(
      input, services, "add_scatter_plot", kind = "plot",
      title   = function() sprintf("%s vs %s", input$scatter_y, input$scatter_x),
      payload = pc$get("scatter_plot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s, y = %s)) +\n  geom_point(alpha = 0.6) +\n  geom_smooth(method = "lm")',
        input$scatter_x, input$scatter_y))

    register_report_button(
      input, services, "add_corr_heatmap", kind = "plot",
      title   = "Correlation heatmap",
      payload = pc$get("corr_heatmap"),
      code    = function() sprintf(
        'cor(df[, c(%s)], use = "complete.obs")',
        paste(sprintf('"%s"', numeric_cols()), collapse = ", ")))

    register_report_button(
      input, services, "add_qq_plot", kind = "plot",
      title   = function() paste("Q-Q plot:", input$qq_var),
      payload = pc$get("qq_plot"),
      code    = function() sprintf('qqnorm(df$%s); qqline(df$%s)',
                                   input$qq_var, input$qq_var))

    register_report_button(
      input, services, "add_residual_plot", kind = "plot",
      title   = function() sprintf("Residuals vs fitted: %s ~ %s",
                                   input$diag_outcome, input$diag_predictor),
      payload = pc$get("residual_plot"),
      code    = function() sprintf('fit <- lm(%s ~ %s, data = df)\nplot(fit, which = 1)',
                                   input$diag_outcome, input$diag_predictor))

    register_report_button(
      input, services, "add_scale_location_plot", kind = "plot",
      title   = function() sprintf("Scale-location: %s ~ %s",
                                   input$diag_outcome, input$diag_predictor),
      payload = pc$get("scale_location_plot"),
      code    = function() sprintf('fit <- lm(%s ~ %s, data = df)\nplot(fit, which = 3)',
                                   input$diag_outcome, input$diag_predictor))

  })
}

# =============================================================================
# END OF MODULE 7: VISUALIZATION
# =============================================================================

message("[SAP] Visualization v2.7-20260708 loaded")  # SAP_MODULE_BANNER
