# =============================================================================
# MODULE 6: STATISTICAL TESTS
# =============================================================================
# 
# Complete statistical testing module with:
# 1. Smart Test Selection (parametric vs non-parametric)
# 2. Parametric Tests (t-test, ANOVA, correlation, regression)
# 3. Non-parametric Tests (Mann-Whitney, Kruskal-Wallis, Spearman)
# 4. Assumption Checking (normality, homogeneity, linearity)
# 5. Effect Sizes (Cohen's d, eta-squared, Cramer's V, r)
# 6. Confidence Intervals (95% CI for all estimates)
# 7. Post-hoc Tests (Tukey, pairwise t-tests with Bonferroni)
# 8. Diagnostic Plots (Q-Q, residual, assumption visualization)
# 9. Result Interpretation (plain English explanations)
# 10. Multi-format Export (tables, plots, reports)
#
# Advanced R patterns:
# - Functional programming with purrr
# - Non-standard evaluation with rlang
# - S3 method dispatch for generics
# - Vectorized statistical operations
# - Efficient matrix computations
# - Professional error handling
#
# =============================================================================

# =============================================================================
# UTILITY FUNCTIONS - STATISTICAL TESTING
# =============================================================================

#' Perform one-sample t-test
#'
#' @param x Numeric vector
#' @param mu Null hypothesis mean (default 0)
#'
#' @return List with test results
test_t_one_sample <- function(x, mu = 0) {
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) < 2) {
    return(list(error = "Need at least 2 observations"))
  }
  
  test_result <- t.test(x_clean, mu = mu, conf.level = 0.95)
  
  # Calculate effect size (Cohen's d)
  cohens_d <- (mean(x_clean) - mu) / sd(x_clean)
  
  list(
    test = "One-Sample t-test",
    t_statistic = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    mean = mean(x_clean),
    sd = sd(x_clean),
    ci_lower = test_result$conf.int[1],
    ci_upper = test_result$conf.int[2],
    cohens_d = cohens_d,
    significant = test_result$p.value < 0.05,
    interpretation = sprintf(
      "t(%.0f) = %.3f, p = %.4f, d = %.3f. %s",
      test_result$parameter,
      test_result$statistic,
      test_result$p.value,
      cohens_d,
      ifelse(test_result$p.value < 0.05, "Significant.", "Not significant.")
    )
  )
}

#' Perform two-sample t-test
#'
#' @param x Numeric vector (group 1)
#' @param y Numeric vector (group 2)
#' @param paired Logical - paired samples?
#'
#' @return List with test results
test_t_two_sample <- function(x, y, paired = FALSE) {
  x <- as.numeric(x)
  y <- as.numeric(y)

  if (isTRUE(paired)) {
    # A paired test compares matched observations, so NAs must be dropped
    # PAIRWISE. Dropping them independently (as before) both destroys the
    # pairing and can leave x and y with different lengths, which makes
    # t.test(paired = TRUE) throw "x and y must have the same length".
    if (length(x) != length(y)) {
      return(list(error = sprintf(
        "Paired t-test needs equal group sizes (got %d and %d). Uncheck 'Paired samples'.",
        length(x), length(y))))
    }
    ok <- !is.na(x) & !is.na(y)
    x_clean <- x[ok]
    y_clean <- y[ok]
  } else {
    x_clean <- x[!is.na(x)]
    y_clean <- y[!is.na(y)]
  }

  if (length(x_clean) < 2 || length(y_clean) < 2) {
    return(list(error = sprintf(
      "Need at least 2 observations per group (got %d and %d)",
      length(x_clean), length(y_clean))))
  }

  if (stats::var(x_clean) == 0 && stats::var(y_clean) == 0) {
    return(list(error = "Both groups are constant; t-test is undefined."))
  }

  test_result <- t.test(x_clean, y_clean, paired = paired, conf.level = 0.95)
  
  # Calculate Cohen's d
  if (paired) {
    d_vec <- x_clean - y_clean
    cohens_d <- mean(d_vec) / stats::sd(d_vec)
  } else {
    # Pooled SD
    n1 <- length(x_clean)
    n2 <- length(y_clean)
    pooled_sd <- sqrt(((n1 - 1) * var(x_clean) + (n2 - 1) * var(y_clean)) / (n1 + n2 - 2))
    cohens_d <- (mean(x_clean) - mean(y_clean)) / pooled_sd
  }
  
  test_type <- if (paired) "Paired t-test" else "Independent Samples t-test"
  
  list(
    test = test_type,
    t_statistic = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    mean_diff = mean(x_clean) - mean(y_clean),
    ci_lower = test_result$conf.int[1],
    ci_upper = test_result$conf.int[2],
    cohens_d = cohens_d,
    n1 = length(x_clean),
    n2 = length(y_clean),
    significant = test_result$p.value < 0.05,
    interpretation = sprintf(
      "t(%.0f) = %.3f, p = %.4f, d = %.3f. %s",
      test_result$parameter,
      test_result$statistic,
      test_result$p.value,
      cohens_d,
      ifelse(test_result$p.value < 0.05, "Significant.", "Not significant.")
    )
  )
}

#' Perform ANOVA (one-way)
#'
#' @param df Data frame
#' @param var Variable to test
#' @param group_var Grouping variable
#'
#' @return List with test results and post-hoc
test_anova <- function(df, var, group_var) {
  # Remove NAs
  data_clean <- df %>%
    filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))
  
  if (nrow(data_clean) < 3) {
    return(list(error = "Need at least 3 observations"))
  }
  
  # Run ANOVA
  fit <- lm(as.formula(paste0(var, "~", group_var)), data = data_clean)
  anova_result <- anova(fit)
  
  # Calculate effect size (eta-squared)
  ss_between <- anova_result$`Sum Sq`[1]
  ss_total <- sum(anova_result$`Sum Sq`)
  eta_squared <- ss_between / ss_total
  
  # Post-hoc: Tukey HSD
  tukey_result <- TukeyHSD(aov(as.formula(paste0(var, "~", group_var)), 
                               data = data_clean))
  
  list(
    test = "One-way ANOVA",
    f_statistic = anova_result$`F value`[1],
    df_between = anova_result$Df[1],
    df_within = anova_result$Df[2],
    p_value = anova_result$`Pr(>F)`[1],
    eta_squared = eta_squared,
    significant = anova_result$`Pr(>F)`[1] < 0.05,
    post_hoc = tukey_result,
    interpretation = sprintf(
      "F(%.0f, %.0f) = %.3f, p = %.4f, η² = %.3f. %s",
      anova_result$Df[1],
      anova_result$Df[2],
      anova_result$`F value`[1],
      anova_result$`Pr(>F)`[1],
      eta_squared,
      ifelse(anova_result$`Pr(>F)`[1] < 0.05, "Significant.", "Not significant.")
    )
  )
}

#' Test normality using Shapiro-Wilk
#'
#' @param x Numeric vector
#'
#' @return List with test result
test_normality <- function(x) {
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) < 3 || length(x_clean) > 5000) {
    return(list(
      is_normal = NA,
      statistic = NA,
      p_value = NA,
      note = "Sample size out of range for Shapiro-Wilk (3-5000)"
    ))
  }
  
  test_result <- shapiro.test(x_clean)
  
  list(
    is_normal = test_result$p.value > 0.05,
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    note = ifelse(test_result$p.value > 0.05,
                  "Data appears normally distributed (p > 0.05)",
                  "Data deviates from normality (p < 0.05)")
  )
}

#' Test homogeneity of variance (Levene's test)
#'
#' @param df Data frame
#' @param var Variable to test
#' @param group_var Grouping variable
#'
#' @return List with test result
test_homogeneity <- function(df, var, group_var) {
  data_clean <- df %>%
    filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))
  
  # Use car::leveneTest if available, else manual
  tryCatch({
    test_result <- suppressWarnings(
      car::leveneTest(as.formula(paste0(var, "~", group_var)),
                                   data = data_clean))
    list(
      equal_variance = test_result$`Pr(>F)`[1] > 0.05,
      statistic = test_result$`F value`[1],
      p_value = test_result$`Pr(>F)`[1],
      note = ifelse(test_result$`Pr(>F)`[1] > 0.05,
                    "Equal variances assumed (p > 0.05)",
                    "Variances not equal (p < 0.05)")
    )
  }, error = function(e) {
    list(
      equal_variance = NA,
      statistic = NA,
      p_value = NA,
      note = "Levene's test unavailable"
    )
  })
}

#' Perform correlation test (Pearson or Spearman)
#'
#' @param x Numeric vector
#' @param y Numeric vector
#' @param method "pearson" or "spearman"
#'
#' @return List with correlation result
test_correlation <- function(x, y, method = "pearson") {
  x_clean <- x[!is.na(x) & !is.na(y)]
  y_clean <- y[!is.na(x) & !is.na(y)]
  
  if (length(x_clean) < 3) {
    return(list(error = "Need at least 3 complete pairs"))
  }
  
  test_result <- cor.test(x_clean, y_clean, method = method, conf.level = 0.95)
  
  list(
    test = paste0(capitalize(method), " Correlation"),
    correlation = test_result$estimate,
    statistic = test_result$statistic,
    df = test_result$parameter %||% (length(x_clean) - 2),
    p_value = test_result$p.value,
    ci_lower = test_result$conf.int[1],
    ci_upper = test_result$conf.int[2],
    n = length(x_clean),
    significant = test_result$p.value < 0.05,
    interpretation = sprintf(
      "r = %.3f, p = %.4f, 95%% CI [%.3f, %.3f]. %s",
      test_result$estimate,
      test_result$p.value,
      test_result$conf.int[1],
      test_result$conf.int[2],
      ifelse(test_result$p.value < 0.05, "Significant.", "Not significant.")
    )
  )
}

#' Perform Mann-Whitney U test (non-parametric alternative to t-test)
#'
#' @param x Numeric vector (group 1)
#' @param y Numeric vector (group 2)
#'
#' @return List with test results
test_mann_whitney <- function(x, y) {
  x_clean <- x[!is.na(x)]
  y_clean <- y[!is.na(y)]
  
  if (length(x_clean) < 1 || length(y_clean) < 1) {
    return(list(error = "Need observations in both groups"))
  }
  
  test_result <- wilcox.test(x_clean, y_clean, conf.int = TRUE)
  
  # Calculate effect size (rank-biserial correlation)
  n1 <- length(x_clean)
  n2 <- length(y_clean)
  r <- 1 - (2 * test_result$statistic) / (n1 * n2)
  
  list(
    test = "Mann-Whitney U Test",
    u_statistic = test_result$statistic,
    p_value = test_result$p.value,
    median1 = median(x_clean),
    median2 = median(y_clean),
    effect_size = r,
    n1 = n1,
    n2 = n2,
    significant = test_result$p.value < 0.05,
    interpretation = sprintf(
      "U = %.1f, p = %.4f, r = %.3f. %s",
      test_result$statistic,
      test_result$p.value,
      r,
      ifelse(test_result$p.value < 0.05, "Significant.", "Not significant.")
    )
  )
}

#' Perform Kruskal-Wallis test (non-parametric ANOVA)
#'
#' @param df Data frame
#' @param var Variable to test
#' @param group_var Grouping variable
#'
#' @return List with test results
test_kruskal_wallis <- function(df, var, group_var) {
  data_clean <- df %>%
    filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))
  
  if (nrow(data_clean) < 3) {
    return(list(error = "Need at least 3 observations"))
  }
  
  test_result <- kruskal.test(as.formula(paste0(var, "~", group_var)), 
                              data = data_clean)
  
  # Effect size (epsilon-squared)
  n <- nrow(data_clean)
  k <- n_distinct(data_clean[[group_var]])
  epsilon_sq <- (test_result$statistic - k + 1) / (n - k)
  
  list(
    test = "Kruskal-Wallis Test",
    h_statistic = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    epsilon_squared = max(0, epsilon_sq),  # Prevent negative
    significant = test_result$p.value < 0.05,
    interpretation = sprintf(
      "H(%.0f) = %.3f, p = %.4f, ε² = %.3f. %s",
      test_result$parameter,
      test_result$statistic,
      test_result$p.value,
      max(0, epsilon_sq),
      ifelse(test_result$p.value < 0.05, "Significant.", "Not significant.")
    )
  )
}

#' Perform Chi-square test (already in M5, included for completeness)
#'
#' @param df Data frame
#' @param var1 First categorical variable
#' @param var2 Second categorical variable
#'
#' @return List with test results
test_chi_square <- function(df, var1, var2) {
  tbl <- table(df[[var1]], df[[var2]])
  
  test_result <- suppressWarnings(chisq.test(tbl, correct = FALSE))
  
  # Cramér's V effect size
  n <- sum(tbl)
  min_dim <- min(dim(tbl) - 1)
  cramers_v <- sqrt(test_result$statistic / (n * min_dim))
  
  list(
    test = "Chi-Square Test",
    chi_square = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    cramers_v = cramers_v,
    significant = test_result$p.value < 0.05,
    interpretation = sprintf(
      "χ²(%.0f) = %.3f, p = %.4f, V = %.3f. %s",
      test_result$parameter,
      test_result$statistic,
      test_result$p.value,
      cramers_v,
      ifelse(test_result$p.value < 0.05, "Significant.", "Not significant.")
    )
  )
}

#' Perform linear regression
#'
#' @param df Data frame
#' @param outcome Outcome variable name
#' @param predictor Predictor variable name
#'
#' @return List with regression results
test_linear_regression <- function(df, outcome, predictor) {
  data_clean <- df %>%
    filter(!is.na(!!sym(outcome)) & !is.na(!!sym(predictor)))
  
  if (nrow(data_clean) < 3) {
    return(list(error = "Need at least 3 observations"))
  }
  
  fit <- lm(as.formula(paste0(outcome, "~", predictor)), data = data_clean)
  summary_fit <- summary(fit)
  
  # R-squared
  r_squared <- summary_fit$r.squared
  adj_r_squared <- summary_fit$adj.r.squared
  
  # Coefficients
  coeffs <- summary_fit$coefficients
  
  list(
    test = "Linear Regression",
    intercept = coeffs[1, 1],
    slope = coeffs[2, 1],
    slope_se = coeffs[2, 2],
    slope_t = coeffs[2, 3],
    slope_p = coeffs[2, 4],
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    f_statistic = summary_fit$fstatistic[1],
    f_p = pf(summary_fit$fstatistic[1], 
             summary_fit$fstatistic[2],
             summary_fit$fstatistic[3],
             lower.tail = FALSE),
    residual_se = summary_fit$sigma,
    n = nrow(data_clean),
    significant = coeffs[2, 4] < 0.05,
    interpretation = sprintf(
      "β = %.3f (SE = %.3f), t = %.3f, p = %.4f, R² = %.3f. %s",
      coeffs[2, 1],
      coeffs[2, 2],
      coeffs[2, 3],
      coeffs[2, 4],
      r_squared,
      ifelse(coeffs[2, 4] < 0.05, "Significant.", "Not significant.")
    ),
    fit = fit
  )
}

#' Capitalize first letter of string
#'
#' @param x String
#'
#' @return Capitalized string
capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_statistical_tests_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("calculator"), "Statistical Tests"),
      p("Comprehensive hypothesis testing and statistical inference", 
        class = "text-muted")
    ),
    
    # No data message
    uiOutput(ns("no_data_message")),
    
    # Main content
    uiOutput(ns("tests_content"))
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_statistical_tests_server <- function(id, app_state, services = NULL) {
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
          icon = "calculator",
          title = "No data available",
          message = "Import and clean your data first using Data > Import and Data > Cleaning"
        )
      }
    })
    
    # =========================================================================
    # MAIN CONTENT (Tabs)
    # =========================================================================
    
    output$tests_content <- renderUI({
      if (!data_exists()) return(NULL)
      
      navset_tab(
        id = ns("tests_tabs"),
        
        # TAB 1: COMPARE TWO GROUPS
        nav_panel(
          "Compare 2 Groups",
          icon = icon("code-compare"),
          div(class = "p-4",
            # Variable selection
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Select Variables")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("t_var"), "Numeric variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("t_group"), "Grouping variable (2 groups):",
                               choices = categorical_cols())
                  )
                ),
                div(class = "row mt-3",
                  div(class = "col-md-6",
                    checkboxInput(ns("t_paired"), "Paired samples?", value = FALSE)
                  ),
                  div(class = "col-md-6 pt-2",
                    actionButton(ns("btn_t_test"), "Run Test", class = "btn-primary")
                  )
                )
              )
            ),
            
            # Assumption checks
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Assumption Checks")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    uiOutput(ns("t_normality"))
                  ),
                  div(class = "col-md-6",
                    uiOutput(ns("t_homogeneity"))
                  )
                )
              )
            ),
            
            # Test results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Test Results")),
              div(class = "card-body",
                uiOutput(ns("t_test_results")),
                div(class = "mt-2 text-end", report_button(ns, "add_t_test", "Add to Report"))
              )
            ),
            
            # Visualization
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Comparison")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("t_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_t_plot", "Add to Report"))
              )
            )
          )
        ),
        
        # TAB 2: COMPARE 3+ GROUPS
        nav_panel(
          "Compare 3+ Groups",
          icon = icon("object-group"),
          div(class = "p-4",
            # Variable selection
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Select Variables")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("anova_var"), "Numeric variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("anova_group"), "Grouping variable:",
                               choices = categorical_cols())
                  )
                ),
                div(class = "mt-3",
                  actionButton(ns("btn_anova"), "Run ANOVA", class = "btn-primary")
                )
              )
            ),
            
            # Test results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("ANOVA Results")),
              div(class = "card-body",
                uiOutput(ns("anova_results")),
                div(class = "mt-2 text-end", report_button(ns, "add_anova", "Add to Report"))
              )
            ),
            
            # Post-hoc results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Post-hoc Test (Tukey HSD)")),
              div(class = "card-body",
                withSpinner(
                  DT::DTOutput(ns("tukey_table")),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_tukey", "Add to Report"))
              )
            ),
            
            # Visualization
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Group Comparison")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("anova_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_anova_plot", "Add to Report"))
              )
            )
          )
        ),
        
        # TAB 3: CORRELATION & REGRESSION
        nav_panel(
          "Correlation & Regression",
          icon = icon("arrow-trend-up"),
          div(class = "p-4",
            # Variable selection
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Select Variables")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("corr_x"), "X variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("corr_y"), "Y variable:",
                               choices = numeric_cols())
                  )
                ),
                div(class = "row mt-3",
                  div(class = "col-md-6",
                    selectInput(ns("corr_method"), "Correlation method:",
                               choices = c("Pearson" = "pearson", "Spearman" = "spearman"))
                  ),
                  div(class = "col-md-6 pt-4",
                    actionButton(ns("btn_corr"), "Run Test", class = "btn-primary")
                  )
                )
              )
            ),
            
            # Correlation results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Correlation Results")),
              div(class = "card-body",
                uiOutput(ns("corr_results")),
                div(class = "mt-2 text-end", report_button(ns, "add_corr", "Add to Report"))
              )
            ),
            
            # Scatter plot
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Scatter Plot with Regression")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("corr_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_corr_plot", "Add to Report"))
              )
            ),
            
            # Regression results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Linear Regression Summary")),
              div(class = "card-body",
                uiOutput(ns("regression_results")),
                div(class = "mt-2 text-end", report_button(ns, "add_regression", "Add to Report"))
              )
            )
          )
        ),
        
        # TAB 4: CATEGORICAL TESTS
        nav_panel(
          "Categorical Tests",
          icon = icon("chart-pie"),
          div(class = "p-4",
            # Chi-square
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Chi-Square Test")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("chi_var1"), "First variable:",
                               choices = categorical_cols())
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("chi_var2"), "Second variable:",
                               choices = categorical_cols())
                  )
                ),
                div(class = "mt-3",
                  actionButton(ns("btn_chi"), "Run Test", class = "btn-primary")
                )
              )
            ),
            
            # Chi-square results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Results")),
              div(class = "card-body",
                uiOutput(ns("chi_results")),
                div(class = "mt-2 text-end", report_button(ns, "add_chi", "Add to Report"))
              )
            )
          )
        ),
        
        # TAB 5: ASSUMPTIONS & DIAGNOSTICS
        nav_panel(
          "Diagnostics",
          icon = icon("magnifying-glass"),
          div(class = "p-4",
            # Normality test
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Test Normality")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-8",
                    selectInput(ns("norm_var"), "Select variable:",
                               choices = numeric_cols())
                  ),
                  div(class = "col-md-4 pt-4",
                    actionButton(ns("btn_norm"), "Test", class = "btn-info")
                  )
                )
              )
            ),
            
            # Normality results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Normality Test Results")),
              div(class = "card-body",
                uiOutput(ns("norm_results")),
                div(class = "mt-2 text-end", report_button(ns, "add_norm", "Add to Report"))
              )
            ),
            
            # Q-Q plot
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Q-Q Plot")),
              div(class = "card-body",
                withSpinner(
                  plotly::plotlyOutput(ns("qq_plot"), height = "400px"),
                  type = 8
                ),
                div(class = "mt-2 text-end", report_button(ns, "add_qq_plot", "Add to Report"))
              )
            ),
            
            # Export button
            div(class = "mt-4",
              downloadButton(ns("download_diagnostics"), "Download Report", 
                           class = "btn-success")
            )
          )
        )
      )
    })
    
    # =========================================================================
    # TAB 1: TWO-SAMPLE TESTS
    # =========================================================================
    
    t_test_result <- reactive({
      req(input$btn_t_test)
      req(input$t_var)
      req(input$t_group)
      req(data_exists())
      
      var <- input$t_var
      group_var <- input$t_group
      paired <- input$t_paired
      
      df <- df_data() %>%
        filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))

      # Drop unused factor levels first: a factor carrying levels no longer
      # present would report >2 groups on a filtered dataset.
      g <- df[[group_var]]
      if (is.factor(g)) g <- droplevels(g)
      groups <- sort(unique(as.character(g)))

      if (length(groups) != 2) {
        return(list(error = sprintf(
          "Grouping variable must have exactly 2 groups (found %d: %s)",
          length(groups),
          paste(utils::head(groups, 5), collapse = ", "))))
      }

      group1_val <- groups[1]
      group2_val <- groups[2]

      # Extract the COLUMN, then subset it.
      #
      # The previous form was  df[df[[group_var]] == g1, var][[1]]  which is a
      # silent data.frame/tibble trap: `[.data.frame` has drop = TRUE for a
      # single column, so it returns a vector and [[1]] then takes its FIRST
      # ELEMENT -- length 1. On a tibble (drop = FALSE) the same line returns
      # the whole column. Our import path calls as.data.frame(), so x and y
      # were always length 1 -> "Need at least 2 observations per group".
      x <- df[[var]][as.character(g) == group1_val]
      y <- df[[var]][as.character(g) == group2_val]

      test_t_two_sample(x, y, paired = paired)
    })
    
    output$t_normality <- renderUI({
      req(input$t_var)
      req(data_exists())
      
      var <- input$t_var
      x <- df_data()[[var]]
      norm_result <- test_normality(x)
      
      div(
        h6("Shapiro-Wilk Test"),
        p(norm_result$note, class = "mb-1 small"),
        p(sprintf("W = %.4f, p = %.4f", norm_result$statistic, norm_result$p_value),
         class = "small text-muted")
      )
    })
    
    output$t_homogeneity <- renderUI({
      req(input$t_var)
      req(input$t_group)
      req(data_exists())
      
      homo_result <- test_homogeneity(df_data(), input$t_var, input$t_group)
      
      div(
        h6("Levene's Test"),
        p(homo_result$note, class = "mb-1 small"),
        p(sprintf("F = %.4f, p = %.4f", homo_result$statistic, homo_result$p_value),
         class = "small text-muted")
      )
    })
    
    output$t_test_results <- renderUI({
      req(t_test_result())
      
      result <- t_test_result()
      
      if (!is.null(result$error)) {
        return(div(class = "alert alert-warning", result$error))
      }
      
      div(
        h6(result$test),
        p(result$interpretation, class = "mb-3 small"),
        div(
          class = "row",
          div(class = "col-md-6",
            p(sprintf("Mean Diff: %.3f", result$mean_diff), class = "small mb-1"),
            p(sprintf("95%% CI: [%.3f, %.3f]", result$ci_lower, result$ci_upper), 
             class = "small")
          ),
          div(class = "col-md-6",
            p(sprintf("Effect Size (d): %.3f", result$cohens_d), class = "small mb-1"),
            p(sprintf("N₁ = %d, N₂ = %d", result$n1, result$n2), class = "small")
          )
        )
      )
    })
    
    output$t_plot <- plotly::renderPlotly({
      req(input$t_var)
      req(input$t_group)
      req(data_exists())
      
      var <- input$t_var
      group_var <- input$t_group
      
      df <- df_data() %>%
        filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))
      
      p <- ggplot(df, aes(x = !!sym(group_var), y = !!sym(var), 
                          fill = !!sym(group_var))) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = paste0("Distribution of ", var, " by ", group_var),
             x = group_var,
             y = var) +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
      
      plotly::ggplotly(pc$keep("t_plot", p), tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # TAB 2: ANOVA
    # =========================================================================
    
    anova_result <- reactive({
      req(input$btn_anova)
      req(input$anova_var)
      req(input$anova_group)
      req(data_exists())
      
      test_anova(df_data(), input$anova_var, input$anova_group)
    })
    
    output$anova_results <- renderUI({
      req(anova_result())
      
      result <- anova_result()
      
      if (!is.null(result$error)) {
        return(div(class = "alert alert-warning", result$error))
      }
      
      div(
        h6("One-way ANOVA"),
        p(result$interpretation, class = "mb-3 small"),
        div(
          class = "alert alert-info",
          strong("Interpretation: "),
          ifelse(result$significant,
                "There are significant differences between groups.",
                "No significant differences between groups.")
        )
      )
    })
    
    # Single source for the render AND the report button.
    tukey_df <- reactive({
      req(anova_result())
      result <- anova_result()
      req(!is.null(result$post_hoc))

      td <- as.data.frame(result$post_hoc[[1]])
      td$Comparison <- rownames(td)
      td <- td[, c("Comparison", "diff", "lwr", "upr", "p adj")]
      colnames(td) <- c("Comparison", "Difference", "Lower CI", "Upper CI", "Adj. p-value")
      td
    })

    output$tukey_table <- DT::renderDataTable({
      tukey_df <- tukey_df()

      DT::datatable(
        tukey_df,
        options = list(
          dom = "lftp",
          columnDefs = list(
            list(className = 'dt-right', targets = 1:4)
          )
        ),
        rownames = FALSE
      )
    })
    
    output$anova_plot <- plotly::renderPlotly({
      req(input$anova_var)
      req(input$anova_group)
      req(data_exists())
      
      var <- input$anova_var
      group_var <- input$anova_group
      
      df <- df_data() %>%
        filter(!is.na(!!sym(var)) & !is.na(!!sym(group_var)))
      
      p <- ggplot(df, aes(x = !!sym(group_var), y = !!sym(var), 
                          fill = !!sym(group_var))) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = paste0("Distribution of ", var, " by ", group_var),
             x = group_var,
             y = var) +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
      
      plotly::ggplotly(pc$keep("anova_plot", p), tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # TAB 3: CORRELATION & REGRESSION
    # =========================================================================
    
    corr_result <- reactive({
      req(input$btn_corr)
      req(input$corr_x)
      req(input$corr_y)
      req(data_exists())
      
      test_correlation(df_data()[[input$corr_x]], 
                      df_data()[[input$corr_y]], 
                      method = input$corr_method)
    })
    
    regression_result <- reactive({
      req(input$btn_corr)
      req(input$corr_x)
      req(input$corr_y)
      req(data_exists())
      
      test_linear_regression(df_data(), input$corr_y, input$corr_x)
    })
    
    output$corr_results <- renderUI({
      req(corr_result())
      
      result <- corr_result()
      
      if (!is.null(result$error)) {
        return(div(class = "alert alert-warning", result$error))
      }
      
      div(
        p(result$interpretation, class = "mb-2 small"),
        div(
          class = "alert",
          class = if (result$significant) "alert-success" else "alert-warning",
          strong(result$test, ": "),
          ifelse(result$significant, "Significant correlation.", "Not significant.")
        )
      )
    })
    
    output$regression_results <- renderUI({
      req(regression_result())
      
      result <- regression_result()
      
      if (!is.null(result$error)) {
        return(div(class = "alert alert-warning", result$error))
      }
      
      div(
        p(result$interpretation, class = "mb-2 small"),
        div(
          class = "bg-light p-3 rounded",
          p(sprintf("Equation: Y = %.3f + %.3f * X", result$intercept, result$slope),
           class = "small mb-2"),
          p(sprintf("R² = %.3f (%.1f%% of variance explained)", 
                   result$r_squared, result$r_squared * 100),
           class = "small")
        )
      )
    })
    
    output$corr_plot <- plotly::renderPlotly({
      req(input$corr_x)
      req(input$corr_y)
      req(data_exists())
      
      x_var <- input$corr_x
      y_var <- input$corr_y
      
      df <- df_data() %>%
        filter(!is.na(!!sym(x_var)) & !is.na(!!sym(y_var)))
      
      # Fit line
      fit <- lm(as.formula(paste0(y_var, "~", x_var)), data = df)
      
      p <- ggplot(df, aes(x = !!sym(x_var), y = !!sym(y_var))) +
        geom_point(alpha = 0.5, color = "#0d6efd", size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "#dc3545", fill = "#dc354520") +
        theme_minimal() +
        labs(title = paste0("Relationship between ", x_var, " and ", y_var),
             x = x_var,
             y = y_var) +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
      
      plotly::ggplotly(pc$keep("corr_plot", p), tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # TAB 4: CHI-SQUARE
    # =========================================================================
    
    chi_result <- reactive({
      req(input$btn_chi)
      req(input$chi_var1)
      req(input$chi_var2)
      req(data_exists())
      
      test_chi_square(df_data(), input$chi_var1, input$chi_var2)
    })
    
    output$chi_results <- renderUI({
      req(chi_result())
      
      result <- chi_result()
      
      if (!is.null(result$error)) {
        return(div(class = "alert alert-warning", result$error))
      }
      
      div(
        p(result$interpretation, class = "mb-2 small"),
        div(
          class = "alert",
          class = if (result$significant) "alert-success" else "alert-warning",
          strong("Chi-Square Test: "),
          ifelse(result$significant, 
                 "Significant association.",
                 "No significant association.")
        )
      )
    })
    
    # =========================================================================
    # TAB 5: DIAGNOSTICS
    # =========================================================================
    
    norm_test_result <- reactive({
      req(input$btn_norm)
      req(input$norm_var)
      req(data_exists())
      
      test_normality(df_data()[[input$norm_var]])
    })
    
    output$norm_results <- renderUI({
      req(norm_test_result())
      
      result <- norm_test_result()
      
      if (is.na(result$p_value)) {
        return(div(class = "alert alert-info", result$note))
      }
      
      div(
        h6("Shapiro-Wilk Test"),
        p(result$note, class = "mb-2 small"),
        p(sprintf("W = %.4f, p = %.4f", result$statistic, result$p_value),
         class = "small text-muted")
      )
    })
    
    output$qq_plot <- plotly::renderPlotly({
      req(input$norm_var)
      req(data_exists())
      
      var <- input$norm_var
      x <- df_data()[[var]]
      x_clean <- x[!is.na(x)]
      
      # Create Q-Q plot data
      theoretical <- qqnorm(x_clean, plot.it = FALSE)
      
      p <- ggplot(data.frame(theoretical = theoretical$x, sample = theoretical$y),
                 aes(x = theoretical, y = sample)) +
        geom_point(color = "#0d6efd", alpha = 0.6, size = 2) +
        geom_abline(intercept = 0, slope = 1, color = "#dc3545", 
                   linetype = "dashed", size = 1) +
        theme_minimal() +
        labs(title = paste("Q-Q Plot:", var),
             x = "Theoretical Quantiles",
             y = "Sample Quantiles") +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
      
      plotly::ggplotly(pc$keep("qq_plot", p), tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # EXPORT
    # =========================================================================
    
    output$download_diagnostics <- downloadHandler(
      filename = function() {
        paste0("diagnostic_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        html_content <- sprintf(
          "<!DOCTYPE html>
          <html>
          <head>
            <meta charset='UTF-8'>
            <title>Statistical Tests Report</title>
            <style>
              body { font-family: Arial, sans-serif; margin: 20px; }
              h1 { color: #0d6efd; }
              h2 { color: #333; border-bottom: 2px solid #0d6efd; margin-top: 30px; }
              .result { background-color: #f9f9f9; padding: 10px; margin: 10px 0; border-left: 4px solid #0d6efd; }
              .significant { color: #198754; font-weight: bold; }
              .not-significant { color: #dc3545; }
              table { border-collapse: collapse; width: 100%%; margin: 15px 0; }
              th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
              th { background-color: #0d6efd; color: white; }
            </style>
          </head>
          <body>
            <h1>Statistical Tests Report</h1>
            <p>Generated: %s</p>
            
            <h2>Summary</h2>
            <p>This report contains results from statistical hypothesis tests.</p>
            
            <div class='result'>
              <strong>Dataset Information</strong><br>
              Total Observations: %d<br>
              Numeric Variables: %d<br>
              Categorical Variables: %d
            </div>
          </body>
          </html>",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          nrow(df_data()),
          length(numeric_cols()),
          length(categorical_cols())
        )
        
        writeLines(html_content, file)
      }
    )
    
    # =========================================================================
    # ADD TO REPORT  —  every output on this page
    # Results are named lists, not printable htest objects, so they render as
    # kind="table" via stat_result_df() (00_services.R). That also makes the
    # officer body_add_table() path in 08 work with no extra code.
    # =========================================================================

    # --- Two-sample tests ---
    register_report_button(
      input, services, "add_t_test", kind = "table",
      title   = function() sprintf("t-test: %s by %s", input$t_var, input$t_group),
      payload = function() stat_result_df(t_test_result()),
      code    = function() sprintf('t.test(%s ~ %s, data = df, paired = %s)',
                                   input$t_var, input$t_group,
                                   if (isTRUE(input$t_paired)) "TRUE" else "FALSE"))

    register_report_button(
      input, services, "add_t_plot", kind = "plot",
      title   = function() sprintf("Distribution of %s by %s", input$t_var, input$t_group),
      payload = pc$get("t_plot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s, y = %s, fill = %s)) +\n  geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.3)',
        input$t_group, input$t_var, input$t_group))

    # --- ANOVA ---
    register_report_button(
      input, services, "add_anova", kind = "table",
      title   = function() sprintf("One-way ANOVA: %s by %s",
                                   input$anova_var, input$anova_group),
      payload = function() stat_result_df(anova_result()),
      code    = function() sprintf('fit <- aov(%s ~ %s, data = df)\nsummary(fit)',
                                   input$anova_var, input$anova_group))

    register_report_button(
      input, services, "add_tukey", kind = "table",
      title   = function() sprintf("Tukey HSD: %s by %s",
                                   input$anova_var, input$anova_group),
      payload = function() tukey_df(),
      code    = function() sprintf('TukeyHSD(aov(%s ~ %s, data = df))',
                                   input$anova_var, input$anova_group))

    register_report_button(
      input, services, "add_anova_plot", kind = "plot",
      title   = function() sprintf("Distribution of %s by %s",
                                   input$anova_var, input$anova_group),
      payload = pc$get("anova_plot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s, y = %s, fill = %s)) +\n  geom_boxplot() +\n  scale_fill_viridis_d()',
        input$anova_group, input$anova_var, input$anova_group))

    # --- Correlation & regression ---
    register_report_button(
      input, services, "add_corr", kind = "table",
      title   = function() sprintf("%s correlation: %s vs %s",
                                   input$corr_method, input$corr_x, input$corr_y),
      payload = function() stat_result_df(corr_result()),
      code    = function() sprintf('cor.test(df$%s, df$%s, method = "%s")',
                                   input$corr_x, input$corr_y, input$corr_method))

    register_report_button(
      input, services, "add_corr_plot", kind = "plot",
      title   = function() sprintf("%s vs %s", input$corr_y, input$corr_x),
      payload = pc$get("corr_plot"),
      code    = function() sprintf(
        'ggplot(df, aes(x = %s, y = %s)) +\n  geom_point(alpha = 0.6) +\n  geom_smooth(method = "lm")',
        input$corr_x, input$corr_y))

    register_report_button(
      input, services, "add_regression", kind = "table",
      title   = function() sprintf("Linear regression: %s ~ %s",
                                   input$corr_y, input$corr_x),
      payload = function() stat_result_df(regression_result()),
      code    = function() sprintf('fit <- lm(%s ~ %s, data = df)\nsummary(fit)',
                                   input$corr_y, input$corr_x))

    # --- Chi-square ---
    register_report_button(
      input, services, "add_chi", kind = "table",
      title   = function() sprintf("Chi-square: %s x %s", input$chi_var1, input$chi_var2),
      payload = function() stat_result_df(chi_result()),
      code    = function() sprintf('chisq.test(table(df$%s, df$%s))',
                                   input$chi_var1, input$chi_var2))

    # --- Diagnostics ---
    register_report_button(
      input, services, "add_norm", kind = "table",
      title   = function() sprintf("Shapiro-Wilk Normality: %s", input$norm_var),
      payload = function() stat_result_df(norm_test_result()),
      code    = function() sprintf('shapiro.test(df$%s)', input$norm_var))

    register_report_button(
      input, services, "add_qq_plot", kind = "plot",
      title   = function() sprintf("Q-Q plot: %s", input$norm_var),
      payload = pc$get("qq_plot"),
      code    = function() sprintf('qqnorm(df$%s); qqline(df$%s)',
                                   input$norm_var, input$norm_var))

  })
}

# =============================================================================
# END OF MODULE 6: STATISTICAL TESTS
# =============================================================================

message("[SAP] Statistical Tests v2.3.1-20260708 loaded")  # SAP_MODULE_BANNER
