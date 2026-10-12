# =============================================================================
# NEXUS STUDIO - Global Configuration & Utilities
# VERSION: Alternative (WITHOUT gtsummary dependency)
# =============================================================================
# This version uses 'gt' instead of 'gtsummary' for summary tables
# gtsummary has heavy dependencies; this is a lightweight alternative
# =============================================================================

# Load all required packages
library(shiny)
library(bslib)
library(shinyWidgets)
library(bsicons)
library(fontawesome)
library(plotly)
library(DT)
library(gt)
library(reactable)
library(janitor)
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(jsonlite)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(broom)
library(officer)
library(rmarkdown)
library(shinycssloaders)
# Password hashing for the authentication layer (Phase 2). sodium provides
# password_store()/password_verify() (argon2) — purpose-built for credentials.
suppressWarnings(suppressMessages({
  .sap_has_sodium <- requireNamespace("sodium", quietly = TRUE)
}))
if (.sap_has_sodium) library(sodium)

# =============================================================================
# RUNTIME LIMITS
# -----------------------------------------------------------------------------
# Shiny caps fileInput() uploads at 5 MB by default and rejects anything larger
# client-side with "Maximum upload size exceeded" -- the read function is never
# reached. Raise it here, before any UI is built.
#
# Override without editing this file:
#   options(sap.max_upload_mb = 4096)   # in startup.R, BEFORE sourcing global
#
# NOTE: this is a *transfer* limit, not a memory limit. Shiny spools the upload
# to a temp file, then read_any_file() pulls it into RAM. A data.frame needs
# roughly 8 bytes per numeric cell plus overhead, so a 10M x 20 numeric table
# is ~1.6 GB before any copy. Raising this option does not create RAM.
# =============================================================================

.sap_upload_env <- suppressWarnings(as.numeric(Sys.getenv("SAP_MAX_UPLOAD_MB", "100")))
if (!is.finite(.sap_upload_env) || .sap_upload_env < 1) .sap_upload_env <- 100
options(shiny.maxRequestSize =
          getOption("sap.max_upload_mb", .sap_upload_env) * 1024^2)   # safe default; override deliberately
rm(.sap_upload_env)

#' Human-readable current upload cap, for UI hints.
sap_max_upload_label <- function() {
  mb <- getOption("shiny.maxRequestSize", 5 * 1024^2) / 1024^2
  if (mb >= 1024) sprintf("%.0f GB", mb / 1024) else sprintf("%.0f MB", mb)
}

# =============================================================================
# THEME CONFIGURATION
# =============================================================================

create_app_theme <- function(mode = "light") {
  if (mode == "dark") {
    bs_theme(
      preset = "darkly",
      primary = "#7777ee",
      secondary = "#14b8a6",
      success = "#22c55e",
      warning = "#f59e0b",
      danger = "#fb7185",
      info = "#38bdf8",
      font_scale = 1.0
    )
  } else {
    bs_theme(
      preset = "bootstrap",
      primary = "#5b5bd6",
      secondary = "#64748b",
      success = "#14b8a6",
      warning = "#f59e0b",
      danger = "#e11d48",
      info = "#0ea5e9",
      font_scale = 1.0
    )
  }
}

# =============================================================================
# DEFAULT GLOBAL SETTINGS
# =============================================================================

.sap_hosted_runtime <- nzchar(trimws(Sys.getenv("PORT", ""))) ||
  nzchar(Sys.getenv("POSIT_PRODUCT", "")) ||
  nzchar(Sys.getenv("CONNECT_SERVER", "")) ||
  tolower(Sys.getenv("R_CONFIG_ACTIVE", "")) %in% c("shinyapps", "connect_cloud")
.sap_default_ollama_url <- trimws(Sys.getenv(
  "OLLAMA_BASE_URL",
  if (.sap_hosted_runtime) "" else "http://localhost:11434"
))
.sap_default_ai_provider <- trimws(Sys.getenv("SAP_AI_PROVIDER", "ollama"))
if (!nzchar(.sap_default_ai_provider)) .sap_default_ai_provider <- "ollama"

DEFAULT_SETTINGS <- list(
  theme_mode = "light",
  accent_color = "blue",
  interface_density = "comfortable",
  font_scale = 1.0,
  decimal_places = 3,
  confidence_level = 0.95,
  significance_level = 0.05,
  correlation_method = "auto",
  missing_value_labels = c("NA", "N/A", "", "NULL", "99", "999", "-99"),
  imp_clean_names = TRUE,
  imp_detect_dates = TRUE,
  imp_trim = TRUE,
  imp_encoding = "UTF-8",
  ml_split = 0.8,
  random_seed = 123L,
  ml_autosave = FALSE,
  max_file_size = 100,  # MB
  # AI interpretation engine. Ollama is the default so interpretations are
  # real, local, and private without requiring an API key. The model can be
  # discovered from the installed Ollama models in Settings.
  ai_provider = .sap_default_ai_provider,
  ai_model = trimws(Sys.getenv("OLLAMA_MODEL", "")),
  ai_base_url = .sap_default_ollama_url,
  ai_temperature = 0.2,
  ai_max_tokens = 1200,
  ai_timeout = 300,
  ai_context_window = 4096,
  ai_keep_alive = "10m"
)

# =============================================================================
# GLOBAL REACTIVE STATE CONTAINER
# =============================================================================

# Structure:
# app_state$raw_data        - Original imported dataset
# app_state$processed_data  - Working dataset (can be modified)
# app_state$data_imported   - Logical flag: has data been imported?
# app_state$variable_types  - Data frame with variable metadata
# app_state$settings        - User settings
# app_state$import_log      - Log of import operations

# =============================================================================
# UTILITY FUNCTIONS - DATA IMPORT & VALIDATION
# =============================================================================

#' Detect variable types in a data frame
#'
#' @param df Data frame to analyze
#'
#' @return Data frame with variable metadata
detect_variable_types <- function(df) {
  
  result <- tibble(
    variable = colnames(df),
    type = NA_character_,
    missing_count = NA_integer_,
    missing_pct = NA_real_,
    unique_values = NA_integer_,
    suggested_analysis = NA_character_
  )
  
  for (i in seq_along(colnames(df))) {
    col <- df[[i]]
    col_name <- colnames(df)[i]
    
    # Count missing values
    missing_count <- sum(is.na(col))
    missing_pct <- missing_count / nrow(df) * 100
    
    # Detect type
    if (is.numeric(col)) {
      if (all(col[!is.na(col)] %% 1 == 0, na.rm = TRUE)) {
        detected_type <- "integer"
        suggested <- "numeric_analysis"
      } else {
        detected_type <- "numeric"
        suggested <- "numeric_analysis"
      }
    } else if (is.logical(col)) {
      detected_type <- "logical"
      suggested <- "categorical_analysis"
    } else if (inherits(col, "Date")) {
      detected_type <- "date"
      suggested <- "time_series"
    } else if (inherits(col, "POSIXct")) {
      detected_type <- "datetime"
      suggested <- "time_series"
    } else if (is.character(col)) {
      unique_vals <- n_distinct(col[!is.na(col)])
      # If very few unique values, suggest treating as categorical
      if (unique_vals <= 20) {
        detected_type <- "categorical_character"
        suggested <- "categorical_analysis"
      } else {
        detected_type <- "character"
        suggested <- "text_analysis"
      }
    } else {
      detected_type <- "unknown"
      suggested <- "unknown"
    }
    
    result$type[i] <- detected_type
    result$missing_count[i] <- missing_count
    result$missing_pct[i] <- round(missing_pct, 2)
    result$unique_values[i] <- n_distinct(col[!is.na(col)])
    result$suggested_analysis[i] <- suggested
  }
  
  return(result)
}

#' Infer survey-specific semantic roles without changing the base storage type.
#' These roles feed the codebook and can always be overridden by the analyst.
infer_survey_roles <- function(df) {
  if (!is.data.frame(df)) return(character())
  roles <- stats::setNames(rep("measure", ncol(df)), names(df))
  n <- nrow(df)
  for (name in names(df)) {
    x <- df[[name]]
    key <- tolower(name)
    nonmissing <- x[!is.na(x)]
    unique_n <- length(unique(nonmissing))
    unique_ratio <- if (n > 0) unique_n / n else 0

    if (grepl("(^|_)(weight|wgt|final_weight|sample_weight)(_|$)", key)) {
      roles[[name]] <- "weight"
    } else if (grepl("(^|_)(lat|latitude|lon|lng|longitude)(_|$)", key) ||
               grepl("(^|_)(country|province|state|region|district|county|sector|cell|village)(_|$)", key)) {
      roles[[name]] <- "geographic"
    } else if (grepl("(^|_)(id|uuid|guid|respondent_id|case_id|record_id)(_|$)", key) ||
               (unique_ratio >= 0.98 && unique_n > 20 && !is.numeric(x))) {
      roles[[name]] <- "identifier"
    } else if (grepl("(^|_)(mr|multi|multiple|option|select|choice)[0-9_]*($|_)", key) && unique_n <= 2) {
      roles[[name]] <- "multiple_response_option"
    } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      roles[[name]] <- "date"
    } else if (unique_n == 2) {
      roles[[name]] <- "binary"
    } else if (is.numeric(x) && unique_n >= 3 && unique_n <= 7 &&
               all(abs(nonmissing - round(nonmissing)) < .Machine$double.eps^0.5, na.rm = TRUE) &&
               diff(range(nonmissing, na.rm = TRUE)) <= 6) {
      roles[[name]] <- "likert"
    } else if (is.character(x) && (unique_n > 30 || unique_ratio > 0.5)) {
      roles[[name]] <- "free_text"
    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      roles[[name]] <- "categorical"
    } else {
      roles[[name]] <- "measure"
    }
  }
  roles
}

#' Clean column names and optionally clean the data
#'
#' @param df Data frame
#' @param options List of cleaning options
#'
#' @return Cleaned data frame
clean_data <- function(df, options = list()) {
  
  # Extract options with defaults
  clean_names_flag <- options$clean_names %||% TRUE
  trim_whitespace <- options$trim_whitespace %||% TRUE
  remove_empty_rows <- options$remove_empty_rows %||% TRUE
  remove_empty_cols <- options$remove_empty_cols %||% TRUE
  convert_utf8 <- options$convert_utf8 %||% TRUE
  auto_detect_dates <- options$auto_detect_dates %||% TRUE
  auto_detect_logical <- options$auto_detect_logical %||% TRUE
  
  # Clean column names
  if (clean_names_flag) {
    df <- clean_names(df, case = "snake")
  }
  
  # Trim whitespace from character columns. Defensive per-column trim (see the
  # matching note in 01_import_wizard.R): across(where(is.character), str_trim)
  # aborts the whole pipeline on a single odd column (list-column, labelled
  # haven column). trimws() per atomic character column can't do that.
  if (isTRUE(trim_whitespace)) {
    chr_cols <- names(df)[vapply(df, function(x)
      is.character(x) && is.null(dim(x)), logical(1))]
    for (cn in chr_cols) {
      df[[cn]] <- tryCatch(trimws(df[[cn]]), error = function(e) df[[cn]])
    }
  }
  
  # Remove empty rows (all NA)
  if (remove_empty_rows) {
    df <- df %>%
      filter(!if_all(everything(), is.na))
  }
  
  # Remove empty columns (all NA)
  if (remove_empty_cols) {
    df <- df %>%
      select(where(~!all(is.na(.x))))
  }
  
  # Convert to UTF-8
  if (convert_utf8) {
    df <- df %>%
      mutate(across(where(is.character), 
                   ~iconv(., to = "UTF-8")))
  }
  
  return(df)
}

#' Handle missing value codes
#'
#' @param df Data frame
#' @param missing_codes Character vector of codes to treat as NA
#'
#' @return Data frame with missing codes converted to NA
handle_missing_codes <- function(df, missing_codes = c("99", "999", "-99", "NA", "N/A", "")) {
  
  df %>%
    mutate(across(where(is.character), 
                 ~na_if(., ""))) %>%
    mutate(across(everything(),
                 ~replace(., . %in% missing_codes, NA)))
}

#' Validate imported dataset
#'
#' @param df Data frame to validate
#'
#' @return List with validation results
validate_data <- function(df) {
  
  issues <- list()
  
  # Check for duplicate columns
  dup_cols <- names(df)[duplicated(names(df))]
  if (length(dup_cols) > 0) {
    issues$duplicate_columns <- dup_cols
  }
  
  # Check for completely empty rows
  empty_rows <- which(rowSums(is.na(df)) == ncol(df))
  if (length(empty_rows) > 0) {
    issues$empty_rows <- length(empty_rows)
  }
  
  # Check for completely empty columns
  empty_cols <- names(df)[colSums(is.na(df)) == nrow(df)]
  if (length(empty_cols) > 0) {
    issues$empty_columns <- empty_cols
  }
  
  # Check for duplicate rows
  dup_rows <- sum(duplicated(df))
  if (dup_rows > 0) {
    issues$duplicate_rows <- dup_rows
  }
  
  return(issues)
}

# =============================================================================
# UTILITY FUNCTIONS - UI HELPERS
# =============================================================================

#' Create a KPI card
#'
#' @param title Card title
#' @param value Main value to display
#' @param subtitle Subtitle text
#' @param icon Icon name (fontawesome)
#' @param color Card color (primary, success, danger, etc.)
#'
#' @return HTML output
kpi_card <- function(title, value, subtitle = NULL, icon = NULL, color = "primary") {
  allowed_colors <- c("primary", "success", "info", "warning", "danger", "secondary")
  color <- if (color %in% allowed_colors) color else "primary"
  display_value <- if (is.null(value) || !length(value) || is.na(value[[1]])) {
    "—"
  } else {
    as.character(value[[1]])
  }

  div(
    class = paste("card sap-kpi-card", paste0("sap-kpi-card-", color), "mb-3"),
    role = "group",
    `aria-label` = paste(title, display_value),
    div(
      class = "card-body sap-kpi-card-body",
      div(
        class = "d-flex justify-content-between align-items-center",
        div(
          h6(class = "card-title sap-kpi-card-title", title),
          h3(class = "card-text sap-kpi-card-value", display_value),
          if (!is.null(subtitle)) p(class = "small sap-kpi-card-subtitle", subtitle) else NULL
        ),
        if (!is.null(icon)) {
          div(
            class = "sap-kpi-card-icon",
            shiny::icon(icon)
          )
        }
      )
    )
  )
}

#' Create validation message boxes
#'
#' @param type Type: success, warning, error, info
#' @param title Message title
#' @param message Message content
#'
#' @return HTML output
validation_box <- function(type = "info", title = NULL, message = "") {
  
  type_class <- switch(type,
    "success" = "alert-success",
    "warning" = "alert-warning",
    "error" = "alert-danger",
    "info" = "alert-info",
    "alert-info"
  )
  
  icon_name <- switch(type,
    "success" = "circle-check",
    "warning" = "triangle-exclamation",
    "error" = "circle-exclamation",
    "info" = "circle-info",
    "circle-info"
  )
  
  div(
    class = paste("alert", type_class),
    role = "alert",
    div(
      style = "display: flex; gap: 10px;",
      div(
        style = "flex-shrink: 0;",
        icon(icon_name, class = "fs-5")
      ),
      div(
        if (!is.null(title)) {
          h4(class = "alert-heading", title)
        },
        message
      )
    )
  )
}

#' Create empty state placeholder
#'
#' @param icon Icon name
#' @param title Title text
#' @param message Message text
#' @param action_text Optional action button text
#'
#' @return HTML output
empty_state <- function(icon = "inbox", title = "No Data", message = "Import a dataset to get started", 
                       action_text = NULL) {
  
  div(
    class = "text-center py-5",
    style = "color: #999;",
    div(
      style = "font-size: 3rem; margin-bottom: 1rem;",
      icon(icon)
    ),
    h3(title),
    p(message),
    if (!is.null(action_text)) {
      actionButton("goto_import", action_text, class = "btn btn-primary mt-3")
    }
  )
}

# =============================================================================
# UTILITY FUNCTIONS - ANALYSIS HELPERS
# =============================================================================

#' Get recommended charts for variable combinations
#'
#' @param var_types Character vector of variable types
#'
#' @return Character vector of recommended chart types
recommend_charts <- function(var_types) {
  
  n_vars <- length(var_types)
  
  # Single numeric variable
  if (n_vars == 1 && var_types[1] %in% c("numeric", "integer")) {
    return(c("Histogram", "Density Plot", "Box Plot"))
  }
  
  # Single categorical variable
  if (n_vars == 1 && var_types[1] %in% c("categorical", "logical")) {
    return(c("Bar Chart", "Pie Chart"))
  }
  
  # Two numeric variables
  if (n_vars == 2 && all(var_types %in% c("numeric", "integer"))) {
    return(c("Scatter Plot", "Line Chart", "Hexbin Plot"))
  }
  
  # Two categorical variables
  if (n_vars == 2 && all(var_types %in% c("categorical", "logical"))) {
    return(c("Mosaic Plot", "Stacked Bar Chart", "Grouped Bar Chart"))
  }
  
  # One numeric, one categorical
  if (n_vars == 2 && 
      ((var_types[1] %in% c("numeric", "integer") && var_types[2] %in% c("categorical", "logical")) ||
       (var_types[2] %in% c("numeric", "integer") && var_types[1] %in% c("categorical", "logical")))) {
    return(c("Box Plot", "Violin Plot", "Grouped Histogram"))
  }
  
  return(c("Bar Chart", "Scatter Plot"))
}

#' Validate chart inputs
#'
#' @param chart_type Type of chart
#' @param x_var X variable type
#' @param y_var Y variable type (optional)
#'
#' @return List with is_valid (logical) and message (character)
validate_chart_input <- function(chart_type, x_var, y_var = NULL) {
  
  result <- list(is_valid = TRUE, message = "")
  
  # Histogram validation
  if (chart_type == "Histogram") {
    if (!(x_var %in% c("numeric", "integer"))) {
      result$is_valid <- FALSE
      result$message <- "Histogram requires a numeric variable. Recommended charts: Bar Chart, Pie Chart"
      return(result)
    }
  }
  
  # Pie Chart validation
  if (chart_type == "Pie Chart") {
    if (!(x_var %in% c("categorical", "logical"))) {
      result$is_valid <- FALSE
      result$message <- "Pie Chart requires a categorical variable. Recommended charts: Histogram, Density Plot"
      return(result)
    }
  }
  
  # Scatter Plot validation
  if (chart_type == "Scatter Plot") {
    if (!(!is.null(y_var) && 
          x_var %in% c("numeric", "integer") && 
          y_var %in% c("numeric", "integer"))) {
      result$is_valid <- FALSE
      result$message <- "Scatter Plot requires two numeric variables."
      return(result)
    }
  }
  
  return(result)
}

# =============================================================================
# UTILITY FUNCTIONS - SUMMARY STATISTICS (ALTERNATIVE TO GTSUMMARY)
# =============================================================================

#' Create summary statistics for continuous variables
#'
#' @param df Data frame
#' @param variables Character vector of variable names
#'
#' @return Data frame with summary statistics
summarize_continuous <- function(df, variables) {
  
  result <- data.frame(
    Variable = character(),
    N = integer(),
    Mean = numeric(),
    SD = numeric(),
    Median = numeric(),
    Min = numeric(),
    Max = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in variables) {
    col <- df[[var]]
    if (is.numeric(col)) {
      result <- rbind(result, data.frame(
        Variable = var,
        N = sum(!is.na(col)),
        Mean = round(mean(col, na.rm = TRUE), 3),
        SD = round(sd(col, na.rm = TRUE), 3),
        Median = round(median(col, na.rm = TRUE), 3),
        Min = round(min(col, na.rm = TRUE), 3),
        Max = round(max(col, na.rm = TRUE), 3)
      ))
    }
  }
  
  return(result)
}

#' Create summary statistics for categorical variables
#'
#' @param df Data frame
#' @param variables Character vector of variable names
#'
#' @return Data frame with frequency counts
summarize_categorical <- function(df, variables) {
  
  result <- list()
  
  for (var in variables) {
    col <- df[[var]]
    if (is.factor(col) || is.character(col)) {
      freq_table <- table(col, useNA = "ifany")
      pct_table <- round(prop.table(freq_table) * 100, 2)
      
      result[[var]] <- data.frame(
        Variable = var,
        Category = names(freq_table),
        Frequency = as.numeric(freq_table),
        Percent = as.numeric(pct_table),
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(do.call(rbind, result))
}

#' Create a summary table using gt
#'
#' @param df Data frame with summary statistics
#' @param title Table title
#'
#' @return gt object
create_summary_table <- function(df, title = "Summary Statistics") {
  
  gt(df) %>%
    tab_header(title = title) %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_style(
      style = cell_fill(color = "#f3f3f3"),
      locations = cells_column_labels()
    )
}

# =============================================================================
# UTILITY FUNCTIONS - EXPORT & REPORTING
# =============================================================================

#' Format numbers with specified decimal places
#'
#' @param x Numeric vector
#' @param decimal_places Number of decimal places
#'
#' @return Formatted character vector
format_numbers <- function(x, decimal_places = 3) {
  format(round(x, decimal_places), nsmall = decimal_places)
}

# =============================================================================
# UTILITY FUNCTIONS - TEXT & STRING HELPERS
# =============================================================================

#' Capitalize first letter of string
#'
#' @param x Character string
#'
#' @return Capitalized string
capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

#' Convert snake_case to Title Case
#'
#' @param x Character string in snake_case
#'
#' @return String in Title Case
snake_to_title <- function(x) {
  words <- str_split(x, "_")[[1]]
  paste(capitalize_first(words), collapse = " ")
}

# =============================================================================
# NULL COALESCING OPERATOR
# =============================================================================
`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# UTILITY FUNCTIONS - DATA CLEANING & TRANSFORMATION
# =============================================================================

#' Calculate Data Quality Score
#'
#' Calculates a 0-100 quality score based on data issues
#' Factors: missing data, duplicates, outliers, inconsistencies
#'
#' @param df Data frame to analyze
#'
#' @return List with score (0-100), issues, and recommendations
calculate_quality_score <- function(df) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(list(score = 0, issues = "Empty dataset", recommendations = "Import data first"))
  }
  
  score <- 100
  issues <- list()
  
  # 1. Check missing values (each 5% = -5 points, max -30)
  missing_pct <- sum(is.na(df)) / (nrow(df) * ncol(df)) * 100
  missing_penalty <- min(30, floor(missing_pct / 5) * 5)
  score <- score - missing_penalty
  if (missing_pct > 0) {
    issues$missing <- sprintf("%.1f%% missing values", missing_pct)
  }
  
  # 2. Check duplicates (each duplicate = -2 points, max -20)
  dup_count <- sum(duplicated(df))
  dup_penalty <- min(20, dup_count * 2)
  score <- score - dup_penalty
  if (dup_count > 0) {
    issues$duplicates <- sprintf("%d duplicate rows", dup_count)
  }
  
  # 3. Check for outliers in numeric columns
  numeric_cols <- sapply(df, is.numeric)
  outlier_count <- 0
  for (col in colnames(df)[numeric_cols]) {
    col_data <- df[[col]][!is.na(df[[col]])]
    if (length(col_data) > 0) {
      Q1 <- quantile(col_data, 0.25)
      Q3 <- quantile(col_data, 0.75)
      IQR <- Q3 - Q1
      outliers <- sum(col_data < (Q1 - 1.5 * IQR) | col_data > (Q3 + 1.5 * IQR))
      outlier_count <- outlier_count + outliers
    }
  }
  outlier_penalty <- min(20, floor(outlier_count / 10) * 5)
  score <- score - outlier_penalty
  if (outlier_count > 0) {
    issues$outliers <- sprintf("%d potential outliers detected", outlier_count)
  }
  
  # 4. Check for empty columns
  empty_cols <- sum(colSums(is.na(df)) == nrow(df))
  if (empty_cols > 0) {
    score <- score - (empty_cols * 5)
    issues$empty_cols <- sprintf("%d empty columns", empty_cols)
  }
  
  # 5. Check for high cardinality character columns (might be IDs)
  for (col in colnames(df)) {
    if (is.character(df[[col]])) {
      unique_vals <- n_distinct(df[[col]][!is.na(df[[col]])])
      if (unique_vals > 0.9 * nrow(df)) {
        issues$high_cardinality <- sprintf("Column '%s' has very high cardinality", col)
        score <- score - 5
      }
    }
  }
  
  # Ensure score is between 0 and 100
  score <- max(0, min(100, score))
  
  # Generate recommendations
  recommendations <- list()
  if (missing_pct > 10) {
    recommendations$missing <- "Consider imputing or removing rows with missing values"
  }
  if (dup_count > 0) {
    recommendations$duplicates <- "Remove duplicate rows to ensure data integrity"
  }
  if (outlier_count > 0) {
    recommendations$outliers <- "Review and handle outliers using appropriate methods"
  }
  if (empty_cols > 0) {
    recommendations$empty <- "Remove empty columns from your dataset"
  }
  
  return(list(
    score = round(score),
    issues = issues,
    recommendations = recommendations,
    missing_pct = round(missing_pct, 2),
    duplicate_rows = dup_count,
    outlier_count = outlier_count,
    empty_columns = empty_cols
  ))
}

#' Detect and Remove Duplicates
#'
#' @param df Data frame
#' @param type Type of duplicates: "rows", "columns", or "ids"
#' @param id_column Column name for ID-based duplicate detection (if type="ids")
#'
#' @return List with cleaned data and summary
detect_duplicates <- function(df, type = "rows", id_column = NULL) {
  
  result <- list(
    original_count = NULL,
    removed_count = 0,
    data = df,
    message = ""
  )
  
  if (type == "rows") {
    result$original_count <- nrow(df)
    dup_indices <- duplicated(df)
    result$removed_count <- sum(dup_indices)
    result$data <- df[!dup_indices, ]
    result$message <- sprintf("Removed %d duplicate rows", result$removed_count)
    
  } else if (type == "columns") {
    result$original_count <- ncol(df)
    dup_cols <- duplicated(as.list(df))
    result$removed_count <- sum(dup_cols)
    result$data <- df[, !dup_cols]
    result$message <- sprintf("Removed %d duplicate columns", result$removed_count)
    
  } else if (type == "ids" && !is.null(id_column)) {
    if (id_column %in% colnames(df)) {
      result$original_count <- nrow(df)
      dup_ids <- duplicated(df[[id_column]])
      result$removed_count <- sum(dup_ids)
      result$data <- df[!dup_ids, ]
      result$message <- sprintf("Removed %d rows with duplicate ID values", result$removed_count)
    } else {
      result$message <- sprintf("Column '%s' not found", id_column)
    }
  }
  
  return(result)
}

#' Detect Outliers Using Multiple Methods
#'
#' @param x Numeric vector
#' @param method "iqr", "zscore", or "mahalanobis"
#' @param threshold Threshold parameter (default: IQR=1.5, zscore=3, etc.)
#'
#' @return Logical vector indicating outliers
detect_outliers <- function(x, method = "iqr", threshold = NULL) {
  
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) == 0) {
    return(rep(FALSE, length(x)))
  }
  
  outliers <- rep(FALSE, length(x))
  
  if (method == "iqr") {
    if (is.null(threshold)) threshold <- 1.5
    Q1 <- quantile(x_clean, 0.25)
    Q3 <- quantile(x_clean, 0.75)
    IQR <- Q3 - Q1
    lower <- Q1 - threshold * IQR
    upper <- Q3 + threshold * IQR
    outliers <- x < lower | x > upper
    
  } else if (method == "zscore") {
    if (is.null(threshold)) threshold <- 3
    z_scores <- abs((x - mean(x_clean, na.rm = TRUE)) / sd(x_clean, na.rm = TRUE))
    outliers <- z_scores > threshold
    
  } else if (method == "mahalanobis") {
    if (length(x_clean) > 1) {
      center <- mean(x_clean)
      spread <- sd(x_clean)
      md <- abs(x - center) / spread
      threshold <- if (is.null(threshold)) 3 else threshold
      outliers <- md > threshold
    }
  }
  
  outliers[is.na(outliers)] <- FALSE
  return(outliers)
}

#' Handle Outliers with Multiple Options
#'
#' @param x Numeric vector
#' @param method Detection method: "iqr", "zscore", "mahalanobis"
#' @param action Action to take: "remove", "winsorize", "replace"
#' @param replace_value Value to use if action="replace"
#'
#' @return Modified vector
handle_outliers <- function(x, method = "iqr", action = "remove", replace_value = NA) {
  
  outlier_mask <- detect_outliers(x, method = method)
  
  if (action == "remove") {
    x[!outlier_mask]
  } else if (action == "winsorize") {
    # Winsorize: replace outliers with nearest non-outlier value
    x_clean <- x[!outlier_mask]
    min_val <- min(x_clean, na.rm = TRUE)
    max_val <- max(x_clean, na.rm = TRUE)
    x[x < min_val] <- min_val
    x[x > max_val] <- max_val
    x
  } else if (action == "replace") {
    x[outlier_mask] <- replace_value
    x
  } else {
    x
  }
}

#' Impute Missing Values
#'
#' @param x Numeric or character vector with missing values
#' @param method "mean", "median", "mode", "zero", "forward_fill", "backward_fill"
#' @param value Custom value for imputation
#'
#' @return Vector with imputed values
impute_missing <- function(x, method = "mean", value = NULL) {
  
  if (sum(is.na(x)) == 0) return(x)
  
  if (method == "mean") {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else if (method == "median") {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  } else if (method == "mode") {
    # Mode for categorical or numeric
    if (is.numeric(x)) {
      tbl <- table(x)
      x[is.na(x)] <- as.numeric(names(tbl)[which.max(tbl)])
    } else {
      tbl <- table(x)
      x[is.na(x)] <- names(tbl)[which.max(tbl)]
    }
  } else if (method == "zero") {
    x[is.na(x)] <- 0
  } else if (method == "forward_fill") {
    # Forward fill: use previous value
    for (i in 2:length(x)) {
      if (is.na(x[i]) && !is.na(x[i-1])) {
        x[i] <- x[i-1]
      }
    }
  } else if (method == "backward_fill") {
    # Backward fill: use next value
    for (i in (length(x)-1):1) {
      if (is.na(x[i]) && !is.na(x[i+1])) {
        x[i] <- x[i+1]
      }
    }
  } else if (method == "custom" && !is.null(value)) {
    x[is.na(x)] <- value
  }
  
  return(x)
}

#' Transform Numeric Variables
#'
#' @param x Numeric vector
#' @param transformation Type: "log", "sqrt", "square", "reciprocal", "standardize", "normalize"
#'
#' @return Transformed vector
transform_variable <- function(x, transformation = "log") {
  
  if (transformation == "log") {
    # Log transformation (handle zeros and negatives)
    x_clean <- x[!is.na(x) & x > 0]
    if (length(x_clean) > 0) {
      log(x)
    } else {
      warning("Cannot apply log to non-positive values")
      x
    }
  } else if (transformation == "log1p") {
    # Log(1 + x) - handles zeros
    log1p(x)
  } else if (transformation == "sqrt") {
    # Square root (handle negatives)
    x_clean <- x[!is.na(x) & x >= 0]
    if (length(x_clean) > 0) {
      sqrt(x)
    } else {
      warning("Cannot apply sqrt to negative values")
      x
    }
  } else if (transformation == "square") {
    x^2
  } else if (transformation == "reciprocal") {
    # 1/x (handle zeros)
    x_nonzero <- x[!is.na(x) & x != 0]
    if (length(x_nonzero) > 0) {
      1 / x
    } else {
      warning("Cannot apply reciprocal to zero values")
      x
    }
  } else if (transformation == "standardize") {
    # Standardize: (x - mean) / sd
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else if (transformation == "normalize") {
    # Normalize: (x - min) / (max - min)
    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)
    (x - x_min) / (x_max - x_min)
  } else if (transformation == "center") {
    # Center: x - mean
    x - mean(x, na.rm = TRUE)
  } else if (transformation == "rank") {
    # Rank transformation
    rank(x, na.last = "keep")
  } else {
    x
  }
}

#' Recode Variable Values
#'
#' @param x Vector to recode
#' @param mapping Named list/vector: old_value = new_value
#' @param default Value for unmapped items
#'
#' @return Recoded vector
recode_variable <- function(x, mapping, default = NA) {
  
  result <- rep(default, length(x))
  
  for (old_val in names(mapping)) {
    new_val <- mapping[[old_val]]
    
    if (is.na(old_val)) {
      mask <- is.na(x)
    } else {
      mask <- x == old_val
    }
    
    result[mask] <- new_val
  }
  
  # Keep unmapped values
  unmapped <- !(1:length(x) %in% which(!is.na(result)))
  if (any(unmapped)) {
    result[unmapped] <- x[unmapped]
  }
  
  return(result)
}

#' Rename Columns with Multiple Options
#'
#' @param df Data frame
#' @param mapping Named vector: old_name = new_name
#' @param prefix String to add as prefix to all names
#' @param suffix String to add as suffix to all names
#' @param to_lower Convert all names to lowercase
#'
#' @return Data frame with renamed columns
rename_columns <- function(df, mapping = NULL, prefix = NULL, suffix = NULL, to_lower = FALSE) {
  
  names_new <- names(df)
  
  # Apply mapping
  if (!is.null(mapping)) {
    for (old in names(mapping)) {
      if (old %in% names_new) {
        names_new[names_new == old] <- mapping[[old]]
      }
    }
  }
  
  # Apply prefix/suffix
  if (!is.null(prefix)) {
    names_new <- paste0(prefix, names_new)
  }
  if (!is.null(suffix)) {
    names_new <- paste0(names_new, suffix)
  }
  
  # Convert to lowercase
  if (to_lower) {
    names_new <- tolower(names_new)
  }
  
  names(df) <- names_new
  return(df)
}

#' Create Summary of Data Quality Issues
#'
#' @param df Data frame
#' @param quality_score Result from calculate_quality_score()
#'
#' @return Formatted text summary
summarize_quality_issues <- function(df, quality_score) {
  
  summary <- sprintf("Data Quality Score: %d/100\n", quality_score$score)
  summary <- paste0(summary, sprintf("Missing: %.1f%%\n", quality_score$missing_pct))
  summary <- paste0(summary, sprintf("Duplicates: %d rows\n", quality_score$duplicate_rows))
  summary <- paste0(summary, sprintf("Outliers: %d potential\n", quality_score$outlier_count))
  summary <- paste0(summary, sprintf("Empty columns: %d\n", quality_score$empty_columns))
  
  if (length(quality_score$issues) > 0) {
    summary <- paste0(summary, "\nIssues detected:\n")
    for (issue in quality_score$issues) {
      summary <- paste0(summary, sprintf("• %s\n", issue))
    }
  }
  
  if (length(quality_score$recommendations) > 0) {
    summary <- paste0(summary, "\nRecommendations:\n")
    for (rec in quality_score$recommendations) {
      summary <- paste0(summary, sprintf("• %s\n", rec))
    }
  }
  
  return(summary)
}

# =============================================================================
print("✓ Global configuration loaded successfully (gtsummary-free version)")
# =============================================================================

message("[SAP] Global utilities v2.7-20260709 loaded")  # SAP_MODULE_BANNER
