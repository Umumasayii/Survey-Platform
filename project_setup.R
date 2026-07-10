# =============================================================================
# PROJECT DEVELOPMENT UTILITIES
# =============================================================================
# 
# Run this file at the start of each development session:
# source("project_setup.R")
#
# This file provides:
# - Package installation check
# - Sample dataset generators
# - Development helper functions
# - Quick testing utilities
#
# =============================================================================

# =============================================================================
# 1. PACKAGE MANAGEMENT
# =============================================================================

#' Check and install required packages
#'
#' @param verbose Print installation messages
#'
#' @return Invisible logical vector of installation success
check_packages <- function(verbose = TRUE) {
  
  required_packages <- c(
    # Core Shiny
    "shiny", "bslib", "shinyWidgets", "bsicons", "fontawesome",
    # Tables & Data Display
    "DT", "gt", "reactable", "shinycssloaders",
    # Data Import
    "readxl", "haven", "jsonlite", "googlesheets4",
    # Data Wrangling
    "dplyr", "tidyr", "janitor", "stringr", "forcats",
    # Dates & Times
    "lubridate",
    # Visualization
    "plotly", "ggplot2",
    # Statistics & Reporting
    "gtsummary", "broom", "officer", "rmarkdown"
  )
  
  installed <- required_packages %in% installed.packages()[, "Package"]
  missing <- required_packages[!installed]
  
  if (length(missing) > 0) {
    if (verbose) {
      cat("📦 Installing missing packages:", paste(missing, collapse = ", "), "\n")
    }
    install.packages(missing, dependencies = TRUE)
  }
  
  if (verbose) {
    cat("✓ All required packages are available\n")
  }
  
  invisible(installed)
}

# =============================================================================
# 2. SAMPLE DATA GENERATORS
# =============================================================================

#' Generate Sample Household Survey Dataset
#'
#' Creates a realistic survey dataset for testing
#'
#' @param n_observations Number of survey responses (default: 500)
#'
#' @return Data frame with survey data
#'
#' @examples
#' survey_data <- generate_household_survey(200)
#' head(survey_data)
#'
generate_household_survey <- function(n_observations = 500) {
  
  set.seed(12345)
  
  data.frame(
    respondent_id = 1:n_observations,
    
    # Demographics
    age = sample(18:80, n_observations, replace = TRUE),
    gender = sample(c("Male", "Female", "Other"), n_observations, 
                    replace = TRUE, prob = c(0.48, 0.50, 0.02)),
    province = sample(c("North", "South", "East", "West", "Central"), 
                     n_observations, replace = TRUE),
    
    # Education
    education_level = sample(
      c("Primary", "Secondary", "Tertiary", "Post-graduate"),
      n_observations, replace = TRUE,
      prob = c(0.15, 0.40, 0.35, 0.10)
    ),
    
    # Employment
    employment_status = sample(
      c("Employed", "Unemployed", "Self-employed", "Student", "Retired"),
      n_observations, replace = TRUE,
      prob = c(0.55, 0.15, 0.15, 0.10, 0.05)
    ),
    
    # Income (USD per month)
    monthly_income = round(abs(rnorm(n_observations, mean = 1500, sd = 800))),
    
    # Household
    household_size = sample(1:8, n_observations, replace = TRUE,
                           prob = c(0.10, 0.15, 0.20, 0.20, 0.15, 0.12, 0.05, 0.03)),
    
    # Likert scale responses (1-5)
    satisfied_healthcare = sample(1:5, n_observations, replace = TRUE),
    satisfied_education = sample(1:5, n_observations, replace = TRUE),
    access_water = sample(0:1, n_observations, replace = TRUE),
    
    # Dates
    survey_date = seq(from = as.Date("2023-01-01"), 
                     by = "days", 
                     length.out = n_observations),
    
    # Missing values (realistic patterns)
    income_missing = sample(c(NA_real_, NA_real_, NA_real_, 
                              NA_real_, NA_character_), 
                           n_observations)[1:n_observations]
  ) %>%
    # Introduce some missing values
    dplyr::mutate(
      monthly_income = ifelse(is.na(income_missing) | runif(n_observations) < 0.05,
                             NA_real_, monthly_income),
      satisfied_healthcare = ifelse(runif(n_observations) < 0.08, NA_real_, 
                                    satisfied_healthcare)
    ) %>%
    dplyr::select(-income_missing)
}

#' Generate Health Survey Dataset
#'
#' Creates a health/medical survey dataset
#'
#' @param n_observations Number of observations
#'
#' @return Data frame with health data
#'
generate_health_survey <- function(n_observations = 300) {
  
  set.seed(54321)
  
  data.frame(
    patient_id = 1:n_observations,
    age = sample(18:85, n_observations, replace = TRUE),
    gender = sample(c("M", "F"), n_observations, replace = TRUE, prob = c(0.48, 0.52)),
    
    # Measurements
    systolic_bp = round(rnorm(n_observations, mean = 125, sd = 15)),
    diastolic_bp = round(rnorm(n_observations, mean = 80, sd = 10)),
    heart_rate = round(rnorm(n_observations, mean = 72, sd = 12)),
    bmi = round(rnorm(n_observations, mean = 25, sd = 4), 2),
    
    # Medical history
    diabetes = sample(0:1, n_observations, replace = TRUE, prob = c(0.85, 0.15)),
    hypertension = sample(0:1, n_observations, replace = TRUE, prob = c(0.75, 0.25)),
    heart_disease = sample(0:1, n_observations, replace = TRUE, prob = c(0.95, 0.05)),
    
    # Medications
    n_medications = sample(0:8, n_observations, replace = TRUE),
    
    # Adherence (Likert 1-5)
    medication_adherence = sample(1:5, n_observations, replace = TRUE),
    
    # Visit date
    last_visit = as.Date("2024-01-01") - sample(0:365, n_observations, replace = TRUE)
  )
}

#' Generate Likert Scale Survey Dataset
#'
#' Creates survey data with Likert scale responses
#'
#' @param n_observations Number of respondents
#' @param n_items Number of survey items
#'
#' @return Data frame with Likert responses
#'
generate_likert_survey <- function(n_observations = 200, n_items = 10) {
  
  set.seed(99999)
  
  item_names <- paste0("q", 1:n_items)
  
  df <- data.frame(
    respondent_id = 1:n_observations,
    age_group = sample(c("18-25", "26-35", "36-50", "51+"), 
                      n_observations, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Generate Likert responses (weighted toward middle)
  for (i in 1:n_items) {
    df[[item_names[i]]] <- sample(
      1:5, n_observations, replace = TRUE,
      prob = c(0.05, 0.15, 0.50, 0.20, 0.10)
    )
  }
  
  df
}

#' Save sample datasets to data/examples/
#'
#' Generates and saves example datasets for testing
#'
#' @return Invisible NULL (saves files to disk)
#'
create_sample_datasets <- function() {
  
  # Create directory if needed
  if (!dir.exists("data/examples")) {
    dir.create("data/examples", showWarnings = FALSE, recursive = TRUE)
  }
  
  cat("Generating sample datasets...\n")
  
  # Household survey
  household <- generate_household_survey(500)
  write.csv(household, "data/examples/household_survey.csv", row.names = FALSE)
  cat("✓ Saved: household_survey.csv (500 rows)\n")
  
  # Health survey
  health <- generate_health_survey(300)
  write.csv(health, "data/examples/health_survey.csv", row.names = FALSE)
  cat("✓ Saved: health_survey.csv (300 rows)\n")
  
  # Likert survey
  likert <- generate_likert_survey(200, 10)
  write.csv(likert, "data/examples/likert_survey.csv", row.names = FALSE)
  cat("✓ Saved: likert_survey.csv (200 rows, 10 items)\n")
  
  invisible(NULL)
}

# =============================================================================
# 3. DEVELOPMENT HELPERS
# =============================================================================

#' Quick test of the application
#'
#' Runs the Shiny app with development settings
#'
run_app <- function() {
  shiny::runApp(
    launch.browser = TRUE,
    display.mode = "normal"
  )
}

#' Generate a summary report of the project
#'
#' Shows what's been implemented
#'
project_status <- function() {
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("Survey Analytics Platform - Project Status\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  # Check files
  files_created <- c("app.R", "global.R", "README.md", "DEVELOPMENT.md")
  files_exist <- file.exists(files_created)
  
  cat("📁 Project Files:\n")
  for (i in seq_along(files_created)) {
    status <- if (files_exist[i]) "✓" else "✗"
    cat(sprintf("  %s %s\n", status, files_created[i]))
  }
  
  cat("\n📦 Package Status:\n")
  req_packages <- c("shiny", "bslib", "plotly", "dplyr", "ggplot2")
  for (pkg in req_packages) {
    installed <- pkg %in% installed.packages()[, "Package"]
    status <- if (installed) "✓" else "✗"
    cat(sprintf("  %s %s\n", status, pkg))
  }
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
}

#' Create sample data and run app
#'
#' Quick setup for development
#'
dev_setup <- function() {
  cat("🚀 Development Setup\n\n")
  
  cat("1️⃣  Checking packages...\n")
  check_packages(verbose = TRUE)
  
  cat("\n2️⃣  Generating sample datasets...\n")
  create_sample_datasets()
  
  cat("\n3️⃣  Project status:\n")
  project_status()
  
  cat("✨ Setup complete! Run run_app() to start the application\n\n")
}

# =============================================================================
# 4. TESTING UTILITIES
# =============================================================================

#' Test variable type detection
#'
#' Verify that detect_variable_types() works correctly
#'
test_variable_detection <- function() {
  
  cat("Testing variable type detection...\n")
  
  # Load global utilities
  source("global.R", echo = FALSE)
  
  # Create test data
  test_data <- data.frame(
    numeric_var = rnorm(100),
    integer_var = sample(1:100, 100),
    char_var = sample(letters, 100, replace = TRUE),
    logical_var = sample(c(TRUE, FALSE), 100, replace = TRUE),
    date_var = seq(as.Date("2024-01-01"), by = "days", length.out = 100)
  )
  
  # Test detection
  result <- detect_variable_types(test_data)
  
  print(result)
  
  # Verify
  expected_types <- c("numeric", "integer", "character", "logical", "date")
  detected_types <- result$type
  
  if (all(detected_types == expected_types)) {
    cat("✓ All types detected correctly\n")
    return(invisible(TRUE))
  } else {
    cat("✗ Type detection mismatch\n")
    return(invisible(FALSE))
  }
}

#' Test data cleaning functions
#'
#' Verify data cleaning utilities
#'
test_data_cleaning <- function() {
  
  cat("Testing data cleaning functions...\n")
  
  source("global.R", echo = FALSE)
  
  # Create test data with issues
  test_data <- data.frame(
    var1 = c(1, 2, NA, 4, 5),
    var2 = c("a", "b", "", "d", "e"),
    var3 = c("99", "10", "999", "30", NA)
  )
  
  # Test cleaning
  cleaned <- clean_data(
    test_data,
    options = list(
      trim_whitespace = TRUE,
      remove_empty_cols = FALSE
    )
  )
  
  # Test missing value handling
  handled <- handle_missing_codes(cleaned, missing_codes = c("99", "999"))
  
  cat("✓ Data cleaning functions working\n")
  
  return(invisible(TRUE))
}

# =============================================================================
# 5. AUTO-RUN SETUP
# =============================================================================

# Uncomment the line below to run setup automatically when this file is sourced
# dev_setup()

# =============================================================================
# QUICK START COMMANDS
# =============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║   Survey Analytics Platform - Development Environment     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Available commands:\n\n")
cat("  run_app()              - Start the Shiny application\n")
cat("  dev_setup()            - Full development setup\n")
cat("  check_packages()       - Verify all packages are installed\n")
cat("  create_sample_datasets()  - Generate test data\n")
cat("  project_status()       - Show project status\n")
cat("  test_variable_detection()  - Test variable detection\n")
cat("  test_data_cleaning()   - Test data cleaning\n\n")

cat("Quick start: run dev_setup() then run_app()\n\n")

# =============================================================================
print("✓ Development utilities loaded")
# =============================================================================
