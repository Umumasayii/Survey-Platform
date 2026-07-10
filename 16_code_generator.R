# =============================================================================
# MODULE 16: CODE GENERATOR
# =============================================================================
# Automatic R code generation:
# - Generate code for every analysis
# - Display, copy, download code
# - Reproducible research
# - Educational value (users learn R)
# - Code templates and snippets
# =============================================================================

# =============================================================================
# CODE GENERATION FUNCTIONS
# =============================================================================

#' Generate code for data import
generate_import_code <- function(filename, sheet = NULL) {
  code <- sprintf('
# Import data from %s
library(readxl)
library(readr)

df <- read_csv("%s")
# or for Excel:
# df <- read_excel("%s", sheet = "%s")

str(df)  # Check structure
head(df)  # Preview data
', basename(filename), filename, filename, sheet %||% "Sheet1")
  
  trimws(code)
}

#' Generate code for data cleaning
generate_cleaning_code <- function(operations) {
  if (length(operations) == 0) {
    return("# No cleaning operations performed")
  }
  
  lines <- c(
    "# Data cleaning pipeline",
    "library(dplyr)",
    "library(tidyr)",
    ""
  )
  
  for (op in operations) {
    lines <- c(
      lines,
      sprintf("# %s", op$description),
      op$code,
      ""
    )
  }
  
  paste(lines, collapse = "\n")
}

#' Generate code for descriptive statistics
generate_descriptive_code <- function(variables, type = "continuous") {
  code <- "# Unsupported variable type for descriptive code generation"

  if (type == "continuous") {
    code <- sprintf('
# Descriptive statistics for continuous variables
library(dplyr)

df %%>%%
  select(%s) %%>%%
  summarise(
    across(
      everything(),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        q25 = ~quantile(., 0.25, na.rm = TRUE),
        q75 = ~quantile(., 0.75, na.rm = TRUE),
        n = ~sum(!is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  )
', paste(variables, collapse = ", "))
  }
  
  trimws(code)
}

#' Generate code for visualization
generate_visualization_code <- function(x, y, plot_type = "scatter") {
  code <- sprintf("# Unsupported plot_type: %s", plot_type)

  if (plot_type == "scatter") {
    code <- sprintf('
library(ggplot2)

ggplot(df, aes(x = %s, y = %s)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(
    title = "Relationship between %s and %s",
    x = "%s",
    y = "%s"
  )
', x, y, x, y, x, y)
  } else if (plot_type == "histogram") {
    code <- sprintf('
library(ggplot2)

ggplot(df, aes(x = %s)) +
  geom_histogram(bins = 30, fill = "#0d6efd", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Distribution of %s")
', x, x)
  }
  
  trimws(code)
}

#' Generate code for statistical test
generate_test_code <- function(test_type, variables) {
  code <- sprintf("# Unsupported test_type: %s", test_type)

  if (test_type == "t_test") {
    code <- sprintf('
# Two-sample t-test
t_result <- t.test(%s ~ %s, data = df)
print(t_result)

# Effect size (Cohen\'s d)
cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  dof <- nx + ny - 2
  return((mean(x) - mean(y)) / sqrt(((nx-1)*sd(x)^2 + (ny-1)*sd(y)^2) / dof))
}
', variables[1], variables[2])
  } else if (test_type == "anova") {
    code <- sprintf('
# One-way ANOVA
anova_result <- aov(%s ~ %s, data = df)
summary(anova_result)

# Post-hoc test (Tukey HSD)
tukey_result <- TukeyHSD(anova_result)
plot(tukey_result)
', variables[1], variables[2])
  } else if (test_type == "correlation") {
    code <- sprintf('
# Pearson correlation
cor_result <- cor.test(%s, %s, data = df)
print(cor_result)

# Correlation matrix
library(ggcorrplot)
cor_matrix <- cor(df[, c(%s)], use = "complete.obs")
ggcorrplot(cor_matrix, method = "circle", type = "lower")
', variables[1], variables[2], paste(sprintf("\"%s\"", variables), collapse = ", "))
  }
  
  trimws(code)
}

#' Generate code for machine learning model
generate_ml_code <- function(algorithm, predictors, outcome, data_split = 0.8) {
  code <- sprintf("# Unsupported algorithm: %s", algorithm)

  if (algorithm == "linear_regression") {
    code <- sprintf('
# Linear regression
library(caret)

# Split data
set.seed(123)
train_index <- createDataPartition(df$%s, p = %.2f, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Fit model
model <- lm(%s ~ %s, data = train_data)
summary(model)

# Predictions
predictions <- predict(model, test_data)
rmse <- sqrt(mean((test_data$%s - predictions)^2))
r_squared <- cor(test_data$%s, predictions)^2

cat("RMSE:", rmse, "\\n")
cat("R-squared:", r_squared, "\\n")
', outcome, data_split, outcome, paste(predictors, collapse = " + "),
outcome, outcome)
  } else if (algorithm == "random_forest") {
    code <- sprintf('
# Random Forest
library(randomForest)
library(caret)

# Split data
set.seed(123)
train_index <- createDataPartition(df$%s, p = %.2f, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Fit model
model <- randomForest(
  %s ~ %s,
  data = train_data,
  ntree = 500,
  mtry = sqrt(length(c(%s)))
)

# Predictions
predictions <- predict(model, test_data)
importance(model)

# Evaluation
if (is.factor(test_data$%s)) {
  # Classification
  confusion_matrix <- table(test_data$%s, predictions)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  cat("Accuracy:", accuracy, "\\n")
} else {
  # Regression
  rmse <- sqrt(mean((test_data$%s - predictions)^2))
  cat("RMSE:", rmse, "\\n")
}
', outcome, data_split, outcome, paste(predictors, collapse = " + "),
paste(sprintf('"%s"', predictors), collapse = ", "),
outcome, outcome, outcome)
  }
  
  trimws(code)
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_code_generator_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("code"), "Generated R Code"),
      p("View and copy R code for all your analyses", class = "text-muted")
    ),
    
    # Code display
    div(
      class = "card",
      div(
        class = "card-header",
        div(
          class = "d-flex justify-content-between align-items-center",
          h5("Reproducible Code"),
          div(
            class = "btn-group",
            actionButton(ns("btn_copy"), "Copy Code",
                        class = "btn-sm btn-outline-primary",
                        icon = icon("copy")),
            downloadButton(ns("btn_download"), "Download R Script",
                         class = "btn-sm btn-outline-primary")
          )
        )
      ),
      div(
        class = "card-body",
        style = "background-color: #f5f5f5; font-family: monospace;",
        div(
          style = "max-height: 600px; overflow-y: auto; padding: 15px;
                   background-color: #2b2b2b; color: #f8f8f2;
                   border-radius: 4px;",
          verbatimTextOutput(ns("code_display"))
        )
      )
    ),
    
    # Code snippets
    br(),
    div(
      class = "card",
      div(class = "card-header", h5("Code Snippets")),
      div(class = "card-body",
        div(
          class = "list-group",
          lapply(c("import", "cleaning", "descriptive", "visualization", "testing", "ml"),
                function(snippet) {
                  actionLink(
                    ns(paste0("snippet_", snippet)),
                    paste0(toupper(substr(snippet, 1, 1)), 
                          substring(snippet, 2)),
                    class = "list-group-item list-group-item-action"
                  )
                })
        )
      )
    )
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_code_generator_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    current_code <- reactiveVal("")
    
    # Display code
    output$code_display <- renderText({
      current_code()
    })
    
    # Copy to clipboard
    observeEvent(input$btn_copy, {
      shinyjs::runjs(sprintf(
        "
        var code = %s;
        navigator.clipboard.writeText(code).then(() => {
          alert('Code copied to clipboard!');
        });
        ",
        jsonlite::toJSON(current_code(), auto_unbox = TRUE)
      ))
    })
    
    # Download R script
    output$btn_download <- downloadHandler(
      filename = function() {
        paste0("analysis_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) {
        writeLines(current_code(), file)
      }
    )
    
    # Snippet handlers
    observeEvent(input$snippet_import, {
      code <- generate_import_code("data.csv")
      current_code(code)
    })
    
    observeEvent(input$snippet_descriptive, {
      code <- generate_descriptive_code(c("var1", "var2", "var3"))
      current_code(code)
    })
    
    observeEvent(input$snippet_visualization, {
      code <- generate_visualization_code("x", "y", "scatter")
      current_code(code)
    })
    
    observeEvent(input$snippet_testing, {
      code <- generate_test_code("t_test", c("outcome", "group"))
      current_code(code)
    })
    
    observeEvent(input$snippet_ml, {
      code <- generate_ml_code("linear_regression",
                              c("var1", "var2", "var3"),
                              "outcome")
      current_code(code)
    })
    
    # Return functions for other modules to use
    list(
      set_code = current_code,
      generate_import_code = generate_import_code,
      generate_cleaning_code = generate_cleaning_code,
      generate_descriptive_code = generate_descriptive_code,
      generate_visualization_code = generate_visualization_code,
      generate_test_code = generate_test_code,
      generate_ml_code = generate_ml_code
    )
  })
}

message("[SAP] Code Generator v2.2.2-20260708 loaded")  # SAP_MODULE_BANNER

# =============================================================================
# END OF MODULE 16
# =============================================================================
