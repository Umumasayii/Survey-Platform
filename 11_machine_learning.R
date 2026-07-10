# =============================================================================
# MODULE 11: MACHINE LEARNING CENTER
# =============================================================================
# Complete ML pipeline:
# - 8+ algorithms (LR, LogReg, DT, RF, XGBoost, SVM, KNN, NB)
# - Train/test split
# - Cross-validation
# - Hyperparameter tuning
# - Model comparison
# - Feature importance
# - ROC curves, confusion matrices
# 
# Architecture: Modular trainers with consistent interface
# =============================================================================

# =============================================================================
# ML UTILITY FUNCTIONS
# =============================================================================

#' Validate ML inputs
validate_ml_inputs <- function(df, predictors, outcome) {
  errors <- character()
  
  if (is.null(df) || nrow(df) == 0) {
    errors <- c(errors, "Dataset is empty")
  }
  
  if (length(predictors) == 0) {
    errors <- c(errors, "Select at least one predictor")
  }
  
  if (is.null(outcome) || outcome == "") {
    errors <- c(errors, "Select outcome variable")
  }
  
  if (outcome %in% predictors) {
    errors <- c(errors, "Outcome cannot be a predictor")
  }
  
  if (length(errors) > 0) {
    return(errors)
  } else {
    return(TRUE)
  }
}

#' Split data train/test
split_data <- function(df, split_ratio = 0.8, seed = 123) {
  set.seed(seed)
  n <- nrow(df)
  train_idx <- sample(1:n, size = floor(n * split_ratio))
  
  list(
    train = df[train_idx, ],
    test = df[-train_idx, ],
    train_idx = train_idx,
    split_ratio = split_ratio
  )
}

#' Train linear regression
train_linear_regression <- function(df, predictors, outcome) {
  formula <- as.formula(sprintf("%s ~ %s", outcome, paste(predictors, collapse = " + ")))
  
  model <- lm(formula, data = df)
  
  list(
    model = model,
    algorithm = "Linear Regression",
    type = "regression",
    formula = formula
  )
}

#' Train logistic regression
train_logistic_regression <- function(df, predictors, outcome) {
  formula <- as.formula(sprintf("%s ~ %s", outcome, paste(predictors, collapse = " + ")))
  
  # Convert outcome to factor if needed
  df[[outcome]] <- as.factor(df[[outcome]])
  
  model <- glm(formula, data = df, family = binomial())
  
  list(
    model = model,
    algorithm = "Logistic Regression",
    type = "classification",
    formula = formula,
    classes = levels(df[[outcome]])
  )
}

#' Train decision tree
train_decision_tree <- function(df, predictors, outcome, max_depth = 5) {
  tryCatch({
    library(rpart)
    
    formula <- as.formula(sprintf("%s ~ %s", outcome, paste(predictors, collapse = " + ")))
    
    model <- rpart(formula, data = df,
                  control = rpart.control(maxdepth = max_depth))
    
    list(
      model = model,
      algorithm = "Decision Tree",
      type = ifelse(is.numeric(df[[outcome]]), "regression", "classification"),
      formula = formula,
      max_depth = max_depth
    )
  }, error = function(e) {
    list(error = e$message)
  })
}

#' Train random forest
train_random_forest <- function(df, predictors, outcome, ntrees = 500) {
  tryCatch({
    library(randomForest)
    
    formula <- as.formula(sprintf("%s ~ %s", outcome, paste(predictors, collapse = " + ")))
    
    model <- randomForest(formula, data = df, ntree = ntrees, importance = TRUE)
    
    list(
      model = model,
      algorithm = "Random Forest",
      type = ifelse(is.numeric(df[[outcome]]), "regression", "classification"),
      formula = formula,
      ntrees = ntrees,
      importance = importance(model)
    )
  }, error = function(e) {
    list(error = e$message)
  })
}

#' Train XGBoost
train_xgboost <- function(df, predictors, outcome, nrounds = 100, max_depth = 5) {
  tryCatch({
    library(xgboost)
    
    # Prepare data
    X <- as.matrix(df[, predictors, drop = FALSE])
    y <- df[[outcome]]
    
    # Numeric outcome (or encode classification)
    if (!is.numeric(y)) {
      y <- as.numeric(y) - 1
      is_classification <- TRUE
    } else {
      is_classification <- FALSE
    }
    
    # Train model
    dtrain <- xgb.DMatrix(X, label = y)
    
    params <- list(
      objective = ifelse(is_classification, "binary:logistic", "reg:squarederror"),
      max_depth = max_depth,
      eta = 0.1,
      verbose = 0
    )
    
    model <- xgb.train(params, dtrain, nrounds = nrounds)
    
    list(
      model = model,
      X = X,
      y = y,
      algorithm = "XGBoost",
      type = ifelse(is_classification, "classification", "regression"),
      nrounds = nrounds,
      max_depth = max_depth,
      predictors = predictors
    )
  }, error = function(e) {
    list(error = e$message)
  })
}

#' Train Support Vector Machine
train_svm <- function(df, predictors, outcome, kernel = "radial") {
  tryCatch({
    library(e1071)
    
    formula <- as.formula(sprintf("%s ~ %s", outcome, paste(predictors, collapse = " + ")))
    
    model <- svm(formula, data = df, kernel = kernel, scale = TRUE)
    
    list(
      model = model,
      algorithm = "Support Vector Machine",
      type = ifelse(is.factor(df[[outcome]]), "classification", "regression"),
      formula = formula,
      kernel = kernel
    )
  }, error = function(e) {
    list(error = e$message)
  })
}

#' Train KNN
train_knn <- function(df, predictors, outcome, k = 5) {
  tryCatch({
    library(class)
    
    X <- df[, predictors]
    y <- df[[outcome]]
    
    list(
      X = X,
      y = y,
      k = k,
      algorithm = "K-Nearest Neighbors",
      type = ifelse(is.factor(y), "classification", "regression"),
      predictors = predictors
    )
  }, error = function(e) {
    list(error = e$message)
  })
}

#' Train Naive Bayes
train_naive_bayes <- function(df, predictors, outcome) {
  tryCatch({
    library(naivebayes)
    
    formula <- as.formula(sprintf("%s ~ %s", outcome, paste(predictors, collapse = " + ")))
    
    model <- naive_bayes(formula, data = df)
    
    list(
      model = model,
      algorithm = "Naive Bayes",
      type = "classification",
      formula = formula
    )
  }, error = function(e) {
    list(error = e$message)
  })
}

#' Generate predictions
generate_predictions <- function(model_obj, test_data, outcome = NULL) {
  algorithm <- model_obj$algorithm
  
  if (algorithm == "XGBoost") {
    X_test <- as.matrix(test_data[, model_obj$predictors])
    preds <- predict(model_obj$model, X_test)
  } else if (algorithm == "K-Nearest Neighbors") {
    library(class)
    preds <- knn(model_obj$X, test_data[, model_obj$predictors], 
                model_obj$y, k = model_obj$k)
  } else {
    preds <- predict(model_obj$model, newdata = test_data)
  }
  
  preds
}

#' Predict the outcome for a SINGLE user-entered row.
#'
#' Returns a list describing the prediction for display:
#'   type            "regression" | "classification"
#'   prediction      point estimate (numeric) or predicted class (character)
#'   ci_lower/upper  95% confidence interval for the MEAN response (lm only)
#'   pi_lower/upper  95% prediction interval for a NEW observation (lm only)
#'   interval_note   why intervals are/aren't available
#'   probabilities   named numeric vector of class probabilities (classifiers)
#'
#' `newrow` is a one-row data.frame already coerced to the training column
#' types (see coerce_newrow() in the server).
predict_single <- function(model_obj, newrow) {
  algo <- model_obj$algorithm
  type <- model_obj$type

  out <- list(type = type, prediction = NA, ci_lower = NA, ci_upper = NA,
              pi_lower = NA, pi_upper = NA, interval_note = "", probabilities = NULL)

  # ---- Linear regression: real confidence + prediction intervals ----------
  if (identical(algo, "Linear Regression")) {
    ci <- predict(model_obj$model, newdata = newrow, interval = "confidence",
                  level = 0.95)
    pi <- predict(model_obj$model, newdata = newrow, interval = "prediction",
                  level = 0.95)
    out$prediction    <- unname(ci[, "fit"])
    out$ci_lower      <- unname(ci[, "lwr"])
    out$ci_upper      <- unname(ci[, "upr"])
    out$pi_lower      <- unname(pi[, "lwr"])
    out$pi_upper      <- unname(pi[, "upr"])
    out$interval_note <- "95% CI = interval for the mean response; 95% PI = interval for a single new observation."
    return(out)
  }

  # ---- Logistic regression: class + probability ---------------------------
  if (identical(algo, "Logistic Regression")) {
    p <- as.numeric(predict(model_obj$model, newdata = newrow,
                            type = "response"))
    classes <- model_obj$classes
    if (length(classes) < 2) classes <- c("0", "1")
    out$prediction    <- classes[(p >= 0.5) + 1L]
    probs <- c(1 - p, p); names(probs) <- classes[seq_len(2)]
    out$probabilities <- probs
    out$interval_note <- "Probability from the fitted logistic model; class assigned at a 0.5 threshold."
    return(out)
  }

  # ---- Other classifiers: try to get class probabilities ------------------
  if (identical(type, "classification")) {
    cls <- tryCatch({
      if (identical(algo, "K-Nearest Neighbors")) {
        # knn needs both frames as numeric matrices with matching columns.
        library(class)
        tr <- data.matrix(model_obj$X[, model_obj$predictors, drop = FALSE])
        te <- data.matrix(newrow[, model_obj$predictors, drop = FALSE])
        as.character(class::knn(tr, te, model_obj$y, k = model_obj$k))
      } else if (identical(algo, "Naive Bayes")) {
        as.character(predict(model_obj$model, newrow))
      } else {
        as.character(predict(model_obj$model, newdata = newrow))
      }
    }, error = function(e) NA_character_)
    out$prediction <- cls

    probs <- tryCatch({
      if (identical(algo, "Random Forest")) {
        pr <- predict(model_obj$model, newdata = newrow, type = "prob")
        stats::setNames(as.numeric(pr[1, ]), colnames(pr))
      } else if (identical(algo, "Naive Bayes")) {
        pr <- predict(model_obj$model, newrow, type = "raw")
        stats::setNames(as.numeric(pr[1, ]), colnames(pr))
      } else if (identical(algo, "Support Vector Machine")) {
        pr <- attr(predict(model_obj$model, newrow, probability = TRUE),
                   "probabilities")
        if (!is.null(pr)) stats::setNames(as.numeric(pr[1, ]), colnames(pr)) else NULL
      } else NULL
    }, error = function(e) NULL)
    out$probabilities <- probs
    out$interval_note <- if (is.null(probs))
      "This model returns a class label; probability estimates are not available for it."
    else "Class probabilities from the fitted model."
    return(out)
  }

  # ---- Other regressors: point estimate only ------------------------------
  pt <- tryCatch({
    if (identical(algo, "XGBoost")) {
      as.numeric(predict(model_obj$model,
                         as.matrix(newrow[, model_obj$predictors])))
    } else {
      as.numeric(predict(model_obj$model, newdata = newrow))
    }
  }, error = function(e) NA_real_)
  out$prediction    <- pt
  out$interval_note <- "Point estimate only; prediction intervals are available for Linear Regression."
  out
}

#' Calculate evaluation metrics
calculate_metrics <- function(actual, predicted, type = "regression") {
  if (type == "regression") {
    mae <- mean(abs(actual - predicted))
    mse <- mean((actual - predicted)^2)
    rmse <- sqrt(mse)
    r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
    
    return(list(
      MAE = mae,
      MSE = mse,
      RMSE = rmse,
      R_squared = r2
    ))
  } else {
    # Classification
    confusion <- table(actual, predicted)
    accuracy <- sum(diag(confusion)) / sum(confusion)
    precision <- diag(confusion) / colSums(confusion)
    recall <- diag(confusion) / rowSums(confusion)
    f1 <- 2 * (precision * recall) / (precision + recall)
    
    return(list(
      Accuracy = accuracy,
      Precision = mean(precision, na.rm = TRUE),
      Recall = mean(recall, na.rm = TRUE),
      F1 = mean(f1, na.rm = TRUE),
      Confusion_Matrix = confusion
    ))
  }
}

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_machine_learning_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("robot"), "Machine Learning"),
      p("Build, train, and compare ML models", class = "text-muted")
    ),
    
    # No data message
    uiOutput(ns("no_data_message")),
    
    # ML interface
    uiOutput(ns("ml_content"))
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_machine_learning_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # Base-graphics plots cannot be captured for the report, so the
    # performance plot is now a ggplot; pc holds it for "Add to Report".
    pc <- plot_capture()
    
    data_exists <- reactive({
      req(app_state$data_imported)
      !is.null(app_state$processed_data) && nrow(app_state$processed_data) > 0
    })
    
    df_data <- reactive({
      req(data_exists())
      app_state$processed_data
    })
    
    models <- reactiveValues()
    
    # No data message
    output$no_data_message <- renderUI({
      if (!data_exists()) {
        empty_state(
          icon = "robot",
          title = "No data loaded",
          message = "Import data first to train ML models"
        )
      }
    })
    
    # ML content
    output$ml_content <- renderUI({
      if (!data_exists()) return(NULL)
      
      numeric_cols <- names(df_data())[sapply(df_data(), is.numeric)]
      
      tagList(
        div(
          class = "row",
          # Model selection
          div(class = "col-md-3",
            div(
              class = "card",
              div(class = "card-header", h5("Configuration")),
              div(class = "card-body",
                # Algorithm selection
                selectInput(ns("algorithm"),
                           "Algorithm:",
                           choices = c(
                             "Linear Regression",
                             "Logistic Regression",
                             "Decision Tree",
                             "Random Forest",
                             "XGBoost",
                             "Support Vector Machine",
                             "K-Nearest Neighbors",
                             "Naive Bayes"
                           )),
                
                # Outcome variable
                selectInput(ns("outcome"),
                           "Outcome Variable:",
                           choices = numeric_cols),
                
                # Predictors
                selectInput(ns("predictors"),
                           "Predictors:",
                           choices = numeric_cols,
                           multiple = TRUE),
                
                # Train/test split
                sliderInput(ns("split_ratio"),
                           "Train/Test Split:",
                           min = 0.5, max = 0.9, value = 0.8, step = 0.05),
                
                # Train button
                actionButton(ns("btn_train"),
                           "Train Model",
                           class = "btn-primary w-100")
              )
            )
          ),
          
          # Results
          div(class = "col-md-9",
            navset_tab(
              nav_panel(
                title = tagList(icon("gauge-high"), " Performance"),
                div(class = "card border-0",
                  div(class = "card-body",
                    tableOutput(ns("metrics_table")),
                    div(class = "mt-2 text-end",
                        report_button(ns, "add_ml_metrics", "Add metrics"),
                        report_button(ns, "add_ml_confusion", "Add confusion matrix")),
                    br(),
                    plotOutput(ns("performance_plot")),
                    div(class = "mt-2 text-end", report_button(ns, "add_ml_performance_plot", "Add to Report"))
                  )
                )
              ),
              nav_panel(
                title = tagList(icon("wand-magic-sparkles"), " Predict"),
                div(class = "card border-0",
                  div(class = "card-body",
                    uiOutput(ns("predict_panel"))
                  )
                )
              )
            )
          )
        )
      )
    })
    
    # Train model
    observeEvent(input$btn_train, {
      tryCatch({
        # Validate inputs
        validation <- validate_ml_inputs(df_data(), input$predictors, input$outcome)
        if (is.character(validation)) {
          showNotification(paste(validation, collapse = "\n"), type = "error")
          return()
        }
        
        # Split data
        split <- split_data(df_data(), input$split_ratio)
        
        # Train based on algorithm
        if (input$algorithm == "Linear Regression") {
          model_obj <- train_linear_regression(split$train, input$predictors, input$outcome)
        } else if (input$algorithm == "Logistic Regression") {
          model_obj <- train_logistic_regression(split$train, input$predictors, input$outcome)
        } else if (input$algorithm == "Decision Tree") {
          model_obj <- train_decision_tree(split$train, input$predictors, input$outcome)
        } else if (input$algorithm == "Random Forest") {
          model_obj <- train_random_forest(split$train, input$predictors, input$outcome)
        } else if (input$algorithm == "XGBoost") {
          model_obj <- train_xgboost(split$train, input$predictors, input$outcome)
        } else if (input$algorithm == "Support Vector Machine") {
          model_obj <- train_svm(split$train, input$predictors, input$outcome)
        } else if (input$algorithm == "K-Nearest Neighbors") {
          model_obj <- train_knn(split$train, input$predictors, input$outcome)
        } else if (input$algorithm == "Naive Bayes") {
          model_obj <- train_naive_bayes(split$train, input$predictors, input$outcome)
        }
        
        if ("error" %in% names(model_obj)) {
          showNotification(model_obj$error, type = "error")
          return()
        }

        # Attach everything the Prediction tab needs, uniformly, so we don't
        # have to touch each train_*() return value. `train_meta` captures the
        # type/levels/range of each predictor from the TRAINING data, which is
        # what the input widgets are built from.
        model_obj$predictors <- input$predictors
        model_obj$outcome    <- input$outcome
        model_obj$train_meta <- lapply(input$predictors, function(v) {
          col <- split$train[[v]]
          if (is.numeric(col)) {
            list(kind = "numeric",
                 min  = suppressWarnings(min(col, na.rm = TRUE)),
                 max  = suppressWarnings(max(col, na.rm = TRUE)),
                 med  = suppressWarnings(stats::median(col, na.rm = TRUE)))
          } else {
            list(kind = "categorical",
                 levels = sort(unique(as.character(col[!is.na(col)]))))
          }
        })
        names(model_obj$train_meta) <- input$predictors

        # Generate predictions
        preds <- generate_predictions(model_obj, split$test, input$outcome)
        
        # Calculate metrics
        metrics <- calculate_metrics(split$test[[input$outcome]], preds, 
                                    model_obj$type)
        
        # Store model
        models$current <- model_obj
        models$split <- split
        models$metrics <- metrics
        models$predictions <- preds
        
        showNotification("Model trained successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Training failed:", e$message), type = "error")
      })
    })
    
    # Scalar metrics only. calculate_metrics() also returns Confusion_Matrix
    # (a `table`), so `unlist(models$metrics)` is longer than
    # `names(models$metrics)` for classification -> data.frame() errors with
    # "arguments imply differing number of rows". Filter to length-1 atomics.
    ml_metrics_df <- reactive({
      req(models$current)
      m <- models$metrics
      keep <- vapply(m, function(v) is.atomic(v) && length(v) == 1L, logical(1))
      data.frame(
        Metric = names(m)[keep],
        Value  = round(unlist(m[keep], use.names = FALSE), 4),
        row.names = NULL, stringsAsFactors = FALSE
      )
    })

    # Confusion matrix as a tidy data.frame (classification only).
    ml_confusion_df <- reactive({
      req(models$current)
      cm <- models$metrics$Confusion_Matrix
      req(!is.null(cm))
      out <- as.data.frame.matrix(cm, stringsAsFactors = FALSE)
      cn <- colnames(out); cn[is.na(cn) | cn == ""] <- "(Missing)"
      colnames(out) <- make.unique(cn)
      data.frame(Actual = rownames(out), out, row.names = NULL,
                 check.names = FALSE, stringsAsFactors = FALSE)
    })

    # Display metrics
    output$metrics_table <- renderTable({
      req(models$current)
      ml_metrics_df()
    })

    # Performance plot. Rebuilt as ggplot (was base graphics) so it can be
    # captured and rasterized for the report.
    ml_performance_plot <- reactive({
      req(models$current)

      actual    <- models$split$test[[input$outcome]]
      predicted <- models$predictions

      if (identical(models$current$type, "regression")) {
        d <- data.frame(actual = as.numeric(actual),
                        predicted = as.numeric(predicted))
        ggplot2::ggplot(d, ggplot2::aes(x = actual, y = predicted)) +
          ggplot2::geom_point(colour = "#0d6efd", alpha = 0.6, size = 2) +
          ggplot2::geom_abline(slope = 1, intercept = 0,
                               colour = "red", linewidth = 1) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "Actual vs Predicted",
                        x = "Actual", y = "Predicted")
      } else {
        cm <- models$metrics$Confusion_Matrix
        req(!is.null(cm))
        d <- as.data.frame(cm, stringsAsFactors = FALSE)
        names(d) <- c("Actual", "Predicted", "Freq")
        ggplot2::ggplot(d, ggplot2::aes(x = Predicted, y = Actual, fill = Freq)) +
          ggplot2::geom_tile(colour = "white") +
          ggplot2::geom_text(ggplot2::aes(label = Freq), colour = "white",
                             size = 4) +
          ggplot2::scale_fill_viridis_c() +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "Confusion Matrix")
      }
    })

    output$performance_plot <- renderPlot({
      print(pc$keep("performance_plot", ml_performance_plot()))
    })

    # =========================================================================
    # PREDICTION TAB
    # =========================================================================

    # One input widget per predictor, built from the TRAINING column metadata
    # captured at train time (numeric -> slider seeded at the median;
    # categorical -> dropdown of the levels seen in training).
    output$predict_panel <- renderUI({
      mo <- models$current
      if (is.null(mo) || is.null(mo$train_meta)) {
        return(div(class = "alert alert-info",
                   icon("circle-info"),
                   " Train a model first, then come back here to make predictions."))
      }

      inputs <- lapply(mo$predictors, function(v) {
        meta <- mo$train_meta[[v]]
        iid  <- ns(paste0("pred_in_", v))
        if (identical(meta$kind, "numeric")) {
          rng <- c(meta$min, meta$max)
          if (!all(is.finite(rng)) || rng[1] == rng[2]) {
            numericInput(iid, v, value = meta$med)
          } else {
            step <- signif((rng[2] - rng[1]) / 100, 2)
            sliderInput(iid, v, min = round(rng[1], 4), max = round(rng[2], 4),
                        value = round(meta$med, 4), step = step)
          }
        } else {
          selectInput(iid, v, choices = meta$levels,
                      selected = meta$levels[1])
        }
      })

      tagList(
        div(class = "mb-2 text-muted small",
            sprintf("Enter predictor values for the trained %s model. Outcome: %s",
                    mo$algorithm, mo$outcome)),
        div(class = "row",
            lapply(inputs, function(w) div(class = "col-md-6 mb-2", w))),
        div(class = "mt-2",
            actionButton(ns("btn_predict"), "Predict",
                         icon = icon("play"), class = "btn btn-primary"),
            report_button(ns, "add_ml_prediction", "Add prediction to Report")),
        hr(),
        uiOutput(ns("predict_result"))
      )
    })

    # Coerce the entered row to the training column types. Character factors
    # from selectInput must become factors with the training levels, or
    # predict() complains about "new levels".
    coerce_newrow <- function(mo) {
      vals <- lapply(mo$predictors, function(v) {
        meta <- mo$train_meta[[v]]
        raw  <- input[[paste0("pred_in_", v)]]
        if (identical(meta$kind, "numeric")) as.numeric(raw)
        else factor(as.character(raw), levels = meta$levels)
      })
      names(vals) <- mo$predictors
      as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    }

    prediction_result <- eventReactive(input$btn_predict, {
      mo <- models$current
      req(mo)
      newrow <- coerce_newrow(mo)
      tryCatch(
        list(res = predict_single(mo, newrow), row = newrow, algo = mo$algorithm,
             outcome = mo$outcome),
        error = function(e) list(error = e$message))
    })

    output$predict_result <- renderUI({
      pr <- prediction_result()
      if (!is.null(pr$error)) {
        return(div(class = "alert alert-danger",
                   icon("triangle-exclamation"), " ", pr$error))
      }
      r <- pr$res

      fmt <- function(x) if (is.numeric(x)) formatC(x, format = "f", digits = 3) else as.character(x)

      headline <- if (identical(r$type, "regression")) {
        div(class = "display-6 text-primary", fmt(r$prediction))
      } else {
        div(class = "display-6 text-primary", as.character(r$prediction))
      }

      interval_block <- NULL
      if (identical(r$type, "regression") && is.finite(r$ci_lower)) {
        interval_block <- div(class = "mt-2",
          div(sprintf("95%% Confidence interval (mean): [%s, %s]",
                      fmt(r$ci_lower), fmt(r$ci_upper))),
          div(sprintf("95%% Prediction interval (new obs.): [%s, %s]",
                      fmt(r$pi_lower), fmt(r$pi_upper)))
        )
      }

      prob_block <- NULL
      if (!is.null(r$probabilities)) {
        ord  <- order(-as.numeric(r$probabilities))
        cls  <- names(r$probabilities)[ord]
        prob <- round(as.numeric(r$probabilities)[ord], 4)
        # Build the table with tags$ (a nested renderTable inside renderUI does
        # not register as its own output and silently fails to draw).
        rows <- lapply(seq_along(cls), function(i) {
          tags$tr(tags$td(cls[i]),
                  tags$td(class = "text-end", formatC(prob[i], format = "f", digits = 4)))
        })
        prob_block <- div(class = "mt-3",
          strong("Class probabilities"),
          tags$table(class = "table table-sm w-auto mt-1",
            tags$thead(tags$tr(tags$th("Class"), tags$th(class = "text-end", "Probability"))),
            tags$tbody(rows)
          )
        )
      }

      div(
        div(class = "text-muted small", sprintf("Predicted %s", pr$outcome)),
        headline,
        interval_block,
        prob_block,
        div(class = "mt-2 text-muted small fst-italic", r$interval_note)
      )
    })

    # A tidy one-row-per-field table of the prediction, for the report.
    ml_prediction_df <- reactive({
      pr <- prediction_result()
      req(is.null(pr$error))
      r <- pr$res

      rows <- list(
        data.frame(Field = sprintf("Predicted %s", pr$outcome),
                   Value = if (is.numeric(r$prediction))
                             formatC(r$prediction, format = "f", digits = 4)
                           else as.character(r$prediction),
                   stringsAsFactors = FALSE)
      )
      if (identical(r$type, "regression") && is.finite(r$ci_lower)) {
        rows <- c(rows, list(
          data.frame(Field = "95% CI (mean)",
                     Value = sprintf("[%.4f, %.4f]", r$ci_lower, r$ci_upper),
                     stringsAsFactors = FALSE),
          data.frame(Field = "95% PI (new obs.)",
                     Value = sprintf("[%.4f, %.4f]", r$pi_lower, r$pi_upper),
                     stringsAsFactors = FALSE)))
      }
      if (!is.null(r$probabilities)) {
        for (nm in names(r$probabilities)) {
          rows <- c(rows, list(
            data.frame(Field = sprintf("P(%s)", nm),
                       Value = formatC(r$probabilities[[nm]], format = "f", digits = 4),
                       stringsAsFactors = FALSE)))
        }
      }
      # inputs used
      row <- pr$row
      for (nm in names(row)) {
        rows <- c(rows, list(
          data.frame(Field = sprintf("input: %s", nm),
                     Value = as.character(row[[nm]][1]),
                     stringsAsFactors = FALSE)))
      }
      do.call(rbind, rows)
    })

    # =========================================================================
    # ADD TO REPORT
    # =========================================================================

    register_report_button(
      input, services, "add_ml_prediction", kind = "table",
      title   = function() {
        mo <- models$current
        sprintf("Prediction: %s (%s)",
                if (!is.null(mo)) mo$outcome else "outcome",
                if (!is.null(mo)) mo$algorithm else "model")
      },
      payload = function() tryCatch(ml_prediction_df(), error = function(e) NULL),
      code    = function() {
        mo <- models$current
        if (is.null(mo)) return("")
        sprintf("predict(model, newdata = new_observation, interval = \"prediction\")")
      })

    register_report_button(
      input, services, "add_ml_metrics", kind = "table",
      title   = function() sprintf("Model performance: %s (%s)",
                                   input$algorithm %||% "model",
                                   input$outcome %||% "outcome"),
      payload = function() ml_metrics_df(),
      code    = function() sprintf(
        '# %s predicting %s\nmetrics <- calculate_metrics(test[["%s"]], predictions)',
        input$algorithm %||% "model", input$outcome %||% "outcome",
        input$outcome %||% "outcome"))

    register_report_button(
      input, services, "add_ml_confusion", kind = "table",
      title   = function() sprintf("Confusion matrix: %s", input$outcome %||% "outcome"),
      payload = function() tryCatch(ml_confusion_df(), error = function(e) NULL),
      code    = function() sprintf('table(test[["%s"]], predictions)',
                                   input$outcome %||% "outcome"))

    register_report_button(
      input, services, "add_ml_performance_plot", kind = "plot",
      title   = function() if (identical(models$current$type, "regression"))
                             "Actual vs Predicted" else "Confusion Matrix",
      payload = pc$get("performance_plot"),
      code    = "# see model performance plot")

  })
}

# =============================================================================
# END OF MODULE 11
# =============================================================================

message("[SAP] Machine Learning v2.4-20260708 loaded")  # SAP_MODULE_BANNER
