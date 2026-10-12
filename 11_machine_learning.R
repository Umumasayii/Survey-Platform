# =============================================================================
# MODULE 11: MACHINE LEARNING CENTER
# =============================================================================
# v3.3.8 upgrades:
# - categorical predictors are supported across the model catalogue;
# - categorical outcomes are supported for classification models;
# - binary and multinomial logistic regression;
# - one-hot encoding for XGBoost/KNN without losing factor information;
# - expanded metrics (sensitivity, specificity, PPV, NPV, F1, balanced
#   accuracy, AUC, log loss and Brier score where applicable);
# - coefficient tables, 95% confidence intervals and odds ratios;
# - fit statistics, variable importance, ROC and residual diagnostics;
# - data-aware model recommendations.
# =============================================================================

# =============================================================================
# ML UTILITY FUNCTIONS
# =============================================================================

ml_is_categorical <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x) || is.ordered(x)
}

ml_is_count <- function(x) {
  is.numeric(x) && all(is.na(x) | (x >= 0 & abs(x - round(x)) < .Machine$double.eps^0.5))
}

ml_clean_frame <- function(df, vars) {
  out <- as.data.frame(df[, vars, drop = FALSE], stringsAsFactors = FALSE)
  for (v in names(out)) {
    if (is.character(out[[v]]) || is.logical(out[[v]])) out[[v]] <- factor(out[[v]])
    if (is.factor(out[[v]])) out[[v]] <- droplevels(out[[v]])
  }
  out
}

ml_resolve_task <- function(df, outcome, algorithm, requested = "auto") {
  if (identical(algorithm, "Linear Regression")) return("regression")
  if (algorithm %in% c("Logistic / Multinomial Regression", "Naive Bayes")) return("classification")
  if (identical(requested, "regression")) return("regression")
  if (identical(requested, "classification")) return("classification")
  y <- df[[outcome]]
  if (ml_is_categorical(y)) return("classification")
  # Numeric 0/1 or other two-code outcomes are extremely common in surveys.
  # Treat exactly two observed numeric values as classification in Auto mode;
  # users can explicitly choose Regression when those values are truly numeric.
  if (is.numeric(y) && length(unique(y[!is.na(y)])) == 2L) return("classification")
  "regression"
}

validate_ml_inputs <- function(df, predictors, outcome, algorithm = NULL, task = "auto") {
  errors <- character()
  if (is.null(df) || nrow(df) == 0) errors <- c(errors, "Dataset is empty")
  if (length(predictors) == 0) errors <- c(errors, "Select at least one predictor")
  if (is.null(outcome) || !nzchar(outcome)) errors <- c(errors, "Select an outcome variable")
  if (!is.null(outcome) && outcome %in% predictors) errors <- c(errors, "Outcome cannot also be a predictor")
  missing_vars <- setdiff(c(outcome, predictors), names(df))
  if (length(missing_vars)) errors <- c(errors, paste("Variables not found:", paste(missing_vars, collapse = ", ")))
  if (length(errors)) return(errors)

  resolved <- ml_resolve_task(df, outcome, algorithm %||% "", task)
  y <- df[[outcome]]
  complete_rows <- stats::complete.cases(df[, c(outcome, predictors), drop = FALSE])
  n_complete <- sum(complete_rows)
  if (n_complete < 10) errors <- c(errors, "At least 10 complete observations are recommended for model training")
  for (v in predictors) {
    uv <- unique(df[[v]][complete_rows & !is.na(df[[v]])])
    if (length(uv) < 2L) errors <- c(errors, sprintf("Predictor '%s' has fewer than two observed values after removing incomplete rows", v))
  }

  if (resolved == "regression" && !is.numeric(y)) {
    errors <- c(errors, "Regression requires a numeric outcome. Choose Classification for a categorical outcome.")
  }
  if (resolved == "classification") {
    cls <- unique(as.character(y[!is.na(y)]))
    if (length(cls) < 2) errors <- c(errors, "Classification requires at least two outcome classes")
    if (identical(algorithm, "Logistic / Multinomial Regression") && length(cls) > 20) {
      errors <- c(errors, "The logistic/multinomial outcome has more than 20 classes; consider recoding or a tree-based model")
    }
  }
  if (identical(algorithm, "Naive Bayes") && resolved != "classification") {
    errors <- c(errors, "Naive Bayes is available for classification outcomes")
  }
  if (length(errors)) errors else TRUE
}

ml_prepare_dataset <- function(df, predictors, outcome, task) {
  dat <- ml_clean_frame(df, c(outcome, predictors))
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (task == "classification") {
    dat[[outcome]] <- droplevels(factor(dat[[outcome]]))
  } else {
    dat[[outcome]] <- as.numeric(dat[[outcome]])
  }
  dat
}

split_data <- function(df, split_ratio = 0.8, seed = 123, outcome = NULL, type = "regression") {
  set.seed(seed)
  n <- nrow(df)
  if (type == "classification" && !is.null(outcome)) {
    y <- factor(df[[outcome]])
    idx_by_class <- split(seq_len(n), y)
    train_idx <- sort(unique(unlist(lapply(idx_by_class, function(idx) {
      if (length(idx) <= 1L) return(idx)
      n_train <- min(length(idx) - 1L, max(1L, floor(length(idx) * split_ratio)))
      sample(idx, n_train)
    }), use.names = FALSE)))
  } else {
    n_train <- min(n - 1L, max(1L, floor(n * split_ratio)))
    train_idx <- sample(seq_len(n), size = n_train)
  }
  list(
    train = df[train_idx, , drop = FALSE],
    test = df[setdiff(seq_len(n), train_idx), , drop = FALSE],
    train_idx = train_idx,
    split_ratio = split_ratio
  )
}

ml_matrix_fit <- function(df, predictors, scale = FALSE) {
  # Preserve factor levels from the full prepared dataset. This prevents a rare
  # level that happens not to occur in the training rows from becoming an
  # unknown level when the held-out/test row is encoded.
  xdf <- as.data.frame(df[, predictors, drop = FALSE], stringsAsFactors = FALSE)
  for (v in names(xdf)) {
    if (is.character(xdf[[v]]) || is.logical(xdf[[v]])) xdf[[v]] <- factor(xdf[[v]])
  }
  safe <- paste0("x", seq_along(predictors))
  names(xdf) <- safe
  xlevels <- lapply(xdf, function(x) if (is.factor(x)) levels(x) else NULL)
  mm <- stats::model.matrix(~ . - 1, data = xdf)
  means <- if (scale) colMeans(mm) else rep(0, ncol(mm))
  sds <- if (scale) apply(mm, 2, stats::sd) else rep(1, ncol(mm))
  sds[!is.finite(sds) | sds == 0] <- 1
  if (scale) mm <- sweep(sweep(mm, 2, means, "-"), 2, sds, "/")
  list(
    matrix = mm,
    blueprint = list(predictors = predictors, safe = safe, xlevels = xlevels,
                     columns = colnames(mm), scale = scale, means = means, sds = sds)
  )
}

ml_matrix_apply <- function(df, blueprint) {
  xdf <- as.data.frame(df[, blueprint$predictors, drop = FALSE], stringsAsFactors = FALSE)
  names(xdf) <- blueprint$safe
  for (i in seq_along(xdf)) {
    lev <- blueprint$xlevels[[i]]
    if (!is.null(lev)) xdf[[i]] <- factor(as.character(xdf[[i]]), levels = lev)
  }
  mm <- stats::model.matrix(~ . - 1, data = xdf)
  missing <- setdiff(blueprint$columns, colnames(mm))
  if (length(missing)) {
    extra <- matrix(0, nrow = nrow(mm), ncol = length(missing),
                    dimnames = list(NULL, missing))
    mm <- cbind(mm, extra)
  }
  mm <- mm[, blueprint$columns, drop = FALSE]
  if (isTRUE(blueprint$scale)) {
    mm <- sweep(sweep(mm, 2, blueprint$means, "-"), 2, blueprint$sds, "/")
  }
  mm
}

ml_formula <- function(outcome, predictors) {
  quote_name <- function(x) paste0("`", gsub("`", "", as.character(x), fixed = TRUE), "`")
  lhs <- quote_name(outcome)
  rhs <- paste(vapply(predictors, quote_name, character(1)), collapse = " + ")
  stats::as.formula(paste(lhs, "~", rhs), env = parent.frame())
}

train_linear_regression <- function(df, predictors, outcome) {
  model <- stats::lm(ml_formula(outcome, predictors), data = df)
  list(model = model, algorithm = "Linear Regression", type = "regression",
       formula = stats::formula(model))
}

train_logistic_regression <- function(df, predictors, outcome) {
  df[[outcome]] <- droplevels(factor(df[[outcome]]))
  classes <- levels(df[[outcome]])
  if (length(classes) == 2L) {
    model <- stats::glm(ml_formula(outcome, predictors), data = df, family = stats::binomial())
    return(list(model = model, algorithm = "Logistic / Multinomial Regression",
                subtype = "binary_logistic", type = "classification",
                formula = stats::formula(model), classes = classes))
  }
  if (!requireNamespace("nnet", quietly = TRUE)) {
    return(list(error = "Package 'nnet' is required for multinomial logistic regression"))
  }
  model <- nnet::multinom(ml_formula(outcome, predictors), data = df, trace = FALSE)
  list(model = model, algorithm = "Logistic / Multinomial Regression",
       subtype = "multinomial_logistic", type = "classification",
       formula = stats::formula(model), classes = classes)
}

train_decision_tree <- function(df, predictors, outcome, type, max_depth = 5) {
  tryCatch({
    if (!requireNamespace("rpart", quietly = TRUE)) stop("Package 'rpart' is required")
    if (type == "classification") df[[outcome]] <- factor(df[[outcome]])
    model <- rpart::rpart(ml_formula(outcome, predictors), data = df,
                          method = if (type == "classification") "class" else "anova",
                          control = rpart::rpart.control(maxdepth = max_depth))
    list(model = model, algorithm = "Decision Tree", type = type,
         formula = stats::formula(model), max_depth = max_depth,
         classes = if (type == "classification") levels(df[[outcome]]) else NULL)
  }, error = function(e) list(error = e$message))
}

train_random_forest <- function(df, predictors, outcome, type, ntrees = 500) {
  tryCatch({
    if (!requireNamespace("randomForest", quietly = TRUE)) stop("Package 'randomForest' is required")
    if (type == "classification") df[[outcome]] <- factor(df[[outcome]])
    model <- randomForest::randomForest(ml_formula(outcome, predictors), data = df,
                                        ntree = ntrees, importance = TRUE)
    list(model = model, algorithm = "Random Forest", type = type,
         formula = stats::formula(model), ntrees = ntrees,
         classes = if (type == "classification") levels(df[[outcome]]) else NULL)
  }, error = function(e) list(error = e$message))
}

train_xgboost <- function(df, predictors, outcome, type, nrounds = 100, max_depth = 5) {
  tryCatch({
    if (!sap_optional_available("xgboost")) {
      stop("XGBoost is an optional feature. Install the 'xgboost' package and restart the application.")
    }
    enc <- ml_matrix_fit(df, predictors, scale = FALSE)
    y <- df[[outcome]]
    params <- list(max_depth = max_depth, eta = 0.1, verbose = 0)
    classes <- NULL
    if (type == "classification") {
      yf <- droplevels(factor(y)); classes <- levels(yf)
      y_num <- as.integer(yf) - 1L
      if (length(classes) == 2L) {
        params$objective <- "binary:logistic"
        params$eval_metric <- "logloss"
      } else {
        params$objective <- "multi:softprob"
        params$num_class <- length(classes)
        params$eval_metric <- "mlogloss"
      }
    } else {
      y_num <- as.numeric(y)
      params$objective <- "reg:squarederror"
      params$eval_metric <- "rmse"
    }
    dtrain <- sap_optional_call(
      "xgboost", "xgb.DMatrix", enc$matrix, label = y_num,
      feature = "XGBoost model training"
    )
    model <- sap_optional_call(
      "xgboost", "xgb.train", params = params, data = dtrain,
      nrounds = nrounds, verbose = 0, feature = "XGBoost model training"
    )
    list(model = model, algorithm = "XGBoost", type = type, classes = classes,
         nrounds = nrounds, max_depth = max_depth, predictors = predictors,
         blueprint = enc$blueprint)
  }, error = function(e) list(error = e$message))
}

train_svm <- function(df, predictors, outcome, type, kernel = "radial") {
  tryCatch({
    if (!requireNamespace("e1071", quietly = TRUE)) stop("Package 'e1071' is required")
    if (type == "classification") df[[outcome]] <- factor(df[[outcome]])
    model <- e1071::svm(ml_formula(outcome, predictors), data = df, kernel = kernel,
                        scale = TRUE, probability = (type == "classification"))
    list(model = model, algorithm = "Support Vector Machine", type = type,
         formula = stats::formula(model), kernel = kernel,
         classes = if (type == "classification") levels(df[[outcome]]) else NULL)
  }, error = function(e) list(error = e$message))
}

train_knn <- function(df, predictors, outcome, type, k = 5) {
  tryCatch({
    if (!requireNamespace("class", quietly = TRUE)) stop("Package 'class' is required")
    enc <- ml_matrix_fit(df, predictors, scale = TRUE)
    y <- if (type == "classification") factor(df[[outcome]]) else as.numeric(df[[outcome]])
    list(X = enc$matrix, y = y, k = k, algorithm = "K-Nearest Neighbors",
         type = type, predictors = predictors, blueprint = enc$blueprint,
         classes = if (type == "classification") levels(y) else NULL)
  }, error = function(e) list(error = e$message))
}

train_naive_bayes <- function(df, predictors, outcome) {
  tryCatch({
    if (!requireNamespace("naivebayes", quietly = TRUE)) stop("Package 'naivebayes' is required")
    df[[outcome]] <- factor(df[[outcome]])
    model <- naivebayes::naive_bayes(ml_formula(outcome, predictors), data = df)
    list(model = model, algorithm = "Naive Bayes", type = "classification",
         formula = stats::formula(model), classes = levels(df[[outcome]]))
  }, error = function(e) list(error = e$message))
}

ml_knn_regression_predict <- function(train_x, train_y, test_x, k) {
  vapply(seq_len(nrow(test_x)), function(i) {
    d <- rowSums((sweep(train_x, 2, test_x[i, ], "-"))^2)
    mean(train_y[order(d)[seq_len(min(k, length(d)))]], na.rm = TRUE)
  }, numeric(1))
}

generate_predictions <- function(model_obj, test_data, threshold = 0.5) {
  algo <- model_obj$algorithm
  type <- model_obj$type
  if (type == "regression") {
    pred <- if (identical(algo, "XGBoost")) {
      as.numeric(stats::predict(model_obj$model, ml_matrix_apply(test_data, model_obj$blueprint)))
    } else if (identical(algo, "K-Nearest Neighbors")) {
      te <- ml_matrix_apply(test_data, model_obj$blueprint)
      ml_knn_regression_predict(model_obj$X, model_obj$y, te, model_obj$k)
    } else {
      as.numeric(stats::predict(model_obj$model, newdata = test_data))
    }
    return(list(labels = pred, probabilities = NULL, numeric = pred))
  }

  classes <- model_obj$classes
  probs <- NULL
  labels <- NULL
  if (identical(algo, "Logistic / Multinomial Regression")) {
    if (identical(model_obj$subtype, "binary_logistic")) {
      p <- as.numeric(stats::predict(model_obj$model, newdata = test_data, type = "response"))
      probs <- cbind(1 - p, p); colnames(probs) <- classes
      labels <- classes[ifelse(p >= threshold, 2L, 1L)]
    } else {
      probs <- as.matrix(stats::predict(model_obj$model, newdata = test_data, type = "probs"))
      if (is.null(dim(probs))) probs <- matrix(probs, nrow = 1)
      if (is.null(colnames(probs))) colnames(probs) <- classes
      labels <- colnames(probs)[max.col(probs, ties.method = "first")]
    }
  } else if (identical(algo, "Decision Tree")) {
    labels <- as.character(stats::predict(model_obj$model, newdata = test_data, type = "class"))
    probs <- tryCatch(as.matrix(stats::predict(model_obj$model, newdata = test_data, type = "prob")), error = function(e) NULL)
  } else if (identical(algo, "Random Forest")) {
    labels <- as.character(stats::predict(model_obj$model, newdata = test_data, type = "response"))
    probs <- tryCatch(as.matrix(stats::predict(model_obj$model, newdata = test_data, type = "prob")), error = function(e) NULL)
  } else if (identical(algo, "XGBoost")) {
    te <- ml_matrix_apply(test_data, model_obj$blueprint)
    raw <- as.numeric(stats::predict(model_obj$model, te))
    if (length(classes) == 2L) {
      probs <- cbind(1 - raw, raw); colnames(probs) <- classes
      labels <- classes[ifelse(raw >= threshold, 2L, 1L)]
    } else {
      probs <- matrix(raw, ncol = length(classes), byrow = TRUE)
      colnames(probs) <- classes
      labels <- classes[max.col(probs, ties.method = "first")]
    }
  } else if (identical(algo, "Support Vector Machine")) {
    pr <- stats::predict(model_obj$model, newdata = test_data, probability = TRUE)
    labels <- as.character(pr)
    probs <- attr(pr, "probabilities")
    if (!is.null(probs)) probs <- as.matrix(probs)
  } else if (identical(algo, "K-Nearest Neighbors")) {
    te <- ml_matrix_apply(test_data, model_obj$blueprint)
    labels <- as.character(class::knn(model_obj$X, te, model_obj$y, k = model_obj$k))
  } else if (identical(algo, "Naive Bayes")) {
    labels <- as.character(stats::predict(model_obj$model, test_data))
    probs <- tryCatch(as.matrix(stats::predict(model_obj$model, test_data, type = "prob")),
                      error = function(e) tryCatch(as.matrix(stats::predict(model_obj$model, test_data, type = "raw")), error = function(e2) NULL))
  }
  list(labels = factor(labels, levels = classes), probabilities = probs, numeric = NULL)
}

ml_binary_auc <- function(actual, prob, positive) {
  keep <- is.finite(prob) & !is.na(actual)
  y <- as.character(actual[keep]) == positive
  p <- prob[keep]
  n1 <- sum(y); n0 <- sum(!y)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

ml_roc_df <- function(actual, prob, positive) {
  keep <- is.finite(prob) & !is.na(actual)
  y <- as.character(actual[keep]) == positive
  p <- prob[keep]
  if (!any(y) || all(y)) return(NULL)
  thresholds <- sort(unique(c(Inf, p, -Inf)), decreasing = TRUE)
  out <- lapply(thresholds, function(th) {
    pred <- p >= th
    tp <- sum(pred & y); fp <- sum(pred & !y); fn <- sum(!pred & y); tn <- sum(!pred & !y)
    data.frame(threshold = th, TPR = if ((tp + fn) > 0) tp/(tp+fn) else NA_real_,
               FPR = if ((fp + tn) > 0) fp/(fp+tn) else NA_real_)
  })
  do.call(rbind, out)
}

calculate_metrics <- function(actual, prediction_obj, type = "regression", classes = NULL) {
  if (type == "regression") {
    predicted <- as.numeric(prediction_obj$labels)
    actual <- as.numeric(actual)
    keep <- is.finite(actual) & is.finite(predicted)
    actual <- actual[keep]; predicted <- predicted[keep]
    mae <- mean(abs(actual - predicted)); mse <- mean((actual - predicted)^2)
    denom <- sum((actual - mean(actual))^2)
    r2 <- if (denom > 0) 1 - sum((actual - predicted)^2) / denom else NA_real_
    return(list(MAE = mae, MSE = mse, RMSE = sqrt(mse), R_squared = r2,
                N_test = length(actual)))
  }

  actual <- factor(actual, levels = classes)
  predicted <- factor(prediction_obj$labels, levels = classes)
  confusion <- table(Actual = actual, Predicted = predicted)
  total <- sum(confusion)
  accuracy <- if (total > 0) sum(diag(confusion)) / total else NA_real_
  per <- lapply(seq_along(classes), function(i) {
    tp <- confusion[i, i]; fn <- sum(confusion[i, ]) - tp
    fp <- sum(confusion[, i]) - tp; tn <- total - tp - fn - fp
    sensitivity <- if ((tp + fn) > 0) tp/(tp+fn) else NA_real_
    specificity <- if ((tn + fp) > 0) tn/(tn+fp) else NA_real_
    precision <- if ((tp + fp) > 0) tp/(tp+fp) else NA_real_
    npv <- if ((tn + fn) > 0) tn/(tn+fn) else NA_real_
    f1 <- if (is.finite(precision) && is.finite(sensitivity) && (precision+sensitivity)>0) 2*precision*sensitivity/(precision+sensitivity) else NA_real_
    data.frame(Class = classes[i], Sensitivity = sensitivity, Specificity = specificity,
               Precision_PPV = precision, NPV = npv, F1 = f1,
               Support = sum(confusion[i, ]), stringsAsFactors = FALSE)
  })
  per_class <- do.call(rbind, per)
  out <- list(
    Accuracy = accuracy,
    Balanced_Accuracy = mean(per_class$Sensitivity, na.rm = TRUE),
    Macro_Precision = mean(per_class$Precision_PPV, na.rm = TRUE),
    Macro_Recall = mean(per_class$Sensitivity, na.rm = TRUE),
    Macro_F1 = mean(per_class$F1, na.rm = TRUE),
    N_test = total,
    Confusion_Matrix = confusion,
    Per_Class = per_class
  )

  probs <- prediction_obj$probabilities
  if (!is.null(probs) && nrow(probs) == length(actual)) {
    probs <- as.matrix(probs)
    missing_cols <- setdiff(classes, colnames(probs))
    if (!length(missing_cols)) {
      probs <- probs[, classes, drop = FALSE]
      idx <- match(as.character(actual), classes)
      p_actual <- probs[cbind(seq_len(nrow(probs)), idx)]
      p_actual <- pmax(pmin(p_actual, 1 - 1e-15), 1e-15)
      out$Log_Loss <- -mean(log(p_actual), na.rm = TRUE)
      if (length(classes) == 2L) {
        positive <- classes[2]
        ppos <- probs[, positive]
        out$ROC_AUC <- ml_binary_auc(actual, ppos, positive)
        out$Brier_Score <- mean((as.numeric(actual == positive) - ppos)^2, na.rm = TRUE)
        pc <- per_class[per_class$Class == positive, , drop = FALSE]
        out$Sensitivity <- pc$Sensitivity[1]; out$Specificity <- pc$Specificity[1]
        out$Precision_PPV <- pc$Precision_PPV[1]; out$NPV <- pc$NPV[1]; out$F1 <- pc$F1[1]
      }
    }
  }
  out
}

ml_coefficient_table <- function(model_obj) {
  if (identical(model_obj$algorithm, "Linear Regression")) {
    s <- summary(model_obj$model)$coefficients
    ci <- suppressWarnings(stats::confint(model_obj$model, level = 0.95))
    return(data.frame(Term = rownames(s), Estimate = s[,1], Std_Error = s[,2],
                      Statistic = s[,3], P_value = s[,4], CI_Lower = ci[,1], CI_Upper = ci[,2],
                      row.names = NULL, check.names = FALSE))
  }
  if (identical(model_obj$algorithm, "Logistic / Multinomial Regression") &&
      identical(model_obj$subtype, "binary_logistic")) {
    s <- summary(model_obj$model)$coefficients
    ci <- suppressWarnings(stats::confint.default(model_obj$model, level = 0.95))
    out <- data.frame(Term = rownames(s), Estimate_Log_Odds = s[,1], Std_Error = s[,2],
                      Z_value = s[,3], P_value = s[,4], CI_LogOdds_Lower = ci[,1],
                      CI_LogOdds_Upper = ci[,2], row.names = NULL, check.names = FALSE)
    out$Odds_Ratio <- exp(out$Estimate_Log_Odds)
    out$OR_CI_Lower <- exp(out$CI_LogOdds_Lower)
    out$OR_CI_Upper <- exp(out$CI_LogOdds_Upper)
    return(out)
  }
  if (identical(model_obj$algorithm, "Logistic / Multinomial Regression") &&
      identical(model_obj$subtype, "multinomial_logistic")) {
    sm <- summary(model_obj$model)
    cf <- as.matrix(sm$coefficients); se <- as.matrix(sm$standard.errors)
    if (is.null(rownames(cf))) rownames(cf) <- model_obj$classes[-1]
    rows <- list(); zcrit <- stats::qnorm(0.975); k <- 1L
    for (cl in rownames(cf)) for (term in colnames(cf)) {
      est <- cf[cl, term]; serr <- se[cl, term]; z <- est/serr
      lo <- est - zcrit*serr; hi <- est + zcrit*serr
      rows[[k]] <- data.frame(Outcome_Class = cl, Reference_Class = model_obj$classes[1],
                              Term = term, Estimate_Log_Odds = est, Std_Error = serr,
                              Z_value = z, P_value = 2*stats::pnorm(abs(z), lower.tail = FALSE),
                              CI_LogOdds_Lower = lo, CI_LogOdds_Upper = hi,
                              Odds_Ratio = exp(est), OR_CI_Lower = exp(lo), OR_CI_Upper = exp(hi),
                              stringsAsFactors = FALSE, check.names = FALSE)
      k <- k + 1L
    }
    return(do.call(rbind, rows))
  }
  NULL
}

ml_fit_table <- function(model_obj) {
  m <- model_obj$model
  if (identical(model_obj$algorithm, "Linear Regression")) {
    sm <- summary(m); fs <- sm$fstatistic
    fp <- if (!is.null(fs)) stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE) else NA_real_
    return(data.frame(Metric = c("N", "R-squared", "Adjusted R-squared", "Residual SE", "AIC", "BIC", "logLik", "F statistic", "Model p-value"),
                      Value = c(stats::nobs(m), sm$r.squared, sm$adj.r.squared, sm$sigma,
                                stats::AIC(m), stats::BIC(m), as.numeric(stats::logLik(m)),
                                if (!is.null(fs)) unname(fs[1]) else NA_real_, fp)))
  }
  if (identical(model_obj$algorithm, "Logistic / Multinomial Regression")) {
    ll <- as.numeric(stats::logLik(m))
    null_ll <- tryCatch(as.numeric(stats::logLik(stats::update(m, . ~ 1))), error = function(e) NA_real_)
    pseudo <- if (is.finite(null_ll) && null_ll != 0) 1 - ll/null_ll else NA_real_
    vals <- c(stats::nobs(m), stats::AIC(m), stats::BIC(m), ll, pseudo)
    labs <- c("N", "AIC", "BIC", "logLik", "McFadden pseudo-R2")
    if (inherits(m, "glm")) {
      vals <- c(vals, stats::deviance(m), m$null.deviance, stats::df.residual(m))
      labs <- c(labs, "Residual deviance", "Null deviance", "Residual df")
    }
    return(data.frame(Metric = labs, Value = vals))
  }
  data.frame(Metric = c("Algorithm", "Task"), Value = c(model_obj$algorithm, model_obj$type),
             stringsAsFactors = FALSE)
}

ml_importance_table <- function(model_obj) {
  tryCatch({
    if (identical(model_obj$algorithm, "Random Forest")) {
      imp <- as.data.frame(randomForest::importance(model_obj$model))
      imp$Variable <- rownames(imp); rownames(imp) <- NULL
      return(imp[, c("Variable", setdiff(names(imp), "Variable")), drop = FALSE])
    }
    if (identical(model_obj$algorithm, "Decision Tree")) {
      vi <- model_obj$model$variable.importance
      if (is.null(vi)) return(NULL)
      return(data.frame(Variable = names(vi), Importance = as.numeric(vi), row.names = NULL))
    }
    if (identical(model_obj$algorithm, "XGBoost")) {
      imp <- sap_optional_call(
        "xgboost", "xgb.importance", feature_names = model_obj$blueprint$columns,
        model = model_obj$model, feature = "XGBoost feature importance"
      )
      return(as.data.frame(imp))
    }
    NULL
  }, error = function(e) NULL)
}

ml_recommendation <- function(df, outcome, predictors = character(0)) {
  y <- df[[outcome]]
  if (is.ordered(y)) {
    return("Ordered categorical outcome: consider ordinal logistic regression for inference; tree/forest models are useful for prediction. Report proportional-odds checks, effect estimates and confidence intervals.")
  }
  if (ml_is_categorical(y)) {
    k <- length(unique(as.character(y[!is.na(y)])))
    if (k == 2L) return("Binary outcome: logistic regression is the primary interpretable model (odds ratios + 95% CI). Compare predictive performance with tree, random forest, SVM, XGBoost, KNN or Naive Bayes.")
    return("Multiclass outcome: multinomial logistic regression provides class-vs-reference odds ratios; tree, random forest, SVM, XGBoost, KNN and Naive Bayes are suitable predictive alternatives.")
  }
  if (is.numeric(y) && length(unique(y[!is.na(y)])) == 2L) {
    return("Two-code numeric outcome detected: Auto treats this as binary classification. Logistic regression provides odds ratios + 95% CI; you can explicitly choose Regression if the two values are genuinely quantitative.")
  }
  if (ml_is_count(y) && length(unique(y[!is.na(y)])) < max(30, nrow(df)/5)) {
    return("Non-negative count outcome: Poisson or negative-binomial regression is usually preferable for inference. ML regressors can still be used for prediction, but evaluate count-specific residual/dispersion behaviour.")
  }
  "Continuous numeric outcome: linear regression is the interpretable baseline. Compare RMSE/MAE/R² against tree, random forest, XGBoost, SVM or KNN; inspect residuals and confidence intervals for the linear model."
}

predict_single <- function(model_obj, newrow, threshold = 0.5) {
  out <- list(type = model_obj$type, prediction = NA, ci_lower = NA, ci_upper = NA,
              pi_lower = NA, pi_upper = NA, interval_note = "", probabilities = NULL)
  if (identical(model_obj$algorithm, "Linear Regression")) {
    ci <- stats::predict(model_obj$model, newdata = newrow, interval = "confidence", level = 0.95)
    pi <- stats::predict(model_obj$model, newdata = newrow, interval = "prediction", level = 0.95)
    out$prediction <- unname(ci[,"fit"]); out$ci_lower <- unname(ci[,"lwr"]); out$ci_upper <- unname(ci[,"upr"])
    out$pi_lower <- unname(pi[,"lwr"]); out$pi_upper <- unname(pi[,"upr"])
    out$interval_note <- "95% CI is for the mean response; 95% PI is for a new observation."
    return(out)
  }
  pr <- generate_predictions(model_obj, newrow, threshold = threshold)
  if (model_obj$type == "classification") {
    out$prediction <- as.character(pr$labels[1])
    if (!is.null(pr$probabilities)) {
      probs <- as.numeric(pr$probabilities[1, ]); names(probs) <- colnames(pr$probabilities)
      out$probabilities <- probs
    }
    out$interval_note <- sprintf("Class prediction%s.", if (length(model_obj$classes) == 2L) paste0(" using threshold ", threshold) else "")
  } else {
    out$prediction <- as.numeric(pr$labels[1]); out$interval_note <- "Point prediction from the fitted model."
  }
  out
}

# =============================================================================
# UI
# =============================================================================

mod_machine_learning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "mb-4", h2(icon("robot"), "Machine Learning & Models"),
        p("Mixed-type predictors, classification/regression, inference, diagnostics and prediction", class = "text-muted")),
    uiOutput(ns("no_data_message")),
    uiOutput(ns("ml_content"))
  )
}

# =============================================================================
# SERVER
# =============================================================================

mod_machine_learning_server <- function(id, app_state, services = NULL, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pc <- plot_capture()
    ml_setting <- function(key, fallback) {
      if (!is.null(services) && !is.null(services$settings)) {
        return(services$settings$get(key, fallback))
      }
      fallback
    }

    data_exists <- reactive({
      req(app_state$data_imported)
      !is.null(app_state$processed_data) && nrow(app_state$processed_data) > 0
    })
    df_data <- reactive({ req(data_exists()); app_state$processed_data })
    models <- reactiveValues()

    output$no_data_message <- renderUI({
      if (!data_exists()) empty_state(icon = "robot", title = "No data loaded",
                                      message = "Import data first to train ML models")
    })

    output$ml_content <- renderUI({
      if (!data_exists()) return(NULL)
      vars <- names(df_data())
      tagList(
        div(class = "row",
          div(class = "col-md-3",
            div(class = "card mb-3",
              div(class = "card-header", h5("Configuration")),
              div(class = "card-body",
                selectInput(ns("algorithm"), "Algorithm:", choices = c(
                  "Linear Regression", "Logistic / Multinomial Regression", "Decision Tree",
                  "Random Forest", "XGBoost", "Support Vector Machine", "K-Nearest Neighbors", "Naive Bayes")),
                selectInput(ns("task_type"), "Task type:",
                            choices = c("Automatic from outcome" = "auto", "Regression" = "regression", "Classification" = "classification"),
                            selected = "auto"),
                selectInput(ns("outcome"), "Outcome variable:", choices = vars),
                selectInput(ns("predictors"), "Predictors (numeric or categorical):",
                            choices = vars, multiple = TRUE),
                sliderInput(ns("split_ratio"), "Train/Test split:", min = 0.5, max = 0.9,
                            value = as.numeric(ml_setting("ml_split", 0.8)), step = 0.05),
                sliderInput(ns("class_threshold"), "Binary classification threshold:", min = 0.05, max = 0.95, value = 0.5, step = 0.05),
                actionButton(ns("btn_train"), "Train Model", class = "btn-primary w-100")
              )
            ),
            uiOutput(ns("ml_recommendation"))
          ),
          div(class = "col-md-9",
            navset_tab(
              nav_panel(title = tagList(icon("gauge-high"), " Performance"),
                div(class = "card border-0", div(class = "card-body",
                  h5("Overall metrics"), tableOutput(ns("metrics_table")),
                  uiOutput(ns("per_class_block")),
                  div(class = "mt-2 d-flex justify-content-end gap-2 align-items-center flex-wrap",
                      ai_assistant_ui(ns("ai_ml_metrics")), report_button(ns, "add_ml_metrics", "Add metrics"),
                      report_button(ns, "add_ml_confusion", "Add confusion matrix")),
                  br(), plotOutput(ns("performance_plot")), plotOutput(ns("roc_plot")),
                  div(class = "mt-2 text-end", report_button(ns, "add_ml_performance_plot", "Add plot to Report"))
                ))
              ),
              nav_panel(title = tagList(icon("chart-line"), " Model Details"),
                div(class = "card border-0", div(class = "card-body",
                  h5("Model fit"), tableOutput(ns("fit_table")),
                  hr(), h5("Coefficients / Effects"),
                  p("For logistic models, odds ratios and 95% confidence intervals are shown. For linear models, coefficient confidence intervals are shown.", class = "text-muted small"),
                  DT::DTOutput(ns("coefficient_table")),
                  hr(), h5("Variable importance"), DT::DTOutput(ns("importance_table")),
                  div(class = "mt-2 d-flex justify-content-end gap-2",
                      report_button(ns, "add_ml_coefficients", "Add coefficients"),
                      report_button(ns, "add_ml_fit", "Add fit statistics"))
                ))
              ),
              nav_panel(title = tagList(icon("stethoscope"), " Diagnostics"),
                div(class = "card border-0", div(class = "card-body",
                  uiOutput(ns("diagnostic_note")), plotOutput(ns("diagnostic_plot"))
                ))
              ),
              nav_panel(title = tagList(icon("wand-magic-sparkles"), " Predict"),
                div(class = "card border-0", div(class = "card-body", uiOutput(ns("predict_panel"))))
              )
            )
          )
        )
      )
    })

    observeEvent(input$outcome, {
      req(data_exists(), input$outcome)
      choices <- setdiff(names(df_data()), input$outcome)
      current <- intersect(input$predictors %||% character(0), choices)
      updateSelectInput(session, "predictors", choices = choices, selected = current)
    }, ignoreInit = TRUE)

    output$ml_recommendation <- renderUI({
      req(input$outcome, data_exists())
      div(class = "card border-info",
          div(class = "card-header", h6(icon("lightbulb"), " Model guidance")),
          div(class = "card-body small", ml_recommendation(df_data(), input$outcome, input$predictors %||% character(0))))
    })

    observeEvent(input$btn_train, {
      tryCatch({
        validation <- validate_ml_inputs(df_data(), input$predictors, input$outcome, input$algorithm, input$task_type)
        if (is.character(validation)) { showNotification(paste(validation, collapse = "\n"), type = "error"); return() }
        type <- ml_resolve_task(df_data(), input$outcome, input$algorithm, input$task_type)
        dat <- ml_prepare_dataset(df_data(), input$predictors, input$outcome, type)
        if (nrow(dat) < 10) { showNotification("Too few complete observations after removing missing values", type = "error"); return() }
        if (type == "classification" && any(table(dat[[input$outcome]]) < 2)) {
          showNotification("Every outcome class needs at least two complete observations for train/test evaluation", type = "error"); return()
        }
        seed <- suppressWarnings(as.integer(ml_setting("random_seed", 123L)))
        if (!is.finite(seed) || seed < 1L) seed <- 123L
        split <- split_data(dat, input$split_ratio, seed = seed,
                            outcome = input$outcome, type = type)
        if (!nrow(split$test)) { showNotification("Train/test split produced no test observations", type = "error"); return() }

        model_obj <- switch(input$algorithm,
          "Linear Regression" = train_linear_regression(split$train, input$predictors, input$outcome),
          "Logistic / Multinomial Regression" = train_logistic_regression(split$train, input$predictors, input$outcome),
          "Decision Tree" = train_decision_tree(split$train, input$predictors, input$outcome, type),
          "Random Forest" = train_random_forest(split$train, input$predictors, input$outcome, type),
          "XGBoost" = train_xgboost(split$train, input$predictors, input$outcome, type),
          "Support Vector Machine" = train_svm(split$train, input$predictors, input$outcome, type),
          "K-Nearest Neighbors" = train_knn(split$train, input$predictors, input$outcome, type),
          "Naive Bayes" = train_naive_bayes(split$train, input$predictors, input$outcome)
        )
        if (!is.null(model_obj$error)) { showNotification(model_obj$error, type = "error"); return() }

        model_obj$predictors <- input$predictors; model_obj$outcome <- input$outcome
        model_obj$input_keys <- stats::setNames(make.unique(make.names(input$predictors)), input$predictors)
        if (is.null(model_obj$classes) && type == "classification") model_obj$classes <- levels(factor(split$train[[input$outcome]]))
        model_obj$train_meta <- lapply(input$predictors, function(v) {
          col <- split$train[[v]]
          if (is.numeric(col)) list(kind = "numeric", min = min(col, na.rm = TRUE), max = max(col, na.rm = TRUE), med = stats::median(col, na.rm = TRUE))
          else list(kind = "categorical", levels = levels(factor(col)))
        }); names(model_obj$train_meta) <- input$predictors

        pred_obj <- generate_predictions(model_obj, split$test, threshold = input$class_threshold)
        metrics <- calculate_metrics(split$test[[input$outcome]], pred_obj, model_obj$type, model_obj$classes)
        models$current <- model_obj; models$split <- split; models$metrics <- metrics; models$predictions <- pred_obj
        models$coefficients <- ml_coefficient_table(model_obj); models$fit <- ml_fit_table(model_obj); models$importance <- ml_importance_table(model_obj)
        app_state$last_model <- model_obj
        app_state$model_predictions <- pred_obj

        saved_suffix <- ""
        if (isTRUE(ml_setting("ml_autosave", FALSE))) {
          key <- sprintf("model_%03d", length(app_state$trained_models %||% list()) + 1L)
          record <- list(
            name = paste(input$algorithm, input$outcome, sep = " — "),
            algorithm = input$algorithm,
            task = type,
            outcome = input$outcome,
            predictors = input$predictors,
            model = model_obj,
            metrics = metrics,
            split = list(train_rows = nrow(split$train), test_rows = nrow(split$test),
                         ratio = input$split_ratio, seed = seed),
            created_at = Sys.time()
          )
          app_state$trained_models[[key]] <- record
          if (!is.null(pool) && !is.null(app_state$current_project) &&
              exists("db_save_model", mode = "function")) {
            db_id <- tryCatch(
              db_save_model(pool, app_state$current_project, model_obj,
                            input$algorithm, metrics, input$predictors,
                            input$outcome, record$name),
              error = function(e) {
                showNotification(paste("Model trained, but database save failed:", e$message),
                                 type = "warning", duration = 8)
                NULL
              }
            )
            if (!is.null(db_id)) app_state$trained_models[[key]]$database_id <- db_id
          }
          saved_suffix <- " Saved to the current project."
        }
        showNotification(sprintf("%s trained successfully (%s).%s",
                                 input$algorithm, type, saved_suffix), type = "message")
      }, error = function(e) showNotification(paste("Training failed:", e$message), type = "error"))
    })

    ml_metrics_df <- reactive({
      req(models$current)
      m <- models$metrics
      keep <- vapply(m, function(v) is.atomic(v) && length(v) == 1L, logical(1))
      data.frame(Metric = names(m)[keep], Value = suppressWarnings(round(as.numeric(unlist(m[keep], use.names = FALSE)), 4)), row.names = NULL)
    })
    ml_confusion_df <- reactive({
      req(models$current); cm <- models$metrics$Confusion_Matrix; req(!is.null(cm))
      out <- as.data.frame.matrix(cm); data.frame(Actual = rownames(out), out, row.names = NULL, check.names = FALSE)
    })
    output$metrics_table <- renderTable({ req(models$current); ml_metrics_df() })
    output$per_class_block <- renderUI({
      req(models$current)
      if (is.null(models$metrics$Per_Class)) return(NULL)
      tagList(hr(), h5("Per-class metrics"), DT::DTOutput(ns("per_class_table")))
    })
    output$per_class_table <- DT::renderDT({
      req(models$metrics$Per_Class)
      DT::datatable(models$metrics$Per_Class, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    output$fit_table <- renderTable({ req(models$fit); models$fit }, striped = TRUE)
    output$coefficient_table <- DT::renderDT({
      df <- models$coefficients
      if (is.null(df) || !nrow(df)) return(DT::datatable(data.frame(Note = "Coefficient inference is not available for this algorithm."), rownames = FALSE))
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })
    output$importance_table <- DT::renderDT({
      df <- models$importance
      if (is.null(df) || !nrow(df)) return(DT::datatable(data.frame(Note = "Native variable importance is not available for this algorithm."), rownames = FALSE))
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    ml_performance_plot <- reactive({
      req(models$current)
      actual <- models$split$test[[models$current$outcome]]
      if (models$current$type == "regression") {
        predicted <- as.numeric(models$predictions$labels)
        d <- data.frame(actual = as.numeric(actual), predicted = predicted)
        ggplot2::ggplot(d, ggplot2::aes(actual, predicted)) + ggplot2::geom_point(alpha = .6, size = 2) +
          ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") + ggplot2::theme_minimal() +
          ggplot2::labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted")
      } else {
        cm <- models$metrics$Confusion_Matrix; d <- as.data.frame(cm); names(d) <- c("Actual", "Predicted", "Freq")
        ggplot2::ggplot(d, ggplot2::aes(Predicted, Actual, fill = Freq)) + ggplot2::geom_tile() +
          ggplot2::geom_text(ggplot2::aes(label = Freq)) + ggplot2::scale_fill_viridis_c() + ggplot2::theme_minimal() +
          ggplot2::labs(title = "Confusion Matrix")
      }
    })
    output$performance_plot <- renderPlot({ print(pc$keep("performance_plot", ml_performance_plot())) })

    output$roc_plot <- renderPlot({
      req(models$current)
      if (models$current$type != "classification" || length(models$current$classes) != 2L || is.null(models$predictions$probabilities)) return(invisible(NULL))
      positive <- models$current$classes[2]; probs <- models$predictions$probabilities
      if (!(positive %in% colnames(probs))) return(invisible(NULL))
      d <- ml_roc_df(models$split$test[[models$current$outcome]], probs[, positive], positive)
      if (is.null(d)) return(invisible(NULL))
      auc <- models$metrics$ROC_AUC %||% NA_real_
      print(ggplot2::ggplot(d, ggplot2::aes(FPR, TPR)) + ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") + ggplot2::coord_equal() + ggplot2::theme_minimal() +
        ggplot2::labs(title = sprintf("ROC Curve (AUC = %.3f)", auc), x = "False Positive Rate", y = "True Positive Rate"))
    })

    output$diagnostic_note <- renderUI({
      req(models$current)
      if (models$current$type == "regression") {
        div(class = "alert alert-info", "Regression diagnostics show residuals versus fitted values. Linear-regression inference additionally assumes approximately normal, independent residuals with stable variance.")
      } else if (identical(models$current$algorithm, "Logistic / Multinomial Regression")) {
        div(class = "alert alert-info", "Logistic inference is based on log-odds. Review odds ratios, 95% CI, class balance, discrimination (AUC for binary outcomes), calibration-oriented scores (Brier/log loss), and influential observations when making final inferences.")
      } else {
        div(class = "alert alert-secondary", "For predictive models, prioritize held-out performance, class-specific metrics, calibration/probabilities where available, and variable importance. Coefficient-style odds ratios do not apply to every ML algorithm.")
      }
    })
    output$diagnostic_plot <- renderPlot({
      req(models$current)
      if (models$current$type == "regression") {
        actual <- as.numeric(models$split$test[[models$current$outcome]])
        fitted <- as.numeric(models$predictions$labels); residual <- actual - fitted
        d <- data.frame(fitted = fitted, residual = residual)
        print(ggplot2::ggplot(d, ggplot2::aes(fitted, residual)) + ggplot2::geom_point(alpha = .6) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + ggplot2::theme_minimal() +
          ggplot2::labs(title = "Held-out Residuals vs Predicted", x = "Predicted", y = "Residual"))
      }
    })

    output$predict_panel <- renderUI({
      mo <- models$current
      if (is.null(mo) || is.null(mo$train_meta)) return(div(class = "alert alert-info", icon("circle-info"), " Train a model first, then make a prediction."))
      inputs <- lapply(mo$predictors, function(v) {
        meta <- mo$train_meta[[v]]; iid <- ns(paste0("pred_in_", mo$input_keys[[v]]))
        if (meta$kind == "numeric") {
          if (!all(is.finite(c(meta$min, meta$max))) || meta$min == meta$max) numericInput(iid, v, value = meta$med)
          else sliderInput(iid, v, min = meta$min, max = meta$max, value = meta$med, step = signif((meta$max-meta$min)/100, 2))
        } else selectInput(iid, v, choices = meta$levels, selected = meta$levels[1])
      })
      tagList(div(class = "mb-2 text-muted small", sprintf("Enter values for %s. Outcome: %s", mo$algorithm, mo$outcome)),
              div(class = "row", lapply(inputs, function(w) div(class = "col-md-6 mb-2", w))),
              div(class = "mt-2 d-flex align-items-center gap-2 flex-wrap", actionButton(ns("btn_predict"), "Predict", icon = icon("play"), class = "btn btn-primary"),
                  ai_assistant_ui(ns("ai_ml_pred")), report_button(ns, "add_ml_prediction", "Add prediction to Report")),
              hr(), uiOutput(ns("predict_result")))
    })

    coerce_newrow <- function(mo) {
      vals <- lapply(mo$predictors, function(v) {
        meta <- mo$train_meta[[v]]; raw <- input[[paste0("pred_in_", mo$input_keys[[v]])]]
        if (meta$kind == "numeric") as.numeric(raw) else factor(as.character(raw), levels = meta$levels)
      }); names(vals) <- mo$predictors
      as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    }
    prediction_result <- eventReactive(input$btn_predict, {
      mo <- models$current; req(mo); row <- coerce_newrow(mo)
      tryCatch(list(res = predict_single(mo, row, threshold = input$class_threshold), row = row, algo = mo$algorithm, outcome = mo$outcome),
               error = function(e) list(error = e$message))
    })
    output$predict_result <- renderUI({
      pr <- prediction_result(); if (!is.null(pr$error)) return(div(class = "alert alert-danger", pr$error))
      r <- pr$res; fmt <- function(x) if (is.numeric(x)) formatC(x, format = "f", digits = 4) else as.character(x)
      rows <- NULL
      if (!is.null(r$probabilities)) {
        rows <- tags$table(class = "table table-sm w-auto mt-2", tags$thead(tags$tr(tags$th("Class"), tags$th("Probability"))),
          tags$tbody(lapply(names(r$probabilities), function(nm) tags$tr(tags$td(nm), tags$td(fmt(r$probabilities[[nm]]))))))
      }
      div(div(class = "text-muted small", sprintf("Predicted %s", pr$outcome)), div(class = "display-6 text-primary", fmt(r$prediction)),
          if (is.finite(r$ci_lower)) div(sprintf("95%% CI (mean): [%s, %s]", fmt(r$ci_lower), fmt(r$ci_upper)),
                                         sprintf("95%% prediction interval: [%s, %s]", fmt(r$pi_lower), fmt(r$pi_upper))),
          rows, div(class = "mt-2 text-muted small fst-italic", r$interval_note))
    })

    ml_prediction_df <- reactive({
      pr <- prediction_result(); req(is.null(pr$error)); r <- pr$res
      rows <- list(data.frame(Field = sprintf("Predicted %s", pr$outcome), Value = as.character(r$prediction)))
      if (is.finite(r$ci_lower)) rows <- c(rows, list(data.frame(Field = "95% CI (mean)", Value = sprintf("[%.4f, %.4f]", r$ci_lower, r$ci_upper)),
                                                        data.frame(Field = "95% PI (new observation)", Value = sprintf("[%.4f, %.4f]", r$pi_lower, r$pi_upper))))
      if (!is.null(r$probabilities)) for (nm in names(r$probabilities)) rows <- c(rows, list(data.frame(Field = paste0("P(", nm, ")"), Value = sprintf("%.4f", r$probabilities[[nm]]))))
      do.call(rbind, rows)
    })

    mod_ai_assistant_server("ai_ml_metrics", services, payload = function() {
      mo <- models$current; if (is.null(mo)) return(NULL)
      list(analysis_type = sprintf("%s model performance", mo$type), variables = c(mo$outcome, mo$predictors),
           results = ml_metrics_df(), assumptions = "Held-out train/test evaluation; coefficient inference applies only to statistical models that expose coefficients.",
           metadata = list(algorithm = mo$algorithm, model_type = mo$type, outcome = mo$outcome, n_predictors = length(mo$predictors)))
    })
    mod_ai_assistant_server("ai_ml_pred", services, payload = function() {
      pr <- tryCatch(prediction_result(), error = function(e) NULL); if (is.null(pr) || !is.null(pr$error)) return(NULL)
      list(analysis_type = "Model prediction for a new observation", variables = pr$outcome, results = ml_prediction_df(), metadata = list(outcome = pr$outcome))
    })

    register_report_button(input, services, "add_ml_prediction", kind = "table",
      title = function() sprintf("Prediction: %s (%s)", models$current$outcome, models$current$algorithm),
      payload = function() ml_prediction_df(), code = function() "predict(model, newdata = new_observation)")
    register_report_button(input, services, "add_ml_metrics", kind = "table",
      title = function() sprintf("Model performance: %s", models$current$algorithm), payload = function() ml_metrics_df(), code = function() "# held-out model metrics")
    register_report_button(input, services, "add_ml_confusion", kind = "table",
      title = function() sprintf("Confusion matrix: %s", models$current$outcome), payload = function() tryCatch(ml_confusion_df(), error = function(e) NULL), code = function() "table(actual, predicted)")
    register_report_button(input, services, "add_ml_performance_plot", kind = "plot",
      title = function() if (models$current$type == "regression") "Actual vs Predicted" else "Confusion Matrix",
      payload = pc$get("performance_plot"), code = "# model performance plot")
    register_report_button(input, services, "add_ml_coefficients", kind = "table",
      title = function() sprintf("Model coefficients/effects: %s", models$current$algorithm), payload = function() models$coefficients,
      code = function() "summary(model); confint(model); exp(coef(model)) # odds ratios for logistic models")
    register_report_button(input, services, "add_ml_fit", kind = "table",
      title = function() sprintf("Model fit statistics: %s", models$current$algorithm), payload = function() models$fit,
      code = function() "AIC(model); BIC(model); logLik(model)")
  })
}

message("[SAP] Machine Learning v3.3.8 categorical/inference upgrade loaded")  # SAP_MODULE_BANNER
