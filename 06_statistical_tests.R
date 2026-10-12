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
  x_clean <- as.numeric(x[!is.na(x)])
  if (length(x_clean) < 3 || length(x_clean) > 5000) {
    return(list(is_normal = NA, statistic = NA, p_value = NA,
                note = "Sample size out of range for Shapiro-Wilk (3-5000)"))
  }
  if (length(unique(x_clean)) < 2L) {
    return(list(is_normal = NA, statistic = NA, p_value = NA,
                note = "Normality is undefined because all observed values are identical"))
  }
  test_result <- tryCatch(stats::shapiro.test(x_clean), error = function(e) NULL)
  if (is.null(test_result)) {
    return(list(is_normal = NA, statistic = NA, p_value = NA,
                note = "Shapiro-Wilk could not be computed for this variable"))
  }
  list(
    is_normal = test_result$p.value >= 0.05,
    statistic = unname(test_result$statistic),
    p_value = test_result$p.value,
    note = ifelse(test_result$p.value >= 0.05,
                  "Normality is not rejected at alpha = 0.05 (p >= 0.05)",
                  "Normality is rejected at alpha = 0.05 (p < 0.05)")
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
  
  # Brown-Forsythe/Levene test using group medians. The base-R fallback keeps
  # inference available without forcing the heavyweight optional car package.
  tryCatch({
    if (nrow(data_clean) < 3L || length(unique(data_clean[[group_var]])) < 2L) {
      stop("At least two groups and three complete observations are required.", call. = FALSE)
    }
    if (sap_optional_available("car")) {
      test_result <- suppressWarnings(sap_optional_call(
        "car", "leveneTest", as.formula(paste0(var, "~", group_var)),
        data = data_clean, feature = "Levene's test"
      ))
      statistic <- as.numeric(test_result$`F value`[[1]])
      p_value <- as.numeric(test_result$`Pr(>F)`[[1]])
    } else {
      groups <- factor(data_clean[[group_var]])
      values <- as.numeric(data_clean[[var]])
      medians <- stats::ave(values, groups, FUN = stats::median)
      deviations <- abs(values - medians)
      anova_table <- stats::anova(stats::lm(deviations ~ groups))
      statistic <- as.numeric(anova_table$`F value`[[1]])
      p_value <- as.numeric(anova_table$`Pr(>F)`[[1]])
    }
    list(
      equal_variance = p_value > 0.05,
      statistic = statistic,
      p_value = p_value,
      note = ifelse(p_value > 0.05,
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
  ok <- !is.na(x) & !is.na(y)
  x_clean <- x[ok]; y_clean <- y[ok]
  if (length(x_clean) < 3) return(list(error = "Need at least 3 complete pairs"))
  test_result <- suppressWarnings(stats::cor.test(x_clean, y_clean, method = method, conf.level = 0.95, exact = FALSE))
  ci <- test_result$conf.int
  lo <- if (length(ci)) unname(ci[1]) else NA_real_
  hi <- if (length(ci)) unname(ci[2]) else NA_real_
  est <- unname(test_result$estimate)
  method_label <- switch(method, pearson = "Pearson", spearman = "Spearman", kendall = "Kendall", capitalize(method))
  ci_text <- if (is.finite(lo) && is.finite(hi)) sprintf(", 95%% CI [%.3f, %.3f]", lo, hi) else ""
  list(
    test = paste0(method_label, " Correlation"), correlation = est,
    statistic = unname(test_result$statistic), df = unname(test_result$parameter %||% NA_real_),
    p_value = test_result$p.value, ci_lower = lo, ci_upper = hi,
    n = length(x_clean), significant = test_result$p.value < 0.05, method = method,
    interpretation = sprintf("Association = %.3f, p = %.4f%s. %s", est, test_result$p.value, ci_text,
                             ifelse(test_result$p.value < 0.05, "Significant.", "Not significant."))
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
  
  test_result <- suppressWarnings(stats::wilcox.test(x_clean, y_clean, conf.int = TRUE, exact = FALSE))
  
  # Calculate effect size (rank-biserial correlation)
  n1 <- length(x_clean)
  n2 <- length(y_clean)
  r <- 1 - (2 * unname(test_result$statistic)) / (n1 * n2)
  ci <- test_result$conf.int
  
  list(
    test = "Mann-Whitney U Test",
    u_statistic = unname(test_result$statistic),
    p_value = test_result$p.value,
    median1 = median(x_clean),
    median2 = median(y_clean),
    ci_lower = if (length(ci)) ci[1] else NA_real_,
    ci_upper = if (length(ci)) ci[2] else NA_real_,
    effect_size = r,
    effect_label = "Rank-biserial correlation",
    method_family = "nonparametric",
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


#' Perform paired Wilcoxon signed-rank test
#'
#' Non-parametric alternative to the paired t-test.
test_wilcoxon_paired <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  if (length(x) != length(y)) {
    return(list(error = sprintf("Paired Wilcoxon test needs equal group sizes (got %d and %d)", length(x), length(y))))
  }
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 2) return(list(error = "Need at least two complete pairs"))
  tr <- suppressWarnings(stats::wilcox.test(x, y, paired = TRUE, exact = FALSE, conf.int = TRUE))
  d <- x - y
  # Rank-biserial style effect for paired signed ranks, based on signed rank sums.
  nz <- d[d != 0 & !is.na(d)]
  eff <- NA_real_
  if (length(nz)) {
    rr <- rank(abs(nz)); wp <- sum(rr[nz > 0]); wn <- sum(rr[nz < 0])
    eff <- (wp - wn) / (wp + wn)
  }
  ci <- tr$conf.int
  list(
    test = "Wilcoxon Signed-Rank Test",
    statistic = unname(tr$statistic), p_value = tr$p.value,
    median_difference = stats::median(d, na.rm = TRUE),
    ci_lower = if (length(ci)) ci[1] else NA_real_,
    ci_upper = if (length(ci)) ci[2] else NA_real_,
    effect_size = eff, effect_label = "Rank-biserial correlation",
    n_pairs = length(x), significant = tr$p.value < 0.05,
    method_family = "nonparametric",
    interpretation = sprintf("Wilcoxon signed-rank V = %.1f, p = %.4f, rank-biserial r = %s. %s",
                             unname(tr$statistic), tr$p.value,
                             ifelse(is.finite(eff), sprintf("%.3f", eff), "NA"),
                             ifelse(tr$p.value < 0.05, "Significant.", "Not significant."))
  )
}

#' Shapiro-Wilk diagnostics appropriate to a group comparison.
#'
#' Independent groups are checked within each group. For paired comparisons the
#' assumption is checked on paired differences, not on the pooled outcome.
normality_for_group_comparison <- function(df, var, group_var, paired = FALSE) {
  dat <- df[!is.na(df[[var]]) & !is.na(df[[group_var]]), c(var, group_var), drop = FALSE]
  g <- droplevels(factor(dat[[group_var]])); lev <- levels(g)
  if (length(lev) != 2L) return(list(is_normal = NA, summary = "Exactly two groups are required."))
  x <- dat[[var]][g == lev[1]]; y <- dat[[var]][g == lev[2]]
  if (isTRUE(paired)) {
    if (length(x) != length(y)) return(list(is_normal = NA, summary = "Paired normality requires equal group sizes."))
    nr <- test_normality(x - y)
    return(list(is_normal = nr$is_normal, p_value = nr$p_value,
                details = data.frame(Component = "Paired differences", W = nr$statistic, P_value = nr$p_value),
                summary = if (isTRUE(nr$is_normal)) "Paired differences do not reject normality."
                          else if (identical(nr$is_normal, FALSE)) "Paired differences reject normality; Wilcoxon signed-rank is recommended."
                          else nr$note))
  }
  res <- lapply(lev, function(z) test_normality(dat[[var]][g == z]))
  pv <- vapply(res, function(z) z$p_value %||% NA_real_, numeric(1))
  ok <- vapply(res, function(z) z$is_normal %||% NA, logical(1))
  decision <- if (any(ok == FALSE, na.rm = TRUE)) FALSE else if (all(ok == TRUE)) TRUE else NA
  list(is_normal = decision, p_value = suppressWarnings(min(pv, na.rm = TRUE)),
       details = data.frame(Group = lev, W = vapply(res, function(z) z$statistic %||% NA_real_, numeric(1)), P_value = pv),
       summary = if (identical(decision, FALSE)) "At least one group rejects normality (p < 0.05); a non-parametric comparison is recommended."
                 else if (identical(decision, TRUE)) "Neither group rejects normality at alpha = 0.05."
                 else "Normality could not be determined for every group (Shapiro-Wilk requires 3-5000 observations).")
}

#' Automatically choose a two-group inferential test.
auto_two_group_test <- function(df, var, group_var, paired = FALSE, method = "auto") {
  dat <- df[!is.na(df[[var]]) & !is.na(df[[group_var]]), c(var, group_var), drop = FALSE]
  g <- droplevels(factor(dat[[group_var]])); lev <- levels(g)
  if (length(lev) != 2L) return(list(error = "Grouping variable must contain exactly two observed groups"))
  x <- dat[[var]][g == lev[1]]; y <- dat[[var]][g == lev[2]]
  nd <- normality_for_group_comparison(dat, var, group_var, paired)
  use_nonparam <- identical(method, "nonparametric") || (identical(method, "auto") && identical(nd$is_normal, FALSE))

  if (use_nonparam) {
    res <- if (isTRUE(paired)) test_wilcoxon_paired(x, y) else test_mann_whitney(x, y)
    if (is.null(res$error)) {
      res$method_family <- "nonparametric"
      res$selection_reason <- if (identical(method, "auto")) nd$summary else "Non-parametric method selected manually."
      res$normality <- nd
      if (!isTRUE(paired)) {
        res$effect_label <- "Rank-biserial correlation"
        res$ci_lower <- res$ci_lower %||% NA_real_; res$ci_upper <- res$ci_upper %||% NA_real_
      }
    }
    return(res)
  }

  res <- test_t_two_sample(x, y, paired = paired)
  if (is.null(res$error)) {
    res$method_family <- "parametric"
    res$selection_reason <- if (identical(method, "auto")) {
      if (is.na(nd$is_normal)) "Normality was inconclusive; parametric test retained. Review Q-Q plots and robustness."
      else "Normality was not rejected; parametric comparison retained. Independent t-tests use Welch's unequal-variance form by default."
    } else "Parametric method selected manually."
    res$normality <- nd; res$effect_label <- "Cohen's d"
  }
  res
}

#' Welch one-way ANOVA for unequal variances.
test_welch_anova <- function(df, var, group_var) {
  dat <- df[!is.na(df[[var]]) & !is.na(df[[group_var]]), c(var, group_var), drop = FALSE]
  tr <- stats::oneway.test(stats::reformulate(group_var, response = var), data = dat, var.equal = FALSE)
  pw <- stats::pairwise.t.test(dat[[var]], dat[[group_var]], p.adjust.method = "holm", pool.sd = FALSE)
  phd <- as.data.frame(as.table(pw$p.value), stringsAsFactors = FALSE)
  names(phd) <- c("Group1", "Group2", "Adjusted_P")
  phd <- phd[!is.na(phd$Adjusted_P), , drop = FALSE]
  list(test = "Welch One-Way ANOVA", f_statistic = unname(tr$statistic), df_between = unname(tr$parameter[1]),
       df_within = unname(tr$parameter[2]), p_value = tr$p.value, significant = tr$p.value < 0.05,
       method_family = "parametric_robust_variance", post_hoc_df = phd,
       interpretation = sprintf("Welch F(%.2f, %.2f) = %.3f, p = %.4f. %s",
                                unname(tr$parameter[1]), unname(tr$parameter[2]), unname(tr$statistic), tr$p.value,
                                ifelse(tr$p.value < 0.05, "Significant group differences.", "No significant group differences.")))
}

#' Automatic 3+ group comparison with groupwise normality and variance checks.
auto_multi_group_test <- function(df, var, group_var, method = "auto") {
  dat <- df[!is.na(df[[var]]) & !is.na(df[[group_var]]), c(var, group_var), drop = FALSE]
  dat[[group_var]] <- droplevels(factor(dat[[group_var]]))
  lev <- levels(dat[[group_var]])
  if (length(lev) < 3L) return(list(error = "Select a grouping variable with at least three observed groups"))
  norm <- lapply(lev, function(z) test_normality(dat[[var]][dat[[group_var]] == z]))
  norm_df <- data.frame(Group = lev,
                        W = vapply(norm, function(z) z$statistic %||% NA_real_, numeric(1)),
                        P_value = vapply(norm, function(z) z$p_value %||% NA_real_, numeric(1)))
  violates <- any(norm_df$P_value < 0.05, na.rm = TRUE)
  use_nonparam <- identical(method, "nonparametric") || (identical(method, "auto") && violates)
  if (use_nonparam) {
    res <- test_kruskal_wallis(dat, var, group_var)
    if (is.null(res$error)) {
      pw <- stats::pairwise.wilcox.test(dat[[var]], dat[[group_var]], p.adjust.method = "holm", exact = FALSE)
      phd <- as.data.frame(as.table(pw$p.value), stringsAsFactors = FALSE); names(phd) <- c("Group1", "Group2", "Adjusted_P")
      res$post_hoc_df <- phd[!is.na(phd$Adjusted_P), , drop = FALSE]
      res$method_family <- "nonparametric"; res$normality_df <- norm_df
      res$selection_reason <- if (identical(method, "auto")) "At least one group had Shapiro-Wilk p < 0.05; Kruskal-Wallis with Holm-adjusted pairwise Wilcoxon was selected." else "Non-parametric method selected manually."
    }
    return(res)
  }
  homo <- test_homogeneity(dat, var, group_var)
  if (identical(method, "auto") && identical(homo$equal_variance, FALSE)) {
    res <- test_welch_anova(dat, var, group_var); res$normality_df <- norm_df; res$homogeneity <- homo
    res$selection_reason <- "Normality was not rejected, but Levene's test rejected equal variances; Welch ANOVA was selected."
    return(res)
  }
  res <- test_anova(dat, var, group_var)
  if (is.null(res$error)) {
    td <- as.data.frame(res$post_hoc[[1]]); td$Comparison <- rownames(td); rownames(td) <- NULL
    res$post_hoc_df <- td[, c("Comparison", "diff", "lwr", "upr", "p adj"), drop = FALSE]
    names(res$post_hoc_df) <- c("Comparison", "Difference", "Lower_CI", "Upper_CI", "Adjusted_P")
    res$method_family <- "parametric"; res$normality_df <- norm_df; res$homogeneity <- homo
    res$selection_reason <- if (identical(method, "auto")) {
      if (any(!is.finite(norm_df$P_value))) "Some Shapiro-Wilk checks were inconclusive; classical ANOVA was retained. Inspect Q-Q plots and robustness before final inference."
      else if (is.na(homo$equal_variance)) "Normality was not rejected, but the variance check was unavailable; classical ANOVA was retained with a recommendation to inspect spread/robustness."
      else "Groupwise normality was not rejected and equal variance was acceptable; classical ANOVA with Tukey HSD was selected."
    } else "Parametric method selected manually."
  }
  res
}

stat_variable_type <- function(x) {
  if (is.ordered(x)) return("ordinal")
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    k <- length(unique(as.character(x[!is.na(x)])))
    return(if (k == 2L) "binary categorical" else "nominal categorical")
  }
  if (is.numeric(x)) {
    vals <- x[!is.na(x)]; u <- length(unique(vals))
    if (u == 2L) return("binary numeric")
    if (length(vals) && all(vals >= 0 & abs(vals - round(vals)) < 1e-8) && u < max(30, length(vals)/5)) return("count-like integer")
    return("continuous numeric")
  }
  "other"
}

analysis_advice <- function(df, outcome, predictor = NULL) {
  ot <- stat_variable_type(df[[outcome]])
  if (is.null(predictor) || !nzchar(predictor) || identical(predictor, "(none)")) {
    txt <- switch(ot,
      "continuous numeric" = "Describe mean/SD and median/IQR; assess distribution. For modeling, linear regression is the baseline if residual assumptions are adequate; robust/quantile approaches are alternatives when they are not.",
      "count-like integer" = "This variable looks count-like. If it truly records event/item counts, use counts/rates and consider Poisson regression; if variance materially exceeds the mean, negative-binomial regression is often preferable. If it is an integer-coded continuous/ordinal measure, choose the model for that measurement scale instead.",
      "binary categorical" = "Use proportions with confidence intervals. For modeling, use binary logistic regression and report odds ratios with 95% confidence intervals.",
      "binary numeric" = "If the two numeric values represent categories, treat the outcome as binary and use logistic regression; otherwise clarify its measurement scale.",
      "nominal categorical" = "Use frequency/proportion tables. For modeling, use multinomial logistic regression or classification models such as trees/random forests.",
      "ordinal" = "Use ordered-category summaries. Ordinal logistic regression is the standard inferential model when proportional-odds assumptions are reasonable.",
      "Inspect the measurement scale before choosing an inferential model.")
    return(list(outcome_type = ot, predictor_type = NA_character_, recommendation = txt))
  }
  pt <- stat_variable_type(df[[predictor]])
  rec <- if (ot == "count-like integer") {
    "If this variable truly represents counts, prefer Poisson regression (or negative-binomial regression when overdispersed) so the mean/variance structure is modeled appropriately. A categorical predictor can enter that model directly; rank-based tests are useful for simple unadjusted comparisons but do not replace a count model when adjusted inference is needed."
  } else if (ot == "continuous numeric" && pt %in% c("binary categorical", "binary numeric")) {
    "For a continuous outcome across two independent groups: Welch t-test when assumptions are acceptable; if Shapiro-Wilk rejects normality, use Mann-Whitney U. For paired data use paired t-test or Wilcoxon signed-rank."
  } else if (ot == "continuous numeric" && pt %in% c("nominal categorical", "ordinal")) {
    "For a numeric outcome across 3+ groups: ANOVA when assumptions hold; Welch ANOVA for unequal variances; if groupwise normality is rejected, use Kruskal-Wallis followed by multiplicity-adjusted pairwise Wilcoxon tests."
  } else if (ot == "continuous numeric" && pt == "continuous numeric") {
    "For two continuous variables: Pearson correlation/linear regression when the linear-model assumptions are reasonable. If distributions are strongly non-normal, ordinal, or dominated by outliers, prefer Spearman (or Kendall) correlation; model inference should be based on residual diagnostics."
  } else if (ot %in% c("binary categorical", "binary numeric")) {
    "Binary outcome: use logistic regression for adjusted inference and report odds ratios, 95% CI and p-values. For a single categorical association, chi-square or Fisher's exact test may be appropriate."
  } else if (ot == "nominal categorical") {
    "Multiclass nominal outcome: use multinomial logistic regression for interpretable class-vs-reference effects, or classification ML models for prediction."
  } else if (ot == "ordinal") {
    "Ordered outcome: use ordinal logistic regression where possible; rank-based tests are appropriate for simple unadjusted comparisons."
  } else {
    "Match the analysis to the outcome measurement scale, study design, independence/paired structure, distribution, sample size and modeling objective."
  }
  list(outcome_type = ot, predictor_type = pt, recommendation = rec)
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

    # Captures the ggplot handed to sap_ggplotly() so "Add to Report" can
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

    observeEvent(list(app_state$data_imported, app_state$settings), {
      if (!isTRUE(app_state$data_imported)) return(invisible(NULL))
      method <- if (!is.null(services) && !is.null(services$settings)) {
        as.character(services$settings$get("correlation_method", "auto"))
      } else "auto"
      if (!method %in% c("auto", "pearson", "spearman", "kendall")) method <- "auto"
      session$onFlushed(function() {
        updateSelectInput(session, "corr_method", selected = method)
      }, once = TRUE)
    }, ignoreInit = FALSE)
    
    # -----------------------------------------------------------------------
    # Variable typing + labelling — dictionary-aware (Phase 3c)
    # -----------------------------------------------------------------------
    # Same contract established in Descriptive Stats (Phase 3b): the Metadata
    # Engine is authoritative WHEN PRESENT and never blocking when absent. If the
    # current project has a dictionary we classify variables by its data_type and
    # label the dropdowns from it; otherwise we fall back to live derivation off
    # the data frame, so a dataset loaded outside the import wizard still works.
    meta_dict <- reactive({
      if (is.null(services) || is.null(services$metadata)) return(NULL)
      services$metadata$version()          # refresh when the dictionary changes
      d <- services$metadata$get()
      if (is.data.frame(d) && nrow(d) > 0) d else NULL
    })

    .num_types <- c("numeric", "integer")
    .cat_types <- c("categorical_character", "character", "logical")

    numeric_cols <- reactive({
      req(data_exists())
      df <- df_data()
      d  <- meta_dict()
      if (!is.null(d)) {
        v <- intersect(d$variable[d$data_type %in% .num_types], names(df))
        if (length(v)) return(v)
      }
      names(df)[vapply(df, is.numeric, logical(1))]
    })

    categorical_cols <- reactive({
      req(data_exists())
      df <- df_data()
      d  <- meta_dict()
      if (!is.null(d)) {
        v <- intersect(d$variable[d$data_type %in% .cat_types], names(df))
        if (length(v)) return(v)
      }
      names(df)[vapply(df, function(x) is.factor(x) || is.character(x), logical(1))]
    })

    # Named choices (label -> variable): dropdowns display labels, return the
    # column name. Falls back to the name when no label is stored.
    labelled_choices <- function(vars) {
      if (!length(vars)) return(character(0))
      d <- meta_dict()
      if (is.null(d)) return(stats::setNames(vars, vars))
      lbl <- vapply(vars, function(v) {
        row <- d[d$variable == v, , drop = FALSE]
        if (nrow(row) && !is.na(row$label[1]) && nzchar(row$label[1]))
          row$label[1] else v
      }, character(1))
      stats::setNames(vars, lbl)
    }

    # Single variable's display label for result titles and AI payloads.
    # This helper was missing in this module, although the t-test AI payload
    # called dlabel(). The resulting "could not find function 'dlabel'" error
    # was swallowed by the reusable AI component and shown as the misleading
    # message "Run the analysis first" even when results were already visible.
    dlabel <- function(v) {
      if (is.null(v) || length(v) == 0L || is.na(v[1]) || !nzchar(v[1])) return(v)
      v <- as.character(v[1])
      d <- meta_dict()
      if (is.null(d)) return(v)
      row <- d[d$variable == v, , drop = FALSE]
      if (nrow(row) && "label" %in% names(row) &&
          !is.na(row$label[1]) && nzchar(row$label[1])) {
        as.character(row$label[1])
      } else {
        v
      }
    }
    
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
                               choices = labelled_choices(numeric_cols()))
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("t_group"), "Grouping variable (2 groups):",
                               choices = labelled_choices(categorical_cols()))
                  )
                ),
                div(class = "row mt-3",
                  div(class = "col-md-4",
                    checkboxInput(ns("t_paired"), "Paired samples?", value = FALSE)
                  ),
                  div(class = "col-md-4",
                    selectInput(ns("t_method"), "Inference mode:",
                                choices = c("Automatic (recommended)" = "auto",
                                            "Parametric" = "parametric",
                                            "Non-parametric" = "nonparametric"),
                                selected = "auto")
                  ),
                  div(class = "col-md-4 pt-4",
                    actionButton(ns("btn_t_test"), "Run Comparison", class = "btn-primary")
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
                div(class = "mt-2 d-flex justify-content-end gap-2 align-items-center",
                    ai_assistant_ui(ns("ai_ttest")),
                    report_button(ns, "add_t_test", "Add to Report"))
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
                               choices = labelled_choices(numeric_cols()))
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("anova_group"), "Grouping variable:",
                               choices = labelled_choices(categorical_cols()))
                  )
                ),
                div(class = "row mt-3",
                  div(class = "col-md-7",
                    selectInput(ns("anova_method"), "Inference mode:",
                                choices = c("Automatic (recommended)" = "auto",
                                            "Parametric" = "parametric",
                                            "Non-parametric" = "nonparametric"),
                                selected = "auto")
                  ),
                  div(class = "col-md-5 pt-4",
                    actionButton(ns("btn_anova"), "Run Group Comparison", class = "btn-primary")
                  )
                )
              )
            ),
            
            # Test results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Group Comparison Results")),
              div(class = "card-body",
                uiOutput(ns("anova_results")),
                div(class = "mt-2 text-end", report_button(ns, "add_anova", "Add to Report"))
              )
            ),
            
            # Post-hoc results
            div(
              class = "card mb-4",
              div(class = "card-header", h5("Post-hoc / Pairwise Comparisons")),
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
                               choices = labelled_choices(numeric_cols()))
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("corr_y"), "Y variable:",
                               choices = labelled_choices(numeric_cols()))
                  )
                ),
                div(class = "row mt-3",
                  div(class = "col-md-6",
                    selectInput(ns("corr_method"), "Correlation method:",
                               choices = c("Automatic (normality-aware)" = "auto",
                                           "Pearson" = "pearson",
                                           "Spearman" = "spearman",
                                           "Kendall" = "kendall"), selected = "auto")
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
                               choices = labelled_choices(categorical_cols()))
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("chi_var2"), "Second variable:",
                               choices = labelled_choices(categorical_cols()))
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
        
        # TAB 5: ANALYSIS ADVISOR
        nav_panel(
          "Analysis Advisor",
          icon = icon("route"),
          div(class = "p-4",
            div(class = "card mb-4",
              div(class = "card-header", h5("Choose Variables")),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-6",
                    selectInput(ns("advisor_outcome"), "Outcome / response:", choices = names(df_data()))
                  ),
                  div(class = "col-md-6",
                    selectInput(ns("advisor_predictor"), "Predictor / grouping variable (optional):",
                                choices = c("(none)", names(df_data())))
                  )
                )
              )
            ),
            div(class = "card mb-4",
              div(class = "card-header", h5("Recommended Analysis")),
              div(class = "card-body", uiOutput(ns("advisor_results")))
            ),
            div(class = "alert alert-secondary small",
                strong("Important: "),
                "Automatic recommendations are decision support, not a substitute for study-design knowledge. Independence, repeated measures, survey weights, clustering, sample size, outliers and the scientific question can change the correct test or model.")
          )
        ),

        # TAB 6: ASSUMPTIONS & DIAGNOSTICS
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
                               choices = labelled_choices(numeric_cols()))
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
    
    observeEvent(input$advisor_outcome, {
      req(input$advisor_outcome, data_exists())
      choices <- c("(none)", setdiff(names(df_data()), input$advisor_outcome))
      selected <- if (!is.null(input$advisor_predictor) && input$advisor_predictor %in% choices) input$advisor_predictor else "(none)"
      updateSelectInput(session, "advisor_predictor", choices = choices, selected = selected)
    }, ignoreInit = TRUE)

    # =========================================================================
    # TAB 1: TWO-SAMPLE TESTS
    # =========================================================================
    
    t_test_result <- reactive({
      req(input$btn_t_test, input$t_var, input$t_group, data_exists())
      auto_two_group_test(df_data(), input$t_var, input$t_group,
                          paired = isTRUE(input$t_paired),
                          method = input$t_method %||% "auto")
    })

    t_normality_result <- reactive({
      req(input$t_var, input$t_group, data_exists())
      normality_for_group_comparison(df_data(), input$t_var, input$t_group,
                                     paired = isTRUE(input$t_paired))
    })

    output$t_normality <- renderUI({
      nr <- t_normality_result()
      cls <- if (identical(nr$is_normal, FALSE)) "alert-warning" else if (identical(nr$is_normal, TRUE)) "alert-success" else "alert-info"
      div(
        h6("Normality decision"),
        div(class = paste("alert py-2", cls), nr$summary),
        if (!is.null(nr$details)) tags$table(class = "table table-sm small",
          tags$thead(tags$tr(lapply(names(nr$details), tags$th))),
          tags$tbody(lapply(seq_len(nrow(nr$details)), function(i)
            tags$tr(lapply(nr$details[i, , drop = FALSE], function(z) tags$td(if (is.numeric(z)) sprintf("%.4f", z) else as.character(z)))))))
      )
    })

    output$t_homogeneity <- renderUI({
      req(input$t_var, input$t_group, data_exists())
      if (isTRUE(input$t_paired)) return(div(h6("Variance assumption"), p("Not used for paired comparisons.", class = "small text-muted")))
      homo_result <- test_homogeneity(df_data(), input$t_var, input$t_group)
      div(h6("Levene's Test"), p(homo_result$note, class = "mb-1 small"),
          p(sprintf("F = %.4f, p = %.4f", homo_result$statistic, homo_result$p_value), class = "small text-muted"))
    })

    output$t_test_results <- renderUI({
      result <- t_test_result()
      if (!is.null(result$error)) return(div(class = "alert alert-warning", result$error))
      details <- stat_result_df(result)
      # Keep the result readable in the app: selection rationale first, then
      # effect/CI/statistic fields through the common result flattener.
      div(
        div(class = if (identical(result$method_family, "nonparametric")) "alert alert-warning" else "alert alert-info",
            strong("Selected method: "), result$test, br(),
            span(result$selection_reason %||% "", class = "small sap-translatable-output")),
        p(result$interpretation %||% "", class = "mb-3 small sap-translatable-output"),
        tags$table(class = "table table-sm",
          tags$tbody(lapply(seq_len(nrow(details)), function(i) {
            tags$tr(tags$th(as.character(details$Statistic[i])), tags$td(as.character(details$Value[i])))
          })))
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
      
      pc$keep("t_plot", p)
      
      sap_ggplotly(p, tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # TAB 2: ANOVA
    # =========================================================================
    
    anova_result <- reactive({
      req(input$btn_anova, input$anova_var, input$anova_group, data_exists())
      auto_multi_group_test(df_data(), input$anova_var, input$anova_group,
                            method = input$anova_method %||% "auto")
    })

    output$anova_results <- renderUI({
      result <- anova_result()
      if (!is.null(result$error)) return(div(class = "alert alert-warning", result$error))
      details <- stat_result_df(result)
      div(
        div(class = if (identical(result$method_family, "nonparametric")) "alert alert-warning" else "alert alert-info",
            strong("Selected method: "), result$test, br(), span(result$selection_reason %||% "", class = "small sap-translatable-output")),
        p(result$interpretation %||% "", class = "mb-3 small sap-translatable-output"),
        if (!is.null(result$normality_df)) tagList(
          h6("Groupwise Shapiro-Wilk checks"),
          tags$table(class = "table table-sm small",
            tags$thead(tags$tr(tags$th("Group"), tags$th("W"), tags$th("p-value"))),
            tags$tbody(lapply(seq_len(nrow(result$normality_df)), function(i) {
              r <- result$normality_df[i, ]; tags$tr(tags$td(as.character(r$Group)), tags$td(sprintf("%.4f", r$W)), tags$td(sprintf("%.4f", r$P_value)))
            })))
        ),
        tags$table(class = "table table-sm", tags$tbody(lapply(seq_len(nrow(details)), function(i)
          tags$tr(tags$th(as.character(details$Statistic[i])), tags$td(as.character(details$Value[i]))))))
      )
    })

    tukey_df <- reactive({
      result <- anova_result(); req(is.null(result$error))
      result$post_hoc_df %||% data.frame(Note = "No post-hoc table is available for this result.", stringsAsFactors = FALSE)
    })

    output$tukey_table <- DT::renderDataTable({
      DT::datatable(tukey_df(), options = list(dom = "lftp", scrollX = TRUE), rownames = FALSE)
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
      
      pc$keep("anova_plot", p)
      
      sap_ggplotly(p, tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # TAB 3: CORRELATION & REGRESSION
    # =========================================================================
    
    corr_method_resolved <- reactive({
      req(input$corr_x, input$corr_y, data_exists())
      requested <- input$corr_method %||% "auto"
      if (!identical(requested, "auto")) return(list(method = requested, reason = "Correlation method selected manually."))
      nx <- test_normality(df_data()[[input$corr_x]])
      ny <- test_normality(df_data()[[input$corr_y]])
      violates <- identical(nx$is_normal, FALSE) || identical(ny$is_normal, FALSE)
      if (violates) {
        list(method = "spearman", reason = "At least one variable has Shapiro-Wilk p < 0.05; Spearman rank correlation was selected automatically.")
      } else {
        list(method = "pearson", reason = if (isTRUE(nx$is_normal) && isTRUE(ny$is_normal))
          "Normality was not rejected for either variable; Pearson correlation was selected automatically."
          else "Normality was inconclusive for at least one variable; Pearson is retained, but inspect the scatterplot/outliers and consider Spearman if the relationship is monotonic rather than linear.")
      }
    })

    corr_result <- reactive({
      req(input$btn_corr, input$corr_x, input$corr_y, data_exists())
      sel <- corr_method_resolved()
      res <- test_correlation(df_data()[[input$corr_x]], df_data()[[input$corr_y]], method = sel$method)
      if (is.null(res$error)) res$selection_reason <- sel$reason
      res
    })

    regression_result <- reactive({
      req(input$btn_corr, input$corr_x, input$corr_y, data_exists())
      test_linear_regression(df_data(), input$corr_y, input$corr_x)
    })

    output$corr_results <- renderUI({
      result <- corr_result()
      if (!is.null(result$error)) return(div(class = "alert alert-warning", result$error))
      div(
        div(class = "alert alert-info py-2", strong("Selected: "), result$test, br(), span(result$selection_reason %||% "", class = "small sap-translatable-output")),
        p(result$interpretation, class = "mb-2 small sap-translatable-output"),
        div(class = if (result$significant) "alert alert-success" else "alert alert-warning",
            ifelse(result$significant, "Statistically significant association.", "Association is not statistically significant at alpha = 0.05."))
      )
    })

    output$regression_results <- renderUI({
      result <- regression_result()
      if (!is.null(result$error)) return(div(class = "alert alert-warning", result$error))
      nr <- tryCatch(test_normality(stats::residuals(stats::lm(stats::reformulate(input$corr_x, response = input$corr_y), data = df_data()))), error = function(e) NULL)
      warn <- if (!is.null(nr) && identical(nr$is_normal, FALSE))
        div(class = "alert alert-warning small", "Linear-regression residuals reject normality. Coefficient estimates may still be useful, but small-sample inference can be unreliable; inspect residual plots and consider robust/transformational or generalized models appropriate to the outcome.") else NULL
      div(p(result$interpretation, class = "mb-2 small sap-translatable-output"), warn,
          div(class = "bg-light p-3 rounded",
              p(sprintf("Equation: Y = %.3f + %.3f * X", result$intercept, result$slope), class = "small mb-2"),
              p(sprintf("R² = %.3f (%.1f%% of variance explained)", result$r_squared, result$r_squared * 100), class = "small")))
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
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#dc3545", fill = "#dc354520") +
        theme_minimal() +
        labs(title = paste0("Relationship between ", x_var, " and ", y_var),
             x = x_var,
             y = y_var) +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
      
      pc$keep("corr_plot", p)
      
      sap_ggplotly(p, tooltip = c("x", "y"))
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
        p(result$interpretation, class = "mb-2 small sap-translatable-output"),
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
    # TAB 5: ANALYSIS ADVISOR
    # =========================================================================

    output$advisor_results <- renderUI({
      req(input$advisor_outcome, data_exists())
      pred <- input$advisor_predictor %||% "(none)"
      adv <- analysis_advice(df_data(), input$advisor_outcome,
                             if (identical(pred, "(none)")) NULL else pred)
      div(
        div(class = "row mb-3",
          div(class = "col-md-6", strong("Outcome type: "), adv$outcome_type),
          div(class = "col-md-6", if (!is.na(adv$predictor_type)) tagList(strong("Predictor type: "), adv$predictor_type) else NULL)
        ),
        div(class = "alert alert-primary", adv$recommendation),
        h6("Practical decision guide"),
        tags$ul(
          tags$li(strong("Numeric outcome + 2 groups: "), "Welch/paired t-test if assumptions are reasonable; Mann-Whitney or Wilcoxon signed-rank when normality is rejected."),
          tags$li(strong("Numeric outcome + 3+ groups: "), "ANOVA; Welch ANOVA for unequal variances; Kruskal-Wallis when groupwise normality is rejected."),
          tags$li(strong("Two numeric/ordinal variables: "), "Pearson for linear parametric association; Spearman/Kendall for rank-based non-parametric association."),
          tags$li(strong("Two categorical variables: "), "Chi-square; Fisher's exact test when expected cell counts are too small."),
          tags$li(strong("Binary response: "), "Logistic regression; report odds ratios, 95% CI, p-values and classification performance."),
          tags$li(strong("Multiclass response: "), "Multinomial logistic regression for interpretable effects, or classification ML for prediction."),
          tags$li(strong("Count response: "), "Poisson or negative-binomial regression rather than ordinary linear regression when count assumptions apply."),
          tags$li(strong("Ordered response: "), "Ordinal logistic models or rank-based methods for simpler comparisons.")
        )
      )
    })

    # =========================================================================
    # TAB 6: DIAGNOSTICS
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
      if (is.na(result$p_value)) return(div(class = "alert alert-info", result$note))
      violated <- identical(result$is_normal, FALSE)
      div(
        h6("Shapiro-Wilk Test"),
        div(class = if (violated) "alert alert-warning" else "alert alert-success",
            p(result$note, class = "mb-1"),
            p(sprintf("W = %.4f, p = %.4f", result$statistic, result$p_value), class = "small mb-0")),
        if (violated) div(class = "card border-warning mt-3",
          div(class = "card-header", strong("Non-parametric pathway activated")),
          div(class = "card-body small",
            p("Because p < 0.05, this variable rejects the normality assumption. In Automatic mode the inferential module will prefer rank-based alternatives where the selected analysis requires normality."),
            tags$ul(
              tags$li("2 independent groups → Mann-Whitney U (Wilcoxon rank-sum)."),
              tags$li("2 paired measurements → Wilcoxon signed-rank."),
              tags$li("3+ independent groups → Kruskal-Wallis, followed by Holm-adjusted pairwise Wilcoxon comparisons when needed."),
              tags$li("Correlation → Spearman rank correlation (or Kendall for ordinal/small-sample settings)."),
              tags$li("For regression, test the model residuals rather than requiring the raw outcome itself to be normal; choose a generalized, robust or transformation-based model when the outcome/distribution calls for it.")
            )
          )
        ) else NULL
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
      
      pc$keep("qq_plot", p)
      
      sap_ggplotly(p, tooltip = c("x", "y"))
    })
    
    # =========================================================================
    # EXPORT
    # =========================================================================
    
    output$download_diagnostics <- downloadHandler(
      filename = function() {
        paste0("diagnostic_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        sap_export_assert(services, "statistical diagnostics HTML export")
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

    # --- AI interpretation (Phase A: Statistical Tests is the first module) ---
    # The payload is STRUCTURED — the test-result data frame, variable LABELS,
    # assumptions, and N. No raw dataset is ever sent. dlabel() gives the AI the
    # human-readable variable names from the metadata dictionary.
    mod_ai_assistant_server(
      "ai_ttest", services,
      payload = function() {
        res <- tryCatch(t_test_result(), error = function(e) NULL)
        if (is.null(res) || !is.null(res$error)) return(NULL)
        assumptions <- c(
          res$selection_reason %||% NULL,
          tryCatch(sprintf("Group-aware Shapiro-Wilk normality assessment for %s", dlabel(input$t_var)), error = function(e) NULL),
          if (isTRUE(input$t_paired)) "Paired analysis assumes correctly matched observations" else "Independent-group analysis assumes independent observations")
        list(
          analysis_type = res$test %||% if (isTRUE(input$t_paired)) "Paired comparison" else "Independent two-group comparison",
          variables = c(dlabel(input$t_var), dlabel(input$t_group)),
          results   = stat_result_df(res),
          assumptions = assumptions,
          n = tryCatch(nrow(df_data()), error = function(e) NULL),
          metadata = list(outcome = dlabel(input$t_var),
                          grouping = dlabel(input$t_group))
        )
      })

    # --- Two-sample tests ---
    register_report_button(
      input, services, "add_t_test", kind = "table",
      title   = function() sprintf("Two-group comparison: %s by %s", input$t_var, input$t_group),
      payload = function() stat_result_df(t_test_result()),
      code    = function() {
        res <- t_test_result()
        if (grepl("Wilcoxon|Mann-Whitney", res$test %||% ""))
          sprintf('wilcox.test(%s ~ %s, data = df, paired = %s, exact = FALSE)', input$t_var, input$t_group, if (isTRUE(input$t_paired)) "TRUE" else "FALSE")
        else sprintf('t.test(%s ~ %s, data = df, paired = %s)', input$t_var, input$t_group, if (isTRUE(input$t_paired)) "TRUE" else "FALSE")
      })

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
      title   = function() sprintf("Post-hoc comparisons: %s by %s",
                                   input$anova_var, input$anova_group),
      payload = function() tukey_df(),
      code    = function() "# Post-hoc method follows the selected omnibus test (Tukey, pairwise Welch t, or pairwise Wilcoxon with multiplicity adjustment).")

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
      title   = function() sprintf("%s: %s vs %s", corr_result()$test, input$corr_x, input$corr_y),
      payload = function() stat_result_df(corr_result()),
      code    = function() sprintf('cor.test(df$%s, df$%s, method = "%s")',
                                   input$corr_x, input$corr_y, corr_result()$method))

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

message("[SAP] Statistical Tests v2.5-20260709 loaded")  # SAP_MODULE_BANNER
