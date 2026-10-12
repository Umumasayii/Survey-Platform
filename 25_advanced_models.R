# =============================================================================
# MODULE 25: ADVANCED STATISTICAL MODELS
# =============================================================================

mod_advanced_models_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Advanced Statistical Models",
      "Estimate multivariable, ordinal, count, multilevel, repeated-measures, survival, moderation, and survey-weighted models.",
      "diagram-project"),
    uiOutput(ns("body"))
  )
}

mod_advanced_models_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(app_state$processed_data, {
      vars <- sap_safe_names(app_state)
      updateSelectInput(session, "outcome", choices = vars)
      updateSelectizeInput(session, "predictors", choices = vars, server = TRUE)
      updateSelectizeInput(session, "interactions", choices = vars, server = TRUE)
      updateSelectInput(session, "cluster", choices = c("None" = "", vars))
      updateSelectInput(session, "time", choices = c("None" = "", vars))
      updateSelectInput(session, "event", choices = c("None" = "", vars))
      updateSelectInput(session, "weight", choices = c("None" = "", sap_numeric_names(app_state)))
    }, ignoreInit = FALSE)

    # Keep the response off the right-hand side of model formulas. This also
    # cleans up stale selections when the outcome is changed after predictors.
    observeEvent(input$outcome, {
      outcome <- input$outcome %||% ""
      vars <- sap_safe_names(app_state)
      choices <- if (nzchar(outcome)) setdiff(vars, outcome) else vars
      updateSelectizeInput(session, "predictors", choices = choices,
                           selected = intersect(input$predictors %||% character(0), choices),
                           server = TRUE)
      updateSelectizeInput(session, "interactions", choices = choices,
                           selected = intersect(input$interactions %||% character(0), choices),
                           server = TRUE)
    }, ignoreInit = TRUE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      default_confidence <- if (!is.null(services) && !is.null(services$settings)) {
        100 * as.numeric(services$settings$get("confidence_level", .95))
      } else 95
      if (!is.finite(default_confidence)) default_confidence <- 95
      layout_columns(
        col_widths = c(4, 8),
        card(class = "sap-card",
          card_header(icon("sliders"), " Model specification"),
          card_body(
            selectInput(session$ns("family"), "Model family", choices = c(
              "Multiple linear regression" = "linear", "Binary logistic regression" = "binary",
              "Multinomial logistic regression" = "multinomial", "Ordinal logistic regression" = "ordinal",
              "Poisson regression" = "poisson", "Negative binomial regression" = "negative_binomial",
              "Mixed-effects model" = "mixed", "Generalized estimating equations" = "gee",
              "Cox proportional hazards" = "survival", "Moderation / interaction model" = "moderation")),
            selectInput(session$ns("outcome"), "Outcome", choices = sap_safe_names(app_state)),
            selectizeInput(session$ns("predictors"), "Predictors", choices = sap_safe_names(app_state), multiple = TRUE),
            selectizeInput(session$ns("interactions"), "Interaction variables", choices = sap_safe_names(app_state), multiple = TRUE),
            selectInput(session$ns("cluster"), "Cluster / random-effect variable", choices = c("None" = "", sap_safe_names(app_state))),
            selectInput(session$ns("time"), "Time variable", choices = c("None" = "", sap_safe_names(app_state))),
            selectInput(session$ns("event"), "Event indicator", choices = c("None" = "", sap_safe_names(app_state))),
            selectInput(session$ns("weight"), "Analysis weight", choices = c("None" = "", sap_numeric_names(app_state))),
            checkboxGroupInput(session$ns("robust"), "Inference options", choices = c(
              "HC3 robust standard errors" = "robust_se",
              "Survey-adjusted variance" = "survey",
              "Exponentiated effect ratios" = "ratio"
            ), selected = "robust_se"),
            numericInput(session$ns("confidence"), "Confidence level (%)",
                         min(max(default_confidence, 50), 99.9), min = 50, max = 99.9),
            div(class = "d-grid gap-2",
                actionButton(session$ns("validate"), "Validate specification", icon = icon("list-check"), class = "btn-outline-primary"),
                actionButton(session$ns("run_model"), "Estimate model", icon = icon("play"), class = "btn-primary"),
                actionButton(session$ns("queue"), "Save result to workspace", icon = icon("floppy-disk"), class = "btn-outline-secondary"))
          )
        ),
        div(
          uiOutput(session$ns("formula_card")),
          card(class = "sap-card mt-3", card_header(icon("triangle-exclamation"), " Validation"), card_body(uiOutput(session$ns("validation")))),
          card(class = "sap-card mt-3", card_header(icon("chart-line"), " Model result"), card_body(uiOutput(session$ns("outputs")))),
          card(class = "sap-card mt-3", card_header(icon("table-list"), " Coefficients and intervals"), card_body(DTOutput(session$ns("coefficients")))),
          card(class = "sap-card mt-3", card_header(icon("wave-square"), " Diagnostic view"), card_body(plotlyOutput(session$ns("diagnostic_plot"), height = "320px"))),
          card(class = "sap-card mt-3", card_header(icon("terminal"), " Reproducibility"), card_body(
            uiOutput(session$ns("model_code")),
            div(class = "d-flex flex-wrap gap-2 mt-2",
                downloadButton(session$ns("download_coefficients"), "Download coefficients", class = "btn btn-outline-primary btn-sm"),
                downloadButton(session$ns("download_code"), "Download R code", class = "btn btn-outline-secondary btn-sm"))
          ))
        )
      )
    })

    formula_text <- reactive({
      outcome <- input$outcome %||% "outcome"
      preds <- unique(setdiff(input$predictors %||% character(0), outcome))
      rhs <- if (length(preds)) paste(preds, collapse = " + ") else "1"
      ints <- unique(setdiff(input$interactions %||% character(0), outcome))
      if (length(ints) >= 2) rhs <- paste(rhs, paste(ints, collapse = " * "), sep = " + ")
      quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
      lhs <- quote_name(outcome)
      rhs_terms <- if (length(preds)) vapply(preds, quote_name, character(1)) else "1"
      rhs <- paste(rhs_terms, collapse = " + ")
      if (length(ints) >= 2) rhs <- paste(rhs, paste(vapply(ints, quote_name, character(1)), collapse = " * "), sep = " + ")
      if (identical(input$family, "mixed") && nzchar(input$cluster %||% "")) rhs <- paste0(rhs, " + (1 | ", quote_name(input$cluster), ")")
      if (identical(input$family, "survival")) {
        lhs <- paste0("survival::Surv(", quote_name(input$time %||% "time"), ", ", quote_name(input$event %||% "event"), ")")
      }
      paste(lhs, "~", rhs)
    })

    output$formula_card <- renderUI({
      card(class = "sap-card", card_header(icon("code"), " Model formula preview"), card_body(
        tags$pre(class = "sap-code-preview", formula_text()),
        div(class = "d-flex flex-wrap gap-2",
            sap_status_pill(input$family %||% "model", "neutral"),
            sap_status_pill(paste(length(input$predictors %||% character(0)), "predictors"), "neutral"),
            sap_status_pill(if (nzchar(input$weight %||% "")) "Weighted" else "Unweighted", if (nzchar(input$weight %||% "")) "success" else "warning"))
      ))
    })

    validation_result <- eventReactive(input$validate, {
      df <- app_state$processed_data; req(is.data.frame(df))
      issues <- character(0); warnings <- character(0)
      if (!nzchar(input$outcome %||% "")) issues <- c(issues, "Select an outcome variable.")
      if (length(input$predictors %||% character(0)) == 0) warnings <- c(warnings, "No predictors selected; the specification is intercept-only.")
      if (input$outcome %in% (input$predictors %||% character(0))) issues <- c(issues, "Outcome cannot also be a predictor.")
      if (input$outcome %in% (input$interactions %||% character(0))) issues <- c(issues, "Outcome cannot also be an interaction variable.")
      model_vars <- unique(c(input$outcome, input$predictors %||% character(0), input$interactions %||% character(0)))
      model_vars <- model_vars[nzchar(model_vars) & model_vars %in% names(df)]
      if (length(model_vars)) {
        cc <- stats::complete.cases(df[, model_vars, drop = FALSE])
        for (v in unique(c(input$predictors %||% character(0), input$interactions %||% character(0)))) {
          if (v %in% names(df) && length(unique(df[[v]][cc & !is.na(df[[v]])])) < 2L) {
            issues <- c(issues, sprintf("Predictor '%s' has fewer than two observed values in the estimation rows.", v))
          }
        }
      }
      if (identical(input$family, "survival") && (!nzchar(input$time %||% "") || !nzchar(input$event %||% ""))) issues <- c(issues, "Survival models require time and event variables.")
      if (input$family %in% c("mixed", "gee") && !nzchar(input$cluster %||% "")) issues <- c(issues, "This model family requires a cluster variable.")
      if (input$family %in% c("binary", "ordinal", "multinomial") && nzchar(input$outcome %||% "")) {
        levels_n <- length(unique(df[[input$outcome]][!is.na(df[[input$outcome]])]))
        if (identical(input$family, "binary") && levels_n != 2) warnings <- c(warnings, paste("Binary logistic regression usually requires exactly two outcome categories; detected", levels_n, "."))
      }
      n_complete <- if (nzchar(input$outcome %||% "")) sum(stats::complete.cases(df[c(input$outcome, input$predictors %||% character(0))])) else 0
      list(issues = issues, warnings = warnings, n_complete = n_complete)
    }, ignoreInit = TRUE)

    output$validation <- renderUI({
      v <- validation_result()
      if (is.null(v)) return(p(class = "text-muted mb-0", "Validate the specification to see readiness checks."))
      tagList(
        div(class = "sap-kpi-grid mb-3", sap_kpi("Complete-case preview", format(v$n_complete, big.mark = ","), "users"), sap_kpi("Blocking issues", length(v$issues), "circle-xmark"), sap_kpi("Warnings", length(v$warnings), "triangle-exclamation"), sap_kpi("Status", if (length(v$issues)) "Needs revision" else "Ready to estimate", if (length(v$issues)) "ban" else "circle-check")),
        if (length(v$issues)) div(class = "alert alert-danger", tags$ul(class = "mb-0", lapply(v$issues, tags$li))),
        if (length(v$warnings)) div(class = "alert alert-warning", tags$ul(class = "mb-0", lapply(v$warnings, tags$li))),
        if (!length(v$issues) && !length(v$warnings)) div(class = "alert alert-success mb-0", "No frontend specification issues detected.")
      )
    })

    model_result <- eventReactive(input$run_model, {
      df <- app_state$processed_data
      req(is.data.frame(df), nzchar(input$outcome %||% ""))
      # Enforce formula safety at estimation time even if the user did not press
      # "Validate specification" first.
      if (input$outcome %in% (input$predictors %||% character(0)) ||
          input$outcome %in% (input$interactions %||% character(0))) {
        return(list(error = "Outcome cannot also appear on the right-hand side of the model. Remove it from predictors/interactions and run again."))
      }
      confidence <- min(max(as.numeric(input$confidence %||% 95) / 100, .5), .999)
      family_key <- input$family %||% "linear"
      formula <- stats::as.formula(formula_text())
      weight_name <- input$weight %||% ""
      weights <- if (nzchar(weight_name) && weight_name %in% names(df)) suppressWarnings(as.numeric(df[[weight_name]])) else NULL
      survey_requested <- "survey" %in% (input$robust %||% character()) &&
        isTRUE((app_state$survey_design %||% list())$enabled)

      fit <- tryCatch(withProgress(message = "Estimating model", value = .15, {
        incProgress(.25, detail = "Preparing estimation frame")
        if (survey_requested && family_key %in% c("linear", "binary", "poisson", "moderation")) {
          if (is.null(services) || is.null(services$survey) || !services$survey$is_available()) {
            stop("Survey-adjusted estimation requires the 'survey' package.", call. = FALSE)
          }
          design_spec <- app_state$survey_design %||% list()
          if (nzchar(weight_name)) design_spec$weight <- weight_name
          design <- services$survey$design(df, design_spec)
          glm_family <- switch(family_key,
            binary = stats::binomial(), poisson = stats::quasipoisson(), stats::gaussian())
          incProgress(.4, detail = "Fitting survey-adjusted model")
          survey::svyglm(formula, design = design, family = glm_family, na.action = stats::na.omit)
        } else {
          incProgress(.4, detail = "Fitting analytical model")
          switch(family_key,
            linear = stats::lm(formula, data = df, weights = weights, na.action = stats::na.omit),
            moderation = stats::lm(formula, data = df, weights = weights, na.action = stats::na.omit),
            binary = stats::glm(formula, data = df, family = stats::binomial(), weights = weights, na.action = stats::na.omit),
            poisson = stats::glm(formula, data = df, family = stats::poisson(), weights = weights, na.action = stats::na.omit),
            negative_binomial = {
              if (!requireNamespace("MASS", quietly = TRUE)) stop("Package 'MASS' is required for negative binomial models.", call. = FALSE)
              MASS::glm.nb(formula, data = df, weights = weights, na.action = stats::na.omit)
            },
            ordinal = {
              if (!requireNamespace("MASS", quietly = TRUE)) stop("Package 'MASS' is required for ordinal models.", call. = FALSE)
              model_df <- df; model_df[[input$outcome]] <- ordered(model_df[[input$outcome]])
              MASS::polr(formula, data = model_df, weights = weights, Hess = TRUE, na.action = stats::na.omit)
            },
            multinomial = {
              if (!requireNamespace("nnet", quietly = TRUE)) stop("Package 'nnet' is required for multinomial models.", call. = FALSE)
              model_df <- df; model_df[[input$outcome]] <- factor(model_df[[input$outcome]])
              nnet::multinom(formula, data = model_df, weights = weights, trace = FALSE, na.action = stats::na.omit)
            },
            mixed = {
              sap_optional_call(
                "lme4", "lmer", formula, data = df, weights = weights,
                REML = FALSE, na.action = stats::na.omit,
                feature = "Mixed-effects modelling"
              )
            },
            gee = {
              outcome_values <- unique(df[[input$outcome]][!is.na(df[[input$outcome]])])
              gee_family <- if (length(outcome_values) == 2L) stats::binomial() else stats::gaussian()
              sap_optional_call(
                "geepack", "geeglm", formula, data = df, id = df[[input$cluster]],
                weights = weights, family = gee_family, corstr = "exchangeable",
                na.action = stats::na.omit, feature = "Generalized estimating equations"
              )
            },
            survival = {
              if (!requireNamespace("survival", quietly = TRUE)) stop("Package 'survival' is required for Cox models.", call. = FALSE)
              survival::coxph(formula, data = df, weights = weights, na.action = stats::na.omit, model = TRUE, x = TRUE)
            },
            stop("Unsupported model family.", call. = FALSE)
          )
        }
      }), error = function(e) e)

      if (inherits(fit, "condition")) return(list(error = conditionMessage(fit)))
      robust_applied <- FALSE
      tidy <- NULL
      if ("robust_se" %in% (input$robust %||% character()) && !survey_requested &&
          inherits(fit, c("lm", "glm")) && requireNamespace("sandwich", quietly = TRUE) &&
          requireNamespace("lmtest", quietly = TRUE)) {
        robust_matrix <- tryCatch(lmtest::coeftest(fit, vcov. = sandwich::vcovHC(fit, type = "HC3")), error = function(e) NULL)
        if (!is.null(robust_matrix)) {
          critical <- stats::qnorm(1 - (1 - confidence) / 2)
          tidy <- data.frame(
            term = rownames(robust_matrix), estimate = robust_matrix[, 1],
            std.error = robust_matrix[, 2], statistic = robust_matrix[, 3],
            p.value = robust_matrix[, 4], stringsAsFactors = FALSE
          )
          tidy$conf.low <- tidy$estimate - critical * tidy$std.error
          tidy$conf.high <- tidy$estimate + critical * tidy$std.error
          rownames(tidy) <- NULL
          robust_applied <- TRUE
        }
      }
      if (is.null(tidy)) tidy <- tryCatch(broom::tidy(fit, conf.int = TRUE, conf.level = confidence), error = function(e) NULL)
      if (is.null(tidy)) {
        co <- tryCatch(as.data.frame(summary(fit)$coefficients), error = function(e) data.frame())
        if (nrow(co)) { co$term <- rownames(co); rownames(co) <- NULL; tidy <- co[, c("term", setdiff(names(co), "term")), drop = FALSE] }
      }
      if (is.null(tidy)) tidy <- data.frame(Note = "Coefficient extraction is not supported by the installed broom version.")
      if ("ratio" %in% (input$robust %||% character()) && "estimate" %in% names(tidy) &&
          family_key %in% c("binary", "ordinal", "multinomial", "negative_binomial", "poisson", "survival")) {
        tidy$effect_ratio <- exp(tidy$estimate)
      }

      observed <- tryCatch(stats::model.response(stats::model.frame(fit)), error = function(e) numeric())
      if (is.factor(observed) || is.character(observed)) observed <- as.numeric(factor(observed))
      if (is.matrix(observed)) observed <- numeric()
      predicted <- tryCatch(stats::fitted(fit), error = function(e) numeric())
      if (is.matrix(predicted)) predicted <- apply(predicted, 1, max, na.rm = TRUE)
      predicted <- suppressWarnings(as.numeric(predicted))
      diagnostic <- data.frame()
      if (length(predicted)) {
        n <- min(length(observed), length(predicted))
        diagnostic <- data.frame(Observed = observed[seq_len(n)], Predicted = predicted[seq_len(n)])
        diagnostic <- diagnostic[stats::complete.cases(diagnostic), , drop = FALSE]
        diagnostic$Residual <- diagnostic$Observed - diagnostic$Predicted
      }
      metrics <- list(
        observations = tryCatch(stats::nobs(fit), error = function(e) NA_integer_),
        terms = if (is.data.frame(tidy)) nrow(tidy) else NA_integer_,
        aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
        log_likelihood = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_),
        engine = class(fit)[[1]], survey_adjusted = survey_requested,
        robust_se = robust_applied
      )
      list(fit = fit, tidy = tidy, diagnostic = diagnostic, metrics = metrics,
           family = family_key, formula = formula_text(), confidence = confidence,
           code = paste0("fit <- ", if (survey_requested) "survey::svyglm" else switch(family_key,
             linear = "stats::lm", moderation = "stats::lm", binary = "stats::glm",
             poisson = "stats::glm", negative_binomial = "MASS::glm.nb", ordinal = "MASS::polr",
             multinomial = "nnet::multinom", mixed = "lme4::lmer", gee = "geepack::geeglm",
             survival = "survival::coxph"), "(\n  ", formula_text(), ",\n  data = analysis_data\n)"))
    }, ignoreInit = TRUE)

    output$outputs <- renderUI({
      result <- model_result()
      if (is.null(result)) return(div(class = "alert alert-info mb-0", icon("circle-info"), " Estimate the model to generate coefficients, intervals, diagnostics, and reproducible code."))
      if (!is.null(result$error)) return(div(class = "alert alert-danger mb-0", icon("circle-xmark"), " ", result$error))
      m <- result$metrics
      div(class = "sap-kpi-grid",
          sap_kpi("Observations", if (is.finite(m$observations)) format(m$observations, big.mark = ",") else "—", "users"),
          sap_kpi("Coefficient rows", m$terms %||% "—", "list"),
          sap_kpi("AIC", if (is.finite(m$aic)) sap_fmt(m$aic, 2) else "—", "gauge"),
          sap_kpi("Engine", m$engine, "gears"),
          sap_status_pill(if (isTRUE(m$survey_adjusted)) "Survey-adjusted" else if (isTRUE(m$robust_se)) "HC3 robust SE" else "Model-based variance",
                          if (isTRUE(m$survey_adjusted) || isTRUE(m$robust_se)) "success" else "neutral"))
    })

    output$coefficients <- renderDT({
      result <- model_result(); req(!is.null(result), is.null(result$error), is.data.frame(result$tidy))
      datatable(result$tidy, rownames = FALSE, filter = "top",
                options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })

    output$diagnostic_plot <- renderPlotly({
      result <- model_result(); req(!is.null(result), is.null(result$error), nrow(result$diagnostic) > 1L)
      d <- utils::head(result$diagnostic, 10000L)
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Predicted, y = .data$Residual,
                                           text = paste0("Predicted: ", signif(.data$Predicted, 5), "<br>Residual: ", signif(.data$Residual, 5)))) +
        ggplot2::geom_hline(yintercept = 0, color = "#94a3b8", linetype = 2) +
        ggplot2::geom_point(color = "#5b5bd6", alpha = .58, size = 1.8) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#14b8a6", linewidth = .8) +
        ggplot2::theme_minimal() + ggplot2::labs(x = "Predicted", y = "Residual")
      sap_ggplotly(p, tooltip = "text")
    })

    output$model_code <- renderUI({
      result <- model_result()
      if (is.null(result) || !is.null(result$error)) return(p(class = "text-muted mb-0", "Reproducible code appears after estimation."))
      tags$pre(class = "sap-code-preview mb-0", result$code)
    })

    output$download_coefficients <- downloadHandler(
      filename = function() paste0("model-coefficients-", Sys.Date(), ".csv"),
      content = function(file) {
        result <- model_result(); req(!is.null(result), is.null(result$error))
        if (exists("sap_export_assert_content", mode = "function")) sap_export_assert_content(services, result$tidy, "advanced model coefficient export")
        utils::write.csv(result$tidy, file, row.names = FALSE, na = "")
      }
    )
    output$download_code <- downloadHandler(
      filename = function() paste0("advanced-model-", Sys.Date(), ".R"),
      content = function(file) {
        result <- model_result(); req(!is.null(result), is.null(result$error))
        if (exists("sap_export_assert", mode = "function")) sap_export_assert(services, "advanced model code export")
        writeLines(c("# Survey Analytics Platform reproducible model", "# Replace analysis_data with your data frame.", result$code), file)
      }
    )

    observeEvent(input$queue, {
      result <- model_result(); req(!is.null(result), is.null(result$error))
      spec <- list(family = input$family, outcome = input$outcome, predictors = input$predictors,
                   interactions = input$interactions, cluster = input$cluster, time = input$time,
                   event = input$event, weight = input$weight, options = input$robust,
                   confidence = input$confidence, formula = formula_text(),
                   metrics = result$metrics, coefficients = result$tidy, code = result$code,
                   created_at = Sys.time())
      app_state$advanced_model_queue <- c(app_state$advanced_model_queue %||% list(), list(spec))
      sap_frontend_record(app_state, "Advanced Models", paste("Model result saved:", formula_text()), spec)
      showNotification("Model result saved to this workspace.", type = "message")
    })
  })
}

message("[SAP] Advanced Models engine v4.0.0 loaded")
