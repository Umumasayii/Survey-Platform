# =============================================================================
# MODULE 24: SAMPLE SIZE, POWER & PRECISION — FRONTEND
# =============================================================================

mod_sample_size_power_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header(
      "Sample Size, Power & Precision",
      "Plan fieldwork and evaluate achieved precision using transparent survey formulas.",
      "bullseye"
    ),
    navset_card_tab(
      nav_panel(
        "Single proportion",
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "sap-card",
            card_header("Planning inputs"),
            card_body(
              numericInput(ns("prop_confidence"), "Confidence level (%)", 95, min = 50, max = 99.9, step = .1),
              numericInput(ns("prop_margin"), "Margin of error (percentage points)", 5, min = .1, max = 50, step = .1),
              numericInput(ns("prop_proportion"), "Expected proportion (%)", 50, min = .1, max = 99.9, step = .1),
              numericInput(ns("prop_population"), "Population size (0 = ignore)", 0, min = 0),
              numericInput(ns("prop_design_effect"), "Design effect", 1, min = 1, step = .1),
              numericInput(ns("prop_response_rate"), "Expected response rate (%)", 80, min = 1, max = 100),
              actionButton(ns("prop_calculate"), "Calculate sample size", class = "btn-primary w-100", icon = icon("calculator"))
            )
          ),
          card(
            class = "sap-card",
            card_header("Recommendation"),
            card_body(uiOutput(ns("prop_result")))
          )
        )
      ),
      nav_panel(
        "Difference in proportions",
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "sap-card",
            card_header("Comparison inputs"),
            card_body(
              numericInput(ns("diffprop_p1"), "Group 1 proportion (%)", 50, min = .1, max = 99.9),
              numericInput(ns("diffprop_p2"), "Group 2 proportion (%)", 60, min = .1, max = 99.9),
              numericInput(ns("diffprop_power"), "Power (%)", 80, min = 50, max = 99),
              numericInput(ns("diffprop_alpha"), "Two-sided alpha (%)", 5, min = .1, max = 20),
              numericInput(ns("diffprop_design_effect"), "Design effect", 1, min = 1, step = .1),
              numericInput(ns("diffprop_response_rate"), "Response rate (%)", 80, min = 1, max = 100),
              actionButton(ns("diffprop_calculate"), "Calculate per-group sample", class = "btn-primary w-100", icon = icon("calculator"))
            )
          ),
          card(
            class = "sap-card",
            card_header("Recommendation"),
            card_body(uiOutput(ns("diffprop_result")))
          )
        )
      ),
      nav_panel(
        "Single mean",
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "sap-card",
            card_header("Mean precision inputs"),
            card_body(
              numericInput(ns("mean_confidence"), "Confidence level (%)", 95, min = 50, max = 99.9),
              numericInput(ns("mean_sd"), "Expected standard deviation", 10, min = .0001),
              numericInput(ns("mean_margin"), "Desired half-width", 2, min = .0001),
              numericInput(ns("mean_design_effect"), "Design effect", 1, min = 1, step = .1),
              numericInput(ns("mean_response_rate"), "Response rate (%)", 80, min = 1, max = 100),
              actionButton(ns("mean_calculate"), "Calculate sample size", class = "btn-primary w-100", icon = icon("calculator"))
            )
          ),
          card(
            class = "sap-card",
            card_header("Recommendation"),
            card_body(uiOutput(ns("mean_result")))
          )
        )
      ),
      nav_panel(
        "Cluster survey",
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "sap-card",
            card_header("Cluster-design inputs"),
            card_body(
              numericInput(ns("cluster_confidence"), "Confidence level (%)", 95, min = 50, max = 99.9),
              numericInput(ns("cluster_margin"), "Margin of error (percentage points)", 5, min = .1, max = 30),
              numericInput(ns("cluster_proportion"), "Expected proportion (%)", 50, min = .1, max = 99.9),
              numericInput(ns("cluster_size"), "Completed interviews per cluster", 20, min = 2),
              numericInput(ns("cluster_icc"), "Intracluster correlation (ICC)", .03, min = 0, max = .95, step = .01),
              numericInput(ns("cluster_response"), "Respondent response rate (%)", 80, min = 1, max = 100),
              numericInput(ns("cluster_retention"), "Expected clusters retained (%)", 95, min = 1, max = 100),
              numericInput(ns("cluster_simulations"), "Monte Carlo replications", 2000, min = 250, max = 10000, step = 250),
              actionButton(ns("cluster_calculate"), "Plan and simulate clusters", class = "btn-primary w-100", icon = icon("dice"))
            )
          ),
          card(
            class = "sap-card",
            card_header("Cluster recommendation"),
            card_body(uiOutput(ns("cluster_result")),
              plotlyOutput(ns("cluster_plot"), height = "260px")
            )
          )
        )
      ),
      nav_panel(
        "Achieved precision",
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "sap-card",
            card_header("Observed design"),
            card_body(
              numericInput(ns("achieved_n"), "Unweighted sample size", 400, min = 2),
              numericInput(ns("achieved_proportion"), "Observed proportion (%)", 50, min = 0, max = 100),
              numericInput(ns("achieved_confidence"), "Confidence level (%)", 95, min = 50, max = 99.9),
              numericInput(ns("achieved_design_effect"), "Design effect", 1, min = 1, step = .1),
              numericInput(ns("achieved_population"), "Population size (0 = ignore)", 0, min = 0),
              actionButton(ns("achieved_calculate"), "Calculate achieved precision", class = "btn-primary w-100", icon = icon("calculator"))
            )
          ),
          card(
            class = "sap-card",
            card_header("Achieved precision"),
            card_body(uiOutput(ns("achieved_result")))
          )
        )
      )
    )
  )
}

mod_sample_size_power_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    z_from_conf <- function(conf) stats::qnorm(1 - (1 - conf / 100) / 2)
    valid_population <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0

    observeEvent(app_state$settings, {
      confidence <- if (!is.null(services) && !is.null(services$settings)) {
        100 * suppressWarnings(as.numeric(services$settings$get("confidence_level", .95)))
      } else 95
      if (!is.finite(confidence)) confidence <- 95
      confidence <- min(max(confidence, 50), 99.9)
      lapply(c("prop_confidence", "mean_confidence", "cluster_confidence",
               "achieved_confidence"), function(id) {
        updateNumericInput(session, id, value = confidence)
      })
      alpha <- if (!is.null(services) && !is.null(services$settings)) {
        100 * suppressWarnings(as.numeric(services$settings$get("significance_level", .05)))
      } else 5
      if (is.finite(alpha)) updateNumericInput(session, "diffprop_alpha",
                                                value = min(max(alpha, .1), 20))
    }, ignoreInit = FALSE)

    sample_result_ui <- function(complete, invite, explanation, per_group = FALSE) {
      tagList(
        div(
          class = "sap-kpi-grid",
          sap_kpi(if (per_group) "Completed per group" else "Completed interviews", format(complete, big.mark = ","), "user-check"),
          sap_kpi(if (per_group) "Invitations per group" else "Invitations required", format(invite, big.mark = ","), "paper-plane"),
          sap_kpi("Total completed", format(if (per_group) 2 * complete else complete, big.mark = ","), "users"),
          sap_kpi("Total invitations", format(if (per_group) 2 * invite else invite, big.mark = ","), "envelopes-bulk")
        ),
        div(class = "alert alert-info mt-3 mb-0", icon("circle-info"), " ", explanation)
      )
    }

    observeEvent(input$prop_calculate, {
      z <- z_from_conf(input$prop_confidence)
      p <- input$prop_proportion / 100
      e <- input$prop_margin / 100
      n0 <- z^2 * p * (1 - p) / e^2
      if (valid_population(input$prop_population)) {
        n0 <- n0 / (1 + (n0 - 1) / input$prop_population)
      }
      complete <- ceiling(n0 * input$prop_design_effect)
      invite <- ceiling(complete / (input$prop_response_rate / 100))
      output$prop_result <- renderUI(sample_result_ui(
        complete,
        invite,
        sprintf("Estimate a proportion near %.1f%% within ±%.1f percentage points.", input$prop_proportion, input$prop_margin)
      ))
    })

    observeEvent(input$diffprop_calculate, {
      p1 <- input$diffprop_p1 / 100
      p2 <- input$diffprop_p2 / 100
      req(abs(p1 - p2) > 0)
      za <- stats::qnorm(1 - input$diffprop_alpha / 200)
      zb <- stats::qnorm(input$diffprop_power / 100)
      pbar <- (p1 + p2) / 2
      n <- ((za * sqrt(2 * pbar * (1 - pbar)) + zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2)))^2) / (p1 - p2)^2
      complete <- ceiling(n * input$diffprop_design_effect)
      invite <- ceiling(complete / (input$diffprop_response_rate / 100))
      output$diffprop_result <- renderUI(sample_result_ui(
        complete,
        invite,
        sprintf("Detect a %.1f-point difference with %.0f%% power.", abs(input$diffprop_p2 - input$diffprop_p1), input$diffprop_power),
        per_group = TRUE
      ))
    })

    observeEvent(input$mean_calculate, {
      z <- z_from_conf(input$mean_confidence)
      n <- (z * input$mean_sd / input$mean_margin)^2
      complete <- ceiling(n * input$mean_design_effect)
      invite <- ceiling(complete / (input$mean_response_rate / 100))
      output$mean_result <- renderUI(sample_result_ui(
        complete,
        invite,
        sprintf("Estimate a mean with half-width %.2f.", input$mean_margin)
      ))
    })

    cluster_plan <- eventReactive(input$cluster_calculate, {
      z <- z_from_conf(input$cluster_confidence)
      p <- input$cluster_proportion / 100
      e <- input$cluster_margin / 100
      m <- as.integer(input$cluster_size)
      rho <- input$cluster_icc
      deff <- 1 + (m - 1) * rho
      srs_n <- z^2 * p * (1 - p) / e^2
      completed <- ceiling(srs_n * deff)
      retained_clusters <- ceiling(completed / m)
      field_clusters <- ceiling(retained_clusters / (input$cluster_retention / 100))
      invitations <- ceiling(completed / (input$cluster_response / 100))
      simulations <- as.integer(input$cluster_simulations)
      set.seed(20260815L + field_clusters + m)
      estimates <- replicate(simulations, {
        if (rho > 0) {
          concentration <- 1 / rho - 1
          cluster_p <- stats::rbeta(retained_clusters, p * concentration, (1 - p) * concentration)
        } else cluster_p <- rep(p, retained_clusters)
        sum(stats::rbinom(retained_clusters, m, cluster_p)) / (retained_clusters * m)
      })
      empirical_half_width <- diff(stats::quantile(estimates, c(.025, .975), na.rm = TRUE)) / 2 * 100
      list(srs_n = srs_n, deff = deff, completed = completed, retained_clusters = retained_clusters,
           field_clusters = field_clusters, invitations = invitations,
           empirical_half_width = empirical_half_width, estimates = 100 * estimates)
    }, ignoreInit = TRUE)

    output$cluster_result <- renderUI({
      plan <- cluster_plan()
      if (is.null(plan)) return(div(class = "alert alert-info", "Set the cluster assumptions, then run the planner."))
      tagList(
        div(class = "sap-kpi-grid",
            sap_kpi("Design effect", sap_fmt(plan$deff, 2), "layer-group"),
            sap_kpi("Completed interviews", format(plan$completed, big.mark = ","), "users"),
            sap_kpi("Retained clusters", plan$retained_clusters, "circle-nodes"),
            sap_kpi("Clusters to recruit", plan$field_clusters, "location-dot")),
        div(class = "alert alert-info mt-3 mb-0",
            sprintf("Plan approximately %s invitations. Monte Carlo 95%% half-width: ±%.2f percentage points under the entered beta-binomial ICC assumptions.",
                    format(plan$invitations, big.mark = ","), plan$empirical_half_width))
      )
    })

    output$cluster_plot <- renderPlotly({
      plan <- cluster_plan(); req(!is.null(plan))
      d <- data.frame(estimate = plan$estimates)
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate)) +
        ggplot2::geom_histogram(bins = 35, fill = "#5b5bd6", color = "white") +
        ggplot2::geom_vline(xintercept = input$cluster_proportion, color = "#14b8a6", linewidth = 1) +
        ggplot2::theme_minimal() + ggplot2::labs(x = "Simulated survey estimate (%)", y = "Replications")
      sap_ggplotly(p)
    })

    observeEvent(input$achieved_calculate, {
      neff <- input$achieved_n / input$achieved_design_effect
      p <- input$achieved_proportion / 100
      se <- sqrt(p * (1 - p) / neff)
      if (valid_population(input$achieved_population) && input$achieved_population > input$achieved_n) {
        se <- se * sqrt((input$achieved_population - input$achieved_n) / (input$achieved_population - 1))
      }
      moe <- z_from_conf(input$achieved_confidence) * se * 100
      output$achieved_result <- renderUI(tagList(
        div(
          class = "sap-kpi-grid",
          sap_kpi("Effective sample size", sap_fmt(neff, 0), "users"),
          sap_kpi("Margin of error", sprintf("±%.2f pp", moe), "arrows-left-right"),
          sap_kpi("Lower bound", sprintf("%.2f%%", max(0, input$achieved_proportion - moe)), "arrow-down"),
          sap_kpi("Upper bound", sprintf("%.2f%%", min(100, input$achieved_proportion + moe)), "arrow-up")
        ),
        div(
          class = "alert alert-light border mt-3 mb-0",
          "This is a simple design-effect-adjusted approximation; the backend will use the actual survey design object."
        )
      ))
    })
  })
}

message("[SAP] Sample Size & Power engine v4.0.0 loaded")
