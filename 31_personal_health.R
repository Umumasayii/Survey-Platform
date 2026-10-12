# =============================================================================
# MODULE 31: PERSONAL HEALTH TRACKER
# =============================================================================
# An opt-in, session-local tracker for adult BMI, blood-pressure readings and
# basic lifestyle measures. It is educational and does not diagnose illness.
# =============================================================================

.sap_bmi_category <- function(bmi) {
  if (!is.finite(bmi)) return("Not available")
  if (bmi < 18.5) return("Underweight range")
  if (bmi < 25) return("Healthy-weight range")
  if (bmi < 30) return("Overweight range")
  "Obesity range"
}

.sap_bp_reading_note <- function(systolic, diastolic) {
  if (!is.finite(systolic) || !is.finite(diastolic)) return("Blood pressure not recorded")
  if (systolic >= 140 || diastolic >= 90) {
    return("At or above 140/90 mmHg. One reading does not diagnose hypertension; repeat on a different day and discuss persistent high readings with a health professional.")
  }
  "Below 140/90 mmHg for this reading. Continue tracking as advised by your health professional."
}

mod_personal_health_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "mb-4",
      h2(icon("heart-pulse"), "Personal Health Tracker"),
      p(
        "Record adult weight, BMI, blood pressure and selected daily habits in your current private session.",
        class = "text-muted mb-2"
      ),
      div(
        class = "alert alert-warning border-0 py-2",
        icon("triangle-exclamation"),
        strong(" Educational only. "),
        "This tracker is not a medical device and does not diagnose or replace professional care."
      )
    ),

    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "sap-card h-100",
        card_header(icon("pen-to-square"), " Add a reading"),
        card_body(
          dateInput(ns("reading_date"), "Date", value = Sys.Date()),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("height_cm"), "Height (cm)", value = 170, min = 100, max = 250, step = 0.1),
            numericInput(ns("weight_kg"), "Weight (kg)", value = 70, min = 25, max = 350, step = 0.1)
          ),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("systolic"), "Systolic BP", value = 120, min = 50, max = 260),
            numericInput(ns("diastolic"), "Diastolic BP", value = 80, min = 30, max = 180)
          ),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("heart_rate"), "Resting heart rate", value = 72, min = 25, max = 240),
            numericInput(ns("sleep_hours"), "Sleep (hours)", value = 7, min = 0, max = 24, step = 0.25)
          ),
          numericInput(ns("steps"), "Steps", value = 6000, min = 0, max = 200000, step = 100),
          checkboxInput(ns("diagnosed_hypertension"), "Clinician-diagnosed hypertension", value = FALSE),
          textAreaInput(ns("notes"), "Notes", rows = 2, placeholder = "Optional context, medication change, exercise…"),
          actionButton(ns("add_reading"), "Save reading", icon = icon("plus"),
                       class = "btn btn-primary w-100")
        )
      ),
      card(
        class = "sap-card h-100",
        card_header(icon("gauge-high"), " Current reading"),
        card_body(
          uiOutput(ns("current_summary")),
          hr(),
          selectInput(
            ns("trend_metric"), "Trend to display",
            choices = c(
              "Weight (kg)" = "weight_kg", "BMI" = "bmi",
              "Systolic BP" = "systolic", "Diastolic BP" = "diastolic",
              "Resting heart rate" = "heart_rate", "Sleep hours" = "sleep_hours",
              "Steps" = "steps"
            )
          ),
          plotly::plotlyOutput(ns("trend_plot"), height = "310px")
        )
      )
    ),

    card(
      class = "sap-card mt-4",
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center w-100",
          span(icon("clock-rotate-left"), " Reading history"),
          div(
            downloadButton(ns("download_health"), "Download CSV", class = "btn btn-sm btn-outline-primary"),
            actionButton(ns("clear_health"), "Clear", icon = icon("trash"),
                         class = "btn btn-sm btn-outline-danger ms-1")
          )
        )
      ),
      card_body(DT::DTOutput(ns("health_table")))
    )
  )
}

mod_personal_health_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    empty_log <- function() {
      data.frame(
        date = as.Date(character()), height_cm = numeric(), weight_kg = numeric(),
        bmi = numeric(), bmi_category = character(), systolic = numeric(),
        diastolic = numeric(), bp_note = character(), heart_rate = numeric(),
        sleep_hours = numeric(), steps = numeric(), diagnosed_hypertension = logical(),
        notes = character(), stringsAsFactors = FALSE
      )
    }

    ensure_log <- function() {
      log <- app_state$health_log
      required <- names(empty_log())
      if (!is.data.frame(log) || !all(required %in% names(log))) log <- empty_log()
      log[, required, drop = FALSE]
    }

    reading_preview <- reactive({
      h_m <- as.numeric(input$height_cm) / 100
      bmi <- if (is.finite(h_m) && h_m > 0) as.numeric(input$weight_kg) / h_m^2 else NA_real_
      list(
        bmi = bmi,
        bmi_category = .sap_bmi_category(bmi),
        bp_note = .sap_bp_reading_note(as.numeric(input$systolic), as.numeric(input$diastolic))
      )
    })

    output$current_summary <- renderUI({
      r <- reading_preview()
      div(
        class = "sap-health-summary",
        layout_columns(
          col_widths = c(6, 6),
          div(class = "sap-health-metric", span("BMI", class = "small text-muted"),
              strong(if (is.finite(r$bmi)) sprintf("%.1f", r$bmi) else "—"),
              div(r$bmi_category, class = "small")),
          div(class = "sap-health-metric", span("Blood pressure", class = "small text-muted"),
              strong(sprintf("%s/%s", input$systolic %||% "—", input$diastolic %||% "—")),
              div("mmHg", class = "small"))
        ),
        div(class = "alert alert-light mt-3 mb-0 small", r$bp_note)
      )
    })

    observeEvent(input$add_reading, {
      height <- as.numeric(input$height_cm)
      weight <- as.numeric(input$weight_kg)
      if (!is.finite(height) || height < 100 || height > 250) {
        showNotification("Enter a valid adult height.", type = "error")
        return()
      }
      if (!is.finite(weight) || weight < 25 || weight > 350) {
        showNotification("Enter a valid weight.", type = "error")
        return()
      }
      if (!is.finite(as.numeric(input$systolic)) || !is.finite(as.numeric(input$diastolic)) ||
          as.numeric(input$systolic) <= as.numeric(input$diastolic)) {
        showNotification("Systolic pressure should be greater than diastolic pressure.", type = "error")
        return()
      }

      r <- reading_preview()
      row <- data.frame(
        date = as.Date(input$reading_date),
        height_cm = height,
        weight_kg = weight,
        bmi = round(r$bmi, 2),
        bmi_category = r$bmi_category,
        systolic = as.numeric(input$systolic),
        diastolic = as.numeric(input$diastolic),
        bp_note = r$bp_note,
        heart_rate = as.numeric(input$heart_rate),
        sleep_hours = as.numeric(input$sleep_hours),
        steps = as.numeric(input$steps),
        diagnosed_hypertension = isTRUE(input$diagnosed_hypertension),
        notes = trimws(input$notes %||% ""),
        stringsAsFactors = FALSE
      )

      app_state$health_log <- rbind(ensure_log(), row)
      app_state$last_action <- "Added a personal health reading"
      showNotification("Health reading saved in this session.", type = "message")
    }, ignoreInit = TRUE)

    output$health_table <- DT::renderDT({
      log <- ensure_log()
      display <- log
      if (nrow(display)) display <- display[order(display$date, decreasing = TRUE), , drop = FALSE]
      DT::datatable(
        display, rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, pageLength = 8, lengthChange = FALSE)
      )
    })

    output$trend_plot <- plotly::renderPlotly({
      log <- ensure_log()
      metric <- input$trend_metric
      if (nrow(log) == 0L) {
        return(sap_empty_plotly("Save at least one reading to display a trend."))
      }
      if (is.null(metric) || length(metric) != 1L || !metric %in% names(log)) {
        return(sap_empty_plotly("Choose a valid metric."))
      }
      plot_df <- log[order(log$date), c("date", metric), drop = FALSE]
      names(plot_df)[2] <- "metric_value"
      metric_labels <- c(
        weight_kg = "Weight (kg)", bmi = "BMI", systolic = "Systolic BP",
        diastolic = "Diastolic BP", heart_rate = "Resting heart rate",
        sleep_hours = "Sleep hours", steps = "Steps"
      )
      metric_label <- as.character(metric_labels[[metric]] %||% metric)[1]
      fig <- plotly::plot_ly(
        plot_df, x = ~date, y = ~metric_value,
        type = "scatter", mode = "lines+markers",
        hovertemplate = "%{x}<br>%{y}<extra></extra>"
      )
      fig <- plotly::layout(
        fig,
        xaxis = list(title = "Date"),
        yaxis = list(title = metric_label),
        margin = list(l = 55, r = 20, t = 20, b = 50)
      )
      plotly::config(fig, displaylogo = FALSE, responsive = TRUE)
    })

    output$download_health <- downloadHandler(
      filename = function() sprintf("personal_health_log_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        safe <- if (!is.null(services) && !is.null(services$privacy)) {
          services$privacy$prepare_export(ensure_log(), "personal health export")
        } else ensure_log()
        utils::write.csv(safe, file, row.names = FALSE, na = "")
      }
    )

    observeEvent(input$clear_health, {
      showModal(modalDialog(
        title = "Clear personal health history?",
        p("This removes all readings from the current session."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_clear_health"), "Clear readings",
                       class = "btn btn-danger")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_clear_health, {
      app_state$health_log <- empty_log()
      removeModal()
      showNotification("Personal health history cleared.", type = "message")
    }, ignoreInit = TRUE)
  })
}

message("[SAP] Personal Health Tracker v1.1-20260801 loaded")  # SAP_MODULE_BANNER
