# =============================================================================
# MODULE 29: PRIVACY, DISCLOSURE & GOVERNANCE — FRONTEND
# =============================================================================

mod_privacy_governance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Privacy, Disclosure & Governance",
      "Classify sensitive fields, configure export controls, review disclosure risks, and protect data sent to AI services.",
      "shield-halved"),
    sap_frontend_notice("Variable-level export/AI controls and session export approval are enforced. Organisation roles, durable approval workflows, retention jobs, and external key management remain deployment responsibilities."),
    uiOutput(ns("body"))
  )
}

mod_privacy_governance_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    ensure_register <- function() {
      df <- app_state$processed_data
      if (!is.data.frame(df)) return()
      reg <- app_state$privacy_register
      if (!is.data.frame(reg) || !all(names(df) %in% reg$variable)) {
        old <- if (is.data.frame(reg)) reg else data.frame()
        fresh <- data.frame(variable = names(df), classification = "Internal", treatment = "Keep", export_allowed = TRUE, ai_allowed = FALSE, notes = "", stringsAsFactors = FALSE)
        if (nrow(old)) {
          common <- intersect(fresh$variable, old$variable)
          for (v in common) fresh[fresh$variable == v, intersect(names(fresh), names(old))] <- old[old$variable == v, intersect(names(fresh), names(old))]
        }
        app_state$privacy_register <- fresh
      }
    }

    observeEvent(app_state$processed_data, {
      ensure_register(); updateSelectInput(session, "variable", choices = sap_safe_names(app_state))
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      navset_card_tab(
        nav_panel("Variable classification",
          layout_columns(col_widths = c(4, 8),
            card(class = "sap-card", card_header(icon("tag"), " Classification editor"), card_body(
              selectInput(session$ns("variable"), "Variable", choices = sap_safe_names(app_state)),
              selectInput(session$ns("classification"), "Classification", choices = c("Public", "Internal", "Confidential", "Restricted / direct identifier", "Sensitive attribute")),
              selectInput(session$ns("treatment"), "Default treatment", choices = c("Keep", "Mask", "Hash / pseudonymize", "Generalize", "Suppress", "Remove before export")),
              checkboxInput(session$ns("export_allowed"), "Allow in data exports", TRUE),
              checkboxInput(session$ns("ai_allowed"), "Allow transmission to configured AI provider", FALSE),
              textAreaInput(session$ns("notes"), "Governance notes", rows = 3),
              actionButton(session$ns("save_variable"), "Save classification", icon = icon("floppy-disk"), class = "btn-primary w-100")
            )),
            card(class = "sap-card", card_header(icon("table-list"), " Data classification register"), card_body(DTOutput(session$ns("register_table"))))
          )
        ),
        nav_panel("Disclosure review",
          layout_columns(col_widths = c(4, 8),
            card(class = "sap-card", card_header(icon("sliders"), " Release controls"), card_body(
              numericInput(session$ns("small_cell"), "Small-cell suppression threshold", 5, min = 1),
              numericInput(session$ns("min_base"), "Minimum reportable subgroup base", 30, min = 1),
              checkboxInput(session$ns("redact_ai"), "Redact restricted fields before AI use", TRUE),
              checkboxInput(session$ns("require_approval"), "Require approval before external export", TRUE),
              checkboxInput(session$ns("audit_exports"), "Record export audit events", TRUE),
              selectInput(session$ns("retention"), "Retention policy", choices = c("Session only", "30 days", "90 days", "1 year", "Organisation policy")),
              actionButton(session$ns("scan"), "Run disclosure scan", icon = icon("shield"), class = "btn-primary w-100"),
              actionButton(session$ns("save_policy"), "Save governance policy", icon = icon("floppy-disk"), class = "btn-outline-primary w-100 mt-2"),
              hr(),
              uiOutput(session$ns("approval_status")),
              div(class = "d-grid gap-2",
                  actionButton(session$ns("approve_exports"), "Approve exports for this session", icon = icon("user-check"), class = "btn-outline-success"),
                  actionButton(session$ns("revoke_exports"), "Revoke export approval", icon = icon("ban"), class = "btn-outline-danger"))
            )),
            div(
              uiOutput(session$ns("risk_kpis")),
              card(class = "sap-card", card_header(icon("triangle-exclamation"), " Risk findings"), card_body(DTOutput(session$ns("risk_table"))))
            )
          )
        )
      )
    })

    observeEvent(input$variable, {
      reg <- app_state$privacy_register; req(is.data.frame(reg), input$variable %in% reg$variable)
      row <- reg[reg$variable == input$variable, , drop = FALSE][1, ]
      updateSelectInput(session, "classification", selected = row$classification)
      updateSelectInput(session, "treatment", selected = row$treatment)
      updateCheckboxInput(session, "export_allowed", value = isTRUE(row$export_allowed))
      updateCheckboxInput(session, "ai_allowed", value = isTRUE(row$ai_allowed))
      updateTextAreaInput(session, "notes", value = row$notes %||% "")
    }, ignoreInit = TRUE)

    observeEvent(input$save_variable, {
      reg <- app_state$privacy_register; req(is.data.frame(reg), input$variable %in% reg$variable)
      i <- which(reg$variable == input$variable)[1]
      reg$classification[i] <- input$classification; reg$treatment[i] <- input$treatment
      reg$export_allowed[i] <- isTRUE(input$export_allowed); reg$ai_allowed[i] <- isTRUE(input$ai_allowed); reg$notes[i] <- input$notes %||% ""
      app_state$privacy_register <- reg
      if (!is.null(services$privacy)) services$privacy$revoke_export_approval()
      sap_frontend_record(app_state, "Privacy", paste("Privacy classification saved:", input$variable))
      showNotification("Variable classification saved.", type = "message")
    })

    output$register_table <- renderDT({
      reg <- app_state$privacy_register
      if (!is.data.frame(reg)) return(datatable(data.frame(), options = list(dom = "t")))
      datatable(reg, options = list(pageLength = 15, dom = "tip", scrollX = TRUE), rownames = FALSE, selection = "single")
    })

    observeEvent(input$register_table_rows_selected, {
      reg <- app_state$privacy_register; idx <- input$register_table_rows_selected
      if (is.data.frame(reg) && length(idx) == 1) updateSelectInput(session, "variable", selected = reg$variable[idx])
    })

    risk_result <- eventReactive(input$scan, {
      df <- app_state$processed_data; req(is.data.frame(df))
      reg <- app_state$privacy_register
      name_pattern <- "(^id$|_id$|name|email|phone|mobile|address|passport|national|gps|latitude|longitude|coordinate|ip_)"
      findings <- list()
      add <- function(variable, risk, severity, recommendation) findings[[length(findings)+1]] <<- data.frame(Variable = variable, Risk = risk, Severity = severity, Recommendation = recommendation, stringsAsFactors = FALSE)
      for (nm in names(df)) {
        x <- df[[nm]]; unique_n <- length(unique(x[!is.na(x)]));
        if (grepl(name_pattern, nm, ignore.case = TRUE)) add(nm, "Identifier-like variable name", "High", "Classify, mask, hash, generalize, or remove before release.")
        if (unique_n >= .9 * nrow(df) && unique_n > 20) add(nm, "Near-unique values", "High", "Review for direct or indirect identification risk.")
        if ((is.factor(x) || is.character(x) || is.logical(x)) && unique_n > 0) {
          small <- sum(table(x, useNA = "no") < (input$small_cell %||% 5))
          if (small > 0) add(nm, paste(small, "small categories"), "Medium", "Combine categories or suppress small cells in outputs.")
        }
      }
      if (is.data.frame(reg)) {
        restricted <- reg$variable[reg$classification %in% c("Restricted / direct identifier", "Sensitive attribute") & reg$export_allowed]
        for (nm in restricted) add(nm, "Sensitive variable currently exportable", "High", "Disable direct export or require formal approval.")
      }
      if (!length(findings)) data.frame(Variable = "—", Risk = "No heuristic risks detected", Severity = "Low", Recommendation = "Continue formal disclosure review.", stringsAsFactors = FALSE) else do.call(rbind, findings)
    }, ignoreInit = TRUE)

    output$risk_kpis <- renderUI({
      risk <- risk_result()
      if (is.null(risk)) return(div(class = "alert alert-info", "Run the disclosure scan to identify heuristic privacy risks."))
      high <- sum(risk$Severity == "High"); medium <- sum(risk$Severity == "Medium")
      reg <- app_state$privacy_register
      restricted <- if (is.data.frame(reg)) sum(reg$classification %in% c("Restricted / direct identifier", "Sensitive attribute")) else 0
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("High risks", high, "circle-exclamation"),
          sap_kpi("Medium risks", medium, "triangle-exclamation"),
          sap_kpi("Restricted variables", restricted, "lock"),
          sap_kpi("Approval required", if (isTRUE(input$require_approval)) "Yes" else "No", "user-shield"))
    })

    output$risk_table <- renderDT({
      risk <- risk_result()
      if (is.null(risk)) return(datatable(data.frame(Instruction = "Run a disclosure scan before publishing or exporting data."), options = list(dom = "t"), rownames = FALSE))
      datatable(risk, options = list(pageLength = 15, dom = "tip", scrollX = TRUE), rownames = FALSE)
    })

    observeEvent(input$save_policy, {
      app_state$governance_policy <- list(small_cell = input$small_cell, min_base = input$min_base, redact_ai = input$redact_ai,
        require_approval = input$require_approval, audit_exports = input$audit_exports, retention = input$retention, updated_at = Sys.time())
      if (!is.null(services$privacy)) services$privacy$revoke_export_approval()
      sap_frontend_record(app_state, "Privacy", "Governance policy saved", app_state$governance_policy)
      showNotification("Governance policy saved. Export approval was reset.", type = "message")
    })

    output$approval_status <- renderUI({
      required <- isTRUE((app_state$governance_policy %||% list())$require_approval)
      approved <- isTRUE(app_state$governance_export_approved)
      div(class = paste("alert py-2", if (approved || !required) "alert-success" else "alert-warning"),
          icon(if (approved || !required) "circle-check" else "lock"), " ",
          if (!required) "Approval is not required by the current policy."
          else if (approved) "External exports are approved for this session."
          else "External exports are blocked until session approval is granted.")
    })

    observeEvent(input$approve_exports, {
      req(!is.null(services), !is.null(services$privacy))
      services$privacy$grant_export_approval(app_state$current_user %||% "session-user")
      showNotification("Exports approved for this session.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$revoke_exports, {
      req(!is.null(services), !is.null(services$privacy))
      services$privacy$revoke_export_approval()
      showNotification("Export approval revoked.", type = "warning")
    }, ignoreInit = TRUE)
  })
}

message("[SAP] Privacy & Governance enforcement v1.1-20260809 loaded")
