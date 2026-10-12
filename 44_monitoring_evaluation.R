# Monitoring, Evaluation, Learning and project-performance workspace.
SAP_ME_RESULT_COLUMNS <- c("indicator_id","indicator_code","indicator_name","reporting_period","observed_value","denominator","district","sector","gender","age_group","disability","verification","notes","recorded_by","recorded_at")

sap_me_templates <- function() {
  path <- file.path("data","me_indicator_templates.csv")
  if (!file.exists(path)) return(data.frame())
  x <- utils::read.csv(path, stringsAsFactors=FALSE, check.names=FALSE, fileEncoding="UTF-8")
  if (!"actual" %in% names(x)) x$actual <- 0
  x$actual <- suppressWarnings(as.numeric(x$actual)); x$actual[is.na(x$actual)] <- 0
  x
}

sap_me_empty_indicators <- function() sap_me_templates()
sap_me_empty_results <- function() { x <- as.data.frame(setNames(replicate(length(SAP_ME_RESULT_COLUMNS), character(), simplify=FALSE), SAP_ME_RESULT_COLUMNS), stringsAsFactors=FALSE); x$observed_value <- numeric(); x$denominator <- numeric(); x }

sap_me_progress <- function(baseline, target, actual, direction="increase") {
  baseline <- as.numeric(baseline); target <- as.numeric(target); actual <- as.numeric(actual)
  if (any(!is.finite(c(baseline,target,actual)))) return(NA_real_)
  if (tolower(direction) == "decrease") denom <- baseline-target else denom <- target-baseline
  if (denom == 0) return(ifelse(actual == target, 100, 0))
  value <- if (tolower(direction)=="decrease") (baseline-actual)/denom*100 else (actual-baseline)/denom*100
  round(max(0, min(100, value)), 1)
}

sap_me_normalize_results <- function(x) {
  if (!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors=FALSE)
  for (nm in setdiff(SAP_ME_RESULT_COLUMNS, names(x))) x[[nm]] <- if (nm %in% c("observed_value","denominator")) NA_real_ else ""
  x <- x[, SAP_ME_RESULT_COLUMNS, drop=FALSE]
  x$observed_value <- suppressWarnings(as.numeric(x$observed_value)); x$denominator <- suppressWarnings(as.numeric(x$denominator))
  if (all(!nzchar(x$recorded_at))) x$recorded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  x
}

sap_me_validate_result <- function(x) {
  if (is.data.frame(x)) x <- as.list(x[1,,drop=FALSE])
  indicator <- as.character(x$indicator_id %||% "")[[1]]; period <- as.character(x$reporting_period %||% "")[[1]]; value <- suppressWarnings(as.numeric(x$observed_value %||% NA))[[1]]
  errors <- character(); if (!nzchar(trimws(indicator))) errors<-c(errors,"Indicator is required"); if(!nzchar(trimws(period))) errors<-c(errors,"Reporting period is required"); if(!is.finite(value) || value<0) errors<-c(errors,"Observed value must be zero or greater")
  list(ok=!length(errors), errors=errors)
}

sap_me_report_result_summary <- function(results, include_disaggregation=FALSE) {
  results <- sap_me_normalize_results(results)
  if (!isTRUE(include_disaggregation)) results <- results[, setdiff(names(results), c("district","sector","gender","age_group","disability","notes","recorded_by")), drop=FALSE]
  results
}

sap_me_register_empty <- function(type=c("activities","risks","grievances","actions")) {
  type <- match.arg(type)
  switch(type,
    activities=data.frame(activity_id=character(), activity=character(), output=character(), owner=character(), start_date=character(), due_date=character(), status=character(), progress=numeric(), evidence=character(), stringsAsFactors=FALSE),
    risks=data.frame(risk_id=character(), risk=character(), probability=character(), impact=character(), mitigation=character(), owner=character(), due_date=character(), status=character(), stringsAsFactors=FALSE),
    grievances=data.frame(grievance_id=character(), category=character(), received_date=character(), description=character(), owner=character(), due_date=character(), status=character(), resolution=character(), stringsAsFactors=FALSE),
    actions=data.frame(action_id=character(), source=character(), action=character(), responsible=character(), due_date=character(), status=character(), evidence=character(), stringsAsFactors=FALSE))
}

mod_monitoring_evaluation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_professional_header("MEL & Project Performance", "Manage results frameworks, indicators, reporting-period results, field actions, risks and grievances in one governed workspace.", "bullseye", "Professional MEL"),
    uiOutput(ns("kpis")),
    navset_card_tab(
      nav_panel("Results framework", layout_columns(
        card(card_header("Indicator register"), DT::DTOutput(ns("indicator_table"))),
        card(card_header("Add verified result"), selectInput(ns("result_indicator"),"Indicator", choices=NULL), textInput(ns("result_period"),"Reporting period", placeholder="2026 Q3"), numericInput(ns("result_value"),"Observed value",0,min=0), numericInput(ns("result_denominator"),"Denominator (optional)",NA,min=0), textInput(ns("result_verification"),"Means of verification"), actionButton(ns("add_result"),"Record result",icon=icon("plus"),class="btn-primary")), col_widths=c(8,4))),
      nav_panel("Performance", DT::DTOutput(ns("performance_table"))),
      nav_panel("Activities", DT::DTOutput(ns("activities_table"))),
      nav_panel("Risks & issues", DT::DTOutput(ns("risks_table"))),
      nav_panel("Grievances", DT::DTOutput(ns("grievances_table"))),
      nav_panel("Action tracker", DT::DTOutput(ns("actions_table"))),
      nav_panel("Data quality", DT::DTOutput(ns("quality_table")))
    )
  )
}

mod_monitoring_evaluation_server <- function(id, app_state, services=NULL, pool=NULL) moduleServer(id, function(input, output, session) {
  # app_state is a shiny::reactiveValues object. Reading from it directly while
  # the module server is being constructed is outside a reactive consumer and
  # raises: "Can't access reactive value ... outside of reactive consumer".
  # isolate() provides the correct one-time initialization context while keeping
  # the defaults repairable for older/restored workspaces that lack MEL fields.
  shiny::isolate({
    if (is.null(app_state$me_indicators) || !is.data.frame(app_state$me_indicators)) {
      app_state$me_indicators <- sap_me_empty_indicators()
    }
    if (is.null(app_state$me_results) || !is.data.frame(app_state$me_results)) {
      app_state$me_results <- sap_me_empty_results()
    }
    for (nm in c("activities", "risks", "grievances", "actions")) {
      key <- paste0("me_", nm)
      if (is.null(app_state[[key]]) || !is.data.frame(app_state[[key]])) {
        app_state[[key]] <- sap_me_register_empty(nm)
      }
    }
  })
  observe({ choices <- setNames(app_state$me_indicators$indicator_id, paste(app_state$me_indicators$code, app_state$me_indicators$name, sep=" — ")); updateSelectInput(session,"result_indicator",choices=choices) })
  perf <- reactive({ x<-app_state$me_indicators; if(!nrow(x)) return(x); x$progress_pct<-mapply(sap_me_progress,x$baseline,x$target,x$actual,x$direction); x$status<-vapply(x$progress_pct,sap_progress_status,character(1)); x })
  output$kpis <- renderUI({ p<-perf(); ontrack<-if(nrow(p))sum(p$status=="On track",na.rm=TRUE) else 0; results<-nrow(app_state$me_results); div(class="sap-pro-kpi-grid", sap_pro_kpi("Indicators",nrow(p),"active definitions","bullseye"),sap_pro_kpi("On track",ontrack,"≥ 90% progress","circle-check","success"),sap_pro_kpi("Verified results",results,"reporting-period records","clipboard-check"),sap_pro_kpi("Open grievances",sum(tolower(app_state$me_grievances$status %||% character())!="closed"),"requires follow-up","shield-heart","warning")) })
  output$indicator_table <- DT::renderDT(DT::datatable(app_state$me_indicators, options=list(scrollX=TRUE,pageLength=8), rownames=FALSE))
  output$performance_table <- DT::renderDT(DT::datatable(perf(), options=list(scrollX=TRUE,pageLength=12), rownames=FALSE))
  output$activities_table <- DT::renderDT(DT::datatable(app_state$me_activities, options=list(scrollX=TRUE), rownames=FALSE))
  output$risks_table <- DT::renderDT(DT::datatable(app_state$me_risks, options=list(scrollX=TRUE), rownames=FALSE))
  output$grievances_table <- DT::renderDT(DT::datatable(app_state$me_grievances, options=list(scrollX=TRUE), rownames=FALSE))
  output$actions_table <- DT::renderDT(DT::datatable(app_state$me_actions, options=list(scrollX=TRUE), rownames=FALSE))
  output$quality_table <- DT::renderDT({ d<-app_state$processed_data %||% app_state$raw_data; DT::datatable(sap_quality_audit(d), options=list(dom='tip'), rownames=FALSE) })
  observeEvent(input$add_result,{ ind<-app_state$me_indicators[app_state$me_indicators$indicator_id==input$result_indicator,,drop=FALSE]; req(nrow(ind)==1); row<-data.frame(indicator_id=input$result_indicator,indicator_code=ind$code[[1]],indicator_name=ind$name[[1]],reporting_period=input$result_period,observed_value=input$result_value,denominator=input$result_denominator,district="",sector="",gender="",age_group="",disability="",verification=input$result_verification,notes="",recorded_by=app_state$current_user_profile$username %||% "user",recorded_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),stringsAsFactors=FALSE); v<-sap_me_validate_result(row); if(!v$ok){showNotification(paste(v$errors,collapse="; "),type="error");return()}; app_state$me_results<-rbind(app_state$me_results,sap_me_normalize_results(row)); app_state$me_indicators$actual[match(input$result_indicator,app_state$me_indicators$indicator_id)]<-input$result_value; app_state$last_action<-paste("Recorded MEL result for",ind$code[[1]]); showNotification("Verified result recorded",type="message") })
})
