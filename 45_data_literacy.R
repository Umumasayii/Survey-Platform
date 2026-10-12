# Data-literacy glossary, method advisor and multilingual explanations.
sap_literacy_glossary <- function(language="en") {
  p<-file.path("data","data_literacy_glossary.csv"); if(!file.exists(p)) return(data.frame()); x<-utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8"); y<-x[x$language==language,,drop=FALSE]; if(!nrow(y)) y<-x[x$language=="en",,drop=FALSE]; y
}

sap_literacy_recommendation <- function(question_type, outcome_type) {
  q<-tolower(question_type %||% "describe"); o<-tolower(outcome_type %||% "count")
  if(q %in% c("target","progress","indicator","monitor")) return(list(route="monitoring",title="MEL performance",method="Compare baseline, target and verified actuals",why="The question is about results performance."))
  if(q %in% c("where","location","spatial")) return(list(route="geospatial",title="Geospatial analysis",method="Map, aggregate and test spatial patterns",why="The question depends on place."))
  if(q %in% c("change","trend","before_after")) return(list(route="longitudinal",title="Trend / longitudinal analysis",method="Repeated-measure or time-series analysis",why="The question is about change over time."))
  if(q %in% c("predict","prediction")) return(list(route="ml",title="Predictive modelling",method="Cross-validated model with holdout evaluation",why="The objective is prediction."))
  if(o %in% c("binary","categorical")) list(route="statistics",title="Group/association analysis",method="Cross-tabulation, chi-square or logistic regression",why="The outcome is categorical.") else list(route="descriptive",title="Descriptive & inferential statistics",method="Distribution summaries, confidence intervals and regression where appropriate",why="Start by understanding the outcome distribution.")
}

mod_data_literacy_ui <- function(id){ns<-NS(id);tagList(sap_professional_header("Data Literacy & Analysis Advisor","Plain-language statistical guidance with a controlled multilingual glossary.","book-open-reader","English · Kinyarwanda · French · Kiswahili"),layout_columns(card(card_header("Ask the analysis advisor"),selectInput(ns("question"),"What do you want to know?",c("Describe"="describe","Compare groups"="compare","Track a target"="target","Measure change"="change","Find spatial patterns"="where","Predict"="predict")),selectInput(ns("outcome"),"Outcome type",c("Count / numeric"="count","Binary"="binary","Categorical"="categorical","Continuous"="continuous")),uiOutput(ns("advice"))),card(card_header("Glossary"),selectInput(ns("language"),"Glossary language",SAP_SUPPORTED_LANGUAGES,selected="en"),DT::DTOutput(ns("glossary"))),col_widths=c(5,7)))}
mod_data_literacy_server <- function(id, app_state, services = NULL) moduleServer(id, function(input, output, session) {
  observe({
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    if (!identical(input$language %||% "", lang)) updateSelectInput(session, "language", selected = lang)
  })
  output$advice <- renderUI({
    r <- sap_literacy_recommendation(input$question, input$outcome)
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    div(
      class = "sap-advice-card",
      h4(icon("route"), span(r$title, class = "sap-translatable-output")),
      p(strong(r$method), class = "sap-translatable-output"),
      p(r$why, class = "sap-translatable-output"),
      span(sap_tr("Recommended workspace:", lang), " ", strong(r$route), class = "sap-translatable-output")
    )
  })
  output$glossary <- DT::renderDT(
    DT::datatable(sap_literacy_glossary(input$language %||% sap_normalize_language(app_state$ui_language %||% "en")),
                  options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  )
})
