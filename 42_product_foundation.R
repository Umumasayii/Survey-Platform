# Professional product foundation: shared UI, localization, quality and MEL helpers.
SAP_SUPPORTED_LANGUAGES <- c(en = "English", rw = "Kinyarwanda", fr = "Français", sw = "Kiswahili")
SAP_USER_MODES <- c("Beginner", "MEL Officer", "Data Analyst", "Statistician", "GIS Analyst", "Manager", "Administrator")

sap_escape_html <- function(x) htmltools::htmlEscape(as.character(x %||% ""), attribute = FALSE)
sap_escape_attr <- function(x) htmltools::htmlEscape(as.character(x %||% ""), attribute = TRUE)

sap_professional_header <- function(title, subtitle, icon_name = "chart-line", badge = NULL) {
  div(class="sap-pro-page-header",
      div(class="sap-pro-page-icon", icon(icon_name)),
      div(class="sap-pro-page-copy", h2(title), p(subtitle)),
      if (!is.null(badge)) span(class="badge text-bg-primary sap-pro-badge", badge))
}

sap_pro_kpi <- function(label, value, detail="", icon_name="circle-check", tone="primary") {
  div(class=paste("sap-pro-kpi", paste0("sap-pro-kpi-", tone)),
      div(class="sap-pro-kpi-icon", icon(icon_name)),
      div(class="sap-pro-kpi-copy", span(label, class="sap-pro-kpi-label"),
          strong(as.character(value), class="sap-pro-kpi-value"), span(detail, class="sap-pro-kpi-detail")))
}

sap_progress_status <- function(progress) {
  p <- suppressWarnings(as.numeric(progress))
  if (is.na(p)) return("No data")
  if (p >= 90) "On track" else if (p >= 70) "Watch" else "Off track"
}

sap_rag_class <- function(progress) {
  switch(sap_progress_status(progress), "On track"="success", "Watch"="warning", "Off track"="danger", "secondary")
}

sap_safe_numeric <- function(x, default=NA_real_) {
  y <- suppressWarnings(as.numeric(x)); ifelse(is.finite(y), y, default)
}

sap_quality_audit <- function(data) {
  if (!is.data.frame(data) || !ncol(data)) return(data.frame(category="Availability", issue="No dataset is loaded", records=0L, severity="Info"))
  out <- list()
  miss <- sum(is.na(data)); if (miss > 0) out[[length(out)+1]] <- data.frame(category="Completeness", issue="Missing values detected", records=miss, severity="Warning")
  id_candidates <- intersect(c("beneficiary_id","respondent_id","id","uuid"), names(data))
  if (length(id_candidates)) { ndup <- sum(duplicated(data[[id_candidates[[1]]]])); if (ndup>0) out[[length(out)+1]] <- data.frame(category="Uniqueness", issue=paste("Duplicate", id_candidates[[1]], "values"), records=ndup, severity="Critical") }
  age_candidates <- intersect(c("age","respondent_age"), names(data))
  if (length(age_candidates)) { a <- suppressWarnings(as.numeric(data[[age_candidates[[1]]]])); nbad <- sum(!is.na(a) & (a<0 | a>120)); if(nbad>0) out[[length(out)+1]] <- data.frame(category="Validity", issue="Age outside 0–120", records=nbad, severity="Critical") }
  weight_candidates <- intersect(c("survey_weight","weight","sampling_weight"), names(data))
  if (length(weight_candidates)) { w <- suppressWarnings(as.numeric(data[[weight_candidates[[1]]]])); nbad <- sum(!is.na(w) & w<=0); if(nbad>0) out[[length(out)+1]] <- data.frame(category="Survey design", issue="Non-positive survey weights", records=nbad, severity="Warning") }
  consent_candidates <- intersect(c("consent_recorded","consent","informed_consent"), names(data))
  if (length(consent_candidates)) { v <- tolower(as.character(data[[consent_candidates[[1]]]])); nbad <- sum(!is.na(v) & !v %in% c("yes","true","1","recorded","consented")); if(nbad>0) out[[length(out)+1]] <- data.frame(category="Governance", issue="Consent not affirmatively recorded", records=nbad, severity="Critical") }
  if (!length(out)) out[[1]] <- data.frame(category="Quality", issue="No priority rule violations detected", records=0L, severity="OK")
  do.call(rbind, out)
}

sap_dataset_context <- function(app_state) {
  d <- app_state$processed_data %||% app_state$raw_data
  list(name=app_state$dataset_name %||% "No dataset", rows=if(is.data.frame(d)) nrow(d) else 0L,
       columns=if(is.data.frame(d)) ncol(d) else 0L, project=app_state$current_project_name %||% "Workspace")
}

sap_translation_catalog <- function() list(
  en=c(home="Presentation Home", guided="Guided Workspace", monitoring="MEL & Project Performance", literacy="Data Literacy", integrations="Field Integrations", reports="Programme Reports", operations="Operations Center", language="Language"),
  rw=c(home="Ahabanza", guided="Ubufasha mu Isesengura", monitoring="Gukurikirana no Gusuzuma", literacy="Ubumenyi ku Makuru", integrations="Ihuza ry'Amakuru yo mu Murima", reports="Raporo za Porogaramu", operations="Igenzura ry'Imikorere", language="Ururimi"),
  fr=c(home="Accueil", guided="Espace Guidé", monitoring="Suivi-évaluation & Performance", literacy="Culture des données", integrations="Intégrations terrain", reports="Rapports de programme", operations="Centre des opérations", language="Langue"),
  sw=c(home="Mwanzo", guided="Eneo la Mwongozo", monitoring="Ufuatiliaji, Tathmini na Utendaji", literacy="Uelewa wa Data", integrations="Miunganisho ya Uwanjani", reports="Ripoti za Programu", operations="Kituo cha Uendeshaji", language="Lugha")
)
