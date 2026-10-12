# Deployment preflight for Survey Analytics.
# Run in a clean R session: source("CHECK_APP.R")

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
required_files <- c(
  "app.R", "auth.R", "14_auth.R", "02_data_explorer.R",
  "03_data_cleaning.R", "03b_guided_cleaning_studio.R", "00_services.R",
  "package_manifest.R", "global_no_gtsummary.R", "frontend_components.R",
  "release_metadata.R", "VERSION", "DESCRIPTION", "www/nexus-studio.css",
  "www/sap-analytics-experience.css", "www/sap-analytics-experience.js",
  "www/sap-professional-suite.css", "www/sap-professional-suite.js",
  "www/sap-i18n-catalog.js", "www/sap-i18n.js",
  "tests/navigation_i18n_regression.js", "tests/system_i18n_regression.js",
  "42_product_foundation.R", "49_i18n_system.R", "43_guided_workflow.R",
  "44_monitoring_evaluation.R", "45_data_literacy.R",
  "46_field_integrations.R", "47_operations_center.R", "48_program_reports.R",
  "LOCK_DEPENDENCIES.R", "data/cleaning_function_catalog.csv",
  "data/ui_translations.csv", "data/me_indicator_templates.csv", "data/data_literacy_glossary.csv",
  "data/sample_cacce_monitoring_dataset.csv",
  "db/migrations/007_monitoring_evaluation.sql",
  "db/migrations/008_monitoring_evaluation_compatibility.sql",
  "docs/USER_GUIDE.md", "docs/PRODUCTION_CHECKLIST.md",
  ".github/workflows/release.yml"
)
missing_files <- required_files[!file.exists(file.path(project_root, required_files))]
if (length(missing_files)) {
  stop("Application files are missing: ", paste(missing_files, collapse = ", "), call. = FALSE)
}

source(file.path(project_root, "package_manifest.R"), local = FALSE)
missing_packages <- SAP_CORE_PACKAGES[
  !vapply(SAP_CORE_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Required packages are missing: ", paste(missing_packages, collapse = ", "),
    ". Run source('INSTALL_DEPENDENCIES.R') first.", call. = FALSE
  )
}

r_files <- list.files(project_root, pattern = "\\.R$", recursive = TRUE,
                      full.names = TRUE, include.dirs = FALSE)
for (r_file in r_files) {
  tryCatch(parse(file = r_file, keep.source = FALSE), error = function(e) {
    stop("R syntax error in ", r_file, ": ", conditionMessage(e), call. = FALSE)
  })
}
message("[CHECK] Parsed ", length(r_files), " R source files successfully.")

app_object <- source(file.path(project_root, "app.R"), local = globalenv())$value
if (!inherits(app_object, "shiny.appobj")) {
  stop("app.R did not return a valid Shiny application object.", call. = FALSE)
}
message("[CHECK] app.R returns a valid Shiny application object.")

test_directory <- tempfile(pattern = "sap-auth-check-")
dir.create(test_directory, recursive = TRUE, mode = "0700")
test_state <- shiny::reactiveValues(
  current_user = NULL, current_project = NULL,
  current_project_name = "Preflight Workspace"
)
auth_check <- FileAuthService$new(test_state, storage_dir = test_directory)

shiny::isolate({
  registered <- auth_check$register(
    username = "preflight_user", password = "correct horse battery survey",
    email = "preflight@example.invalid", full_name = "Preflight Check"
  )
  if (!isTRUE(registered$ok)) stop("Standalone account signup failed: ", registered$error)
  invalid <- auth_check$login("preflight_user", "incorrect passphrase")
  if (isTRUE(invalid$ok)) stop("An invalid password was incorrectly accepted.")
  signed_in <- auth_check$login("preflight_user", "correct horse battery survey")
  if (!isTRUE(signed_in$ok) || !isTRUE(auth_check$is_authenticated())) {
    stop("Standalone account sign-in did not create an authenticated session.")
  }
  account_file <- readRDS(auth_check$storage_path())
  if (any(account_file$password_hash == "correct horse battery survey")) {
    stop("An account passphrase was stored without secure hashing.")
  }
  auth_check$logout()
  if (isTRUE(auth_check$is_authenticated())) stop("Standalone account sign-out failed.")
})
message("[CHECK] Secure signup, login, invalid-password rejection and logout passed.")
unlink(test_directory, recursive = TRUE, force = TRUE)

account_source <- paste(readLines("14_auth.R", warn = FALSE), collapse = "\n")
account_isolation_markers <- c(
  "app_state$me_indicators <- sap_me_empty_indicators()",
  "app_state$me_results <- sap_me_empty_results()",
  "app_state$me_grievances <- sap_me_register_empty(\"grievances\")",
  "app_state$governance_export_approved <- FALSE",
  "app_state$workbook_sheets <- list()"
)
missing_account_isolation <- account_isolation_markers[
  !vapply(account_isolation_markers, grepl, logical(1),
          x = account_source, fixed = TRUE)
]
if (length(missing_account_isolation)) {
  stop(
    "Account sign-out does not clear protected workspace state: ",
    paste(missing_account_isolation, collapse = ", "), call. = FALSE
  )
}
message("[CHECK] Sign-out clears protected monitoring, grievance, workbook and approval state.")

sample_frame <- data.frame(
  respondent_id = 1:4,
  district = c("Ngororero", "Musanze", "Ngororero", "Rubavu"),
  income = c(100000, NA_real_, 125000, 140000),
  stringsAsFactors = FALSE
)
explorer_state <- shiny::reactiveValues(
  data_imported = TRUE, processed_data = sample_frame,
  raw_data = sample_frame, variable_types = NULL,
  variable_dictionary = data.frame(), settings = DEFAULT_SETTINGS
)
shiny::testServer(
  mod_data_explorer_server,
  args = list(app_state = explorer_state, services = NULL),
  {
    overview <- paste(unlist(output$dataset_summary), collapse = " ")
    if (!grepl("sap-kpi-card", overview, fixed = TRUE) ||
        !grepl("Rows", overview, fixed = TRUE) ||
        !grepl("Columns", overview, fixed = TRUE)) {
      stop("The Data Explorer overview did not render visible summary cards.")
    }
  }
)
message("[CHECK] Data Explorer renders summary cards with missing import metadata.")

card_markup <- paste(as.character(kpi_card("Rows", "120", "observations", color = "primary")), collapse = "")
card_styles <- paste(readLines("www/sap-analytics-experience.css", warn = FALSE), collapse = "\n")
if (!grepl("sap-kpi-card-primary", card_markup, fixed = TRUE) ||
    !grepl(".card.sap-kpi-card.sap-kpi-card-primary", card_styles, fixed = TRUE)) {
  stop("Data Explorer card colours are not wired to the corrected stylesheet.")
}

cleaning_markup <- paste(
  as.character(mod_data_cleaning_ui("preflight_data_cleaning")),
  collapse = ""
)
cleaning_layout_markers <- c(
  "sap-data-cleaning-page", "sap-clean-workspace-grid",
  "sap-clean-sidebar", "sap-clean-main", "sap-clean-group-picker",
  "sap-clean-preview-grid", "guided_after_preview_state"
)
missing_cleaning_markers <- cleaning_layout_markers[
  !vapply(cleaning_layout_markers, grepl, logical(1),
          x = cleaning_markup, fixed = TRUE)
]
if (length(missing_cleaning_markers)) {
  stop(
    "The responsive Data Cleaning layout is incomplete: ",
    paste(missing_cleaning_markers, collapse = ", "),
    call. = FALSE
  )
}
if (grepl("sap-clean-panel h-100", cleaning_markup, fixed = TRUE)) {
  stop("Data Cleaning still contains a stretched, overlapping panel.")
}
cleaning_style_markers <- c(
  ".sap-clean-workspace-grid", ".sap-clean-group-picker .btn-group",
  "flex-wrap: wrap !important", "height: auto !important",
  "@media (max-width: 1080px)", "@container (max-width: 980px)",
  ".sap-clean-preview-empty"
)
missing_cleaning_styles <- cleaning_style_markers[
  !vapply(cleaning_style_markers, grepl, logical(1),
          x = card_styles, fixed = TRUE)
]
if (length(missing_cleaning_styles)) {
  stop(
    "Responsive Data Cleaning styles are missing: ",
    paste(missing_cleaning_styles, collapse = ", "),
    call. = FALSE
  )
}
message("[CHECK] Responsive Data Cleaning panels, tool categories and previews are wired.")

navigation_markup <- paste(
  as.character(sap_nav_button("preflight_nav_label", "Single navigation label", "house")),
  collapse = ""
)
navigation_markers <- gregexpr(
  'data-sap-nav-label="true"', navigation_markup, fixed = TRUE
)[[1]]
navigation_script <- paste(
  readLines("www/sap-i18n.js", warn = FALSE), collapse = "\n"
)
app_source <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
if (identical(navigation_markers[[1]], -1L) || length(navigation_markers) != 1L ||
    !grepl("[data-sap-nav-label]", navigation_script, fixed = TRUE) ||
    grepl("selectInput(\"global_language\"", app_source, fixed = TRUE) ||
    !grepl("global_language_toggle", app_source, fixed = TRUE) ||
    !grepl("sap-language-icon-button", app_source, fixed = TRUE)) {
  stop("The icon-only multilingual navigation shell is incomplete.")
}
message("[CHECK] Language selection is icon-only and multilingual navigation is replaceable.")

if (!grepl("activeLanguage='en'", navigation_script, fixed = TRUE) ||
    grepl("localStorage.getItem('sap-language')", navigation_script, fixed = TRUE) ||
    !grepl('ui_language = "en"', app_source, fixed = TRUE) ||
    !grepl('identical(marker, "guest")', app_source, fixed = TRUE)) {
  stop("English is not enforced as the authoritative default language for fresh/Guest sessions.")
}
message("[CHECK] English is the authoritative default; signed-in preferences restore only after authentication.")

if (!identical(SAP_VERSION, "6.1.1") ||
    !identical(SAP_RELEASE_NAME, "Multilingual Professional Monitoring & Evaluation Edition")) {
  stop("The project does not expose the expected multilingual Professional 6.1.1 release identity.")
}

i18n_catalog <- sap_i18n_catalog()
if (nrow(i18n_catalog) < 300L ||
    any(!vapply(c("en", "rw", "fr", "sw"), function(lang) all(nzchar(i18n_catalog[[lang]])), logical(1)))) {
  stop("The bundled EN/RW/FR/SW interface translation catalog is incomplete.")
}
if (identical(sap_tr("Data Cleaning", "rw"), "Data Cleaning") ||
    identical(sap_tr("Data Cleaning", "fr"), "Data Cleaning") ||
    identical(sap_tr("Data Cleaning", "sw"), "Data Cleaning")) {
  stop("Core interface phrases are not translated in all supported languages.")
}
message("[CHECK] Bundled system i18n catalog contains ", nrow(i18n_catalog), " translated interface phrases.")

templates <- sap_me_templates()
if (nrow(templates) < 12L || any(templates$actual != 0, na.rm = TRUE)) {
  stop("Rwanda M&E templates are missing or contain unverified project achievements.")
}
if (!identical(as.numeric(sap_me_progress(20, 100, 60)), 50)) {
  stop("Baseline-adjusted indicator progress is not calculated correctly.")
}
if (!identical(as.numeric(sap_me_progress(100, 40, 70, "decrease")), 50)) {
  stop("Decreasing indicator targets are not calculated correctly.")
}
history_record <- sap_me_normalize_results(data.frame(
  indicator_id = templates$indicator_id[[1]],
  indicator_code = templates$code[[1]],
  indicator_name = templates$name[[1]],
  reporting_period = "2027 Q1", observed_value = 45,
  denominator = 100, sector = "Kavumu", gender = "Women",
  verification = "Verified project evidence", stringsAsFactors = FALSE
))
if (!isTRUE(sap_me_validate_result(history_record[1, , drop = FALSE])$ok) ||
    ncol(history_record) != length(SAP_ME_RESULT_COLUMNS)) {
  stop("Verified reporting-period results are not normalized or validated correctly.")
}
if (isTRUE(sap_me_validate_result(list(
  indicator_id = "", reporting_period = "", observed_value = -1
))$ok)) {
  stop("Incomplete or negative reporting-period results were incorrectly accepted.")
}
safe_history <- sap_me_report_result_summary(history_record,
                                               include_disaggregation = FALSE)
if (any(c("district", "sector", "gender", "age_group", "disability",
          "notes", "recorded_by") %in% names(safe_history))) {
  stop("Restricted reporting-history fields remain in an undisaggregated shared report.")
}
register_types <- c("activities", "risks", "grievances", "actions")
registers_valid <- vapply(register_types, function(type) {
  register <- sap_me_register_empty(type)
  is.data.frame(register) && ncol(register) >= 7L
}, logical(1))
if (!all(registers_valid) ||
    !"due_date" %in% names(sap_me_register_empty("grievances"))) {
  stop("Monitoring and safeguarding register structures are incomplete.")
}
message("[CHECK] ", nrow(templates),
        " editable indicator templates, period-result history, and baseline-adjusted M&E calculations passed.")

monitoring_demo <- utils::read.csv(
  "data/sample_cacce_monitoring_dataset.csv", stringsAsFactors = FALSE,
  check.names = FALSE, fileEncoding = "UTF-8"
)
required_demo_fields <- c(
  "beneficiary_id", "district", "sector", "gender", "age",
  "disability_status", "survey_weight", "consent_recorded"
)
if (nrow(monitoring_demo) < 250L ||
    !all(required_demo_fields %in% names(monitoring_demo))) {
  stop("The Rwanda monitoring demonstration lacks the required inclusion-aware fields.")
}
if (anyDuplicated(monitoring_demo$beneficiary_id)) {
  stop("The synthetic monitoring demonstration contains duplicated beneficiary identifiers.")
}
message("[CHECK] Synthetic Rwanda monitoring dataset contains ", nrow(monitoring_demo),
        " unique beneficiary examples.")

audit <- sap_quality_audit(data.frame(
  beneficiary_id = c("A", "A", "C"), age = c(20, 140, 31),
  survey_weight = c(1, 0, 1), stringsAsFactors = FALSE
))
if (!all(c("Uniqueness", "Validity", "Survey design") %in% audit$category)) {
  stop("Automated data-quality screening did not identify invalid core records.")
}
if (nrow(sap_literacy_glossary("fr")) < 25L ||
    nrow(sap_literacy_glossary("rw")) < 25L) {
  stop("French or Kinyarwanda data-literacy explanations are incomplete.")
}
recommendation <- sap_literacy_recommendation("target", "count")
if (!identical(recommendation$route, "monitoring")) {
  stop("The guided analysis advisor does not connect target questions to M&E.")
}
message("[CHECK] Quality screening, trilingual literacy, and analysis recommendations passed.")

blocked_remote <- tryCatch({
  sap_integration_json_request("http://example.invalid/data", "Bearer test")
  FALSE
}, error = function(e) grepl("HTTPS", conditionMessage(e), fixed = TRUE))
if (!isTRUE(blocked_remote)) {
  stop("Field-data connectors must reject insecure HTTP destinations before connecting.")
}
if (length(sap_me_report_types()) < 10L) {
  stop("Professional report templates are incomplete.")
}
report_config <- sap_me_report_config(
  "<unsafe project>", "Verification Team", "Project Partner", "Q1",
  "Preflight Reviewer", "quarterly"
)
report_html <- sap_me_render_html_report(
  report_config, utils::head(templates, 2L), monitoring_demo,
  results = history_record
)
if (!grepl("&lt;unsafe project&gt;", report_html, fixed = TRUE) ||
    !grepl("Indicator performance against targets", report_html, fixed = TRUE) ||
    !grepl("Reporting-period results and verification", report_html, fixed = TRUE)) {
  stop("Professional reports do not safely escape labels and render indicators.")
}
rw_report_config <- sap_me_report_config(
  "CACCE", "M&E Team", "Ngororero District", "Q1", "Reviewer", "quarterly", language = "rw"
)
rw_report_html <- sap_me_render_html_report(rw_report_config, utils::head(templates, 2L), monitoring_demo, results = history_record)
if (!grepl(sap_tr("Indicator performance against targets", "rw"), rw_report_html, fixed = TRUE) ||
    !grepl("<html lang='rw'>", rw_report_html, fixed = TRUE)) {
  stop("Downloaded programme reports do not follow the selected system language.")
}
message("[CHECK] Protected HTTPS integrations and multilingual professional reports passed.")

professional_pages <- list(
  guided = mod_guided_workflow_ui("preflight_guided"),
  monitoring = mod_monitoring_evaluation_ui("preflight_monitoring"),
  literacy = mod_data_literacy_ui("preflight_literacy"),
  integrations = mod_field_integrations_ui("preflight_integrations"),
  reports = mod_program_reports_ui("preflight_reports"),
  operations = mod_operations_center_ui("preflight_operations")
)
for (page_name in names(professional_pages)) {
  html <- paste(as.character(professional_pages[[page_name]]), collapse = "")
  if (!nzchar(html) || nchar(html) < 100L) {
    stop("The ", page_name, " professional application page did not render.")
  }
}
message("[CHECK] Six professional application pages render valid Shiny UI.")

# Server-level regression checks for professional modules. These tests protect
# against reading reactiveValues during module construction (the v6.0.2 startup
# failure) and verify that both MEL and Field Integration state can initialize.
mel_server_state <- shiny::reactiveValues(
  me_indicators = sap_me_empty_indicators(),
  me_results = sap_me_empty_results(),
  me_activities = sap_me_register_empty("activities"),
  me_risks = sap_me_register_empty("risks"),
  me_grievances = sap_me_register_empty("grievances"),
  me_actions = sap_me_register_empty("actions"),
  processed_data = monitoring_demo, raw_data = monitoring_demo,
  current_user_profile = list(username = "preflight_user"),
  last_action = NULL
)
shiny::testServer(
  mod_monitoring_evaluation_server,
  args = list(app_state = mel_server_state, services = NULL, pool = NULL),
  {
    shiny::isolate({
      if (!is.data.frame(mel_server_state$me_indicators) ||
          !is.data.frame(mel_server_state$me_results) ||
          nrow(mel_server_state$me_indicators) < 12L) {
        stop("MEL server state did not initialize safely.", call. = FALSE)
      }
    })
  }
)

field_server_state <- shiny::reactiveValues(
  field_visits = sap_field_visits_empty(),
  current_user_profile = list(username = "preflight_user"),
  last_action = NULL
)
shiny::testServer(
  mod_field_integrations_server,
  args = list(app_state = field_server_state, services = NULL),
  {
    shiny::isolate({
      required_visit_columns <- c("date", "site", "observation", "follow_up", "recorded_by")
      if (!is.data.frame(field_server_state$field_visits) ||
          !all(required_visit_columns %in% names(field_server_state$field_visits))) {
        stop("Field Integration server state did not initialize safely.", call. = FALSE)
      }
    })
  }
)
message("[CHECK] Professional module server initialization is reactive-safe.")

# Migration 007 was shipped in an earlier 6.0.x build with a different immutable
# checksum. 6.1.0 accepts only that exact historical checksum transition and
# keeps rejecting every unknown modification. Migration 008 then reconciles the
# MEL schema idempotently without deleting user data.
source(file.path("db", "migrate.R"), local = FALSE)
legacy_007 <- "7d36fe3757bd91eec0df0738e9d9fa5889d6848f8405d5936c00d5a5b306233c"
canonical_007 <- digest::digest(
  object = file.path("db", "migrations", "007_monitoring_evaluation.sql"),
  algo = "sha256", serialize = FALSE, file = TRUE
)
if (!identical(canonical_007, "66df4cbd21fc40adda57d3b63d4449847bd3dac95446634f2a63ff042f0f7a48")) {
  stop("Migration 007 canonical checksum changed unexpectedly.", call. = FALSE)
}
if (!identical(
  sap_migration_checksum_status("007_monitoring_evaluation.sql", legacy_007, canonical_007),
  "compatible_legacy"
)) {
  stop("Known migration 007 compatibility transition is not recognized.", call. = FALSE)
}
if (!identical(
  sap_migration_checksum_status("007_monitoring_evaluation.sql", paste0("x", legacy_007), canonical_007),
  "mismatch"
)) {
  stop("Unknown migration checksum modifications must remain blocked.", call. = FALSE)
}
message("[CHECK] Migration 007 legacy checksum compatibility is narrow and migration 008 is present.")

message(paste(
  "[CHECK] Multilingual Professional 6.1.1 deployment, authentication, Data Explorer,",
  "Data Cleaning, system-wide EN/RW/FR/SW localization, monitoring, field connectors,",
  "language-aware professional reports and operational readiness checks passed."
))
invisible(TRUE)
