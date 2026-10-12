# =============================================================================
# SURVEY ANALYTICS PLATFORM — DEPENDENCY BOOTSTRAP
# =============================================================================
# Hosted startup must never download packages or require optional capabilities.
# INSTALL_SURVEY_ANALYTICS_PACKAGES_FIXED.R remains available for existing users.
if (!file.exists("package_manifest.R")) {
  stop("package_manifest.R is missing from the application directory.", call. = FALSE)
}
source("package_manifest.R", local = FALSE)
.sap_missing_packages <- SAP_CORE_PACKAGES[
  !vapply(SAP_CORE_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
]

if (length(.sap_missing_packages) > 0L) {
  stop(
    paste0(
      "Core R packages are missing: ", paste(.sap_missing_packages, collapse = ", "),
      ". In RStudio, run source('INSTALL_DEPENDENCIES.R'), restart R, and launch again. ",
      "Hosted deployments install these packages during the build stage."
    ),
    call. = FALSE
  )
}
rm(.sap_missing_packages)

# =============================================================================
# SURVEY ANALYTICS PLATFORM — MULTI-USER CONTROL EDITION
# =============================================================================
# Deployment-aware application shell. PostgreSQL is available for durable,
# multi-instance production, while standalone deployments retain secure accounts
# without refusing to start when an external database has not been configured.
# =============================================================================

source("sap_environment.R", local = FALSE)
sap_load_project_env(getwd(), quiet = TRUE)
sap_apply_database_url(quiet = TRUE)

# Explicit runtime profiles -------------------------------------------------------
# Explicit production remains fail-closed. Automatic deployments without a
# PostgreSQL connection use the clearly labelled standalone-account profile.
.sap_profile_explicit <- trimws(Sys.getenv("SAP_RUNTIME_PROFILE", ""))
.sap_pg_required <- c("SAP_PG_HOST", "SAP_PG_DB", "SAP_PG_USER", "SAP_PG_PASSWORD")
.sap_pg_complete <- all(nzchar(trimws(Sys.getenv(.sap_pg_required, unset = ""))))
.sap_probably_hosted <- nzchar(trimws(Sys.getenv("PORT", ""))) ||
  nzchar(Sys.getenv("POSIT_PRODUCT", "")) || nzchar(Sys.getenv("CONNECT_SERVER", "")) ||
  tolower(Sys.getenv("R_CONFIG_ACTIVE", "")) %in% c("shinyapps", "connect_cloud", "production")
SAP_RUNTIME_PROFILE <- if (nzchar(.sap_profile_explicit)) {
  tolower(.sap_profile_explicit)
} else if (isTRUE(.sap_pg_complete)) {
  if (isTRUE(.sap_probably_hosted)) "production" else "development"
} else {
  "standalone"
}
if (!SAP_RUNTIME_PROFILE %in% c("standalone", "development", "demo", "production")) {
  stop("SAP_RUNTIME_PROFILE must be standalone, development, demo, or production.", call. = FALSE)
}
Sys.setenv(SAP_RUNTIME_PROFILE = SAP_RUNTIME_PROFILE)
if (identical(SAP_RUNTIME_PROFILE, "production") && !.sap_pg_complete) {
  stop(
    paste0(
      "Production runtime requires complete PostgreSQL configuration. Set ",
      paste(.sap_pg_required, collapse = ", "),
      " or choose SAP_RUNTIME_PROFILE=standalone for secure app-instance accounts."
    ),
    call. = FALSE
  )
}

if (identical(SAP_RUNTIME_PROFILE, "demo")) {
  Sys.setenv(
    SAP_DEMO_MODE = "true",
    SAP_DB_REQUIRED = "false",
    SAP_AUTH_REQUIRED = "false"
  )
} else if (identical(SAP_RUNTIME_PROFILE, "production")) {
  Sys.setenv(SAP_DEMO_MODE = "false", SAP_DB_REQUIRED = "true", SAP_AUTH_REQUIRED = "true")
} else if (identical(SAP_RUNTIME_PROFILE, "standalone")) {
  Sys.setenv(SAP_DEMO_MODE = "false", SAP_DB_REQUIRED = "false", SAP_AUTH_REQUIRED = "true")
} else {
  # Development can use PostgreSQL or the same secure standalone accounts.
  Sys.setenv(
    SAP_DEMO_MODE = "false",
    SAP_DB_REQUIRED = "false",
    SAP_AUTH_REQUIRED = "true"
  )
}
SAP_POSTGRES_ENABLED <- isTRUE(.sap_pg_complete) && !identical(SAP_RUNTIME_PROFILE, "demo")
if (!SAP_POSTGRES_ENABLED && !identical(SAP_RUNTIME_PROFILE, "demo")) {
  message("[SAP] PostgreSQL is not configured; secure standalone sign-in and signup are enabled.")
}
rm(.sap_pg_required, .sap_pg_complete, .sap_profile_explicit, .sap_probably_hosted)

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(shinyFiles)

source("release_metadata.R", local = FALSE)
.sap_release_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
.sap_required_release_assets <- c(
  "03b_guided_cleaning_studio.R",
  if (SAP_POSTGRES_ENABLED) c(
    "db/migrations/006_enterprise_platform.sql",
    "db/migrations/007_monitoring_evaluation.sql",
    "db/migrations/008_monitoring_evaluation_compatibility.sql"
  )
)
.sap_missing_release_assets <- .sap_required_release_assets[!file.exists(.sap_required_release_assets)]
if (length(.sap_missing_release_assets)) {
  stop(
    "Release directory is incomplete or R is running from the wrong folder. Missing: ",
    paste(.sap_missing_release_assets, collapse = ", "),
    ". Current directory: ", .sap_release_root,
    call. = FALSE
  )
}
message("[SAP] Release identity: ", SAP_PRESENTATION_VERSION)
message("[SAP] Release root: ", .sap_release_root)
rm(.sap_required_release_assets, .sap_missing_release_assets)
SAP_BRAND_TITLE <- "Survey Analytics"
SAP_BRAND_TAGLINE <- "From raw responses to confident decisions."
SAP_DEMO_MODE <- !tolower(Sys.getenv("SAP_DEMO_MODE", "false")) %in% c("false", "0", "no")
SAP_AUTH_REQUIRED <- !tolower(Sys.getenv("SAP_AUTH_REQUIRED", if (SAP_DEMO_MODE) "false" else "true")) %in%
  c("false", "0", "no")
SAP_DB_REQUIRED <- !tolower(Sys.getenv("SAP_DB_REQUIRED", if (SAP_DEMO_MODE) "false" else "true")) %in%
  c("false", "0", "no")
SAP_ALLOW_SIGNUP <- !tolower(Sys.getenv("SAP_ALLOW_SIGNUP", "true")) %in%
  c("false", "0", "no")
SAP_ALLOW_GUEST <- !tolower(Sys.getenv("SAP_ALLOW_GUEST", "false")) %in%
  c("false", "0", "no")
SAP_REQUIRE_SIGNUP_APPROVAL <- !tolower(Sys.getenv("SAP_REQUIRE_SIGNUP_APPROVAL", "false")) %in%
  c("false", "0", "no")

SAP_HOSTED_RUNTIME <- nzchar(trimws(Sys.getenv("PORT", ""))) ||
  nzchar(Sys.getenv("POSIT_PRODUCT", "")) ||
  nzchar(Sys.getenv("CONNECT_SERVER", "")) ||
  tolower(Sys.getenv("R_CONFIG_ACTIVE", "")) %in% c("shinyapps", "connect_cloud")
SAP_RUNTIME_LABEL <- switch(
  SAP_RUNTIME_PROFILE,
  production = "Production",
  demo = "Demo",
  standalone = "Standalone accounts",
  development = if (SAP_POSTGRES_ENABLED) "Development DB" else "Development accounts"
)

# =============================================================================
# SOURCE HELPERS
# =============================================================================

source_required <- function(f) {
  if (!file.exists(f)) {
    stop(sprintf("Required file '%s' not found in %s. Cannot start.", f, getwd()),
         call. = FALSE)
  }
  source(f)
}

source_required("global_no_gtsummary.R")
source_required("00_services.R")
source_required("auth.R")
invisible(capture.output(source_required("project_setup.R")))
source_required("frontend_components.R")

# The database file loads DBI/pool/blob packages. Keep it completely out of the
# frontend-only local path so development does not require PostgreSQL or its
# R packages. The service layer works in memory when pool = NULL.
if (SAP_POSTGRES_ENABLED) {
  source_required("db_backend.R")

  # Optional launch-safety probe. For local/desktop use this prevents a stopped
  # PostgreSQL service or a temporary connection problem from making the whole
  # Shiny process look like a launch timeout. If the database is reachable, the
  # probe also performs first-run database/schema migrations before the UI opens.
  .sap_allow_db_fallback <- !tolower(trimws(Sys.getenv("SAP_STARTUP_ALLOW_DEMO_FALLBACK", "false"))) %in%
    c("false", "0", "no")
  # A PostgreSQL-authenticated deployment must never hide Sign in/Create account
  # by silently switching to Demo mode. Fallback is allowed only when authentication
  # is explicitly not required.
  if (isTRUE(.sap_allow_db_fallback) && !isTRUE(SAP_AUTH_REQUIRED)) {
    .sap_probe_con <- NULL
    .sap_db_probe_ok <- tryCatch({
      .sap_probe_con <- db_connect_direct()
      db_init(.sap_probe_con)
      TRUE
    }, error = function(e) {
      message("[SAP] PostgreSQL startup probe unavailable: ", conditionMessage(e))
      FALSE
    }, finally = {
      if (!is.null(.sap_probe_con)) db_disconnect(.sap_probe_con)
    })
    if (!isTRUE(.sap_db_probe_ok)) {
      message("[SAP] Continuing in safe local workspace mode so the application can open.")
      SAP_POSTGRES_ENABLED <- FALSE
      SAP_DEMO_MODE <- FALSE
      SAP_AUTH_REQUIRED <- TRUE
      SAP_DB_REQUIRED <- FALSE
      SAP_RUNTIME_LABEL <- "Standalone accounts"
      Sys.setenv(SAP_DEMO_MODE = "false", SAP_AUTH_REQUIRED = "true", SAP_DB_REQUIRED = "false")
    }
    rm(.sap_allow_db_fallback, .sap_db_probe_ok)
    if (exists(".sap_probe_con")) rm(.sap_probe_con)
  }
}

source_required("01_import_wizard.R")
source_required("02_data_explorer.R")
source_required("03b_guided_cleaning_studio.R")
source_required("03_data_cleaning.R")
source_required("04_dashboard.R")
source_required("05_descriptive_stats.R")
source_required("06_statistical_tests.R")
source_required("07_visualization.R")
source_required("08_report_generator.R")
source_required("09_project_manager.R")
source_required("10_analysis_history.R")
source_required("11_machine_learning.R")
source_required("12_system_diagnostics.R")
source_required("13_settings.R")
source_required("14_auth.R")
source_required("15_ai_assistant.R")
source_required("16_code_generator.R")
source_required("17_variable_manager.R")
source_required("18_presentation_home.R")
source_required("19_survey_design.R")
source_required("20_table_builder.R")
source_required("21_scale_analysis.R")
source_required("22_missing_nonresponse.R")
source_required("23_questionnaire_manager.R")
source_required("24_sample_size_power.R")
source_required("25_advanced_models.R")
source_required("26_text_analysis.R")
source_required("27_geospatial_analysis.R")
source_required("28_longitudinal_analysis.R")
source_required("29_privacy_governance.R")
source_required("30_data_lab.R")
source_required("31_personal_health.R")
source_required("32_admin_control_center.R")
source_required("33_bi_core.R")
source_required("34_data_connections.R")
source_required("35_semantic_model.R")
source_required("36_transform_pipeline.R")
source_required("37_visual_builder.R")
source_required("38_dashboard_studio.R")
source_required("39_publish_center.R")
source_required("40_studio_home.R")
source_required("41_enterprise_center.R")
source_required("42_product_foundation.R")
source_required("49_i18n_system.R")
source_required("43_guided_workflow.R")
source_required("44_monitoring_evaluation.R")
source_required("45_data_literacy.R")
source_required("46_field_integrations.R")
source_required("47_operations_center.R")
source_required("48_program_reports.R")

# =============================================================================
# PRESENTATION STYLES
# =============================================================================

.sap_presentation_css <- shiny::HTML("
:root {
  --sap-radius: 18px;
  --sap-shadow: 0 12px 30px rgba(20, 35, 60, .08);
}

html, body { overflow-x: hidden; max-width: 100%; }
.bslib-sidebar-layout > .main, .bslib-sidebar-layout .main,
main, .main, .card, .card-body, .tab-content, .tab-pane,
.bslib-grid, .bslib-grid-item, .html-fill-item, .html-fill-container,
.shiny-html-output { min-width: 0 !important; }

table.table:not(.dataTable), .shiny-table:not(.dataTable),
.shiny-html-output > table:not(.dataTable) {
  display: block; max-width: 100%; overflow-x: auto; white-space: nowrap;
}
.dataTables_wrapper { width: 100% !important; max-width: 100% !important; overflow-x: auto; }
.table-responsive { max-width: 100%; overflow-x: auto; }

.sap-hero {
  display: grid; grid-template-columns: minmax(0, 1.7fr) minmax(240px, .7fr);
  gap: 2rem; align-items: center; padding: clamp(1.5rem, 4vw, 3.5rem);
  border-radius: var(--sap-radius); color: white;
  background: linear-gradient(125deg, #075985 0%, #0d6efd 55%, #6f42c1 100%);
  box-shadow: var(--sap-shadow);
}
.sap-hero h1 { font-size: clamp(2rem, 5vw, 3.8rem); line-height: 1.02; font-weight: 750; }
.sap-hero-route { padding: 1.25rem; border: 1px solid rgba(255,255,255,.3);
  border-radius: 14px; background: rgba(255,255,255,.12); backdrop-filter: blur(6px); }

.sap-card { border: 0; border-radius: var(--sap-radius); box-shadow: var(--sap-shadow); }
.sap-metric-grid { display: grid; grid-template-columns: repeat(6, minmax(135px, 1fr)); gap: 1rem; }
.sap-metric-card { display: flex; gap: .85rem; align-items: center; padding: 1rem;
  border: 1px solid var(--bs-border-color); border-radius: 14px; background: var(--bs-body-bg); }
.sap-metric-icon { display: grid; place-items: center; width: 42px; height: 42px;
  border-radius: 12px; background: rgba(var(--bs-primary-rgb), .1); color: var(--bs-primary); }
.sap-metric-value { font-size: 1.35rem; font-weight: 750; line-height: 1.1; }
.sap-metric-label { color: var(--bs-secondary-color); font-size: .82rem; }
.sap-workflow { display: grid; gap: .9rem; }
.sap-workflow-step { display: grid; grid-template-columns: 38px 1fr; gap: .8rem; align-items: start; }
.sap-step-number { display: grid; place-items: center; width: 34px; height: 34px;
  border-radius: 50%; background: var(--bs-primary); color: white; font-weight: 700; }
.sap-workspace-card { padding:1rem; border:1px solid rgba(var(--bs-primary-rgb),.24);
  border-radius:16px; background:linear-gradient(145deg, rgba(var(--bs-primary-rgb),.10), var(--bs-body-bg) 58%);
  box-shadow:0 8px 24px rgba(15,23,42,.07); }
.sap-workspace-heading { display:flex; align-items:flex-start; gap:.7rem; }
.sap-workspace-heading-icon { display:grid; place-items:center; width:36px; height:36px; flex:0 0 auto;
  border-radius:11px; background:var(--bs-primary); color:white; }
.sap-workspace-eyebrow { margin:0 0 .08rem; color:var(--bs-secondary-color); font-size:.68rem;
  font-weight:750; letter-spacing:.08em; text-transform:uppercase; }
.sap-workspace-project { margin:0; font-size:.98rem; font-weight:750; line-height:1.25; overflow-wrap:anywhere; }
.sap-dataset-banner { display:flex; gap:.65rem; align-items:flex-start; margin-top:.85rem; padding:.8rem;
  border-radius:13px; border:1px solid var(--bs-border-color); background:var(--bs-body-bg); }
.sap-dataset-banner-empty { border-color:rgba(13,202,240,.38); background:rgba(13,202,240,.12); }
.sap-dataset-banner-active { border-color:rgba(25,135,84,.32); background:rgba(25,135,84,.10); }
.sap-dataset-status-icon { margin-top:.1rem; font-size:1rem; }
.sap-dataset-banner-empty .sap-dataset-status-icon { color:#087990; }
.sap-dataset-banner-active .sap-dataset-status-icon { color:var(--bs-success); }
.sap-dataset-status-label { font-size:.67rem; font-weight:800; letter-spacing:.07em; text-transform:uppercase; }
.sap-dataset-name { margin-top:.1rem; font-size:.9rem; font-weight:750; line-height:1.25; overflow-wrap:anywhere; }
.sap-dataset-guidance { margin-top:.25rem; color:var(--bs-secondary-color); font-size:.75rem; line-height:1.35; }
.sap-workspace-metrics { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:.5rem; margin-top:.75rem; }
.sap-workspace-metric { min-width:0; padding:.55rem .6rem; border:1px solid var(--bs-border-color);
  border-radius:11px; background:rgba(var(--bs-body-bg-rgb),.78); }
.sap-workspace-metric-value { display:block; font-size:.88rem; font-weight:780; line-height:1.15; overflow-wrap:anywhere; }
.sap-workspace-metric-label { display:block; margin-top:.12rem; color:var(--bs-secondary-color); font-size:.66rem; }
.sap-workspace-context { display:grid; gap:.38rem; margin-top:.75rem; padding-top:.7rem;
  border-top:1px solid var(--bs-border-color); }
.sap-workspace-context-row { display:grid; grid-template-columns:18px 54px minmax(0,1fr); gap:.35rem;
  align-items:start; font-size:.72rem; }
.sap-workspace-context-row svg { margin-top:.1rem; color:var(--bs-primary); }
.sap-workspace-context-label { color:var(--bs-secondary-color); }
.sap-workspace-context-value { font-weight:650; text-align:right; overflow-wrap:anywhere; }
.sap-workspace-updated { margin-top:.7rem; color:var(--bs-secondary-color); font-size:.68rem; line-height:1.35; }
.sap-mode-badge { font-size: .72rem; letter-spacing: .02em; }

.btn:focus-visible, .form-control:focus-visible, .form-select:focus-visible,
.nav-link:focus-visible, a:focus-visible {
  outline: 3px solid rgba(var(--bs-primary-rgb), .4) !important;
  outline-offset: 2px;
}

.selectize-control.multi .selectize-input { max-width: 100%; flex-wrap: wrap; }
.selectize-control.multi .selectize-input > div,
.selectize-dropdown .option, .selectize-dropdown-content .option {
  max-width: 100%; white-space: normal; word-break: break-word; overflow-wrap: anywhere;
}


.sap-module-hero { display:flex; gap:1rem; align-items:center; padding:1.25rem 1.4rem;
  border:1px solid var(--bs-border-color); border-radius:var(--sap-radius);
  background:linear-gradient(135deg, rgba(var(--bs-primary-rgb),.08), rgba(111,66,193,.06)); }
.sap-module-icon { width:52px; height:52px; display:grid; place-items:center; flex:0 0 auto;
  border-radius:16px; background:var(--bs-primary); color:white; font-size:1.25rem; }
.sap-backend-notice { border-radius:14px; }
.sap-empty-state { padding:4rem 2rem; text-align:center; border:1px dashed var(--bs-border-color);
  border-radius:var(--sap-radius); background:var(--bs-tertiary-bg); }
.sap-empty-icon { width:58px; height:58px; display:grid; place-items:center; margin:0 auto 1rem;
  border-radius:50%; background:rgba(var(--bs-primary-rgb),.1); color:var(--bs-primary); font-size:1.3rem; }
.sap-kpi-grid { display:grid; grid-template-columns:repeat(4,minmax(150px,1fr)); gap:.85rem; }
.sap-kpi { display:flex; align-items:flex-start; gap:.75rem; padding:.9rem; border:1px solid var(--bs-border-color);
  border-radius:14px; background:var(--bs-body-bg); min-width:0; }
.sap-kpi-icon { display:grid; place-items:center; width:38px; height:38px; flex:0 0 auto;
  border-radius:11px; background:rgba(var(--bs-primary-rgb),.1); color:var(--bs-primary); }
.sap-kpi-value { font-size:1.15rem; font-weight:750; line-height:1.15; overflow-wrap:anywhere; }
.sap-kpi-label { font-size:.78rem; color:var(--bs-secondary-color); margin-top:.15rem; }
.sap-kpi-note { font-size:.7rem; color:var(--bs-secondary-color); }
.sap-status-pill { display:inline-flex; align-items:center; padding:.3rem .6rem; border-radius:999px;
  font-size:.75rem; font-weight:650; border:1px solid transparent; }
.sap-status-neutral { background:var(--bs-tertiary-bg); border-color:var(--bs-border-color); }
.sap-status-success { background:rgba(25,135,84,.1); color:var(--bs-success); border-color:rgba(25,135,84,.25); }
.sap-status-warning { background:rgba(255,193,7,.14); color:#8a6500; border-color:rgba(255,193,7,.35); }
.sap-status-danger { background:rgba(220,53,69,.1); color:var(--bs-danger); border-color:rgba(220,53,69,.25); }
.sap-code-preview { margin:0; padding:1rem; border-radius:12px; background:var(--bs-tertiary-bg);
  border:1px solid var(--bs-border-color); white-space:pre-wrap; overflow-wrap:anywhere; }
.sap-check-list { list-style:none; padding-left:0; display:grid; gap:.65rem; }
.sap-check-list li { display:flex; gap:.55rem; align-items:flex-start; }
.sap-check-list svg { color:var(--bs-success); margin-top:.2rem; }

@media (max-width: 1100px) {
  .sap-metric-grid { grid-template-columns: repeat(3, minmax(140px, 1fr)); }
  .sap-kpi-grid { grid-template-columns: repeat(2, minmax(145px, 1fr)); }
}
@media (max-width: 760px) {
  .sap-hero { grid-template-columns: 1fr; }
  .sap-metric-grid { grid-template-columns: repeat(2, minmax(130px, 1fr)); }
  .sap-kpi-grid { grid-template-columns: 1fr; }
}

/* --------------------------------------------------------------------------
   Branded shell and single-source vertical navigation
   -------------------------------------------------------------------------- */
.sap-app-brand {
  display:flex; align-items:center; gap:.78rem; min-width:0; padding:.15rem 0;
}
.sap-brand-mark {
  position:relative; display:grid; place-items:center; width:46px; height:46px;
  flex:0 0 46px; overflow:hidden; border-radius:15px; color:#fff;
  background:linear-gradient(145deg, #075985 0%, #0d6efd 58%, #6f42c1 100%);
  box-shadow:0 8px 18px rgba(13,110,253,.24);
}
.sap-brand-mark svg { width:31px; height:31px; }
.sap-brand-copy { display:flex; flex-direction:column; min-width:0; line-height:1.1; }
.sap-brand-title { color:var(--bs-body-color); font-size:1.08rem; font-weight:820; letter-spacing:-.015em; }
.sap-brand-tagline {
  margin-top:.22rem; color:var(--bs-secondary-color); font-size:.68rem;
  font-weight:560; letter-spacing:.015em; white-space:nowrap;
}
.sap-page-shell { min-width:0; }
.sap-main-footer { padding:1rem 1.5rem 1.4rem; }
.sap-app-sidebar { border-right:1px solid var(--bs-border-color); }
.sap-sidebar-section-label {
  margin:.15rem .25rem .45rem; color:var(--bs-secondary-color); font-size:.67rem;
  font-weight:800; letter-spacing:.095em; text-transform:uppercase;
}
.sap-sidebar-nav { display:grid; gap:.25rem; }
.sap-nav-button.btn {
  display:flex; align-items:center; justify-content:flex-start; gap:.65rem;
  width:100%; min-height:38px; padding:.52rem .68rem; border:1px solid transparent;
  border-radius:10px; background:transparent; color:var(--bs-body-color);
  font-size:.81rem; font-weight:620; text-align:left; box-shadow:none;
  transition:background-color .16s ease, border-color .16s ease, color .16s ease,
    transform .16s ease, box-shadow .16s ease;
}
.sap-nav-button.btn svg { width:16px; color:var(--bs-secondary-color); }
.sap-nav-button.btn:hover {
  border-color:rgba(var(--bs-primary-rgb),.18);
  background:rgba(var(--bs-primary-rgb),.075); color:var(--bs-primary);
  transform:translateX(2px);
}
.sap-nav-button.btn.active {
  border-color:transparent; color:#fff;
  background:linear-gradient(110deg, #075985 0%, #0d6efd 68%, #4f46e5 100%);
  box-shadow:0 7px 16px rgba(13,110,253,.22);
}
.sap-nav-button.btn.active svg { color:#fff; }
.sap-nav-group {
  border:1px solid transparent; border-radius:12px; overflow:hidden;
}
.sap-nav-group[open] {
  border-color:var(--bs-border-color); background:var(--bs-tertiary-bg);
}
.sap-nav-group > summary {
  display:flex; align-items:center; gap:.6rem; padding:.56rem .65rem; cursor:pointer;
  border-radius:10px; color:var(--bs-body-color); font-size:.78rem; font-weight:760;
  list-style:none; user-select:none;
}
.sap-nav-group > summary::-webkit-details-marker { display:none; }
.sap-nav-group > summary::after {
  content:'›'; margin-left:auto; color:var(--bs-secondary-color); font-size:1.05rem;
  transform:rotate(0deg); transition:transform .16s ease;
}
.sap-nav-group[open] > summary::after { transform:rotate(90deg); }
.sap-nav-group > summary:hover { background:rgba(var(--bs-primary-rgb),.06); color:var(--bs-primary); }
.sap-nav-group-items { display:grid; gap:.2rem; padding:0 .35rem .4rem; }
.sap-nav-group-items .sap-nav-button.btn { padding-left:1rem; }
.sap-sidebar-utility { display:grid; gap:.45rem; margin-top:.35rem; }
.sap-sidebar-mode {
  display:flex; align-items:center; justify-content:space-between; gap:.5rem;
  margin-top:.25rem; padding:.62rem .7rem; border:1px solid var(--bs-border-color);
  border-radius:11px; background:var(--bs-tertiary-bg); font-size:.7rem;
}
.sap-sidebar-mode strong { font-size:.72rem; }
.sap-sidebar-session { color:var(--bs-secondary-color); font-size:.68rem; }

/* Full-width command bar: branding, current context, and utilities only. */
.navbar.navbar-static-top {
  min-height:68px; padding:0; color:#f8fafc;
  background:linear-gradient(100deg,#111827 0%,#172033 62%,#202938 100%);
  border-bottom:1px solid rgba(255,255,255,.08);
  box-shadow:0 4px 18px rgba(15,23,42,.16);
}
.navbar.navbar-static-top > .container-fluid { width:100%; padding:.5rem .8rem; }
.sap-shell-header {
  display:grid; grid-template-columns:minmax(250px,1.15fr) minmax(260px,1fr) auto;
  align-items:center; gap:1rem; width:100%; min-width:0;
}
.sap-shell-header .sap-brand-title { color:#fff; }
.sap-shell-header .sap-brand-tagline { color:rgba(255,255,255,.68); }
.sap-shell-header .sap-brand-mark { width:42px; height:42px; flex-basis:42px; border-radius:13px; box-shadow:none; }
.sap-topbar-context { display:flex; align-items:center; gap:.45rem; min-width:0; justify-content:center; }
.sap-topbar-chip {
  display:inline-flex; align-items:center; gap:.4rem; min-width:0; max-width:250px;
  padding:.42rem .62rem; border:1px solid rgba(255,255,255,.14); border-radius:999px;
  color:rgba(255,255,255,.88); background:rgba(255,255,255,.07);
  font-size:.72rem; line-height:1; white-space:nowrap;
}
.sap-topbar-chip strong { overflow:hidden; text-overflow:ellipsis; }
.sap-topbar-chip svg { color:#93c5fd; flex:0 0 auto; }
.sap-topbar-chip-muted { color:rgba(255,255,255,.64); }
.sap-topbar-actions { display:flex; align-items:center; justify-content:flex-end; gap:.42rem; }
.sap-topbar-action.btn {
  display:inline-flex; align-items:center; gap:.4rem; min-height:36px; padding:.42rem .62rem;
  border:1px solid rgba(255,255,255,.16); border-radius:10px; color:#fff;
  background:rgba(255,255,255,.07); font-size:.72rem; font-weight:700; box-shadow:none;
}
.sap-topbar-action.btn:hover { background:rgba(255,255,255,.14); color:#fff; }
.sap-ai-dot { width:8px; height:8px; border-radius:50%; display:inline-block; background:#f59e0b; }
.sap-ai-dot-ready { background:#22c55e; }
.sap-ai-dot-error { background:#ef4444; }

@media (max-width: 1050px) {
  .sap-shell-header { grid-template-columns:minmax(220px,1fr) auto; }
  .sap-topbar-context { display:none; }
}

@media (max-width: 760px) {
  .sap-brand-mark { width:40px; height:40px; flex-basis:40px; border-radius:13px; }
  .sap-brand-title { font-size:.98rem; }
  .sap-brand-tagline { display:none; }
  .sap-page-shell { padding:1rem !important; }
  .sap-shell-header { grid-template-columns:minmax(0,1fr) auto; gap:.5rem; }
  .sap-shell-header .sap-brand-tagline { display:none; }
  .sap-topbar-action .sap-action-label { display:none; }
}


/* Compact, action-oriented home page. */
.sap-home-hero {
  display:grid; grid-template-columns:minmax(0,1.65fr) minmax(250px,.65fr);
  gap:1.25rem; align-items:center; padding:clamp(1.4rem,3vw,2.35rem);
  border-radius:var(--sap-radius); color:#fff;
  background:linear-gradient(125deg,#075985 0%,#0d6efd 58%,#4f46e5 100%);
  box-shadow:var(--sap-shadow);
}
.sap-home-eyebrow { display:block; margin-bottom:.45rem; font-size:.72rem; font-weight:800;
  letter-spacing:.11em; text-transform:uppercase; opacity:.78; }
.sap-home-copy h1 { max-width:760px; margin:0; font-size:clamp(1.8rem,4vw,3.05rem);
  line-height:1.04; font-weight:780; }
.sap-home-lead { max-width:720px; margin:.8rem 0 0; font-size:clamp(.98rem,1.7vw,1.16rem); opacity:.86; }
.sap-home-route { display:flex; gap:.85rem; align-items:flex-start; padding:1rem;
  border:1px solid rgba(255,255,255,.26); border-radius:15px;
  background:rgba(255,255,255,.11); backdrop-filter:blur(7px); }
.sap-home-route strong { display:block; margin:.2rem 0; }
.sap-home-route-icon { display:grid; place-items:center; width:42px; height:42px; flex:0 0 auto;
  border-radius:12px; background:rgba(255,255,255,.17); }
.sap-home-action-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:1rem; }
.sap-home-action-card { display:grid; grid-template-columns:46px minmax(0,1fr); gap:.85rem;
  align-items:start; padding:1.05rem; border:1px solid var(--bs-border-color);
  border-radius:16px; background:var(--bs-body-bg); box-shadow:0 8px 22px rgba(15,23,42,.055); }
.sap-home-action-card h5 { margin:0 0 .25rem; font-size:1rem; }
.sap-home-action-card p { min-height:2.6em; margin:0 0 .7rem; color:var(--bs-secondary-color); font-size:.82rem; }
.sap-home-action-icon { display:grid; place-items:center; width:44px; height:44px;
  border-radius:13px; color:var(--bs-primary); background:rgba(var(--bs-primary-rgb),.10); }
.sap-home-active, .sap-home-note { display:flex; align-items:center; justify-content:space-between;
  gap:1rem; padding:1rem 1.1rem; border:1px solid var(--bs-border-color);
  border-radius:15px; background:var(--bs-body-bg); box-shadow:0 8px 22px rgba(15,23,42,.05); }
.sap-home-active-copy { display:grid; gap:.15rem; min-width:0; }
.sap-home-active-label { color:var(--bs-success); font-size:.72rem; font-weight:800;
  letter-spacing:.06em; text-transform:uppercase; }
.sap-home-active-actions { display:flex; gap:.45rem; flex-wrap:wrap; justify-content:flex-end; }
.sap-home-note > div:first-child { display:flex; gap:.75rem; align-items:flex-start; }
.sap-home-note > div:first-child > svg { margin-top:.15rem; color:var(--bs-primary); }
.sap-health-metric { display:grid; gap:.15rem; padding:.9rem; border:1px solid var(--bs-border-color);
  border-radius:13px; background:var(--bs-tertiary-bg); }
.sap-health-metric strong { font-size:1.45rem; line-height:1.1; }

@media (max-width: 900px) {
  .sap-home-hero { grid-template-columns:1fr; }
  .sap-home-action-grid { grid-template-columns:1fr; }
  .sap-home-action-card p { min-height:0; }
}
@media (max-width: 620px) {
  .sap-home-active, .sap-home-note { align-items:flex-start; flex-direction:column; }
  .sap-home-active-actions { justify-content:flex-start; }
}

")

# =============================================================================
# SMALL UI HELPERS
# =============================================================================

presentation_export_ui <- function() {
  tagList(
    div(
      class = "mb-4",
      h2(icon("download"), "Export Center"),
      p("Take the cleaned dataset, metadata, and reproducible analysis code with you.",
        class = "text-muted")
    ),
    uiOutput("export_status"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "sap-card",
        card_header(icon("file-csv"), " Cleaned dataset"),
        card_body(
          p("Export the active working dataset after cleaning and transformations."),
          div(class = "d-grid gap-2",
            downloadButton("export_data_csv", "CSV", class = "btn btn-success"),
            downloadButton("export_data_xlsx", "Excel (.xlsx)", class = "btn btn-outline-success"),
            downloadButton("export_data_sav", "SPSS (.sav)", class = "btn btn-outline-primary"),
            downloadButton("export_data_dta", "Stata (.dta)", class = "btn btn-outline-primary"),
            if (sap_optional_available("arrow")) {
              downloadButton("export_data_parquet", "Parquet", class = "btn btn-outline-secondary")
            } else {
              span(class = "small text-muted", "Parquet export becomes available when the optional arrow package is installed.")
            },
            downloadButton("export_data_json", "JSON", class = "btn btn-outline-secondary")
          )
        )
      ),
      card(
        class = "sap-card",
        card_header(icon("book"), " Variable dictionary"),
        card_body(
          p("Export detected variable types, missingness, and unique-value metadata."),
          downloadButton("export_dictionary_csv", "Download Dictionary",
                         class = "btn btn-outline-success w-100")
        )
      ),
      card(
        class = "sap-card",
        card_header(icon("code"), " Reproducible R code"),
        card_body(
          p("Export generated analysis and report code as a reusable R script."),
          downloadButton("export_r_script", "Download R Script",
                         class = "btn btn-outline-primary w-100")
        )
      )
    ),
    card(
      class = "sap-card mt-4",
      card_header(icon("file-lines"), " Analysis report"),
      card_body(
        p("Use Report Builder to arrange tables, charts, and narrative sections before export."),
        actionButton("export_go_report", "Open Report Builder", icon = icon("arrow-right"),
                     class = "btn btn-primary")
      )
    )
  )
}

# =============================================================================
# USER INTERFACE
# =============================================================================

sap_brand_ui <- function() {
  div(
    class = "sap-app-brand",
    div(
      class = "sap-brand-mark",
      role = "img",
      `aria-label` = "Survey Analytics logo",
      tags$svg(
        viewBox = "0 0 48 48", `aria-hidden` = "true",
        tags$rect(x = "7", y = "27", width = "6", height = "12", rx = "2", fill = "currentColor", opacity = ".78"),
        tags$rect(x = "18", y = "20", width = "6", height = "19", rx = "2", fill = "currentColor", opacity = ".9"),
        tags$rect(x = "29", y = "12", width = "6", height = "27", rx = "2", fill = "currentColor"),
        tags$path(
          d = "M8 23.5 L19 16.5 L27 19 L39 8.5",
          fill = "none", stroke = "currentColor", `stroke-width` = "3.1",
          `stroke-linecap` = "round", `stroke-linejoin` = "round"
        ),
        tags$circle(cx = "39", cy = "8.5", r = "3.2", fill = "currentColor")
      )
    ),
    div(
      class = "sap-brand-copy",
      span(SAP_BRAND_TITLE, class = "sap-brand-title"),
      span(SAP_BRAND_TAGLINE, class = "sap-brand-tagline")
    )
  )
}

sap_shell_header_ui <- function() {
  div(
    class = "sap-shell-header",
    sap_brand_ui(),
    uiOutput("topbar_context"),
    div(
      class = "sap-topbar-actions",
      div(
        class = "sap-language-control",
        actionButton(
          "global_language_toggle", NULL, icon = icon("language"),
          class = "sap-topbar-action sap-language-icon-button",
          title = "Language", `aria-label` = "Language", `aria-haspopup` = "true",
          `aria-expanded` = "false"
        ),
        tags$div(
          id = "sap-language-menu", class = "sap-language-menu", hidden = NA,
          role = "radiogroup", `aria-label` = "Language",
          lapply(seq_len(nrow(SAP_LANGUAGE_META)), function(i) {
            tags$button(
              type = "button", class = "sap-language-choice", role = "radio",
              `aria-checked` = if (identical(SAP_LANGUAGE_META$code[[i]], "en")) "true" else "false",
              `data-sap-language` = SAP_LANGUAGE_META$code[[i]],
              tags$span(SAP_LANGUAGE_META$short[[i]], class = "sap-language-code"),
              tags$span(SAP_LANGUAGE_META$native_name[[i]], class = "sap-language-name")
            )
          })
        )
      ),
      auth_account_menu_ui("auth"),
      actionButton(
        "topbar_ai", tagList(span(class = "sap-ai-dot"), span("AI", class = "sap-action-label")),
        icon = icon("wand-magic-sparkles"), class = "sap-topbar-action",
        title = "Open AI settings"
      ),
      actionButton(
        "topbar_help", span("Help", class = "sap-action-label"),
        icon = icon("circle-question"), class = "sap-topbar-action",
        title = "Open workspace help"
      )
    )
  )
}

sap_nav_button <- function(input_id, label, icon_name, active = FALSE) {
  actionButton(
    input_id, span(label, `data-sap-nav-label` = "true"),
    icon = icon(icon_name),
    class = paste("sap-nav-button", if (active) "active" else ""),
    `aria-label` = paste("Open", label)
  )
}

sap_nav_group <- function(label, icon_name, ..., open = FALSE) {
  attrs <- list(class = "sap-nav-group")
  if (isTRUE(open)) attrs$open <- NA
  do.call(
    tags$details,
    c(
      attrs,
      list(
        tags$summary(icon(icon_name), span(label)),
        div(class = "sap-nav-group-items", ...)
      )
    )
  )
}

ui <- page_sidebar(
  title = sap_shell_header_ui(),
  theme = create_app_theme(DEFAULT_SETTINGS$theme_mode),
  window_title = paste("Survey Analytics Platform —", SAP_PRESENTATION_VERSION),
  fillable = FALSE,

  sidebar = sidebar(
    title = div(class = "sidebar-title", icon("compass"), "Workspace & navigation"),
    id = "workspace_sidebar",
    class = "sap-app-sidebar",
    width = "320px",
    open = "desktop",
    resizable = TRUE,
    uiOutput("workspace_summary"),
    hr(class = "my-2"),
    div(
      class = "sap-sidebar-section-label",
      "Navigate"
    ),
    div(
      class = "sap-sidebar-nav",
      sap_nav_button("nav_home", "Presentation Home", "house", active = TRUE),
      sap_nav_group(
        "Guided & MEL", "bullseye",
        sap_nav_button("nav_guided", "Guided Workspace", "route"),
        sap_nav_button("nav_monitoring", "MEL & Project Performance", "bullseye"),
        sap_nav_button("nav_literacy", "Data Literacy", "book-open-reader"),
        sap_nav_button("nav_integrations", "Field Integrations", "satellite-dish"),
        sap_nav_button("nav_program_reports", "Programme Reports", "file-circle-check"),
        open = TRUE
      ),
      sap_nav_group(
        "Advanced BI Studio", "layer-group",
        sap_nav_button("nav_studio_home", "Studio Overview", "compass"),
        sap_nav_button("nav_connect", "Connect & Catalog", "database"),
        sap_nav_button("nav_prepare", "Prepare & Transform", "wand-magic-sparkles"),
        sap_nav_button("nav_model", "Semantic Model", "diagram-project"),
        sap_nav_button("nav_visual_builder", "Visual Builder", "chart-column"),
        sap_nav_button("nav_dashboard_studio", "Dashboard Studio", "gauge-high"),
        sap_nav_button("nav_publish", "Publish Center", "paper-plane")
      ),
      sap_nav_group(
        "Project", "folder-open",
        sap_nav_button("nav_project_manager", "Project Manager", "folder"),
        sap_nav_button("nav_enterprise", "Enterprise Center", "building-shield"),
        sap_nav_button("nav_history", "Analysis History", "clock-rotate-left")
      ),
      sap_nav_group(
        "Data", "database",
        sap_nav_button("nav_import", "Import Data", "upload"),
        sap_nav_button("nav_data_lab", "Data Lab", "flask"),
        sap_nav_button("nav_explorer", "Data Explorer", "magnifying-glass"),
        sap_nav_button("nav_cleaning", "Data Cleaning", "broom"),
        sap_nav_button("nav_variables", "Variable Manager", "table-list"),
        open = TRUE
      ),
      sap_nav_group(
        "Survey Setup", "clipboard-list",
        sap_nav_button("nav_questionnaire", "Questionnaire", "clipboard-list"),
        sap_nav_button("nav_design", "Survey Design", "scale-balanced"),
        sap_nav_button("nav_scales", "Scale Analysis", "list-ol")
      ),
      sap_nav_group(
        "Quality", "shield-heart",
        sap_nav_button("nav_missing", "Missing & Nonresponse", "user-clock"),
        sap_nav_button("nav_privacy", "Privacy & Governance", "shield-halved")
      ),
      sap_nav_group(
        "Analysis", "chart-bar",
        sap_nav_button("nav_dashboard", "Overview Dashboard", "gauge"),
        sap_nav_button("nav_descriptive", "Descriptive Statistics", "chart-line"),
        sap_nav_button("nav_tables", "Table Builder", "table-cells"),
        sap_nav_button("nav_statistics", "Statistical Tests", "calculator"),
        sap_nav_button("nav_advanced", "Advanced Models", "diagram-project"),
        sap_nav_button("nav_visualization", "Visualization", "palette"),
        open = TRUE
      ),
      sap_nav_group(
        "Specialist", "microscope",
        sap_nav_button("nav_health", "Personal Health Tracker", "heart-pulse"),
        sap_nav_button("nav_power", "Sample Size & Power", "bullseye"),
        sap_nav_button("nav_text", "Text Analysis", "comments"),
        sap_nav_button("nav_geospatial", "Geospatial", "map-location-dot"),
        sap_nav_button("nav_longitudinal", "Trends & Panels", "timeline"),
        sap_nav_button("nav_ml", "Machine Learning", "robot")
      ),
      sap_nav_group(
        "Report", "file-lines",
        sap_nav_button("nav_report_builder", "Report Builder", "file-pen"),
        sap_nav_button("nav_export", "Export Center", "download")
      ),
      sap_nav_button("nav_settings", "Settings", "gear"),
      sap_nav_group(
        "Administration", "gears",
        uiOutput("admin_control_nav_ui"),
        sap_nav_button("nav_code", "R Code", "code"),
        sap_nav_button("nav_diagnostics", "System Health", "stethoscope"),
        sap_nav_button("nav_operations", "Operations Center", "tower-broadcast")
      )
    ),
    hr(class = "my-2"),
    div(
      class = "sap-sidebar-utility",
      uiOutput("sidebar_download_ui"),
      actionButton(
        "btn_clear_session", "Clear Session", icon = icon("rotate"),
        class = "btn btn-outline-secondary btn-sm w-100",
        `aria-label` = "Clear the current in-memory session"
      ),
      div(
        class = "sap-sidebar-mode",
        div(
          strong(if (SAP_POSTGRES_ENABLED) "Connected workspace" else if (SAP_DEMO_MODE) "Demo workspace" else "Secure standalone workspace"),
          div(if (SAP_POSTGRES_ENABLED) "Shared PostgreSQL service" else if (SAP_DEMO_MODE) "Private demonstration session" else "Account-protected app instance",
              class = "sap-sidebar-session")
        ),
        span(class = "badge text-bg-primary sap-mode-badge",
             if (SAP_POSTGRES_ENABLED) "LIVE" else if (SAP_DEMO_MODE) "DEMO" else "SECURE")
      )
    )
  ),

  tags$a("Skip to main content", href = "#main_nav", class = "sap-skip-link"),
  useShinyjs(),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "survey-analytics-mark.svg"),
    tags$style(.sap_presentation_css),
    tags$link(rel = "stylesheet", type = "text/css", href = "nexus-studio.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "sap-analytics-experience.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "sap-professional-suite.css"),
    tags$script(src = "sap-analytics-experience.js"),
    tags$script(src = "sap-i18n-catalog.js"),
    tags$script(src = "sap-i18n.js"),
    tags$script(src = "sap-professional-suite.js")
  ),

  navset_hidden(
    id = "main_nav",
    selected = "home",
    nav_panel_hidden("home",
      div(class = "sap-page-shell p-4", mod_presentation_home_ui("presentation_home"))
    ),
    nav_panel_hidden("guided",
      div(class = "sap-page-shell p-4", mod_guided_workflow_ui("guided_workflow"))
    ),
    nav_panel_hidden("monitoring",
      div(class = "sap-page-shell p-4", mod_monitoring_evaluation_ui("monitoring_evaluation"))
    ),
    nav_panel_hidden("literacy",
      div(class = "sap-page-shell p-4", mod_data_literacy_ui("data_literacy"))
    ),
    nav_panel_hidden("integrations",
      div(class = "sap-page-shell p-4", mod_field_integrations_ui("field_integrations"))
    ),
    nav_panel_hidden("program_reports",
      div(class = "sap-page-shell p-4", mod_program_reports_ui("program_reports"))
    ),
    nav_panel_hidden("operations",
      div(class = "sap-page-shell p-4", mod_operations_center_ui("operations_center"))
    ),
    nav_panel_hidden("studio_home",
      div(class = "sap-page-shell p-4", mod_studio_home_ui("studio_home"))
    ),
    nav_panel_hidden("connect",
      div(class = "sap-page-shell p-4", mod_data_connections_ui("data_connections"))
    ),
    nav_panel_hidden("prepare",
      div(class = "sap-page-shell p-4", mod_transform_pipeline_ui("transform_pipeline"))
    ),
    nav_panel_hidden("model",
      div(class = "sap-page-shell p-4", mod_semantic_model_ui("semantic_model"))
    ),
    nav_panel_hidden("visual_builder",
      div(class = "sap-page-shell p-4", mod_visual_builder_ui("visual_builder"))
    ),
    nav_panel_hidden("dashboard_studio",
      div(class = "sap-page-shell p-4", mod_dashboard_studio_ui("dashboard_studio"))
    ),
    nav_panel_hidden("publish",
      div(class = "sap-page-shell p-4", mod_publish_center_ui("publish_center", connected = SAP_POSTGRES_ENABLED))
    ),
    nav_panel_hidden("project_manager",
      div(class = "sap-page-shell p-4", mod_project_manager_ui("project_manager", connected = SAP_POSTGRES_ENABLED))
    ),
    nav_panel_hidden("enterprise",
      div(class = "sap-page-shell p-4", mod_enterprise_center_ui("enterprise_center"))
    ),
    nav_panel_hidden("history",
      div(class = "sap-page-shell p-4", mod_analysis_history_ui("analysis_history"))
    ),
    nav_panel_hidden("import",
      div(class = "sap-page-shell p-4", mod_import_wizard_ui("import_wizard"))
    ),
    nav_panel_hidden("data_lab",
      div(class = "sap-page-shell p-4", mod_data_lab_ui("data_lab"))
    ),
    nav_panel_hidden("explorer",
      div(class = "sap-page-shell p-4", mod_data_explorer_ui("data_explorer"))
    ),
    nav_panel_hidden("cleaning",
      div(class = "sap-page-shell p-4", mod_data_cleaning_ui("data_cleaning"))
    ),
    nav_panel_hidden("variables",
      div(class = "sap-page-shell p-4", mod_variable_manager_ui("variable_manager"))
    ),
    nav_panel_hidden("questionnaire",
      div(class = "sap-page-shell p-4", mod_questionnaire_manager_ui("questionnaire_manager"))
    ),
    nav_panel_hidden("design",
      div(class = "sap-page-shell p-4", mod_survey_design_ui("survey_design"))
    ),
    nav_panel_hidden("scales",
      div(class = "sap-page-shell p-4", mod_scale_analysis_ui("scale_analysis"))
    ),
    nav_panel_hidden("missing",
      div(class = "sap-page-shell p-4", mod_missing_nonresponse_ui("missing_nonresponse"))
    ),
    nav_panel_hidden("privacy",
      div(class = "sap-page-shell p-4", mod_privacy_governance_ui("privacy_governance"))
    ),
    nav_panel_hidden("dashboard",
      div(class = "sap-page-shell p-4", mod_dashboard_ui("dashboard"))
    ),
    nav_panel_hidden("descriptive",
      div(class = "sap-page-shell p-4", mod_descriptive_stats_ui("descriptive_stats"))
    ),
    nav_panel_hidden("tables",
      div(class = "sap-page-shell p-4", mod_table_builder_ui("table_builder"))
    ),
    nav_panel_hidden("statistics",
      div(class = "sap-page-shell p-4", mod_statistical_tests_ui("statistical_tests"))
    ),
    nav_panel_hidden("advanced",
      div(class = "sap-page-shell p-4", mod_advanced_models_ui("advanced_models"))
    ),
    nav_panel_hidden("visualization",
      div(class = "sap-page-shell p-4", mod_visualization_ui("visualization"))
    ),
    nav_panel_hidden("health",
      div(class = "sap-page-shell p-4", mod_personal_health_ui("personal_health"))
    ),
    nav_panel_hidden("power",
      div(class = "sap-page-shell p-4", mod_sample_size_power_ui("sample_size_power"))
    ),
    nav_panel_hidden("text",
      div(class = "sap-page-shell p-4", mod_text_analysis_ui("text_analysis"))
    ),
    nav_panel_hidden("geospatial",
      div(class = "sap-page-shell p-4", mod_geospatial_analysis_ui("geospatial_analysis"))
    ),
    nav_panel_hidden("longitudinal",
      div(class = "sap-page-shell p-4", mod_longitudinal_analysis_ui("longitudinal_analysis"))
    ),
    nav_panel_hidden("ml",
      div(class = "sap-page-shell p-4", mod_machine_learning_ui("machine_learning"))
    ),
    nav_panel_hidden("report_builder",
      div(class = "sap-page-shell p-4", mod_report_generator_ui("report_generator"))
    ),
    nav_panel_hidden("export",
      div(class = "sap-page-shell p-4", presentation_export_ui())
    ),
    nav_panel_hidden("settings",
      div(class = "sap-page-shell p-4", mod_settings_ui("settings"))
    ),
    nav_panel_hidden("admin_control",
      div(class = "sap-page-shell p-4", mod_admin_control_center_ui("admin_control"))
    ),
    nav_panel_hidden("code",
      div(class = "sap-page-shell p-4", mod_code_generator_ui("code_generator"))
    ),
    nav_panel_hidden("diagnostics",
      div(class = "sap-page-shell p-4", mod_system_diagnostics_ui("system_diagnostics"))
    )
  ),

  div(
    class = "sap-main-footer text-center small text-muted border-top",
    "Survey Analytics Platform — ", SAP_PRESENTATION_VERSION,
    br(),
    sprintf("© %s | Governed survey intelligence", format(Sys.Date(), "%Y"))
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  app_state <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    data_imported = FALSE,
    variable_types = NULL,
    variable_dictionary = data.frame(),
    settings = DEFAULT_SETTINGS,
    import_log = data.frame(),
    current_project = NULL,
    current_project_name = "Presentation Workspace",
    current_user = NULL,
    current_user_profile = list(username = "local", role = "owner"),
    project_path = NULL,
    project_saved = FALSE,
    dataset_name = NULL,
    quality_score = NA_real_,
    transformation_count = 0L,
    last_action = "Session started",
    trained_models = list(),
    last_model = NULL,
    model_predictions = NULL,
    analyses = list(),
    last_analysis = NULL,
    survey_design = list(enabled = FALSE, weight = "", strata = "", cluster = "", fpc = "", domain = "", trim_pct = 0, notes = ""),
    questionnaire = data.frame(),
    questionnaire_versions = list(),
    routing_rules = data.frame(),
    scale_definitions = list(),
    nonresponse_plan = list(),
    table_builder_spec = list(),
    advanced_model_queue = list(),
    text_codebook = data.frame(),
    text_coding_results = data.frame(),
    geospatial_spec = list(),
    longitudinal_spec = list(),
    privacy_register = data.frame(),
    governance_policy = list(small_cell = 5, min_base = 30, redact_ai = TRUE, require_approval = TRUE, audit_exports = TRUE, retention = "Session only"),
    governance_export_approved = FALSE,
    governance_export_approval = NULL,
    report_frontend_blocks = list(),
    frontend_activity = list(),
    active_filter_label = "All respondents",
    ai_connection_status = list(state = "unknown", message = "Not checked"),
    health_log = data.frame(),
    current_route = "home",
    resume_route = "home",
    resume_last_analysis = NULL,
    workspace_restored = FALSE,
    ui_language = "en",
    user_mode = "MEL Officer",
    me_indicators = sap_me_empty_indicators(),
    me_results = sap_me_empty_results(),
    me_activities = sap_me_register_empty("activities"),
    me_risks = sap_me_register_empty("risks"),
    me_grievances = sap_me_register_empty("grievances"),
    me_actions = sap_me_register_empty("actions"),
    field_visits = sap_field_visits_empty(),
    bi = sap_bi_default_state(),
    # Multi-sheet workbook cleaning workspace (v5.1). Raw sheets are immutable
    # session copies; workbook_sheets holds the current cleaned version of each.
    workbook_name = NULL,
    workbook_raw_sheets = list(),
    workbook_sheets = list(),
    workbook_sheet_types = list(),
    workbook_cleaning_histories = list(),
    workbook_history_indices = list(),
    guided_cleaning_steps = list(),
    saved_cleaning_recipes = list(),
    active_sheet = "",
    workbook_merge_history = list()
  )

  # ---------------------------------------------------------------------------
  # Persistent connected mode is the default. If PostgreSQL is required and
  # unavailable, startup fails rather than silently entering an unsaved mode.
  # Explicit SAP_DEMO_MODE=true remains available for presentation-only use.
  # ---------------------------------------------------------------------------
  app_pool <- NULL
  if (SAP_POSTGRES_ENABLED) {
    app_pool <- tryCatch(
      db_connect(),
      error = function(e) {
        msg <- paste("PostgreSQL connection failed:", conditionMessage(e))
        if (SAP_DB_REQUIRED) stop(
          paste0(msg, "\nPersistent account mode is enabled. Configure PostgreSQL first (copy .Renviron.example to .Renviron and set SAP_PG_PASSWORD), then restart R."),
          call. = FALSE
        )
        message("[SAP] ", msg)
        NULL
      }
    )
    if (!is.null(app_pool)) {
      init_ok <- tryCatch({
        health <- db_init(app_pool)
        message(sprintf("[SAP DB] Connected to %s as %s; schema %s; PostgreSQL %s",
                        health$database_name[[1]], health$database_user[[1]],
                        health$active_schema[[1]], health$server_version[[1]]))
        TRUE
      }, error = function(e) {
        msg <- paste("Database migration/initialisation failed:", conditionMessage(e))
        if (SAP_DB_REQUIRED) {
          db_disconnect(app_pool)
          stop(msg, call. = FALSE)
        }
        message("[SAP] ", msg)
        FALSE
      })
      if (!isTRUE(init_ok)) {
        db_disconnect(app_pool)
        app_pool <- NULL
      } else {
        onStop(function() db_disconnect(app_pool))
      }
    }
  }

  services <- create_services(app_state, pool = app_pool)
  services$log$info(
    if (SAP_POSTGRES_ENABLED) "Services initialized in connected PostgreSQL mode"
    else if (SAP_DEMO_MODE) "Services initialized in demonstration mode"
    else "Services initialized with secure standalone account authentication",
    "app"
  )

  # Persistent accounts are the normal user identity. Explicit Demo Mode has
  # no account creation: it is a disposable Guest workspace by design.
  auth_api <- mod_auth_server(
    "auth", app_state, services = services,
    allow_signup = SAP_ALLOW_SIGNUP && !SAP_DEMO_MODE,
    auth_required = SAP_AUTH_REQUIRED,
    frontend_only = SAP_DEMO_MODE,
    allow_guest = SAP_ALLOW_GUEST
  )

  # Keep an explicit, non-secret identity context for row-security evaluation
  # and UI permission decisions. Local development is the workspace owner;
  # connected identities use their durable account profile.
  observe({
    services$auth$version()
    user <- tryCatch(services$auth$current_user(), error = function(e) NULL)
    if (is.null(user)) {
      app_state$current_user_profile <- list(
        username = if (SAP_DEMO_MODE) "local" else "anonymous",
        role = if (SAP_DEMO_MODE) "owner" else "viewer"
      )
    } else {
      project_role <- if (!is.null(app_pool) && !is.null(app_state$current_project)) {
        tryCatch(db_project_permission(app_pool, app_state$current_project, user$id[[1]]), error = function(e) NULL)
      } else NULL
      app_state$current_user_profile <- list(
        username = as.character(user$username[[1]] %||% "user"),
        role = as.character(if (identical(as.character(user$role[[1]]), "admin")) "admin" else project_role %||% user$role[[1]] %||% "user")
      )
    }
  })

  # Restore every persisted model table plus the latest semantic model,
  # dashboard and row-security documents when a connected project opens.
  .sap_studio_loaded_key <- reactiveVal(NULL)
  observe({
    if (is.null(app_pool)) return(invisible(NULL))
    uid <- app_state$current_user
    pid <- app_state$current_project
    if (is.null(uid) || is.null(pid)) {
      .sap_studio_loaded_key(NULL)
      return(invisible(NULL))
    }
    key <- paste(uid, pid, sep = ":")
    if (identical(.sap_studio_loaded_key(), key)) return(invisible(NULL))

    # Workspace checkpoints may already contain a model specification. Preserve
    # it as a fallback while the durable database remains the source of truth.
    checkpoint_manifest <- tryCatch(sap_bi_model_manifest(app_state), error = function(e) NULL)
    loaded <- tryCatch(db_load_model_tables(app_pool, pid, uid), error = function(e) {
      services$log$warn(paste("Studio table restore failed:", conditionMessage(e)), "studio")
      list()
    })
    app_state$bi <- sap_bi_default_state()
    if (length(loaded)) {
      for (item in loaded) {
        sap_bi_add_table(
          app_state, item$name, item$data,
          source_type = item$source_type,
          source_config = item$source_config,
          set_active = is.null(app_state$bi$active_table), replace = FALSE
        )
      }
    }
    semantic <- tryCatch(db_get_latest_studio_document(
      app_pool, pid, "semantic_model", user_id = uid
    ), error = function(e) NULL)
    if (!is.null(semantic)) try(sap_bi_restore_manifest(
      app_state, semantic$specification_parsed[[1]]
    ), silent = TRUE)
    if (is.null(semantic) && is.list(checkpoint_manifest)) {
      try(sap_bi_restore_manifest(app_state, checkpoint_manifest), silent = TRUE)
    }
    dashboard_doc <- tryCatch(db_get_latest_studio_document(
      app_pool, pid, "dashboard", user_id = uid
    ), error = function(e) NULL)
    if (!is.null(dashboard_doc)) {
      model <- app_state$bi
      model$dashboard <- dashboard_doc$specification_parsed[[1]]
      app_state$bi <- model
    }
    security <- tryCatch(db_list_row_security_rules(app_pool, pid, uid), error = function(e) data.frame())
    if (nrow(security)) {
      model <- app_state$bi
      model$security_rules <- lapply(seq_len(nrow(security)), function(i) list(
        id = paste0("rls_db_", security$id[[i]]), db_id = security$id[[i]],
        table = security$table_name[[i]], field = security$field_name[[i]],
        values = security$allowed_values_parsed[[i]],
        subject_type = as.character(security$subject_type[[i]]),
        subject_value = as.character(security$subject_value[[i]] %||% ""),
        enabled = isTRUE(as.integer(security$enabled[[i]]) == 1L),
        created_at = as.character(security$created_at[[i]])
      ))
      app_state$bi <- model
    }
    for (table_name in names(app_state$bi$tables)) {
      if (length(app_state$bi$pipelines[[table_name]] %||% list())) {
        try(sap_bi_apply_pipeline(app_state, table_name), silent = TRUE)
      }
    }
    if (!is.null(app_state$bi$active_table)) sap_bi_sync_active(app_state)
    .sap_studio_loaded_key(key)
  })

  # The Control Center navigation is itself permission-gated. The server module
  # repeats the same check, so hiding this button is convenience rather than the
  # security boundary.
  output$admin_control_nav_ui <- renderUI({
    shiny::invalidateLater(15000, session)
    services$auth$version()
    if (!SAP_POSTGRES_ENABLED || !isTRUE(tryCatch(services$auth$is_admin(), error = function(e) FALSE))) return(NULL)
    sap_nav_button("nav_admin_control", "Control Center", "users-gear")
  })

  mod_admin_control_center_server(
    "admin_control", app_state, services = services, pool = app_pool,
    demo_mode = !SAP_POSTGRES_ENABLED
  )

  mod_guided_workflow_server("guided_workflow", app_state, services = services)
  mod_monitoring_evaluation_server("monitoring_evaluation", app_state, services = services, pool = app_pool)
  mod_data_literacy_server("data_literacy", app_state, services = services)
  mod_field_integrations_server("field_integrations", app_state, services = services)
  mod_program_reports_server("program_reports", app_state, services = services)
  mod_operations_center_server("operations_center", app_state, services = services)

  # Language policy:
  #   * English is the authoritative default for every fresh/Guest session.
  #   * A signed-in user's explicit preference may override English after auth.
  #   * Browser localStorage is never authoritative, avoiding stale-language races.
  observeEvent(input$global_language, {
    lang <- sap_normalize_language(input$global_language %||% "en")
    app_state$ui_language <- lang
    # This input is emitted only by an explicit user language-menu selection.
    try(services$settings$set("ui_language", lang, persist = !is.null(app_state$current_user)), silent = TRUE)
    session$sendCustomMessage("sap-i18n-set-language", list(language = lang))
  }, ignoreInit = TRUE)

  .sap_language_user_marker <- reactiveVal(NULL)
  observe({
    uid <- app_state$current_user
    marker <- if (is.null(uid) || !length(uid) || is.na(uid[[1]])) "guest" else paste0("user:", uid[[1]])
    if (identical(.sap_language_user_marker(), marker)) return(invisible(NULL))
    .sap_language_user_marker(marker)

    lang <- if (identical(marker, "guest")) {
      "en"
    } else {
      sap_normalize_language(tryCatch(services$settings$get("ui_language", "en"), error = function(e) "en"))
    }
    app_state$ui_language <- lang
    session$sendCustomMessage("sap-i18n-set-language", list(language = lang))
  })

  # Synchronize the browser whenever application language changes. This does not
  # persist anything by itself; persistence occurs only after a user choice.
  observe({
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    session$sendCustomMessage("sap-i18n-set-language", list(language = lang))
  })

  observeEvent(input$sap_i18n_client_ready, {
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    session$sendCustomMessage("sap-i18n-set-language", list(language = lang))
  }, ignoreInit = TRUE)

  # Dynamic interface translation is a secondary fallback for static UI phrases
  # not yet present in the bundled catalog. The browser only submits text from
  # explicitly safe UI surfaces (labels/buttons/help/status), never raw data cells.
  observeEvent(input$sap_i18n_missing, {
    payload <- input$sap_i18n_missing
    lang <- sap_normalize_language(payload$language %||% app_state$ui_language %||% "en")
    strings <- unique(trimws(as.character(unlist(payload$strings %||% character(), use.names = FALSE))))
    strings <- strings[nzchar(strings)]
    if (identical(lang, "en") || !length(strings) || is.null(services$ai) || !isTRUE(services$ai$is_ready())) return(invisible(NULL))
    translated <- tryCatch(services$ai$translate_batch(strings, lang, max_items = 40L), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    if (isTRUE(translated$ok) && length(translated$translations)) {
      session$sendCustomMessage("sap-i18n-add", list(language = lang, translations = as.list(translated$translations)))
    }
  }, ignoreInit = TRUE)

  code_gen <- mod_code_generator_server("code_generator", app_state, services = services)

  # A single shared history manager now owns real before/after snapshots.
  # The history page restores those snapshots for Undo and Redo.
  history_mgr <- HistoryManager$new()
  history_mgr$seed_state(sap_capture_app_state(app_state, isolate_read = TRUE))
  history_mgr <- mod_analysis_history_server(
    "analysis_history", app_state, history_manager = history_mgr,
    services = services
  )

  history_state <- shiny::debounce(
    shiny::reactive(sap_capture_app_state(app_state)),
    millis = 350
  )
  shiny::observeEvent(history_state(), {
    snapshot <- history_state()
    description <- shiny::isolate(app_state$last_action %||% "Workspace state updated")
    module_name <- sap_history_module_from_action(description)
    history_mgr$record_state(
      action_type = "state_change",
      module = module_name,
      description = description,
      state = snapshot
    )

    # Persist a lightweight audit event. Full undo snapshots remain session-local
    # in this release; respondent-level data are never copied into the audit log.
    pid <- shiny::isolate(app_state$current_project)
    uid <- shiny::isolate(app_state$current_user)
    if (!is.null(app_pool) && !is.null(pid)) {
      try(db_log_action(
        app_pool, project_id = pid, user_id = uid,
        module = module_name, action_type = "state_change",
        description = description, reversible = TRUE,
        metadata = list(dataset_name = shiny::isolate(app_state$dataset_name),
                        data_imported = isTRUE(shiny::isolate(app_state$data_imported)))
      ), silent = TRUE)
    }
  }, ignoreInit = TRUE)
  mod_machine_learning_server("machine_learning", app_state, services = services, pool = app_pool)
  mod_settings_server("settings", app_state, services = services)

  # Advanced BI Studio workflow ----------------------------------------------
  mod_data_connections_server("data_connections", app_state, services = services, pool = app_pool)
  mod_transform_pipeline_server("transform_pipeline", app_state, services = services, pool = app_pool)
  mod_semantic_model_server("semantic_model", app_state, services = services, pool = app_pool)
  mod_visual_builder_server("visual_builder", app_state, services = services, pool = app_pool)
  mod_dashboard_studio_server("dashboard_studio", app_state, services = services, pool = app_pool)
  mod_publish_center_server(
    "publish_center", app_state, services = services, pool = app_pool,
    connected = SAP_POSTGRES_ENABLED
  )

  mod_import_wizard_server("import_wizard", app_state, services = services)
  mod_data_lab_server("data_lab", app_state, services = services)
  mod_data_explorer_server("data_explorer", app_state, services = services)
  mod_data_cleaning_server("data_cleaning", app_state, services = services)
  mod_variable_manager_server("variable_manager", app_state, services = services)
  mod_dashboard_server("dashboard", app_state, services = services)
  mod_descriptive_stats_server("descriptive_stats", app_state, services = services)
  mod_statistical_tests_server("statistical_tests", app_state, services = services)
  mod_visualization_server("visualization", app_state, services = services)
  mod_report_generator_server("report_generator", app_state, services = services)
  mod_project_manager_server(
    "project_manager", app_state, services = services,
    connected = SAP_POSTGRES_ENABLED, pool = app_pool
  )
  mod_enterprise_center_server(
    "enterprise_center", app_state, services = services,
    pool = app_pool, demo_mode = !SAP_POSTGRES_ENABLED
  )
  mod_system_diagnostics_server("system_diagnostics", app_state, services = services)

  mod_questionnaire_manager_server("questionnaire_manager", app_state, services = services)
  mod_survey_design_server("survey_design", app_state, services = services)
  mod_scale_analysis_server("scale_analysis", app_state, services = services)
  mod_missing_nonresponse_server("missing_nonresponse", app_state, services = services)
  mod_privacy_governance_server("privacy_governance", app_state, services = services)
  mod_table_builder_server("table_builder", app_state, services = services)
  mod_advanced_models_server("advanced_models", app_state, services = services)
  mod_personal_health_server("personal_health", app_state, services = services)
  mod_sample_size_power_server("sample_size_power", app_state, services = services)
  mod_text_analysis_server("text_analysis", app_state, services = services)
  mod_geospatial_analysis_server("geospatial_analysis", app_state, services = services, pool = app_pool)
  mod_longitudinal_analysis_server("longitudinal_analysis", app_state, services = services)

  home_actions <- mod_presentation_home_server(
    "presentation_home", app_state = app_state, services = services
  )

  studio_actions <- mod_studio_home_server(
    "studio_home", app_state = app_state, services = services
  )

  # One hidden content router powers the entire application. The left sidebar is
  # now the only visible primary navigation, avoiding duplicate horizontal tabs.
  current_page <- reactiveVal("home")
  route_meta <- list(
    home = c("Presentation Home", "house"), guided = c("Guided Workspace", "route"),
    monitoring = c("MEL & Project Performance", "bullseye"), literacy = c("Data Literacy", "book-open-reader"),
    integrations = c("Field Integrations", "satellite-dish"), program_reports = c("Programme Reports", "file-circle-check"),
    operations = c("Operations Center", "tower-broadcast"), studio_home = c("Studio Overview", "compass"),
    connect = c("Connect & Catalog", "database"), prepare = c("Prepare & Transform", "wand-magic-sparkles"),
    model = c("Semantic Model", "diagram-project"), visual_builder = c("Visual Builder", "chart-column"),
    dashboard_studio = c("Dashboard Studio", "gauge-high"), publish = c("Publish Center", "paper-plane"),
    project_manager = c("Project Manager", "folder"), enterprise = c("Enterprise Center", "building-shield"),
    history = c("Analysis History", "clock-rotate-left"), import = c("Import Data", "upload"),
    data_lab = c("Data Lab", "flask"), explorer = c("Data Explorer", "magnifying-glass"), cleaning = c("Data Cleaning", "broom"),
    variables = c("Variable Manager", "table-list"), questionnaire = c("Questionnaire", "clipboard-list"),
    design = c("Survey Design", "scale-balanced"), scales = c("Scale Analysis", "list-ol"),
    missing = c("Missing & Nonresponse", "user-clock"), privacy = c("Privacy & Governance", "shield-halved"),
    dashboard = c("Overview Dashboard", "gauge"), descriptive = c("Descriptive Statistics", "chart-line"),
    tables = c("Table Builder", "table-cells"), statistics = c("Statistical Tests", "calculator"),
    advanced = c("Advanced Models", "diagram-project"), visualization = c("Visualization", "palette"),
    health = c("Personal Health Tracker", "heart-pulse"), power = c("Sample Size & Power", "bullseye"), text = c("Text Analysis", "comments"),
    geospatial = c("Geospatial", "map-location-dot"), longitudinal = c("Trends & Panels", "timeline"),
    ml = c("Machine Learning", "robot"), report_builder = c("Report Builder", "file-pen"),
    export = c("Export Center", "download"), settings = c("Settings", "gear"),
    admin_control = c("Control Center", "users-gear"),
    code = c("R Code", "code"), diagnostics = c("System Health", "stethoscope")
  )

  route_lookup <- c(
    home = "home",
    guided = "guided",
    monitoring = "monitoring",
    literacy = "literacy",
    integrations = "integrations",
    program_reports = "program_reports",
    operations = "operations",
    studio = "studio_home",
    studio_home = "studio_home",
    connect = "connect",
    prepare = "prepare",
    model = "model",
    visual_builder = "visual_builder",
    dashboard_studio = "dashboard_studio",
    publish = "publish",
    project = "project_manager",
    enterprise = "enterprise",
    data = "import",
    data_lab = "data_lab",
    health = "health",
    survey_setup = "questionnaire",
    quality = "missing",
    analysis = "dashboard",
    report = "report_builder",
    settings = "settings",
    admin_control = "admin_control",
    code = "code",
    diagnostics = "diagnostics",
    power = "power",
    text = "text",
    geospatial = "geospatial",
    longitudinal = "longitudinal",
    ml = "ml"
  )

  resolve_route <- function(top, sub = NULL) {
    if (!is.null(sub) && nzchar(sub)) {
      if (identical(sub, "builder")) return("report_builder")
      return(sub)
    }
    resolved <- unname(route_lookup[[top]])
    if (is.null(resolved) || !nzchar(resolved)) top else resolved
  }

  go_to <- function(top, sub_id = NULL, sub = NULL) {
    target <- resolve_route(top, sub)
    try(bslib::nav_select("main_nav", selected = target, session = session), silent = TRUE)
    invisible(target)
  }

  sidebar_routes <- c(
    nav_home = "home",
    nav_guided = "guided",
    nav_monitoring = "monitoring",
    nav_literacy = "literacy",
    nav_integrations = "integrations",
    nav_program_reports = "program_reports",
    nav_operations = "operations",
    nav_studio_home = "studio_home",
    nav_connect = "connect",
    nav_prepare = "prepare",
    nav_model = "model",
    nav_visual_builder = "visual_builder",
    nav_dashboard_studio = "dashboard_studio",
    nav_publish = "publish",
    nav_project_manager = "project_manager",
    nav_enterprise = "enterprise",
    nav_history = "history",
    nav_import = "import",
    nav_data_lab = "data_lab",
    nav_explorer = "explorer",
    nav_cleaning = "cleaning",
    nav_variables = "variables",
    nav_questionnaire = "questionnaire",
    nav_design = "design",
    nav_scales = "scales",
    nav_missing = "missing",
    nav_privacy = "privacy",
    nav_dashboard = "dashboard",
    nav_descriptive = "descriptive",
    nav_tables = "tables",
    nav_statistics = "statistics",
    nav_advanced = "advanced",
    nav_visualization = "visualization",
    nav_health = "health",
    nav_power = "power",
    nav_text = "text",
    nav_geospatial = "geospatial",
    nav_longitudinal = "longitudinal",
    nav_ml = "ml",
    nav_report_builder = "report_builder",
    nav_export = "export",
    nav_settings = "settings",
    nav_admin_control = "admin_control",
    nav_code = "code",
    nav_diagnostics = "diagnostics"
  )

  lapply(names(sidebar_routes), function(input_id) {
    local({
      id <- input_id
      target <- unname(sidebar_routes[[input_id]])
      observeEvent(input[[id]], {
        try(bslib::nav_select("main_nav", selected = target, session = session), silent = TRUE)
      }, ignoreInit = TRUE)
    })
  })

  observeEvent(input$main_nav, {
    target <- input$main_nav
    current_page(target)
    app_state$current_route <- target
    button_id <- names(sidebar_routes)[match(target, sidebar_routes)]
    if (length(button_id) == 1L && !is.na(button_id)) {
      shinyjs::runjs(sprintf(
        paste0(
          "document.querySelectorAll('.sap-nav-button').forEach(function(el){el.classList.remove('active');});",
          "var btn=document.getElementById('%s');",
          "if(btn){btn.classList.add('active');var grp=btn.closest('details');if(grp){grp.open=true;}}"
        ),
        button_id
      ))
    }
  }, ignoreInit = FALSE)

  # Restore the last saved route once per authenticated identity. Project/data
  # restoration is performed by the auth/workspace service before this observer
  # moves the UI. No raw respondent rows are stored in user_workspace_state.
  .sap_resume_user <- shiny::reactiveVal(NULL)
  shiny::observe({
    services$auth$version()
    uid <- app_state$current_user
    if (is.null(uid)) {
      .sap_resume_user(NULL)
      return(invisible(NULL))
    }
    if (!isTRUE(app_state$workspace_restored)) return(invisible(NULL))
    if (identical(.sap_resume_user(), as.integer(uid))) return(invisible(NULL))
    target <- app_state$resume_route %||% "home"
    if (!target %in% names(route_meta)) target <- "home"
    try(bslib::nav_select("main_nav", selected = target, session = session), silent = TRUE)
    current_page(target)
    app_state$current_route <- target
    .sap_resume_user(as.integer(uid))
  })

  # Lightweight workspace autosave. The dataset itself is already versioned by
  # DatasetService; this saves navigation + analysis configuration so a user can
  # sign in later and continue from the same project/module.
  .sap_workspace_snapshot <- shiny::debounce(
    shiny::reactive({
      if (is.null(app_state$current_user)) return(NULL)
      list(
        project_id = app_state$current_project,
        route = app_state$current_route %||% current_page(),
        last_action = app_state$last_action,
        state = list(
          dataset_name = app_state$dataset_name,
          ui_language = app_state$ui_language,
          user_mode = app_state$user_mode,
          me_indicators = app_state$me_indicators,
          me_results = app_state$me_results,
          me_activities = app_state$me_activities,
          me_risks = app_state$me_risks,
          me_grievances = app_state$me_grievances,
          me_actions = app_state$me_actions,
          field_visits = app_state$field_visits,
          survey_design = app_state$survey_design,
          table_builder_spec = app_state$table_builder_spec,
          nonresponse_plan = app_state$nonresponse_plan,
          questionnaire = app_state$questionnaire,
          questionnaire_versions = app_state$questionnaire_versions,
          routing_rules = app_state$routing_rules,
          scale_definitions = app_state$scale_definitions,
          advanced_model_queue = app_state$advanced_model_queue,
          text_codebook = app_state$text_codebook,
          text_coding_results = app_state$text_coding_results,
          geospatial_spec = app_state$geospatial_spec,
          longitudinal_spec = app_state$longitudinal_spec,
          privacy_register = app_state$privacy_register,
          governance_policy = app_state$governance_policy,
          report_frontend_blocks = app_state$report_frontend_blocks,
          health_log = app_state$health_log,
          active_filter_label = app_state$active_filter_label,
          bi_manifest = sap_bi_model_manifest(app_state)
        )
      )
    }), millis = 1200
  )
  shiny::observeEvent(.sap_workspace_snapshot(), {
    snap <- .sap_workspace_snapshot()
    if (is.null(snap) || is.null(services$workspace)) return(invisible(NULL))
    try(services$workspace$save(
      last_route = snap$route,
      last_module = snap$route,
      last_analysis = snap$last_action,
      state = snap$state
    ), silent = TRUE)
  }, ignoreInit = TRUE)

  # Original Presentation Home actions remain the shortest path into the
  # familiar survey workflow. The BI Studio actions below are additive.
  observeEvent(home_actions$go_import(), go_to("data", "data_nav", "import"), ignoreInit = TRUE)
  observeEvent(home_actions$next_import(), go_to("data", "data_nav", "import"), ignoreInit = TRUE)
  observeEvent(home_actions$go_explorer(), go_to("data", "data_nav", "explorer"), ignoreInit = TRUE)
  observeEvent(home_actions$go_cleaning(), go_to("data", "data_nav", "cleaning"), ignoreInit = TRUE)
  observeEvent(home_actions$review_quality(), go_to("quality", "quality_nav", "missing"), ignoreInit = TRUE)
  observeEvent(home_actions$go_analysis(), go_to("analysis", "analysis_nav", "descriptive"), ignoreInit = TRUE)
  observeEvent(home_actions$go_report(), go_to("report", "report_nav", "builder"), ignoreInit = TRUE)
  observeEvent(home_actions$go_data_lab(), go_to("data_lab"), ignoreInit = TRUE)
  observeEvent(home_actions$go_health_tracker(), go_to("health"), ignoreInit = TRUE)
  observeEvent(home_actions$household_loaded(), go_to("data", "data_nav", "explorer"), ignoreInit = TRUE)
  observeEvent(home_actions$health_loaded(), go_to("data", "data_nav", "explorer"), ignoreInit = TRUE)
  observeEvent(home_actions$likert_loaded(), go_to("data", "data_nav", "explorer"), ignoreInit = TRUE)

  observeEvent(studio_actions$connect(), go_to("connect"), ignoreInit = TRUE)
  observeEvent(studio_actions$prepare(), go_to("prepare"), ignoreInit = TRUE)
  observeEvent(studio_actions$model(), go_to("model"), ignoreInit = TRUE)
  observeEvent(studio_actions$explore(), go_to("visual_builder"), ignoreInit = TRUE)
  observeEvent(studio_actions$dashboard(), go_to("dashboard_studio"), ignoreInit = TRUE)
  observeEvent(studio_actions$publish(), go_to("publish"), ignoreInit = TRUE)
  observeEvent(studio_actions$advanced(), go_to("design"), ignoreInit = TRUE)
  observeEvent(studio_actions$import(), go_to("import"), ignoreInit = TRUE)
  observeEvent(studio_actions$lab(), go_to("data_lab"), ignoreInit = TRUE)

  observeEvent(input$goto_import, go_to("data", "data_nav", "import"))
  observeEvent(input$export_go_report, go_to("report", "report_nav", "builder"))

  # Keep workspace-level metrics current when any module updates the dataset.
  observeEvent(app_state$processed_data, {
    df <- app_state$processed_data
    if (!is.data.frame(df)) return()
    if (is.null(app_state$dataset_name) || !nzchar(app_state$dataset_name)) {
      app_state$dataset_name <- "Imported Survey"
    }
    app_state$quality_score <- tryCatch(
      calculate_quality_score(df)$score,
      error = function(e) NA_real_
    )
    try(services$metadata$populate(df), silent = TRUE)
  }, ignoreInit = TRUE)

  output$topbar_context <- renderUI({
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    tr <- function(x) sap_tr(x, lang)
    meta <- route_meta[[current_page()]] %||% c("Workspace", "compass")
    df <- app_state$processed_data
    dataset_label <- if (isTRUE(app_state$data_imported) && is.data.frame(df)) {
      sprintf("%s · %s %s", app_state$dataset_name %||% tr("Active dataset"),
              format(nrow(df), big.mark = ","), tr("rows"))
    } else {
      tr("No dataset")
    }
    ai_cfg <- tryCatch(services$ai$config(), error = function(e) NULL)
    ai_label <- if (is.null(ai_cfg)) tr("AI unavailable") else {
      endpoint <- if (identical(ai_cfg$provider, "ollama")) {
        if (grepl("localhost|127\\.0\\.0\\.1", ai_cfg$base_url %||% "", ignore.case = TRUE)) tr("local") else tr("remote")
      } else tr("cloud")
      paste(tools::toTitleCase(ai_cfg$provider %||% "AI"), endpoint)
    }
    meta[[1]] <- tr(meta[[1]])
    div(
      class = "sap-topbar-context",
      div(class = "sap-topbar-chip", icon(meta[[2]]), strong(meta[[1]])),
      div(class = "sap-topbar-chip sap-topbar-chip-muted", icon("database"), strong(dataset_label)),
      div(class = "sap-topbar-chip sap-topbar-chip-muted", icon("server"),
          strong(paste(SAP_RUNTIME_LABEL, tr("runtime")))),
      div(class = "sap-topbar-chip sap-topbar-chip-muted", icon("wand-magic-sparkles"), strong(ai_label))
    )
  })

  observeEvent(input$topbar_ai, go_to("settings"), ignoreInit = TRUE)

  observe({
    status <- app_state$ai_connection_status$state %||% "unknown"
    cls <- switch(status, ready = "sap-ai-dot-ready", error = "sap-ai-dot-error", "")
    shinyjs::runjs(sprintf(
      "var d=document.querySelector('#topbar_ai .sap-ai-dot');if(d){d.className='sap-ai-dot %s';}",
      cls
    ))
  })

  observeEvent(input$topbar_help, {
    showModal(modalDialog(
      title = tagList(icon("circle-question"), "Workspace help"),
      size = "m", easyClose = TRUE,
      p("Follow the Studio workflow from Connect to Publish. The command bar shows the current page, active table, runtime profile, and AI mode."),
      tags$ul(
        tags$li("Connect can hold several related tables; the active table also powers the classic statistical modules."),
        tags$li("Prepare saves replayable steps, Model defines relationships and measures, and Explore saves visuals to dashboard pages."),
        tags$li("Publish controls versions, reviewers, refresh schedules, and row-level security."),
        tags$li("Use Settings → AI Assistant to test a private or cloud interpretation provider before use.")
      ),
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  output$workspace_summary <- renderUI({
    lang <- sap_normalize_language(app_state$ui_language %||% "en")
    tr <- function(x) sap_tr(x, lang)
    project_name <- app_state$current_project_name
    if (is.null(project_name) || !nzchar(project_name)) {
      project_name <- tr("Local Studio Workspace")
    }

    heading <- div(
      class = "sap-workspace-heading",
      div(class = "sap-workspace-heading-icon", icon("folder-open")),
      div(
        p(tr("Current workspace"), class = "sap-workspace-eyebrow"),
        p(project_name, class = "sap-workspace-project")
      )
    )

    if (!isTRUE(app_state$data_imported) || !is.data.frame(app_state$processed_data)) {
      return(div(
        class = "sap-workspace-card",
        heading,
        div(
          class = "sap-dataset-banner sap-dataset-banner-empty",
          icon("database", class = "sap-dataset-status-icon"),
          div(
            div(tr("Dataset status"), class = "sap-dataset-status-label"),
            div(tr("No dataset loaded"), class = "sap-dataset-name"),
            div(tr("Start from Home or Data → Import to activate this workspace."),
                class = "sap-dataset-guidance")
          )
        )
      ))
    }

    df <- app_state$processed_data
    dataset_name <- app_state$dataset_name
    if (is.null(dataset_name) || !nzchar(dataset_name)) dataset_name <- tr("Imported Survey")

    total_cells <- nrow(df) * ncol(df)
    missing_pct <- if (total_cells == 0L) 0 else 100 * sum(is.na(df)) / total_cells
    quality <- app_state$quality_score
    quality_label <- if (is.null(quality) || is.na(quality)) "—" else sprintf("%.0f/100", quality)

    design <- app_state$survey_design %||% list()
    weight <- if (isTRUE(design$enabled) && nzchar(design$weight %||% "")) {
      design$weight
    } else {
      tr("Unweighted")
    }
    domain <- if (nzchar(design$domain %||% "")) {
      design$domain
    } else {
      app_state$active_filter_label %||% tr("All respondents")
    }

    transformations <- app_state$transformation_count
    if (is.null(transformations)) transformations <- 0L
    ai_cfg <- tryCatch(services$ai$config(), error = function(e) NULL)
    ai_label <- if (is.null(ai_cfg)) {
      tr("Unavailable")
    } else if (identical(ai_cfg$provider, "ollama")) {
      paste("Ollama", if (nzchar(ai_cfg$model %||% "")) paste0("· ", ai_cfg$model) else "· auto model")
    } else {
      paste(tools::toTitleCase(ai_cfg$provider %||% tr("AI")), ai_cfg$model %||% "")
    }
    last_action <- app_state$last_action
    if (is.null(last_action) || !nzchar(last_action)) last_action <- tr("Dataset ready")

    div(
      class = "sap-workspace-card",
      heading,
      div(
        class = "sap-dataset-banner sap-dataset-banner-active",
        icon("circle-check", class = "sap-dataset-status-icon"),
        div(
          div(tr("Active dataset"), class = "sap-dataset-status-label"),
          div(dataset_name, class = "sap-dataset-name"),
          div(tr("Available across all analysis modules"), class = "sap-dataset-guidance")
        )
      ),
      div(
        class = "sap-workspace-metrics",
        div(class = "sap-workspace-metric",
            span(format(nrow(df), big.mark = ","), class = "sap-workspace-metric-value"),
            span(tr("Respondents"), class = "sap-workspace-metric-label")),
        div(class = "sap-workspace-metric",
            span(format(ncol(df), big.mark = ","), class = "sap-workspace-metric-value"),
            span(tr("Variables"), class = "sap-workspace-metric-label")),
        div(class = "sap-workspace-metric",
            span(sprintf("%.1f%%", missing_pct), class = "sap-workspace-metric-value"),
            span(tr("Missing"), class = "sap-workspace-metric-label")),
        div(class = "sap-workspace-metric",
            span(quality_label, class = "sap-workspace-metric-value"),
            span(tr("Quality"), class = "sap-workspace-metric-label"))
      ),
      div(
        class = "sap-workspace-context",
        div(class = "sap-workspace-context-row", icon("scale-balanced"),
            span(tr("Weight"), class = "sap-workspace-context-label"),
            span(weight, class = "sap-workspace-context-value")),
        div(class = "sap-workspace-context-row", icon("filter"),
            span(tr("Domain"), class = "sap-workspace-context-label"),
            span(domain, class = "sap-workspace-context-value")),
        div(class = "sap-workspace-context-row", icon("wand-magic-sparkles"),
            span(tr("AI"), class = "sap-workspace-context-label"),
            span(ai_label, class = "sap-workspace-context-value")),
        div(class = "sap-workspace-context-row", icon("arrows-rotate"),
            span(tr("Changes"), class = "sap-workspace-context-label"),
            span(transformations, class = "sap-workspace-context-value"))
      ),
      div(class = "sap-workspace-updated", icon("clock"), " ", last_action)
    )
  })

  output$sidebar_download_ui <- renderUI({
    if (!isTRUE(app_state$data_imported)) return(NULL)
    downloadButton("sidebar_download_data", sap_tr("Download Active Data", app_state$ui_language %||% "en"),
                   class = "btn btn-success btn-sm w-100")
  })

  output$export_status <- renderUI({
    if (!isTRUE(app_state$data_imported)) {
      return(div(class = "alert alert-warning",
                 icon("triangle-exclamation"),
                 paste0(" ", sap_tr("Load a dataset before using the Export Center.", app_state$ui_language %||% "en"))))
    }
    div(class = "alert alert-success",
        icon("circle-check"),
        sprintf(" Ready to export %s rows and %s columns.",
                format(nrow(app_state$processed_data), big.mark = ","),
                ncol(app_state$processed_data)))
  })

  output$sidebar_download_data <- downloadHandler(
    filename = function() paste0("survey_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(app_state$data_imported, is.data.frame(app_state$processed_data))
      safe <- services$privacy$prepare_export(app_state$processed_data, "active data download")
      utils::write.csv(safe, file, row.names = FALSE, na = "")
    }
  )

  output$export_data_csv <- downloadHandler(
    filename = function() paste0("cleaned_survey_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(app_state$data_imported, is.data.frame(app_state$processed_data))
      safe <- services$privacy$prepare_export(app_state$processed_data, "Export Center data download")
      utils::write.csv(safe, file, row.names = FALSE, na = "")
    }
  )

  .sap_export_safe_data <- function(context) {
    req(app_state$data_imported, is.data.frame(app_state$processed_data))
    services$privacy$prepare_export(app_state$processed_data, context)
  }

  output$export_data_xlsx <- downloadHandler(
    filename = function() paste0("cleaned_survey_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) openxlsx::write.xlsx(.sap_export_safe_data("Excel data export"), file, overwrite = TRUE)
  )
  output$export_data_sav <- downloadHandler(
    filename = function() paste0("cleaned_survey_", format(Sys.Date(), "%Y%m%d"), ".sav"),
    content = function(file) haven::write_sav(.sap_export_safe_data("SPSS data export"), file)
  )
  output$export_data_dta <- downloadHandler(
    filename = function() paste0("cleaned_survey_", format(Sys.Date(), "%Y%m%d"), ".dta"),
    content = function(file) haven::write_dta(.sap_export_safe_data("Stata data export"), file)
  )
  output$export_data_parquet <- downloadHandler(
    filename = function() paste0("cleaned_survey_", format(Sys.Date(), "%Y%m%d"), ".parquet"),
    content = function(file) sap_optional_call(
      "arrow", "write_parquet", .sap_export_safe_data("Parquet data export"), file,
      feature = "Parquet export"
    )
  )
  output$export_data_json <- downloadHandler(
    filename = function() paste0("cleaned_survey_", format(Sys.Date(), "%Y%m%d"), ".json"),
    content = function(file) jsonlite::write_json(.sap_export_safe_data("JSON data export"), file, dataframe = "rows", pretty = TRUE, na = "null", auto_unbox = TRUE)
  )

  output$export_dictionary_csv <- downloadHandler(
    filename = function() paste0("variable_dictionary_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      req(app_state$data_imported, is.data.frame(app_state$processed_data))
      services$privacy$assert_export_allowed("variable dictionary export")
      safe_data <- services$privacy$prepare_export(app_state$processed_data, "variable dictionary export")
      dictionary <- app_state$variable_types
      if (!is.data.frame(dictionary)) dictionary <- detect_variable_types(app_state$processed_data)
      var_col <- intersect(c("variable", "name", "Variable", "Name"), names(dictionary))
      if (length(var_col)) dictionary <- dictionary[dictionary[[var_col[[1]]]] %in% names(safe_data), , drop = FALSE]
      utils::write.csv(dictionary, file, row.names = FALSE, na = "")
    }
  )

  output$export_r_script <- downloadHandler(
    filename = function() paste0("survey_analysis_", format(Sys.Date(), "%Y%m%d"), ".R"),
    content = function(file) {
      services$privacy$assert_export_allowed("analysis code export")
      generated <- tryCatch(code_gen$set_code(), error = function(e) "")
      report_code <- tryCatch(services$report$get_code(), error = function(e) "")
      services$privacy$assert_content_export_allowed(list(generated = generated, report_code = report_code), "analysis code export")
      parts <- c(
        "# Survey Analytics Platform — Multi-User Control Edition",
        sprintf("# Exported %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        if (nzchar(generated)) generated else "# No code-generator snippet selected.",
        "",
        "# Code recorded from report blocks",
        if (nzchar(report_code)) report_code else "# No report analysis steps recorded yet."
      )
      writeLines(parts, file, useBytes = TRUE)
    }
  )

  observeEvent(input$btn_clear_session, {
    showModal(modalDialog(
      title = "Clear the current session?",
      p("This removes in-memory model tables, dashboard work, and analysis progress. Download anything you need first."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_session", "Clear Session", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_clear_session, {
    app_state$raw_data <- NULL
    app_state$processed_data <- NULL
    app_state$data_imported <- FALSE
    app_state$variable_types <- NULL
    app_state$variable_dictionary <- data.frame()
    app_state$dataset_name <- NULL
    app_state$quality_score <- NA_real_
    app_state$transformation_count <- 0L
    app_state$last_action <- "Session cleared"
    app_state$analyses <- list()
    app_state$trained_models <- list()
    app_state$survey_design <- list(enabled = FALSE, weight = "", strata = "", cluster = "", fpc = "", domain = "", trim_pct = 0, notes = "")
    app_state$questionnaire <- data.frame()
    app_state$questionnaire_versions <- list()
    app_state$routing_rules <- data.frame()
    app_state$scale_definitions <- list()
    app_state$nonresponse_plan <- list()
    app_state$table_builder_spec <- list()
    app_state$advanced_model_queue <- list()
    app_state$text_codebook <- data.frame()
    app_state$text_coding_results <- data.frame()
    app_state$geospatial_spec <- list()
    app_state$longitudinal_spec <- list()
    app_state$privacy_register <- data.frame()
    app_state$governance_policy <- list(small_cell = 5, min_base = 30, redact_ai = TRUE, require_approval = TRUE)
    app_state$report_frontend_blocks <- list()
    app_state$frontend_activity <- list()
    app_state$health_log <- data.frame()
    app_state$bi <- sap_bi_default_state()
    try(services$report$clear(), silent = TRUE)
    removeModal()
    go_to("home")
    showNotification("The local workspace was cleared.", type = "message")
  })

  message("[SAP] Survey Analytics Platform ", SAP_PRESENTATION_VERSION,
          " started with runtime profile '", SAP_RUNTIME_PROFILE, "'.")
}

app <- shinyApp(ui = ui, server = server)

message("[SAP] Main Application v", SAP_RELEASE_ID, " loaded")  # SAP_MODULE_BANNER

# Keep the Shiny application object as the final value returned by app.R.
app
