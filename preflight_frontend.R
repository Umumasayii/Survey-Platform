# =============================================================================
# SURVEY ANALYTICS PLATFORM — MULTILINGUAL PROFESSIONAL MEL RELEASE PREFLIGHT v6.1.0
# =============================================================================

required_files <- c(
  "VERSION", "release_metadata.R", ".rscignore", "app.R", "global_no_gtsummary.R",
  "00_services.R", "db_backend.R", "frontend_components.R", "README.md",
  "DESCRIPTION", "Dockerfile", "docker-compose.yml", "www/nexus-studio.css",
  "49_i18n_system.R", "data/ui_translations.csv", "www/sap-i18n-catalog.js", "www/sap-i18n.js",
  "www/nexus-studio-mark.svg", "www/survey-analytics-mark.svg",
  "18_presentation_home.R",
  "db/migrations/005_nexus_studio.sql", "scripts/validate_release.py",
  sprintf("%02d_%s.R", 33:40, c(
    "bi_core", "data_connections", "semantic_model", "transform_pipeline",
    "visual_builder", "dashboard_studio", "publish_center", "studio_home"
  ))
)
missing <- required_files[!file.exists(required_files)]
if (length(missing)) stop("Missing integrated release files: ", paste(missing, collapse = ", "), call. = FALSE)

r_files <- sort(unique(c(
  list.files(".", "\\.R$", full.names = TRUE),
  list.files("db", "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("scripts", "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("tests", "\\.R$", recursive = TRUE, full.names = TRUE)
)))
r_files <- r_files[!grepl("(^|/)renv/", r_files)]
syntax_errors <- list()
for (file in r_files) {
  issue <- tryCatch({ parse(file = file, keep.source = TRUE); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(issue)) syntax_errors[[file]] <- issue
}
if (length(syntax_errors)) {
  detail <- paste(vapply(names(syntax_errors), function(x) paste0(x, ": ", syntax_errors[[x]]), character(1)), collapse = "\n")
  stop("R syntax check failed:\n", detail, call. = FALSE)
}

read_text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app <- read_text("app.R")
core <- read_text("33_bi_core.R")
services <- read_text("00_services.R")
backend <- read_text("db_backend.R")
publish <- read_text("39_publish_center.R")

if (!identical(trimws(readLines("VERSION", warn = FALSE)[[1]]), "6.1.0")) stop("VERSION is not 6.1.0.", call. = FALSE)
if (!grepl('SAP_RELEASE_NAME <- "Multilingual Professional Monitoring & Evaluation Edition"', read_text("release_metadata.R"), fixed = TRUE)) stop("Multilingual Professional MEL release metadata is missing.", call. = FALSE)
if (!grepl("SAP_RUNTIME_PROFILE", app, fixed = TRUE) || !grepl("Production runtime requires complete PostgreSQL configuration", app, fixed = TRUE)) {
  stop("Explicit fail-closed runtime profiles are not wired.", call. = FALSE)
}
studio_files <- sprintf("%02d_%s.R", 33:40, c(
  "bi_core", "data_connections", "semantic_model", "transform_pipeline",
  "visual_builder", "dashboard_studio", "publish_center", "studio_home"
))
for (file in studio_files) {
  marker <- sprintf('source_required("%s")', file)
  if (!grepl(marker, app, fixed = TRUE)) stop("Studio source is not required by app.R: ", file, call. = FALSE)
}
numbered_files <- list.files(pattern = "^[0-9]{2}_.*\\.R$")
for (number in 1:48) {
  file <- numbered_files[startsWith(numbered_files, sprintf("%02d_", number))]
  if (length(file) != 1L || !grepl(sprintf('source_required("%s")', file), app, fixed = TRUE)) {
    stop("Numbered module is missing or not required by app.R: ", sprintf("%02d", number), call. = FALSE)
  }
}

required_markers <- c(
  "mod_data_connections_server", "mod_transform_pipeline_server",
  "mod_semantic_model_server", "mod_visual_builder_server",
  "mod_dashboard_studio_server", "mod_publish_center_server",
  "mod_presentation_home_ui", "mod_presentation_home_server",
  "page_sidebar(", "navset_hidden(", "Survey Analytics", "Presentation Home",
  "Advanced BI Studio", "nexus-studio.css"
)
missing_markers <- required_markers[!vapply(required_markers, grepl, logical(1), x = app, fixed = TRUE)]
if (length(missing_markers)) stop("App wiring markers missing: ", paste(missing_markers, collapse = ", "), call. = FALSE)

i18n_js <- read_text("www/sap-i18n.js")
if (grepl('selectInput("global_language"', app, fixed = TRUE) ||
    !grepl("global_language_toggle", app, fixed = TRUE) ||
    !grepl("sap_i18n_missing", i18n_js, fixed = TRUE)) {
  stop("System-wide icon-only language switching is not wired.", call. = FALSE)
}

security_markers <- c(
  "sap_bi_validate_remote_url", "followlocation = FALSE", "SAP_ALLOWED_REMOTE_HOSTS",
  "assert_content_export_allowed", "db_require_project_permission", "row_security_rules",
  "db_delete_model_table <- function(pool, project_id, name, user_id)"
)
security_text <- paste(core, services, backend, publish, collapse = "\n")
missing_security <- security_markers[!vapply(security_markers, grepl, logical(1), x = security_text, fixed = TRUE)]
if (length(missing_security)) stop("Security markers missing: ", paste(missing_security, collapse = ", "), call. = FALSE)

studio_features <- c(
  "sap_bi_apply_pipeline", "sap_bi_validate_relationship", "sap_bi_measure_value",
  "plotly_click", "drillthrough", "bookmarks", "db_publish_dashboard",
  "db_upsert_refresh_schedule", "db_add_project_member"
)
studio_text <- paste(vapply(studio_files, read_text, character(1)), collapse = "\n")
missing_features <- studio_features[!vapply(studio_features, grepl, logical(1), x = paste(studio_text, backend), fixed = TRUE)]
if (length(missing_features)) stop("Studio feature markers missing: ", paste(missing_features, collapse = ", "), call. = FALSE)

if (grepl("followlocation = TRUE", studio_text, fixed = TRUE)) stop("Remote redirect following is enabled.", call. = FALSE)
ui_files <- c("app.R", "frontend_components.R", list.files(pattern = "^[0-9]{2}_.*\\.R$"))
ui_text <- paste(vapply(ui_files, read_text, character(1)), collapse = "\n")
if (grepl('icon("sparkles")', ui_text, fixed = TRUE) ||
    grepl('icon("files")', ui_text, fixed = TRUE)) {
  stop("Unsupported icon names remain.", call. = FALSE)
}
if (!grepl('plotly::event_register(interactive_plot, "plotly_click")', read_text("37_visual_builder.R"), fixed = TRUE)) {
  stop("Visual Builder click event is not registered.", call. = FALSE)
}
if (!grepl('session$rootScope()$input[["plotly_click-builder"]]', read_text("37_visual_builder.R"), fixed = TRUE)) {
  stop("Visual Builder warning-safe click listener is missing.", call. = FALSE)
}
if (!grepl("Continue as Guest", read_text("14_auth.R"), fixed = TRUE) ||
    !grepl("SAP_ALLOW_GUEST", read_text("14_auth.R"), fixed = TRUE)) {
  stop("Optional Guest access is not wired.", call. = FALSE)
}
if (grepl("Queue model for backend", read_text("25_advanced_models.R"), fixed = TRUE)) stop("Advanced Models still contains a specification-only queue.", call. = FALSE)
if (grepl("publication exporters remain", read_text("20_table_builder.R"), ignore.case = TRUE)) stop("Table Builder still advertises unfinished exporters.", call. = FALSE)

app_lines <- trimws(readLines("app.R", warn = FALSE))
app_lines <- app_lines[nzchar(app_lines) & !startsWith(app_lines, "#")]
if (!length(app_lines) || tail(app_lines, 1L) != "app") stop("app.R must return the Shiny app object.", call. = FALSE)
if (!".Renviron" %in% trimws(readLines(".rscignore", warn = FALSE))) stop(".rscignore must exclude .Renviron.", call. = FALSE)

cat("✓ Parsed", length(r_files), "R files.\n")
cat("✓ Original survey workflow plus Advanced BI Studio markers passed.\n")
cat("✓ Runtime profiles, deployment assets, and secret exclusions passed.\n")
cat("✓ Multilingual Professional MEL release 6.1.0 is structurally ready.\n")
