# =============================================================================
# SURVEY ANALYTICS PLATFORM — SYSTEM-WIDE INTERNATIONALIZATION FOUNDATION
# =============================================================================
# The UI language is an application preference. It must never modify source
# datasets, variable names, respondent values, identifiers, or statistical
# estimates. Static interface copy uses a bundled translation catalog; generated
# narrative can use the configured AI provider with strict translation prompts.
# =============================================================================

SAP_LANGUAGE_META <- data.frame(
  code = c("en", "rw", "fr", "sw"),
  native_name = c("English", "Kinyarwanda", "Français", "Kiswahili"),
  short = c("EN", "RW", "FR", "SW"),
  html_lang = c("en", "rw", "fr", "sw"),
  stringsAsFactors = FALSE
)

sap_normalize_language <- function(language = "en") {
  code <- tolower(trimws(as.character(language %||% "en")[[1]]))
  if (!code %in% SAP_LANGUAGE_META$code) "en" else code
}

sap_language_name <- function(language = "en") {
  code <- sap_normalize_language(language)
  SAP_LANGUAGE_META$native_name[match(code, SAP_LANGUAGE_META$code)]
}

sap_i18n_catalog <- local({
  cache <- NULL
  function(force = FALSE) {
    if (!isTRUE(force) && is.data.frame(cache)) return(cache)
    path <- file.path("data", "ui_translations.csv")
    if (!file.exists(path)) {
      cache <<- data.frame(key=character(), en=character(), rw=character(), fr=character(), sw=character(), stringsAsFactors=FALSE)
      return(cache)
    }
    x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                         fileEncoding = "UTF-8-BOM")
    required <- c("key", "en", "rw", "fr", "sw")
    if (!all(required %in% names(x))) {
      stop("data/ui_translations.csv must contain key, en, rw, fr and sw columns.", call. = FALSE)
    }
    for (nm in required) x[[nm]] <- enc2utf8(as.character(x[[nm]]))
    cache <<- x
    cache
  }
})

sap_tr <- function(text, language = "en", default = NULL) {
  lang <- sap_normalize_language(language)
  x <- as.character(text)
  if (identical(lang, "en") || !length(x)) return(x)
  catalog <- sap_i18n_catalog()
  if (!nrow(catalog)) return(if (is.null(default)) x else rep_len(default, length(x)))
  translate_one <- function(value) {
    if (is.na(value) || !nzchar(trimws(value))) return(value)
    idx <- match(value, catalog$key)
    if (is.na(idx)) idx <- match(value, catalog$en)
    if (is.na(idx)) return(if (is.null(default)) value else as.character(default)[[1]])
    out <- catalog[[lang]][[idx]]
    if (!nzchar(trimws(out))) value else out
  }
  vapply(x, translate_one, character(1), USE.NAMES = FALSE)
}

sap_tr_or_key <- function(key, language = "en", fallback = key) {
  translated <- sap_tr(key, language = language, default = fallback)
  if (!length(translated)) fallback else translated[[1]]
}

sap_translate_names <- function(x, language = "en") {
  if (is.null(names(x))) return(x)
  names(x) <- sap_tr(names(x), language)
  x
}

sap_translate_frame_headers <- function(data, language = "en") {
  if (!is.data.frame(data)) return(data)
  names(data) <- sap_tr(names(data), language)
  data
}

# Translate only exact interface/report phrases locally. Unknown narrative is
# deliberately left untouched unless the AI translation service is explicitly
# available; this prevents accidental mutation of user data or statistical text.
sap_translate_local_text <- function(text, language = "en") {
  lang <- sap_normalize_language(language)
  if (identical(lang, "en") || length(text) == 0L) return(text)
  vapply(as.character(text), function(value) {
    if (is.na(value) || !nzchar(value)) return(value)
    lines <- strsplit(value, "\\n", fixed = FALSE)[[1]]
    translated <- vapply(lines, function(line) {
      leading <- sub("^([^[:alnum:]]*).*", "\\1", line)
      core <- trimws(line)
      if (!nzchar(core)) return(line)
      hit <- sap_tr(core, lang)
      if (identical(hit, core)) line else paste0(leading, hit)
    }, character(1))
    paste(translated, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

sap_i18n_output_language_instruction <- function(language = "en") {
  lang <- sap_normalize_language(language)
  if (identical(lang, "en")) return("Write the response in English.")
  paste0(
    "Write the complete response in ", sap_language_name(lang),
    ". Preserve variable names, identifiers, formulas, code, p-values, confidence intervals, percentages, dates, and numeric values exactly. ",
    "Translate explanatory prose, headings, labels, recommendations, and interpretation. Do not translate raw respondent values or invent new facts."
  )
}

sap_report_type_labels <- function(language = "en") {
  base <- c(
    "Monthly performance report"="monthly",
    "Quarterly performance report"="quarterly",
    "Annual results report"="annual",
    "Field monitoring report"="field",
    "Indicator performance report"="indicator",
    "Data quality report"="dqa",
    "Risk & issue report"="risk",
    "Grievance summary"="grievance",
    "Action follow-up report"="actions",
    "Learning brief"="learning",
    "Executive dashboard brief"="executive"
  )
  labels <- sap_tr(names(base), language)
  stats::setNames(unname(base), labels)
}

sap_i18n_status <- function(language = "en") {
  catalog <- sap_i18n_catalog()
  list(
    language = sap_normalize_language(language),
    native_name = sap_language_name(language),
    local_entries = nrow(catalog),
    ai_translation_available = FALSE
  )
}

message("[SAP] System-wide i18n foundation v1.0 loaded (EN/RW/FR/SW)")
