# =============================================================================
# GUIDED DATA CLEANING STUDIO
# Survey Analytics Platform 5.2.0
# =============================================================================
# A visual, non-coding cleaning workflow built around the R Data Cleaning Handbook:
# diagnose -> preview -> apply -> validate -> document.  Raw worksheets are
# preserved separately from cleaned worksheet copies.  The existing expert
# cleaning tabs remain available in 03_data_cleaning.R.
# =============================================================================

sap_cleaning_action_catalog <- function() {
  data.frame(
    action = c(
      "clean_names", "remove_empty", "remove_constant", "header_row",
      "trim_squish", "unicode_normalize", "transliterate_ascii", "case_normalize",
      "replace_text", "sentinel_na", "fill_missing", "replace_na",
      "parse_number", "parse_integer", "parse_date", "category_recode",
      "remove_duplicates", "validate_range", "validate_allowed", "regex_validate",
      "pivot_longer", "split_column", "unite_columns"
    ),
    group = c(
      rep("Structure", 4), rep("Text", 5), rep("Missing values", 3),
      rep("Types & dates", 3), "Categories", "Duplicates",
      rep("Validation", 3), rep("Reshape", 3)
    ),
    label = c(
      "Standardize column names", "Remove empty rows / columns", "Remove constant columns", "Promote a row to column headers",
      "Trim & collapse whitespace", "Normalize Unicode text", "Create ASCII search text", "Normalize letter case",
      "Find & replace text", "Convert missing-value codes to NA", "Fill missing values up/down", "Replace NA with a value",
      "Parse numbers safely", "Parse whole numbers safely", "Parse dates / datetimes", "Recode categories with a mapping",
      "Investigate / remove duplicates", "Check a numeric range", "Check allowed values", "Validate a text pattern",
      "Pivot repeated columns longer", "Split one column into several", "Combine columns"
    ),
    summary = c(
      "Makes names stable, unique, and analysis-friendly (normally snake_case).",
      "Removes spreadsheet scaffolding that contains no information.",
      "Removes columns that contain only one distinct non-missing value.",
      "Uses a selected worksheet row as the variable names and removes header scaffolding above it.",
      "Removes leading/trailing spaces and collapses repeated spaces inside text.",
      "Converts text to UTF-8-compatible Unicode and applies NFKC normalization.",
      "Creates ASCII-style text by removing accents/transliterating characters. Use only when the original spelling is not required.",
      "Converts selected text to lower case, UPPER CASE, or Title Case.",
      "Replaces literal text or a regular-expression pattern in one variable.",
      "Maps documented source codes such as -99, N/A, or blank strings to true missing values (NA).",
      "Carries observed values down or up through spreadsheet-style blank cells.",
      "Fills missing cells in one variable with an explicit user-supplied value.",
      "Parses currency/grouped numeric text with explicit decimal and grouping marks.",
      "Parses integer text and makes parse failures visible as NA.",
      "Parses dates using explicit documented orders and a timezone; ambiguous dates are not guessed silently.",
      "Maps spelling variants or source codes to controlled categories.",
      "Finds exact or key-based duplicates and can flag them or keep the first/last record.",
      "Flags values outside a plausible minimum/maximum; filtering is optional and not the default.",
      "Flags values that are not in an explicit allowed set.",
      "Flags text that does not match a supplied regular-expression rule.",
      "Turns repeated columns into tidy long rows so one variable is stored in one column.",
      "Splits a delimited field into multiple new columns.",
      "Combines selected columns into a single field with a chosen separator."
    ),
    why = c(
      "Stable names reduce broken scripts and make later transformations easier to reproduce.",
      "Title rows and blank scaffolding can distort row counts and validation results.",
      "Constant fields often add no analytical information and can confuse models or exports.",
      "Many Excel exports put titles or notes above the real header row.",
      "Whitespace differences create false categories such as 'Kigali' and ' Kigali '.",
      "Unicode normalization prevents visually identical text from behaving like different values.",
      "ASCII keys can help matching/search systems that cannot handle accents, while the raw data remains preserved.",
      "Consistent case reduces duplicate categories caused only by capitalization.",
      "Useful for punctuation cleanup, code normalization, phone cleanup, and controlled pattern replacement.",
      "R should represent missingness as NA so summaries and models treat it consistently.",
      "Spreadsheet reports often show a group label once and leave the following rows blank.",
      "Use only when the replacement has a defensible meaning; otherwise keep NA.",
      "readr parsing exposes failures rather than silently forcing invalid values into a numeric type.",
      "Useful for counts, ages, years, and other whole-number variables.",
      "Date order and timezone are data definitions, not formatting guesses.",
      "Controlled categories improve tables, charts, filters, and statistical models.",
      "Duplicates are not automatically errors; the correct rule depends on the unit of observation and business key.",
      "Range checks separate suspicious values from automatic deletion, preserving auditability.",
      "Allowed-set checks are ideal for survey codes, status fields, districts, and enumerator identifiers.",
      "Pattern checks help screen IDs, phone-like values, email-like values, codes, and other structured text.",
      "Long data is often easier to validate and analyze when repeated measurements are stored consistently.",
      "Splitting can recover separate components from fields such as 'ID / District' or combined names.",
      "Combining is useful for composite labels or keys when the source stores components separately."
    ),
    caution = c(
      "Review any downstream scripts that refer to the old names.",
      "Confirm that apparently empty rows/columns are not meaningful separators required for reporting.",
      "A constant field can still be meaningful metadata; preview before applying.",
      "Choose the row only after inspecting the worksheet preview.",
      "Keep raw text when exact spacing is meaningful.",
      "Normalization preserves text better than transliteration, but still review multilingual identifiers.",
      "Transliteration loses information. The untouched raw worksheet is always retained.",
      "Case can be meaningful in IDs or passwords; apply only to descriptive text.",
      "Regular expressions are powerful; preview the result before applying.",
      "Do not map 'Unknown' to NA unless the survey/data dictionary defines it as missing.",
      "Fill is appropriate for hierarchical spreadsheet blocks, not for ordinary unanswered survey questions.",
      "Do not replace missing responses with zero unless zero is substantively correct.",
      "Choose the locale marks that match the source file.",
      "Identifiers with leading zeros should normally remain character, not integer.",
      "Do not infer whether 03/04 means March 4 or April 3 without source context.",
      "Keep a mapping of old -> new values so the recode is reproducible.",
      "Review duplicate records before removing them; repeated events may be valid.",
      "An out-of-range or unusual value is a signal to investigate, not proof that the record is wrong.",
      "Allowed values should come from the questionnaire/codebook or domain rules.",
      "A simple regex screen is not the same as authoritative verification.",
      "Pivoting changes the unit/shape of the dataset; verify row counts and key structure afterwards.",
      "If some rows contain a different number of delimiters, inspect the result carefully.",
      "Choose a separator that cannot be confused with meaningful content."
    ),
    functions = c(
      "janitor::clean_names()", "janitor::remove_empty()", "janitor::remove_constant()", "janitor::row_to_names()",
      "stringr::str_trim(), stringr::str_squish()", "stringi::stri_enc_toutf8(), stringi::stri_trans_nfkc()",
      "stringi::stri_trans_general()", "stringr::str_to_lower()/str_to_upper()/str_to_title()",
      "stringr::str_replace_all(), stringr::fixed()/regex()", "dplyr::na_if(), tidyr::replace_na()",
      "tidyr::fill()", "tidyr::replace_na()", "readr::parse_number(), readr::locale()", "readr::parse_integer()",
      "lubridate::parse_date_time()", "dplyr::case_when()/recode(), forcats", "dplyr::distinct(), dplyr::count(), janitor::get_dupes()",
      "dplyr::between()", "%in%, dplyr::if_any()/if_all()", "stringr::str_detect(), stringr::regex()",
      "tidyr::pivot_longer()", "tidyr::separate_wider_delim()", "tidyr::unite()"
    ),
    stringsAsFactors = FALSE
  )
}


# Visual workbench metadata. The handbook is an internal implementation reference;
# users interact with plain-language tools and never need to see R functions.
sap_visual_action_catalog <- function() {
  base <- sap_cleaning_action_catalog()
  extra <- data.frame(
    action = c("remove_characters", "drop_missing_rows", "outlier_flag", "cross_field"),
    group = c("Text", "Missing values", "Validation", "Validation"),
    label = c("Remove unwanted characters", "Remove rows with missing values", "Flag unusual numeric values", "Check two columns against each other"),
    summary = c(
      "Removes punctuation or keeps only digits/letters in selected text columns.",
      "Removes rows only when the selected columns meet your missing-value rule.",
      "Adds a review flag for unusually high or low numeric values without deleting them by default.",
      "Flags rows where two selected columns break a logical rule such as End date before Start date."
    ),
    why = c(
      "Useful for phone numbers, identifiers, codes, and text copied from inconsistent systems.",
      "Sometimes complete-case analysis is appropriate, but the rule should be explicit and previewed first.",
      "Outliers can be data errors or important observations. Flagging first keeps the decision auditable.",
      "Cross-field checks catch inconsistencies that a one-column validation cannot detect."
    ),
    caution = c(
      "Removing characters can destroy meaningful punctuation. Always preview the result.",
      "Deleting rows changes the sample. Use this only when missingness rules justify it.",
      "An outlier is not automatically wrong. Investigate before filtering or correcting.",
      "Choose columns with comparable meanings and a rule that follows the data dictionary."
    ),
    functions = c("internal", "internal", "internal", "internal"),
    stringsAsFactors = FALSE
  )
  rbind(base, extra)
}

sap_visual_action_scope <- function(action) {
  if (action %in% c("clean_names", "remove_empty", "remove_constant", "header_row", "remove_duplicates")) return("dataset")
  if (action %in% c("split_column")) return("single")
  if (action %in% c("cross_field")) return("pair")
  "multi"
}

sap_visual_column_kind <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("Date/time")
  if (is.numeric(x) || is.integer(x)) return("Number")
  if (is.logical(x)) return("Yes/No")
  if (is.factor(x)) return("Category")
  if (is.character(x)) return("Text")
  class(x)[1]
}

sap_visual_suggestions <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(data.frame())
  out <- list()
  add <- function(action, title, detail, columns = character(), priority = 2L) {
    out[[length(out) + 1L]] <<- data.frame(action=action, title=title, detail=detail,
      columns=paste(columns, collapse="|||"), priority=priority, stringsAsFactors=FALSE)
  }
  char_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  whitespace <- char_cols[vapply(df[char_cols], function(x) {
    y <- as.character(x); any(!is.na(y) & y != stringr::str_squish(y))
  }, logical(1))]
  if (length(whitespace)) add("trim_squish", "Fix extra spaces", sprintf("%d text column%s contain leading, trailing, or repeated spaces.", length(whitespace), if(length(whitespace)==1) "" else "s"), whitespace, 1L)
  sentinel_set <- c("", "NA", "N/A", "NULL", "NONE", "-99", "999")
  sentinel_cols <- char_cols[vapply(df[char_cols], function(x) {
    y <- toupper(trimws(as.character(x))); any(!is.na(y) & y %in% sentinel_set)
  }, logical(1))]
  if (length(sentinel_cols)) add("sentinel_na", "Review missing-value codes", sprintf("Possible missing-value codes were found in %d column%s.", length(sentinel_cols), if(length(sentinel_cols)==1) "" else "s"), sentinel_cols, 1L)
  numeric_like <- char_cols[vapply(df[char_cols], function(x) {
    y <- trimws(as.character(x)); y <- y[!is.na(y) & nzchar(y)]
    if (length(y) < 3L) return(FALSE)
    parsed <- suppressWarnings(readr::parse_number(y, na = character()))
    mean(!is.na(parsed)) >= .9 && length(unique(parsed[!is.na(parsed)])) > 1L
  }, logical(1))]
  if (length(numeric_like)) add("parse_number", "Convert numeric-looking text", sprintf("%d column%s look numeric but are stored as text.", length(numeric_like), if(length(numeric_like)==1) "" else "s"), numeric_like, 2L)
  if (nrow(df) && any(duplicated(df))) add("remove_duplicates", "Review duplicate rows", sprintf("%d repeated row%s detected. Flag and review them before removal.", sum(duplicated(df)), if(sum(duplicated(df))==1) "" else "s"), character(), 1L)
  constant_cols <- names(df)[vapply(df, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1))]
  if (length(constant_cols)) add("remove_constant", "Review constant columns", sprintf("%d column%s contain only one non-missing value.", length(constant_cols), if(length(constant_cols)==1) "" else "s"), constant_cols, 3L)
  if (length(num_cols)) {
    outlier_cols <- num_cols[vapply(df[num_cols], function(x) {
      x <- x[is.finite(x)]; if(length(x)<8L) return(FALSE)
      q <- stats::quantile(x,c(.25,.75),na.rm=TRUE,names=FALSE); i <- stats::IQR(x,na.rm=TRUE)
      if(!is.finite(i) || i==0) return(FALSE)
      any(x < q[1]-1.5*i | x > q[2]+1.5*i)
    }, logical(1))]
    if(length(outlier_cols)) add("outlier_flag", "Review unusual numeric values", sprintf("IQR screening found possible outliers in %d numeric column%s.", length(outlier_cols), if(length(outlier_cols)==1) "" else "s"), outlier_cols, 3L)
  }
  if(!length(out)) return(data.frame())
  ans <- do.call(rbind,out); ans[order(ans$priority,ans$title),,drop=FALSE]
}

sap_visual_param_summary <- function(step) {
  p <- step$params %||% list(); cols <- p$columns %||% p$column %||% p$keys %||% character()
  if(length(cols)) paste(cols,collapse=", ") else "Whole dataset"
}

sap_guided_profile <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0L) {
    return(list(
      metrics = list(rows = 0L, columns = 0L, missing = 0L, missing_pct = 0,
                     duplicates = 0L, empty_rows = 0L, empty_cols = 0L,
                     constant_cols = 0L, whitespace_cells = 0L, quality = 0),
      issues = data.frame()
    ))
  }

  rows <- nrow(df); cols <- ncol(df)
  total_cells <- max(1, rows * cols)
  missing_n <- sum(is.na(df))
  dup_n <- if (rows) sum(duplicated(df)) else 0L
  empty_rows <- if (rows) sum(rowSums(is.na(df)) == cols) else 0L
  empty_cols <- if (rows) sum(vapply(df, function(x) all(is.na(x)), logical(1))) else cols
  constant_cols <- sum(vapply(df, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1)))
  whitespace_cells <- 0L
  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.character(x)) {
      ok <- !is.na(x)
      whitespace_cells <- whitespace_cells + sum(ok & (x != trimws(x) | grepl("[[:space:]]{2,}", x)), na.rm = TRUE)
    }
  }
  q <- tryCatch(calculate_quality_score(df)$score, error = function(e) NA_real_)

  issue_rows <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    nonmissing <- x[!is.na(x)]
    class_name <- if (length(class(x))) class(x)[1] else typeof(x)
    miss <- sum(is.na(x))
    unique_n <- tryCatch(length(unique(nonmissing)), error = function(e) NA_integer_)
    blank <- if (is.character(x)) sum(!is.na(x) & !nzchar(trimws(x))) else 0L
    ws <- if (is.character(x)) sum(!is.na(x) & (x != trimws(x) | grepl("[[:space:]]{2,}", x)), na.rm = TRUE) else 0L
    sentinels <- if (is.atomic(x)) {
      y <- trimws(as.character(x))
      sum(!is.na(x) & y %in% c("NA", "N/A", "NULL", "-99", "99", "999", "."), na.rm = TRUE)
    } else 0L
    data.frame(
      variable = nm,
      type = class_name,
      missing = miss,
      missing_pct = if (rows) round(100 * miss / rows, 1) else 0,
      unique = unique_n,
      blank_text = blank,
      whitespace_flags = ws,
      possible_missing_codes = sentinels,
      constant = if (length(nonmissing)) length(unique(nonmissing)) <= 1L else TRUE,
      stringsAsFactors = FALSE
    )
  })

  list(
    metrics = list(
      rows = rows, columns = cols, missing = missing_n,
      missing_pct = round(100 * missing_n / total_cells, 1),
      duplicates = dup_n, empty_rows = empty_rows, empty_cols = empty_cols,
      constant_cols = constant_cols, whitespace_cells = whitespace_cells,
      quality = q
    ),
    issues = do.call(rbind, issue_rows)
  )
}

sap_parse_mapping_text <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character())
  lines <- unlist(strsplit(text, "\\r?\\n"))
  out <- character()
  for (line in lines) {
    if (!nzchar(trimws(line)) || !grepl("=", line, fixed = TRUE)) next
    pos <- regexpr("=", line, fixed = TRUE)[1]
    old <- trimws(substr(line, 1, pos - 1))
    new <- trimws(substr(line, pos + 1, nchar(line)))
    if (!nzchar(old)) next
    out[old] <- new
  }
  out
}

sap_coerce_replacement <- function(value, prototype) {
  if (is.factor(prototype)) return(as.character(value))
  if (inherits(prototype, "Date")) return(as.Date(value))
  if (inherits(prototype, c("POSIXct", "POSIXlt"))) return(as.POSIXct(value, tz = attr(prototype, "tzone") %||% "UTC"))
  if (is.integer(prototype)) return(as.integer(value))
  if (is.numeric(prototype)) return(as.numeric(value))
  if (is.logical(prototype)) return(tolower(value) %in% c("true", "t", "1", "yes", "y"))
  as.character(value)
}

sap_guided_apply_action <- function(df, action, params = list()) {
  stopifnot(is.data.frame(df))
  before <- df
  code <- ""
  note <- ""

  need_cols <- function(cols) {
    cols <- intersect(as.character(cols %||% character()), names(df))
    if (!length(cols)) stop("Choose at least one valid variable.", call. = FALSE)
    cols
  }

  if (identical(action, "clean_names")) {
    df <- janitor::clean_names(df, case = "snake")
    code <- "df <- janitor::clean_names(df, case = \"snake\")"

  } else if (identical(action, "remove_empty")) {
    target <- params$target %||% "both"
    if (target %in% c("rows", "both")) df <- janitor::remove_empty(df, which = "rows")
    if (target %in% c("cols", "both")) df <- janitor::remove_empty(df, which = "cols")
    code <- sprintf("df <- janitor::remove_empty(df, which = %s)",
                    if (target == "both") "c(\"rows\", \"cols\")" else sprintf("\"%s\"", target))

  } else if (identical(action, "remove_constant")) {
    df <- janitor::remove_constant(df)
    code <- "df <- janitor::remove_constant(df)"

  } else if (identical(action, "header_row")) {
    row_number <- as.integer(params$row_number %||% 1L)
    if (is.na(row_number) || row_number < 1L || row_number > nrow(df)) stop("Header row is outside the worksheet.", call. = FALSE)
    df <- janitor::row_to_names(df, row_number = row_number, remove_row = TRUE, remove_rows_above = isTRUE(params$remove_above))
    df <- janitor::clean_names(df, case = "snake")
    code <- sprintf("df <- janitor::row_to_names(df, row_number = %d, remove_row = TRUE, remove_rows_above = %s) |> janitor::clean_names(case = \"snake\")",
                    row_number, if (isTRUE(params$remove_above)) "TRUE" else "FALSE")

  } else if (identical(action, "trim_squish")) {
    cols <- need_cols(params$columns)
    for (nm in cols) if (is.character(df[[nm]]) || is.factor(df[[nm]])) df[[nm]] <- stringr::str_squish(as.character(df[[nm]]))
    code <- sprintf("df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(c(%s)), stringr::str_squish))",
                    paste(sprintf("\"%s\"", cols), collapse = ", "))

  } else if (identical(action, "unicode_normalize")) {
    cols <- need_cols(params$columns)
    for (nm in cols) if (is.character(df[[nm]]) || is.factor(df[[nm]])) {
      df[[nm]] <- stringi::stri_enc_toutf8(as.character(df[[nm]]))
      df[[nm]] <- stringi::stri_trans_nfkc(df[[nm]])
    }
    code <- sprintf("df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(c(%s)), ~ stringi::stri_trans_nfkc(stringi::stri_enc_toutf8(.x))))",
                    paste(sprintf("\"%s\"", cols), collapse = ", "))

  } else if (identical(action, "transliterate_ascii")) {
    cols <- need_cols(params$columns)
    for (nm in cols) if (is.character(df[[nm]]) || is.factor(df[[nm]])) df[[nm]] <- stringi::stri_trans_general(as.character(df[[nm]]), "Latin-ASCII")
    code <- sprintf("df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(c(%s)), ~ stringi::stri_trans_general(.x, \"Latin-ASCII\")))",
                    paste(sprintf("\"%s\"", cols), collapse = ", "))
    note <- "Transliteration changes spelling. The original worksheet remains preserved in the workbook workspace."

  } else if (identical(action, "case_normalize")) {
    cols <- need_cols(params$columns)
    style <- params$style %||% "lower"
    fn <- switch(style, lower = stringr::str_to_lower, upper = stringr::str_to_upper, title = stringr::str_to_title, stringr::str_to_lower)
    for (nm in cols) if (is.character(df[[nm]]) || is.factor(df[[nm]])) df[[nm]] <- fn(as.character(df[[nm]]))
    code <- sprintf("# Apply stringr case normalization (%s) to: %s", style, paste(cols, collapse = ", "))

  } else if (identical(action, "replace_text")) {
    cols <- need_cols(params$columns %||% params$column)
    find <- as.character(params$find %||% ""); repl <- as.character(params$replacement %||% "")
    if (!nzchar(find)) stop("Enter text to find.", call. = FALSE)
    for (col in cols) {
      if (!is.character(df[[col]]) && !is.factor(df[[col]])) stop(sprintf("'%s' is not a text/category column.", col), call. = FALSE)
      was_factor <- is.factor(df[[col]]); x <- as.character(df[[col]])
      df[[col]] <- stringr::str_replace_all(x, stringr::fixed(find), repl)
      if (was_factor) df[[col]] <- factor(df[[col]])
    }
    code <- sprintf("# Find/replace on selected columns: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "sentinel_na")) {
    cols <- need_cols(params$columns)
    codes <- trimws(unlist(strsplit(as.character(params$codes %||% ""), "[,\\n]")))
    codes <- unique(codes[nzchar(codes)])
    if (!length(codes)) stop("Enter one or more documented missing-value codes.", call. = FALSE)
    for (nm in cols) {
      x <- df[[nm]]
      match_mask <- !is.na(x) & trimws(as.character(x)) %in% codes
      if (any(match_mask)) x[match_mask] <- NA
      df[[nm]] <- x
    }
    code <- sprintf("# Convert documented missing codes c(%s) to NA in: %s",
                    paste(sprintf("\"%s\"", codes), collapse = ", "), paste(cols, collapse = ", "))

  } else if (identical(action, "fill_missing")) {
    cols <- need_cols(params$columns)
    direction <- params$direction %||% "down"
    df <- tidyr::fill(df, dplyr::all_of(cols), .direction = direction)
    code <- sprintf("df <- tidyr::fill(df, dplyr::all_of(c(%s)), .direction = \"%s\")",
                    paste(sprintf("\"%s\"", cols), collapse = ", "), direction)

  } else if (identical(action, "replace_na")) {
    cols <- need_cols(params$columns %||% params$column)
    for (col in cols) {
      value <- sap_coerce_replacement(as.character(params$value %||% ""), df[[col]])
      x <- df[[col]]; was_factor <- is.factor(x); if (was_factor) x <- as.character(x)
      x[is.na(x)] <- value; df[[col]] <- if (was_factor) factor(x) else x
    }
    code <- sprintf("# Replace missing cells in selected columns: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "parse_number")) {
    cols <- need_cols(params$columns %||% params$column)
    decimal_mark <- as.character(params$decimal_mark %||% "."); grouping_mark <- as.character(params$grouping_mark %||% ",")
    for (col in cols) df[[col]] <- readr::parse_number(as.character(df[[col]]), locale = readr::locale(decimal_mark = decimal_mark, grouping_mark = grouping_mark))
    code <- sprintf("# Parse selected columns as numbers: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "parse_integer")) {
    cols <- need_cols(params$columns %||% params$column)
    for (col in cols) df[[col]] <- readr::parse_integer(as.character(df[[col]]))
    code <- sprintf("# Parse selected columns as whole numbers: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "parse_date")) {
    cols <- need_cols(params$columns %||% params$column)
    orders <- trimws(unlist(strsplit(as.character(params$orders %||% "ymd,dmy"), "[,; ]+"))); orders <- orders[nzchar(orders)]
    tz <- as.character(params$tz %||% "UTC")
    for (col in cols) df[[col]] <- suppressWarnings(lubridate::parse_date_time(as.character(df[[col]]), orders=orders, tz=tz, quiet=TRUE))
    code <- sprintf("# Parse selected date/time columns: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "category_recode")) {
    cols <- need_cols(params$columns %||% params$column)
    mapping <- sap_parse_mapping_text(params$mapping %||% "")
    if (!length(mapping)) stop("Enter mappings as Old value = New value, one per line.", call. = FALSE)
    for (col in cols) {
      x <- as.character(df[[col]])
      for (old in names(mapping)) {
        new <- unname(mapping[[old]]); idx <- !is.na(x) & x == old
        x[idx] <- if (identical(toupper(new), "NA")) NA_character_ else new
      }
      if (isTRUE(params$as_factor)) x <- factor(x); df[[col]] <- x
    }
    code <- sprintf("# Recode selected category columns: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "remove_duplicates")) {
    mode <- params$mode %||% "exact"
    keep <- params$keep %||% "flag"
    if (mode == "key") {
      keys <- need_cols(params$keys)
      key_df <- df[, keys, drop = FALSE]
      dup <- if (keep == "last") duplicated(key_df, fromLast = TRUE) else duplicated(key_df)
    } else {
      dup <- if (keep == "last") duplicated(df, fromLast = TRUE) else duplicated(df)
    }
    if (keep == "flag") {
      df$.duplicate_flag <- duplicated(if (mode == "key") df[, need_cols(params$keys), drop = FALSE] else df) |
        duplicated(if (mode == "key") df[, need_cols(params$keys), drop = FALSE] else df, fromLast = TRUE)
    } else {
      df <- df[!dup, , drop = FALSE]
    }
    code <- if (keep == "flag") "# Flag duplicate groups before deciding whether to remove them" else "df <- dplyr::distinct(df, ..., .keep_all = TRUE)"

  } else if (identical(action, "validate_range")) {
    cols <- need_cols(params$columns %||% params$column)
    if (!all(vapply(df[cols], is.numeric, logical(1)))) stop("Range validation requires numeric columns.", call. = FALSE)
    lo <- as.numeric(params$min); hi <- as.numeric(params$max)
    if (!is.finite(lo) || !is.finite(hi) || lo > hi) stop("Enter a valid minimum and maximum.", call. = FALSE)
    combined <- rep(FALSE,nrow(df))
    for(col in cols){ flag <- !is.na(df[[col]]) & !dplyr::between(df[[col]],lo,hi); combined <- combined | flag; if(!identical(params$handling,"filter")) df[[paste0(col,"_range_issue")]] <- flag }
    if(identical(params$handling,"filter")) df <- df[!combined,,drop=FALSE]
    code <- sprintf("# Range validation on selected numeric columns: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "validate_allowed")) {
    cols <- need_cols(params$columns %||% params$column)
    allowed <- trimws(unlist(strsplit(as.character(params$values %||% ""), "[,\\n]"))); allowed <- unique(allowed[nzchar(allowed)])
    if(!length(allowed)) stop("Enter at least one allowed value.", call. = FALSE)
    combined <- rep(FALSE,nrow(df))
    for(col in cols){ flag <- !is.na(df[[col]]) & !as.character(df[[col]]) %in% allowed; combined <- combined | flag; if(!identical(params$handling,"filter")) df[[paste0(col,"_allowed_issue")]] <- flag }
    if(identical(params$handling,"filter")) df <- df[!combined,,drop=FALSE]
    code <- sprintf("# Allowed-value validation on selected columns: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "regex_validate")) {
    cols <- need_cols(params$columns %||% params$column); pattern <- as.character(params$pattern %||% "")
    if(!nzchar(pattern)) stop("Choose a format rule.", call. = FALSE)
    combined <- rep(FALSE,nrow(df))
    for(col in cols){ matches <- tryCatch(stringr::str_detect(as.character(df[[col]]), stringr::regex(pattern)), error=function(e) stop(conditionMessage(e),call.=FALSE)); flag <- !is.na(df[[col]]) & !matches; combined <- combined | flag; if(!identical(params$handling,"filter")) df[[paste0(col,"_format_issue")]] <- flag }
    if(identical(params$handling,"filter")) df <- df[!combined,,drop=FALSE]
    code <- sprintf("# Format validation on selected text columns: %s", paste(cols, collapse = ", "))

  } else if (identical(action, "remove_characters")) {
    cols <- need_cols(params$columns); mode <- params$mode %||% "punctuation"
    for(col in cols){ x <- as.character(df[[col]]); df[[col]] <- switch(mode, punctuation=stringr::str_remove_all(x,"[[:punct:]]"), digits=stringr::str_remove_all(x,"[^0-9+]"), letters=stringr::str_squish(stringr::str_remove_all(x,"[^[:alpha:] ]")), x) }
    code <- sprintf("# Remove unwanted characters from: %s", paste(cols,collapse=", "))

  } else if (identical(action, "drop_missing_rows")) {
    cols <- need_cols(params$columns); rule <- params$rule %||% "any"; miss <- is.na(df[,cols,drop=FALSE])
    drop <- if(identical(rule,"all")) rowSums(miss)==length(cols) else rowSums(miss)>0L
    df <- df[!drop,,drop=FALSE]; note <- sprintf("%d row(s) would be removed by this missing-value rule.",sum(drop)); code <- sprintf("# Remove rows based on missingness in: %s",paste(cols,collapse=", "))

  } else if (identical(action, "outlier_flag")) {
    cols <- need_cols(params$columns); if(!all(vapply(df[cols],is.numeric,logical(1)))) stop("Outlier review requires numeric columns.",call.=FALSE)
    method <- params$method %||% "iqr"; threshold <- as.numeric(params$threshold %||% if(method=="z") 3 else 1.5)
    for(col in cols){ x <- df[[col]]; flag <- rep(FALSE,length(x)); if(identical(method,"z")){sdv<-stats::sd(x,na.rm=TRUE);mu<-mean(x,na.rm=TRUE);if(is.finite(sdv)&&sdv>0) flag<-!is.na(x)&abs((x-mu)/sdv)>threshold}else{q<-stats::quantile(x,c(.25,.75),na.rm=TRUE,names=FALSE);i<-stats::IQR(x,na.rm=TRUE);if(all(is.finite(q))&&is.finite(i)) flag<-!is.na(x)&(x<q[1]-threshold*i|x>q[2]+threshold*i)};df[[paste0(col,"_outlier_issue")]]<-flag }
    code <- sprintf("# Flag possible outliers in: %s",paste(cols,collapse=", "))

  } else if (identical(action, "cross_field")) {
    cols <- need_cols(params$columns); if(length(cols)!=2L) stop("Select exactly two columns for this check.",call.=FALSE)
    op <- params$operator %||% "le"; a<-df[[cols[1]]]; b<-df[[cols[2]]]
    valid <- switch(op,le=a<=b,lt=a<b,ge=a>=b,gt=a>b,eq=a==b,ne=a!=b,a<=b); df$.cross_field_issue <- !is.na(a)&!is.na(b)&!valid
    code <- sprintf("# Check logical relationship between %s and %s",cols[1],cols[2])

  } else if (identical(action, "pivot_longer")) {
    cols <- need_cols(params$columns)
    names_to <- janitor::make_clean_names(as.character(params$names_to %||% "variable"))[1]
    values_to <- janitor::make_clean_names(as.character(params$values_to %||% "value"))[1]
    if (identical(names_to, values_to)) stop("The name and value output columns must be different.", call. = FALSE)
    df <- tidyr::pivot_longer(df, cols = dplyr::all_of(cols), names_to = names_to, values_to = values_to)
    code <- sprintf("df <- tidyr::pivot_longer(df, dplyr::all_of(c(%s)), names_to = \"%s\", values_to = \"%s\")",
                    paste(sprintf("\"%s\"", cols), collapse = ", "), names_to, values_to)

  } else if (identical(action, "split_column")) {
    col <- need_cols(params$column)[1]
    delim <- as.character(params$delimiter %||% ",")
    new_names <- trimws(unlist(strsplit(as.character(params$new_names %||% "part_1,part_2"), ",")))
    new_names <- janitor::make_clean_names(new_names[nzchar(new_names)])
    if (length(new_names) < 2L) stop("Enter at least two new column names.", call. = FALSE)
    df <- tidyr::separate_wider_delim(df, cols = dplyr::all_of(col), delim = delim, names = new_names, too_many = "merge", too_few = "align_start")
    code <- sprintf("df <- tidyr::separate_wider_delim(df, %s, delim = \"%s\", names = c(%s), too_many = \"merge\")",
                    col, delim, paste(sprintf("\"%s\"", new_names), collapse = ", "))

  } else if (identical(action, "unite_columns")) {
    cols <- need_cols(params$columns)
    new_name <- janitor::make_clean_names(as.character(params$new_name %||% "combined"))[1]
    sep <- as.character(params$separator %||% "_")
    df <- tidyr::unite(df, col = new_name, dplyr::all_of(cols), sep = sep, remove = isTRUE(params$remove_sources), na.rm = isTRUE(params$na_rm))
    code <- sprintf("df <- tidyr::unite(df, %s, dplyr::all_of(c(%s)), sep = \"%s\", remove = %s, na.rm = %s)",
                    new_name, paste(sprintf("\"%s\"", cols), collapse = ", "), sep,
                    if (isTRUE(params$remove_sources)) "TRUE" else "FALSE",
                    if (isTRUE(params$na_rm)) "TRUE" else "FALSE")

  } else {
    stop("Unknown guided cleaning action.", call. = FALSE)
  }

  changed_cells <- NA_integer_
  if (identical(dim(before), dim(df)) && identical(names(before), names(df))) {
    changed_cells <- sum(vapply(names(df), function(nm) {
      a <- before[[nm]]; b <- df[[nm]]
      sum((is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & as.character(a) != as.character(b)))
    }, integer(1)))
  }
  list(
    data = as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE),
    rows_before = nrow(before), rows_after = nrow(df),
    cols_before = ncol(before), cols_after = ncol(df),
    changed_cells = changed_cells,
    changed = !identical(before, df), code = code, note = note
  )
}

sap_workbook_merge_readiness <- function(sheets, key = NULL) {
  if (is.null(sheets) || !length(sheets)) return(data.frame())
  sheets <- sheets[vapply(sheets, is.data.frame, logical(1))]
  if (!length(sheets)) return(data.frame())
  base_name <- names(sheets)[1]
  base <- sheets[[1]]
  base_classes <- vapply(base, function(x) class(x)[1], character(1))
  union_names <- unique(unlist(lapply(sheets, names), use.names = FALSE))

  rows <- lapply(names(sheets), function(nm) {
    x <- sheets[[nm]]
    classes <- vapply(x, function(v) class(v)[1], character(1))
    common <- intersect(names(base), names(x))
    mismatches <- common[vapply(common, function(v) !identical(base_classes[[v]], classes[[v]]), logical(1))]
    key_dups <- NA_integer_
    if (!is.null(key) && length(key) && all(key %in% names(x))) key_dups <- sum(duplicated(x[, key, drop = FALSE]))
    data.frame(
      worksheet = nm,
      rows = nrow(x), columns = ncol(x),
      missing_vs_first = paste(setdiff(names(base), names(x)), collapse = ", "),
      extra_vs_first = paste(setdiff(names(x), names(base)), collapse = ", "),
      type_mismatches = paste(mismatches, collapse = ", "),
      key_duplicates = key_dups,
      append_ready = length(mismatches) == 0L,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

sap_harmonize_sheets_for_append <- function(sheets) {
  all_names <- unique(unlist(lapply(sheets, names), use.names = FALSE))
  target_type <- setNames(rep("character", length(all_names)), all_names)
  for (nm in all_names) {
    classes <- unique(vapply(sheets, function(df) {
      if (!nm %in% names(df)) return("missing")
      x <- df[[nm]]
      if (is.integer(x)) "integer" else if (is.numeric(x)) "numeric" else if (inherits(x, "Date")) "Date" else if (inherits(x, c("POSIXct", "POSIXlt"))) "datetime" else if (is.logical(x)) "logical" else "character"
    }, character(1)))
    classes <- setdiff(classes, "missing")
    target_type[[nm]] <- if (!length(classes)) "character" else if (all(classes %in% c("integer", "numeric"))) "numeric" else if (length(classes) == 1L) classes else "character"
  }

  lapply(sheets, function(df) {
    for (nm in setdiff(all_names, names(df))) df[[nm]] <- NA
    df <- df[, all_names, drop = FALSE]
    for (nm in all_names) {
      typ <- target_type[[nm]]
      if (typ == "character") df[[nm]] <- as.character(df[[nm]])
      else if (typ == "numeric") df[[nm]] <- as.numeric(df[[nm]])
      else if (typ == "logical") df[[nm]] <- as.logical(df[[nm]])
      else if (typ == "Date") df[[nm]] <- as.Date(df[[nm]])
      else if (typ == "datetime") df[[nm]] <- as.POSIXct(df[[nm]], tz = "UTC")
    }
    df
  })
}

sap_merge_workbook_sheets <- function(sheets, strategy = "append", key = NULL,
                                      add_source = TRUE, join_type = "full") {
  if (is.null(sheets) || length(sheets) < 2L) stop("Choose at least two worksheets to merge.", call. = FALSE)
  sheets <- sheets[vapply(sheets, is.data.frame, logical(1))]
  if (length(sheets) < 2L) stop("At least two valid worksheets are required.", call. = FALSE)

  if (identical(strategy, "append")) {
    ready <- sap_harmonize_sheets_for_append(sheets)
    if (isTRUE(add_source)) {
      for (nm in names(ready)) ready[[nm]]$.source_sheet <- nm
    }
    return(dplyr::bind_rows(ready))
  }

  key <- as.character(key %||% character())
  if (!length(key)) stop("Choose one or more join-key variables.", call. = FALSE)
  if (!all(vapply(sheets, function(df) all(key %in% names(df)), logical(1)))) stop("Every selected worksheet must contain the join key(s).", call. = FALSE)

  safe_prefix <- function(x) janitor::make_clean_names(x)[1]
  prepared <- lapply(names(sheets), function(nm) {
    df <- sheets[[nm]]
    nonkey <- setdiff(names(df), key)
    if (length(nonkey)) names(df)[match(nonkey, names(df))] <- paste0(safe_prefix(nm), "__", janitor::make_clean_names(nonkey))
    df
  })
  names(prepared) <- names(sheets)
  join_fun <- switch(join_type,
    left = dplyr::left_join,
    inner = dplyr::inner_join,
    full = dplyr::full_join,
    dplyr::full_join
  )
  Reduce(function(x, y) join_fun(x, y, by = key), prepared)
}

sap_guided_cleaning_ui <- function(ns) {
  tool_js <- sprintf(
    paste(
      "document.addEventListener('click', function(event) {",
      "  var tool = event.target.closest('.sap-clean-tool');",
      "  if (tool && tool.dataset.input === '%s') {",
      "    Shiny.setInputValue('%s', tool.dataset.action, {priority: 'event'});",
      "  }",
      "  var suggestion = event.target.closest('.sap-clean-suggestion');",
      "  if (suggestion && suggestion.dataset.input === '%s') {",
      "    Shiny.setInputValue('%s', {",
      "      action: suggestion.dataset.action,",
      "      columns: suggestion.dataset.columns || ''",
      "    }, {priority: 'event'});",
      "  }",
      "});",
      sep = "\n"
    ),
    ns("guided_tool_click"), ns("guided_tool_click"),
    ns("guided_suggestion_click"), ns("guided_suggestion_click")
  )

  nav_panel(
    "Visual Workbench",
    icon = icon("sliders"),
    div(
      class = "sap-cleaning-workbench",
      tags$script(HTML(tool_js)),
      div(
        class = "sap-clean-hero",
        div(
          class = "sap-clean-hero-heading",
          div(
            div(class = "sap-clean-kicker", "Visual Cleaning Workbench"),
            h3("Shape clean, analysis-ready data without code"),
            p(
              "Clean data visually — no coding required. Choose related columns, review every change, and keep your original import protected."
            )
          ),
          span(class = "sap-clean-safety-badge", icon("eye"), " Preview before applying")
        ),
        div(
          class = "sap-clean-steps",
          div(class = "sap-clean-step", shiny::tags$b("1 · Select"), "Choose the columns to improve."),
          div(class = "sap-clean-step", shiny::tags$b("2 · Clean"), "Pick a clear, purpose-built tool."),
          div(class = "sap-clean-step", shiny::tags$b("3 · Preview"), "Inspect the changes before saving."),
          div(class = "sap-clean-step", shiny::tags$b("4 · Apply"), "Apply safely or undo later.")
        )
      ),
      div(
        class = "sap-clean-tabs-shell",
        navset_tab(
          id = ns("guided_tabs"),
          nav_panel(
            "Clean Data",
            div(
              class = "sap-clean-tab-content",
              uiOutput(ns("guided_profile_cards")),
              uiOutput(ns("guided_suggestions")),
              div(
                class = "sap-clean-workspace-grid",
                div(
                  class = "sap-clean-sidebar",
                  div(
                    class = "sap-clean-panel sap-clean-selection-panel",
                    div(class = "sap-clean-title", "1. Select columns"),
                    div(class = "sap-clean-panel-note", "Start with one column or a group of related variables."),
                    uiOutput(ns("guided_active_sheet_inline")),
                    uiOutput(ns("guided_column_picker")),
                    div(
                      class = "sap-clean-shortcuts",
                      actionButton(ns("guided_select_text"), "All text", class = "btn btn-sm btn-outline-secondary"),
                      actionButton(ns("guided_select_numeric"), "All numbers", class = "btn btn-sm btn-outline-secondary"),
                      actionButton(ns("guided_select_dates"), "All dates", class = "btn btn-sm btn-outline-secondary"),
                      actionButton(ns("guided_select_missing"), "Has missing", class = "btn btn-sm btn-outline-secondary"),
                      actionButton(ns("guided_clear_columns"), "Clear", class = "btn btn-sm btn-light")
                    ),
                    div(class = "sap-clean-selection-summary", uiOutput(ns("guided_column_selection_summary")))
                  ),
                  div(
                    class = "sap-clean-panel sap-clean-health-panel",
                    div(class = "sap-clean-title", "Column health"),
                    div(class = "sap-clean-panel-note", "Review missing values and quality flags without crowding the workspace."),
                    div(class = "sap-clean-table-wrap", DT::DTOutput(ns("guided_issue_table")))
                  )
                ),
                div(
                  class = "sap-clean-main",
                  div(
                    class = "sap-clean-panel sap-clean-tool-panel",
                    div(class = "sap-clean-title", "2. Choose a cleaning tool"),
                    div(class = "sap-clean-panel-note", "Choose a category, then select the cleaning action you need."),
                    div(
                      class = "sap-clean-group-picker",
                      shinyWidgets::radioGroupButtons(
                        ns("guided_tool_group"),
                        label = NULL,
                        choices = c("Text", "Missing", "Numbers & dates", "Categories", "Validation", "Structure", "Reshape"),
                        selected = "Text",
                        size = "sm",
                        justified = FALSE,
                        status = "primary"
                      )
                    ),
                    div(
                      class = "sap-clean-hidden-action",
                      selectInput(
                        ns("guided_action"),
                        label = NULL,
                        choices = setNames(sap_visual_action_catalog()$action, sap_visual_action_catalog()$label),
                        selected = "trim_squish"
                      )
                    ),
                    uiOutput(ns("guided_toolbox"))
                  ),
                  div(
                    class = "sap-clean-panel sap-clean-config-panel",
                    div(class = "sap-clean-title", "3. Configure and review"),
                    uiOutput(ns("guided_action_explanation")),
                    div(class = "sap-clean-parameters", uiOutput(ns("guided_action_params"))),
                    div(
                      class = "sap-clean-actions",
                      actionButton(ns("guided_preview_action"), "Preview changes", class = "btn btn-outline-primary", icon = icon("eye")),
                      actionButton(ns("guided_apply_action"), "Apply to dataset", class = "btn btn-primary", icon = icon("check"))
                    )
                  )
                )
              ),
              div(
                class = "sap-preview-shell sap-clean-comparison-panel",
                div(
                  class = "sap-preview-head",
                  div(
                    strong("4. Compare before and after"),
                    div(class = "sap-clean-panel-note", "The working dataset changes only after you choose Apply.")
                  ),
                  span(class = "sap-clean-safe-label", icon("eye"), " Safe preview")
                ),
                div(
                  class = "sap-clean-comparison-body",
                  uiOutput(ns("guided_preview_summary")),
                  uiOutput(ns("guided_preview_note")),
                  div(
                    class = "sap-clean-preview-grid",
                    div(
                      class = "sap-clean-preview-pane",
                      div(class = "sap-clean-preview-title", "Current data"),
                      div(class = "sap-clean-table-wrap", DT::DTOutput(ns("guided_before_preview")))
                    ),
                    div(
                      class = "sap-clean-preview-pane",
                      div(class = "sap-clean-preview-title", "After preview"),
                      uiOutput(ns("guided_after_preview_state")),
                      div(class = "sap-clean-table-wrap", DT::DTOutput(ns("guided_action_preview")))
                    )
                  )
                )
              )
            )
          ),
          nav_panel("Workbook",div(class="sap-clean-tab-content",uiOutput(ns("guided_workbook_summary")),uiOutput(ns("guided_workbook_nav")),DT::DTOutput(ns("guided_sheet_status")),div(class="d-flex gap-2 flex-wrap mt-3",downloadButton(ns("guided_download_clean_workbook"),"Download cleaned workbook",class="btn btn-success"),downloadButton(ns("guided_download_quality_workbook"),"Download quality report",class="btn btn-outline-success")),hr(),h5("Current worksheet preview"),DT::DTOutput(ns("guided_sheet_preview")))),
          nav_panel("Reuse Cleaning",div(class="sap-clean-tab-content",div(class="alert alert-info",strong("Reuse the same cleaning choices on similar worksheets.")," The saved recipe stores your clicks and settings internally; users do not need to write or see code."),div(class="row g-3",
            div(class="col-lg-6",div(class="sap-clean-panel",h5("Current cleaning steps"),DT::DTOutput(ns("guided_recipe_steps")),textInput(ns("guided_recipe_name"),"Recipe name",value="Workbook cleaning recipe"),actionButton(ns("guided_save_recipe"),"Save this recipe",class="btn btn-primary",icon=icon("save")))),
            div(class="col-lg-6",div(class="sap-clean-panel",h5("Apply to other worksheets"),selectInput(ns("guided_recipe_select"),"Saved recipe",choices=""),uiOutput(ns("guided_recipe_targets_ui")),checkboxInput(ns("guided_recipe_stop_error"),"Stop a worksheet when a step is incompatible",TRUE),actionButton(ns("guided_apply_recipe"),"Apply to selected worksheets",class="btn btn-success"),uiOutput(ns("guided_recipe_result"))))
          ))),
          nav_panel("Merge Worksheets",div(class="sap-clean-tab-content",div(class="alert alert-warning",strong("Clean first, merge second.")," The system checks column structure and key risks before building a combined dataset."),div(class="row g-3",
            div(class="col-lg-4",div(class="sap-clean-panel",h5("Merge setup"),uiOutput(ns("guided_merge_sheets_ui")),radioButtons(ns("guided_merge_strategy"),"How should worksheets combine?",choices=c("Stack rows"="append","Join by matching key(s)"="join")),conditionalPanel(condition="input.guided_merge_strategy == 'append'",ns=ns,checkboxInput(ns("guided_merge_source"),"Keep source worksheet name",TRUE)),conditionalPanel(condition="input.guided_merge_strategy == 'join'",ns=ns,uiOutput(ns("guided_merge_key_ui")),selectInput(ns("guided_join_type"),"Join type",choices=c("Keep all records"="full","Keep first worksheet records"="left","Keep only matched records"="inner"))),actionButton(ns("guided_check_merge"),"Check readiness",class="btn btn-outline-primary"),actionButton(ns("guided_preview_merge"),"Preview merge",class="btn btn-primary mt-2"),actionButton(ns("guided_apply_merge"),"Use merged data",class="btn btn-success mt-2"))),
            div(class="col-lg-8",div(class="sap-clean-panel",h5("Merge readiness"),uiOutput(ns("guided_merge_readiness_summary")),DT::DTOutput(ns("guided_merge_readiness")),hr(),h6("Merged preview"),DT::DTOutput(ns("guided_merge_preview")),div(class="d-flex gap-2 mt-2",downloadButton(ns("guided_download_merged_csv"),"Download CSV",class="btn btn-outline-success"),downloadButton(ns("guided_download_merged_xlsx"),"Download Excel",class="btn btn-outline-success"))))
          )))
        )
      )
    )
  )
}

sap_guided_cleaning_server <- function(input, output, session, ns, app_state, services,
                                       cleaning_state, validate_data_available) {
  catalog <- sap_visual_action_catalog()
  preview_result <- reactiveVal(NULL)
  merge_preview <- reactiveVal(NULL)
  merge_readiness <- reactiveVal(NULL)
  recipe_message <- reactiveVal(NULL)

  active_key <- reactive({
    nm <- app_state$active_sheet %||% ""
    if (nzchar(nm) && length(app_state$workbook_sheets %||% list())) nm else "__single_dataset__"
  })

  get_guided_steps <- function(key = active_key()) {
    all_steps <- app_state$guided_cleaning_steps %||% list()
    all_steps[[key]] %||% list()
  }

  set_guided_steps <- function(steps, key = active_key()) {
    all_steps <- isolate(app_state$guided_cleaning_steps %||% list())
    all_steps[[key]] <- steps
    app_state$guided_cleaning_steps <- all_steps
    invisible(steps)
  }

  current_workbook <- reactive({
    sheets <- app_state$workbook_sheets %||% list()
    if (!length(sheets)) {
      df <- tryCatch(validate_data_available(), error = function(e) NULL)
      if (is.data.frame(df)) sheets <- setNames(list(df), app_state$dataset_name %||% "Current dataset")
    }
    sheets
  })

  sync_current_to_workbook <- function() {
    nm <- isolate(app_state$active_sheet %||% "")
    sheets <- isolate(app_state$workbook_sheets %||% list())
    if (nzchar(nm) && length(sheets) && nm %in% names(sheets) && is.data.frame(isolate(cleaning_state$current_data))) {
      sheets[[nm]] <- isolate(cleaning_state$current_data)
      app_state$workbook_sheets <- sheets
      histories <- isolate(app_state$workbook_cleaning_histories %||% list())
      histories[[nm]] <- isolate(cleaning_state$history)
      app_state$workbook_cleaning_histories <- histories
      indices <- isolate(app_state$workbook_history_indices %||% list())
      indices[[nm]] <- isolate(cleaning_state$history_index)
      app_state$workbook_history_indices <- indices
    }
    invisible(NULL)
  }

  observeEvent(cleaning_state$current_data, {
    if (!isTRUE(app_state$data_imported) || is.null(cleaning_state$current_data)) return(invisible(NULL))
    sync_current_to_workbook()
  }, ignoreInit = TRUE)

  output$guided_workbook_summary <- renderUI({
    sheets <- current_workbook()
    if (!length(sheets)) return(div(class = "alert alert-info", "Import data to start a cleaning workspace."))
    cleaned_steps <- sum(vapply(names(sheets), function(nm) length((app_state$guided_cleaning_steps %||% list())[[nm]] %||% list()), integer(1)))
    active_sheet <- app_state$active_sheet %||% ""
    if (!nzchar(active_sheet) || !active_sheet %in% names(sheets)) active_sheet <- names(sheets)[1]
    raw_status <- if (length(app_state$workbook_raw_sheets %||% list())) "Preserved" else "Single dataset"
    div(class = "sap-workbook-metric-grid",
      div(class = "sap-workbook-metric sap-tone-cyan",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Worksheets"), span(class="sap-clean-metric-icon",icon("table-cells"))),
        div(class="sap-clean-metric-value", length(sheets)), div(class="sap-clean-metric-note","Sheets available in this cleaning workspace")
      ),
      div(class = "sap-workbook-metric sap-tone-violet",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Active sheet"), span(class="sap-clean-metric-icon",icon("layer-group"))),
        div(class="sap-clean-metric-value", style="font-size:1.12rem;overflow-wrap:anywhere;", active_sheet), div(class="sap-clean-metric-note","Currently open for cleaning")
      ),
      div(class = "sap-workbook-metric sap-tone-teal",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Cleaning steps"), span(class="sap-clean-metric-icon",icon("list-check"))),
        div(class="sap-clean-metric-value", cleaned_steps), div(class="sap-clean-metric-note","Applied across the workbook")
      ),
      div(class = "sap-workbook-metric sap-tone-amber",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Raw copies"), span(class="sap-clean-metric-icon",icon("copy"))),
        div(class="sap-clean-metric-value", style="font-size:1.12rem;", raw_status), div(class="sap-clean-metric-note","Original data stay protected")
      )
    )
  })

  output$guided_workbook_nav <- renderUI({
    sheets <- current_workbook()
    if (!length(sheets)) return(NULL)
    selected <- app_state$active_sheet %||% names(sheets)[1]
    if (!selected %in% names(sheets)) selected <- names(sheets)[1]
    div(class = "card mb-3", div(class = "card-body",
      div(class = "row align-items-end",
        div(class = "col-md-7", selectInput(ns("guided_sheet_selector"), "Worksheet to clean", choices = names(sheets), selected = selected)),
        div(class = "col-md-5",
          div(class = "d-flex gap-2",
            actionButton(ns("guided_prev_sheet"), "Previous", class = "btn btn-outline-secondary", icon = icon("arrow-left")),
            actionButton(ns("guided_next_sheet"), "Next", class = "btn btn-outline-secondary", icon = icon("arrow-right"))
          )
        )
      ),
      span(class = "small text-muted", "Switching worksheets saves the current cleaned copy and restores that sheet's own cleaning history.")
    ))
  })

  switch_sheet <- function(sel) {
    sheets <- isolate(app_state$workbook_sheets %||% list())
    if (!length(sheets) || !sel %in% names(sheets)) return(invisible(FALSE))
    old <- isolate(app_state$active_sheet %||% "")
    if (nzchar(old) && old %in% names(sheets) && is.data.frame(isolate(cleaning_state$current_data))) {
      sheets[[old]] <- isolate(cleaning_state$current_data)
      histories <- isolate(app_state$workbook_cleaning_histories %||% list())
      histories[[old]] <- isolate(cleaning_state$history)
      app_state$workbook_cleaning_histories <- histories
      indices <- isolate(app_state$workbook_history_indices %||% list())
      indices[[old]] <- isolate(cleaning_state$history_index)
      app_state$workbook_history_indices <- indices
    }
    raw_sheets <- isolate(app_state$workbook_raw_sheets %||% list())
    histories <- isolate(app_state$workbook_cleaning_histories %||% list())
    indices <- isolate(app_state$workbook_history_indices %||% list())
    app_state$workbook_sheets <- sheets
    app_state$active_sheet <- sel
    cleaning_state$original_data <- raw_sheets[[sel]] %||% sheets[[sel]]
    cleaning_state$current_data <- sheets[[sel]]
    cleaning_state$history <- histories[[sel]] %||% list()
    cleaning_state$history_index <- as.integer(indices[[sel]] %||% length(cleaning_state$history))
    cleaning_state$history_index <- max(0L, min(cleaning_state$history_index, length(cleaning_state$history)))
    cleaning_state$variable_types <- tryCatch(detect_variable_types(sheets[[sel]]), error = function(e) NULL)
    app_state$raw_data <- raw_sheets[[sel]] %||% sheets[[sel]]
    app_state$processed_data <- sheets[[sel]]
    app_state$dataset_name <- paste0(tools::file_path_sans_ext(app_state$workbook_name %||% "workbook"), " - ", sel)
    app_state$variable_types <- cleaning_state$variable_types
    app_state$last_action <- sprintf("Opened worksheet '%s' for cleaning", sel)
    preview_result(NULL)
    merge_preview(NULL)
    invisible(TRUE)
  }

  observeEvent(input$guided_sheet_selector, {
    sel <- input$guided_sheet_selector
    if (!is.null(sel) && nzchar(sel) && !identical(sel, isolate(app_state$active_sheet))) switch_sheet(sel)
  }, ignoreInit = TRUE)

  observeEvent(input$guided_prev_sheet, {
    sheets <- names(current_workbook()); req(length(sheets))
    cur <- app_state$active_sheet %||% sheets[1]
    idx <- match(cur, sheets); if (is.na(idx)) idx <- 1L
    idx <- if (idx <= 1L) length(sheets) else idx - 1L
    switch_sheet(sheets[idx])
  })
  observeEvent(input$guided_next_sheet, {
    sheets <- names(current_workbook()); req(length(sheets))
    cur <- app_state$active_sheet %||% sheets[1]
    idx <- match(cur, sheets); if (is.na(idx)) idx <- 1L
    idx <- if (idx >= length(sheets)) 1L else idx + 1L
    switch_sheet(sheets[idx])
  })

  output$guided_sheet_status <- DT::renderDT({
    sheets <- current_workbook(); req(length(sheets))
    steps <- app_state$guided_cleaning_steps %||% list()
    rows <- lapply(names(sheets), function(nm) {
      p <- sap_guided_profile(sheets[[nm]])$metrics
      data.frame(
        Worksheet = nm, Rows = p$rows, Columns = p$columns,
        `Missing %` = p$missing_pct, Duplicates = p$duplicates,
        `Quality score` = p$quality, `Guided steps` = length(steps[[nm]] %||% list()),
        Status = if (identical(nm, app_state$active_sheet)) "Currently cleaning" else if (length(steps[[nm]] %||% list())) "Edited" else "Not yet cleaned",
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })
    DT::datatable(do.call(rbind, rows), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$guided_sheet_preview <- DT::renderDT({
    df <- validate_data_available()
    DT::datatable(head(df, 200), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  safe_excel_sheet_names <- function(nms) {
    out <- as.character(nms)
    for (bad in c("\\", "/", ":", "*", "?", "[", "]")) out <- gsub(bad, "_", out, fixed = TRUE)
    out <- substr(out, 1, 25)
    out[!nzchar(out)] <- paste0("Sheet", seq_along(out))[!nzchar(out)]
    make.unique(out, sep = "_")
  }

  output$guided_download_clean_workbook <- downloadHandler(
    filename = function() paste0(janitor::make_clean_names(app_state$workbook_name %||% app_state$dataset_name %||% "cleaned_workbook"), "_cleaned.xlsx"),
    content = function(file) {
      sync_current_to_workbook()
      sheets <- isolate(app_state$workbook_sheets %||% list())
      if (!length(sheets)) sheets <- setNames(list(validate_data_available()), app_state$dataset_name %||% "Cleaned data")
      names(sheets) <- safe_excel_sheet_names(names(sheets))
      openxlsx::write.xlsx(sheets, file, overwrite = TRUE)
    }
  )

  output$guided_download_quality_workbook <- downloadHandler(
    filename = function() paste0(janitor::make_clean_names(app_state$workbook_name %||% app_state$dataset_name %||% "workbook"), "_quality_report.xlsx"),
    content = function(file) {
      sync_current_to_workbook()
      sheets <- isolate(app_state$workbook_sheets %||% list())
      if (!length(sheets)) sheets <- setNames(list(validate_data_available()), app_state$dataset_name %||% "Current data")
      summary_rows <- list(); issue_sheets <- list()
      for (nm in names(sheets)) {
        prof <- sap_guided_profile(sheets[[nm]])
        m <- prof$metrics
        summary_rows[[length(summary_rows) + 1L]] <- data.frame(
          Worksheet = nm, Rows = m$rows, Columns = m$columns, Missing = m$missing,
          `Missing %` = m$missing_pct, Duplicates = m$duplicates, Empty_rows = m$empty_rows,
          Empty_columns = m$empty_cols, Constant_columns = m$constant_cols,
          Whitespace_flags = m$whitespace_cells, `Quality score` = m$quality,
          check.names = FALSE, stringsAsFactors = FALSE
        )
        issue_sheets[[paste0("Issues_", nm)]] <- prof$issues
      }
      book <- c(list(Quality_Summary = do.call(rbind, summary_rows)), issue_sheets)
      names(book) <- safe_excel_sheet_names(names(book))
      openxlsx::write.xlsx(book, file, overwrite = TRUE)
    }
  )

  current_profile <- reactive({ sap_guided_profile(validate_data_available()) })

  output$guided_profile_cards <- renderUI({
    p <- current_profile()$metrics
    quality <- ifelse(is.na(p$quality), 0, max(0, min(100, p$quality)))
    quality_label <- ifelse(is.na(p$quality), "—", p$quality)
    div(class = "sap-clean-metric-grid",
      div(class = "sap-clean-metric sap-tone-violet",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Quality score"), span(class="sap-clean-metric-icon",icon("chart-line"))),
        div(class="sap-clean-metric-value", quality_label),
        div(class="sap-clean-quality-track", div(class="sap-clean-quality-fill", style=sprintf("width:%s%%", quality))),
        div(class="sap-clean-metric-note","0–100 diagnostic score")
      ),
      div(class = "sap-clean-metric sap-tone-cyan",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Missing cells"), span(class="sap-clean-metric-icon",icon("circle-minus"))),
        div(class="sap-clean-metric-value", format(p$missing, big.mark=",")), div(class="sap-clean-metric-note",paste0(p$missing_pct,"% of all cells"))
      ),
      div(class = "sap-clean-metric sap-tone-amber",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Duplicate rows"), span(class="sap-clean-metric-icon",icon("copy"))),
        div(class="sap-clean-metric-value", format(p$duplicates, big.mark=",")), div(class="sap-clean-metric-note","Review before removing records")
      ),
      div(class = "sap-clean-metric sap-tone-teal",
        div(class="sap-metric-topline", span(class="sap-clean-metric-label","Structure flags"), span(class="sap-clean-metric-icon",icon("table-columns"))),
        div(class="sap-clean-metric-value", p$empty_rows + p$empty_cols + p$constant_cols), div(class="sap-clean-metric-note",sprintf("%d whitespace flag%s", p$whitespace_cells, ifelse(p$whitespace_cells==1,"","s")))
      )
    )
  })

  output$guided_issue_table <- DT::renderDT({
    issues <- current_profile()$issues
    column_types <- vapply(
      as.character(issues$type),
      function(kind) {
        switch(
          kind,
          integer = "Number",
          numeric = "Number",
          double = "Number",
          character = "Text",
          factor = "Category",
          logical = "Yes/no",
          Date = "Date",
          POSIXct = "Date/time",
          kind
        )
      },
      character(1)
    )
    issue_flags <- issues$blank_text + issues$whitespace_flags +
      issues$possible_missing_codes + as.integer(issues$constant)
    summary <- data.frame(
      Column = issues$variable,
      Type = column_types,
      Missing = issues$missing,
      Flags = issue_flags,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    summary <- summary[order(-summary$Missing, -summary$Flags, summary$Column), , drop = FALSE]

    DT::datatable(
      summary,
      rownames = FALSE,
      filter = "none",
      selection = "none",
      class = "compact stripe hover",
      options = list(
        pageLength = 7,
        lengthChange = FALSE,
        autoWidth = FALSE,
        scrollX = TRUE,
        dom = 't<"sap-dt-footer"ip>',
        ordering = FALSE,
        columnDefs = list(
          list(targets = 0, width = "42%"),
          list(targets = c(2, 3), className = "dt-center")
        ),
        language = list(
          info = "_START_–_END_ of _TOTAL_",
          infoEmpty = "No columns",
          # `next` is an R reserved word, so construct the DataTables names
          # explicitly instead of using it as an unquoted list argument.
          paginate = stats::setNames(
            list("‹", "›"),
            c("previous", "next")
          )
        )
      )
    )
  })

  output$guided_active_sheet_inline <- renderUI({
    sheets <- current_workbook()
    if(length(sheets)<=1L) return(div(class="small text-muted mb-2",paste("Dataset:",app_state$dataset_name %||% "Current data")))
    div(class="mb-2",selectInput(ns("guided_sheet_selector_inline"),"Worksheet",choices=names(sheets),selected=app_state$active_sheet %||% names(sheets)[1]),div(class="small text-muted","Clean each worksheet independently, then merge when ready."))
  })
  observeEvent(input$guided_sheet_selector_inline,{sel<-input$guided_sheet_selector_inline;if(!is.null(sel)&&nzchar(sel)&&!identical(sel,isolate(app_state$active_sheet))) switch_sheet(sel)},ignoreInit=TRUE)
  observeEvent(input$guided_tool_click,{action<-input$guided_tool_click;if(!is.null(action)&&action %in% catalog$action){updateSelectInput(session,"guided_action",selected=action);preview_result(NULL)}})
  observeEvent(input$guided_tool_group,{
    group_map<-c("Text"="Text","Missing"="Missing values","Numbers & dates"="Types & dates","Categories"="Categories","Validation"="Validation","Structure"="Structure","Reshape"="Reshape");grp<-unname(group_map[input$guided_tool_group %||% "Text"] %||% "Text");candidates<-catalog$action[catalog$group==grp]
    if(identical(input$guided_tool_group,"Validation")) candidates<-unique(c(candidates,"remove_duplicates","outlier_flag","cross_field"));if(identical(input$guided_tool_group,"Text")) candidates<-unique(c(candidates,"remove_characters"));if(identical(input$guided_tool_group,"Missing")) candidates<-unique(c(candidates,"drop_missing_rows"))
    current<-input$guided_action %||% "trim_squish";if(length(candidates)&&!current %in% candidates){updateSelectInput(session,"guided_action",selected=candidates[1]);preview_result(NULL)}
  },ignoreInit=TRUE)
  output$guided_column_picker <- renderUI({df<-validate_data_available();all_cols<-names(df);current<-intersect(isolate(input$guided_selected_columns %||% character()),all_cols);selectizeInput(ns("guided_selected_columns"),"Columns to clean",choices=all_cols,selected=current,multiple=TRUE,options=list(plugins=list("remove_button"),placeholder="Select one or more columns..."))})
  select_columns <- function(cols){df<-isolate(validate_data_available());cols<-intersect(cols,names(df));updateSelectizeInput(session,"guided_selected_columns",choices=names(df),selected=cols,server=TRUE)}
  observeEvent(input$guided_select_text,{df<-isolate(validate_data_available());select_columns(names(df)[vapply(df,function(x)is.character(x)||is.factor(x),logical(1))])})
  observeEvent(input$guided_select_numeric,{df<-isolate(validate_data_available());select_columns(names(df)[vapply(df,is.numeric,logical(1))])})
  observeEvent(input$guided_select_dates,{df<-isolate(validate_data_available());select_columns(names(df)[vapply(df,function(x)inherits(x,c("Date","POSIXct","POSIXlt")),logical(1))])})
  observeEvent(input$guided_select_missing,{df<-isolate(validate_data_available());select_columns(names(df)[vapply(df,function(x)any(is.na(x)),logical(1))])})
  observeEvent(input$guided_clear_columns,select_columns(character()))
  output$guided_column_selection_summary <- renderUI({df<-validate_data_available();cols<-intersect(input$guided_selected_columns %||% character(),names(df));if(!length(cols))return(div(class="small text-muted","No columns selected yet. Dataset-wide tools can still be used."));chips<-lapply(cols,function(nm){x<-df[[nm]];span(class="sap-chip",strong(nm),paste0(" · ",sap_visual_column_kind(x)," · ",sum(is.na(x))," missing"))});tagList(div(class="small text-muted mb-1",sprintf("%d column%s selected",length(cols),if(length(cols)==1)"" else "s")),div(chips))})
  output$guided_toolbox <- renderUI({
    group_map<-c("Text"="Text","Missing"="Missing values","Numbers & dates"="Types & dates","Categories"="Categories","Validation"="Validation","Structure"="Structure","Reshape"="Reshape");grp<-unname(group_map[input$guided_tool_group %||% "Text"] %||% "Text");rows<-catalog[catalog$group==grp,,drop=FALSE]
    if(identical(input$guided_tool_group,"Validation")) rows<-rbind(rows,catalog[catalog$action=="remove_duplicates",,drop=FALSE]);if(identical(input$guided_tool_group,"Text")) rows<-rbind(rows,catalog[catalog$action=="remove_characters",,drop=FALSE]);if(identical(input$guided_tool_group,"Missing")) rows<-rbind(rows,catalog[catalog$action=="drop_missing_rows",,drop=FALSE]);if(identical(input$guided_tool_group,"Validation")) rows<-rbind(rows,catalog[catalog$action %in% c("outlier_flag","cross_field"),,drop=FALSE]);rows<-rows[!duplicated(rows$action),,drop=FALSE]
    active<-input$guided_action %||% "trim_squish";symbols<-c(Structure="▦",Text="Aa",`Missing values`="NA",`Types & dates`="123",Categories="≡",Duplicates="⧉",Validation="✓",Reshape="↔")
    div(
      class = "sap-tool-grid",
      lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        selected <- identical(row$action, active)
        tags$button(
          type = "button",
          class = paste("sap-clean-tool", if (selected) "active" else ""),
          `aria-pressed` = if (selected) "true" else "false",
          `data-input` = ns("guided_tool_click"),
          `data-action` = row$action,
          div(
            class = "d-flex align-items-start",
            span(class = "sap-tool-symbol", symbols[[row$group]] %||% "•"),
            div(
              span(class = "sap-tool-name", row$label),
              span(class = "sap-tool-desc", row$summary)
            )
          )
        )
      })
    )
  })
  output$guided_suggestions <- renderUI({
    dataset <- validate_data_available()
    suggestions <- sap_visual_suggestions(dataset)
    if (!nrow(suggestions)) {
      return(
        div(
          class = "alert alert-success mb-0",
          strong("Quick scan: "),
          "No obvious structural, whitespace, numeric-text, duplicate, or outlier warnings were detected."
        )
      )
    }

    shown <- head(suggestions, 6)
    div(
      class = "sap-clean-suggestions",
      div(
        class = "sap-clean-suggestions-heading",
        h5("Suggested checks"),
        span("Select a suggestion to prepare its tool and columns.")
      ),
      div(
        class = "sap-suggestion-grid",
        lapply(seq_len(nrow(shown)), function(index) {
          suggestion <- shown[index, ]
          tags$button(
            type = "button",
            class = "sap-clean-suggestion",
            `data-input` = ns("guided_suggestion_click"),
            `data-action` = suggestion$action,
            `data-columns` = suggestion$columns,
            strong(suggestion$title),
            span(suggestion$detail)
          )
        })
      )
    )
  })
  observeEvent(input$guided_suggestion_click,{x<-input$guided_suggestion_click;req(!is.null(x$action));action<-as.character(x$action);if(action %in% catalog$action)updateSelectInput(session,"guided_action",selected=action);cols<-as.character(x$columns %||% "");cols<-if(nzchar(cols))strsplit(cols,"\\|\\|\\|")[[1]] else character();select_columns(cols);group_map<-c(Text="Text",`Missing values`="Missing",`Types & dates`="Numbers & dates",Categories="Categories",Duplicates="Validation",Validation="Validation",Structure="Structure",Reshape="Reshape");row<-catalog[catalog$action==action,,drop=FALSE];if(nrow(row))shinyWidgets::updateRadioGroupButtons(session,"guided_tool_group",selected=unname(group_map[row$group[1]] %||% "Text"));preview_result(NULL)})

  output$guided_action_explanation <- renderUI({
    row<-catalog[catalog$action==(input$guided_action %||% "trim_squish"),,drop=FALSE];if(!nrow(row))return(NULL);scope<-sap_visual_action_scope(row$action[1]);scope_text<-switch(scope,dataset="Works on the whole dataset",single="Select exactly one column",pair="Select exactly two columns","Works on one or more selected columns")
    div(class="sap-action-help mb-3",div(class="d-flex flex-wrap justify-content-between gap-2",strong(row$label),span(class="badge text-bg-light",scope_text)),div(class="mt-1",row$summary),div(class="small text-muted mt-2",strong("Why: "),row$why),div(class="small text-muted mt-1",strong("Be careful: "),row$caution))
  })

  character_columns <- reactive({
    df <- validate_data_available()
    names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  })
  numeric_columns <- reactive({
    df <- validate_data_available(); names(df)[vapply(df, is.numeric, logical(1))]
  })

  output$guided_action_params <- renderUI({
    df<-validate_data_available();action<-input$guided_action %||% "trim_squish";cols<-intersect(input$guided_selected_columns %||% character(),names(df));selection_note<-function(required="columns")div(class="small text-muted mb-2",sprintf("Current selection: %s",if(length(cols))paste(cols,collapse=", ") else paste("No",required,"selected")))
    switch(action,
      clean_names=div(class="small text-muted","No settings needed. All column names will be standardized and made unique."),
      remove_empty=radioButtons(ns("guided_empty_target"),"Remove",choices=c("Empty rows and columns"="both","Empty rows only"="rows","Empty columns only"="cols"),selected="both"),
      remove_constant=div(class="small text-muted","No settings needed. Preview which one-value columns would be removed."),
      header_row=tagList(numericInput(ns("guided_header_row"),"Which row contains the real column names?",value=1,min=1,max=max(1,nrow(df))),checkboxInput(ns("guided_header_remove_above"),"Remove title/note rows above it",TRUE)),
      trim_squish=selection_note(), unicode_normalize=tagList(selection_note(),div(class="small text-muted","Best for text copied from mixed systems or encodings.")), transliterate_ascii=tagList(selection_note(),div(class="alert alert-warning py-2","This can change spelling by removing accents. Use only when needed for matching/search.")),
      case_normalize=tagList(selection_note(),radioButtons(ns("guided_case_style"),"Text style",choices=c("lower case"="lower","UPPER CASE"="upper","Title Case"="title"),inline=TRUE)),
      replace_text=tagList(selection_note(),textInput(ns("guided_find"),"Find",placeholder="Text to replace"),textInput(ns("guided_replacement"),"Replace with",placeholder="New text; leave blank to remove")),
      remove_characters=tagList(selection_note(),radioButtons(ns("guided_character_mode"),"Keep / remove",choices=c("Remove punctuation"="punctuation","Keep digits and + only"="digits","Keep letters and spaces only"="letters"),selected="punctuation")),
      sentinel_na=tagList(selection_note(),textAreaInput(ns("guided_missing_codes"),"Values that really mean missing",value="NA, N/A, NULL, -99, 999",rows=3,placeholder="Example: -99, N/A, Refused"),div(class="small text-muted","Do not include a meaningful category such as ‘Unknown’ unless your questionnaire defines it as missing.")),
      fill_missing=tagList(selection_note(),selectInput(ns("guided_fill_direction"),"Fill direction",choices=c("Down"="down","Up"="up","Down then up"="downup","Up then down"="updown"))), replace_na=tagList(selection_note(),textInput(ns("guided_fill_value"),"Replacement value",placeholder="Use only a defensible value")),
      drop_missing_rows=tagList(selection_note(),radioButtons(ns("guided_missing_row_rule"),"Remove a row when",choices=c("Any selected column is missing"="any","All selected columns are missing"="all"),selected="any"),div(class="alert alert-warning py-2","This changes the number of records. Preview before applying.")),
      parse_number=tagList(selection_note(),selectInput(ns("guided_number_style"),"Number format in the source",choices=c("1,234.56"="dot_comma","1.234,56"="comma_dot","1234.56"="dot_none","1234,56"="comma_none"),selected="dot_comma")), parse_integer=tagList(selection_note(),div(class="small text-muted","Use for counts/ages/years. IDs with leading zeros should normally stay as text.")),
      parse_date=tagList(selection_note(),selectInput(ns("guided_date_format"),"Date order",choices=c("Year-Month-Day (2026-08-20)"="ymd","Day-Month-Year (20/08/2026)"="dmy","Month-Day-Year (08/20/2026)"="mdy","Mixed documented formats"="mixed"),selected="ymd"),textInput(ns("guided_timezone"),"Timezone for date-times",value="UTC")),
      category_recode=tagList(selection_note(),textAreaInput(ns("guided_mapping"),"Change categories (one per line)",rows=5,placeholder="Female = F\nWoman = F\nMale = M\nMan = M"),checkboxInput(ns("guided_as_factor"),"Treat the cleaned values as controlled categories",FALSE)),
      remove_duplicates=tagList(radioButtons(ns("guided_dup_mode"),"How do you define a duplicate?",choices=c("Entire row is repeated"="exact","Same selected key column(s)"="key"),selected="exact"),div(class="small text-muted mb-2","For key-based duplicates, your selected columns become the key."),radioButtons(ns("guided_dup_keep"),"What should happen?",choices=c("Flag duplicate groups for review"="flag","Keep first record"="first","Keep last record"="last"),selected="flag")),
      validate_range=tagList(selection_note(),div(class="row",div(class="col-6",numericInput(ns("guided_range_min"),"Minimum",value=0)),div(class="col-6",numericInput(ns("guided_range_max"),"Maximum",value=100))),radioButtons(ns("guided_validation_handling"),"If invalid",choices=c("Add issue flag"="flag","Remove invalid rows"="filter"),selected="flag")),
      validate_allowed=tagList(selection_note(),textAreaInput(ns("guided_allowed_values"),"Allowed values",rows=4,placeholder="One per line or comma-separated"),radioButtons(ns("guided_validation_handling"),"If invalid",choices=c("Add issue flag"="flag","Remove invalid rows"="filter"),selected="flag")),
      regex_validate=tagList(selection_note(),selectInput(ns("guided_format_preset"),"Expected format",choices=c("Email-like"="email","Phone: 8–15 digits"="phone","Four-digit year"="year","Letters/numbers/hyphen only"="id","Custom advanced pattern"="custom")),conditionalPanel(condition="input.guided_format_preset == 'custom'",ns=ns,textInput(ns("guided_custom_pattern"),"Custom pattern")),radioButtons(ns("guided_validation_handling"),"If invalid",choices=c("Add issue flag"="flag","Remove invalid rows"="filter"),selected="flag")),
      outlier_flag=tagList(selection_note(),radioButtons(ns("guided_outlier_method"),"Screening method",choices=c("IQR rule"="iqr","Standard deviations from mean"="z"),selected="iqr"),conditionalPanel(condition="input.guided_outlier_method == 'iqr'",ns=ns,numericInput(ns("guided_outlier_iqr"),"IQR multiplier",value=1.5,min=.1,step=.1)),conditionalPanel(condition="input.guided_outlier_method == 'z'",ns=ns,numericInput(ns("guided_outlier_z"),"Standard deviations",value=3,min=.5,step=.5)),div(class="small text-muted","The tool adds review flags; it does not automatically delete outliers.")),
      cross_field=tagList(selection_note(),selectInput(ns("guided_cross_operator"),"The first selected column should be",choices=c("Less than or equal to the second"="le","Less than the second"="lt","Greater than or equal to the second"="ge","Greater than the second"="gt","Equal to the second"="eq","Different from the second"="ne"))),
      pivot_longer=tagList(selection_note(),div(class="row",div(class="col-6",textInput(ns("guided_names_to"),"New column holding old column names",value="variable")),div(class="col-6",textInput(ns("guided_values_to"),"New column holding values",value="value")))), split_column=tagList(selection_note(),textInput(ns("guided_delimiter"),"Split at",value=","),textInput(ns("guided_split_names"),"New column names",value="part_1, part_2"),div(class="small text-muted","Select exactly one text column.")), unite_columns=tagList(selection_note(),textInput(ns("guided_unite_name"),"New column name",value="combined"),textInput(ns("guided_separator"),"Put between values",value="_"),checkboxInput(ns("guided_remove_sources"),"Remove original columns after combining",TRUE),checkboxInput(ns("guided_unite_na_rm"),"Skip missing pieces",TRUE)), NULL)
  })

  collect_params <- reactive({
    action<-input$guided_action %||% "trim_squish";cols<-input$guided_selected_columns %||% character();scope<-sap_visual_action_scope(action)
    if(scope=="single"&&length(cols)!=1L)stop("Select exactly one column for this tool.",call.=FALSE);if(scope=="pair"&&length(cols)!=2L)stop("Select exactly two columns for this check.",call.=FALSE);if(scope=="multi"&&!length(cols))stop("Select at least one column first.",call.=FALSE)
    df<-validate_data_available();text_actions<-c("trim_squish","unicode_normalize","transliterate_ascii","case_normalize","replace_text","remove_characters","regex_validate","split_column","category_recode");numeric_actions<-c("validate_range","outlier_flag")
    if(action %in% text_actions && length(cols) && !all(vapply(df[cols],function(x)is.character(x)||is.factor(x),logical(1)))) stop("This tool is for text/category columns. Select columns with the same data nature.",call.=FALSE)
    if(action %in% numeric_actions && length(cols) && !all(vapply(df[cols],is.numeric,logical(1)))) stop("This tool is for numeric columns. Use the ‘All numbers’ shortcut or choose numeric columns.",call.=FALSE)
    fmt<-input$guided_number_style %||% "dot_comma";marks<-switch(fmt,dot_comma=c(".",","),comma_dot=c(",","."),dot_none=c(".",","),comma_none=c(",","."),c(".",","));date_fmt<-input$guided_date_format %||% "ymd";orders<-switch(date_fmt,ymd="ymd",dmy="dmy",mdy="mdy",mixed="ymd,dmy,mdy","ymd")
    preset<-input$guided_format_preset %||% "email";pattern<-switch(preset,email="^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$",phone="^\\+?[0-9]{8,15}$",year="^[0-9]{4}$",id="^[[:alnum:]-]+$",custom=input$guided_custom_pattern %||% "","^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$")
    switch(action,clean_names=list(),remove_empty=list(target=input$guided_empty_target %||% "both"),remove_constant=list(),header_row=list(row_number=input$guided_header_row %||% 1L,remove_above=isTRUE(input$guided_header_remove_above)),trim_squish=list(columns=cols),unicode_normalize=list(columns=cols),transliterate_ascii=list(columns=cols),case_normalize=list(columns=cols,style=input$guided_case_style %||% "lower"),replace_text=list(columns=cols,find=input$guided_find,replacement=input$guided_replacement,regex=FALSE),remove_characters=list(columns=cols,mode=input$guided_character_mode %||% "punctuation"),sentinel_na=list(columns=cols,codes=input$guided_missing_codes),fill_missing=list(columns=cols,direction=input$guided_fill_direction %||% "down"),replace_na=list(columns=cols,value=input$guided_fill_value),drop_missing_rows=list(columns=cols,rule=input$guided_missing_row_rule %||% "any"),parse_number=list(columns=cols,decimal_mark=marks[1],grouping_mark=marks[2]),parse_integer=list(columns=cols),parse_date=list(columns=cols,orders=orders,tz=input$guided_timezone %||% "UTC"),category_recode=list(columns=cols,mapping=input$guided_mapping,as_factor=isTRUE(input$guided_as_factor)),remove_duplicates=list(mode=input$guided_dup_mode %||% "exact",keys=cols,keep=input$guided_dup_keep %||% "flag"),validate_range=list(columns=cols,min=input$guided_range_min,max=input$guided_range_max,handling=input$guided_validation_handling %||% "flag"),validate_allowed=list(columns=cols,values=input$guided_allowed_values,handling=input$guided_validation_handling %||% "flag"),regex_validate=list(columns=cols,pattern=pattern,handling=input$guided_validation_handling %||% "flag"),outlier_flag=list(columns=cols,method=input$guided_outlier_method %||% "iqr",threshold=if(identical(input$guided_outlier_method,"z"))input$guided_outlier_z %||% 3 else input$guided_outlier_iqr %||% 1.5),cross_field=list(columns=cols,operator=input$guided_cross_operator %||% "le"),pivot_longer=list(columns=cols,names_to=input$guided_names_to,values_to=input$guided_values_to),split_column=list(column=cols,delimiter=input$guided_delimiter,new_names=input$guided_split_names),unite_columns=list(columns=cols,new_name=input$guided_unite_name,separator=input$guided_separator,remove_sources=isTRUE(input$guided_remove_sources),na_rm=isTRUE(input$guided_unite_na_rm)),list())
  })

  run_preview <- function() {
    df <- validate_data_available(); action <- input$guided_action %||% "clean_names"
    sap_guided_apply_action(df, action, collect_params())
  }

  observeEvent(input$guided_preview_action, {
    tryCatch({
      preview_result(run_preview())
      showNotification("Preview ready. Review the row/column changes before applying.", type = "message")
    }, error = function(e) {
      preview_result(list(error = conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  })

  output$guided_preview_summary <- renderUI({
    x<-preview_result();if(is.null(x))return(div(class="sap-clean-preview-summary-note","Select your columns and a tool, then choose Preview changes to compare both versions."));if(!is.null(x$error))return(div(class="alert alert-danger",icon("triangle-exclamation")," ",x$error));cc<-if(is.na(x$changed_cells %||% NA_integer_))"Structure changed" else format(x$changed_cells,big.mark=",")
    div(class="row g-2 mb-3",div(class="col-6 col-lg-3",div(class="card",div(class="card-body py-2",div(class="small text-muted","Rows"),strong(sprintf("%d → %d",x$rows_before,x$rows_after))))),div(class="col-6 col-lg-3",div(class="card",div(class="card-body py-2",div(class="small text-muted","Columns"),strong(sprintf("%d → %d",x$cols_before,x$cols_after))))),div(class="col-6 col-lg-3",div(class="card",div(class="card-body py-2",div(class="small text-muted","Cell changes"),strong(cc)))),div(class="col-6 col-lg-3",div(class="card",div(class="card-body py-2",div(class="small text-muted","Raw source"),strong("Preserved")))))
  })
  output$guided_before_preview <- DT::renderDT({df<-validate_data_available();cols<-intersect(input$guided_selected_columns %||% character(),names(df));show<-if(length(cols))cols else names(df);DT::datatable(head(df[,show,drop=FALSE],100),rownames=FALSE,options=list(pageLength=7,scrollX=TRUE,dom='tip'))})
  output$guided_action_preview <- DT::renderDT({x<-preview_result();req(!is.null(x),is.null(x$error),is.data.frame(x$data));selected<-intersect(input$guided_selected_columns %||% character(),names(x$data));new_cols<-setdiff(names(x$data),names(validate_data_available()));show<-unique(c(selected,new_cols));if(!length(show))show<-names(x$data);DT::datatable(head(x$data[,show,drop=FALSE],100),rownames=FALSE,options=list(pageLength=7,scrollX=TRUE,dom='tip'))})
  output$guided_after_preview_state <- renderUI({
    result <- preview_result()
    if (!is.null(result) && is.null(result$error)) return(NULL)

    div(
      class = "sap-clean-preview-empty",
      icon("eye"),
      strong(if (is.null(result)) "Your preview will appear here" else "Update your cleaning settings"),
      span(
        if (is.null(result)) {
          "Choose a tool and select Preview changes to inspect the proposed result."
        } else {
          "Correct the issue shown above, then generate the preview again."
        }
      )
    )
  })

  output$guided_preview_note <- renderUI({x<-preview_result();if(is.null(x)||!is.null(x$error))return(NULL);if(nzchar(x$note %||% ""))div(class="alert alert-warning py-2",x$note)else NULL})

  observeEvent(input$guided_apply_action, {
    tryCatch({
      before <- validate_data_available(); action <- input$guided_action %||% "clean_names"; params <- collect_params()
      result <- sap_guided_apply_action(before, action, params)
      row <- catalog[catalog$action == action, , drop = FALSE]
      description <- if (nrow(row)) row$label[1] else action
      cleaning_state$history[[length(cleaning_state$history) + 1L]] <- list(
        operation = paste0("guided_", action), data = before,
        description = paste0("Guided Studio: ", description),
        guided_step = list(action = action, params = params, code = result$code, label = description, timestamp = Sys.time())
      )
      cleaning_state$history_index <- length(cleaning_state$history)
      cleaning_state$current_data <- result$data
      steps <- get_guided_steps()
      steps[[length(steps) + 1L]] <- list(action = action, params = params, code = result$code, label = description, timestamp = Sys.time())
      set_guided_steps(steps)
      preview_result(result)
      showNotification(paste0("Applied: ", description), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })

  output$guided_recipe_steps <- DT::renderDT({steps<-get_guided_steps();if(!length(steps))return(DT::datatable(data.frame(Message="No visual cleaning actions recorded on this worksheet yet."),rownames=FALSE,options=list(dom="t")));df<-data.frame(Step=seq_along(steps),Cleaning=vapply(steps,function(x)x$label %||% x$action,character(1)),Applied_to=vapply(steps,sap_visual_param_summary,character(1)),check.names=FALSE,stringsAsFactors=FALSE);DT::datatable(df,rownames=FALSE,options=list(pageLength=8,scrollX=TRUE,dom='tip'))})

  observeEvent(input$guided_save_recipe, {
    steps <- get_guided_steps(); if (!length(steps)) { showNotification("Apply at least one Guided Studio action first.", type = "warning"); return() }
    name <- trimws(input$guided_recipe_name %||% "")
    if (!nzchar(name)) name <- paste("Recipe", format(Sys.time(), "%Y-%m-%d %H:%M"))
    recipes <- isolate(app_state$saved_cleaning_recipes %||% list())
    recipes[[name]] <- list(name = name, created_at = Sys.time(), source_sheet = active_key(), steps = steps)
    app_state$saved_cleaning_recipes <- recipes
    updateSelectInput(session, "guided_recipe_select", choices = names(recipes), selected = name)
    showNotification(sprintf("Saved recipe '%s' with %d steps.", name, length(steps)), type = "message")
  })

  observe({
    recipes <- app_state$saved_cleaning_recipes %||% list()
    updateSelectInput(session, "guided_recipe_select", choices = names(recipes), selected = input$guided_recipe_select %||% if (length(recipes)) names(recipes)[1] else character())
  })

  output$guided_recipe_targets_ui <- renderUI({
    sheets <- current_workbook(); if (length(sheets) < 2L) return(div(class = "text-muted", "Import multiple Excel worksheets to apply a recipe across sheets."))
    choices <- setdiff(names(sheets), app_state$active_sheet %||% "")
    selectInput(ns("guided_recipe_targets"), "Target worksheets", choices = choices, selected = choices, multiple = TRUE)
  })

  observeEvent(input$guided_apply_recipe, {
    recipes <- isolate(app_state$saved_cleaning_recipes %||% list())
    recipe <- recipes[[input$guided_recipe_select %||% ""]]
    targets <- input$guided_recipe_targets %||% character()
    sheets <- isolate(app_state$workbook_sheets %||% list())
    if (is.null(recipe) || !length(recipe$steps)) { showNotification("Choose a saved recipe.", type = "warning"); return() }
    if (!length(targets)) { showNotification("Choose one or more target worksheets.", type = "warning"); return() }
    result_rows <- list()
    guided_all <- isolate(app_state$guided_cleaning_steps %||% list())
    history_all <- isolate(app_state$workbook_cleaning_histories %||% list())
    index_all <- isolate(app_state$workbook_history_indices %||% list())
    for (nm in targets) {
      if (!nm %in% names(sheets)) next
      df <- sheets[[nm]]; ok <- 0L; errors <- character()
      applied_steps <- guided_all[[nm]] %||% list()
      sheet_history <- history_all[[nm]] %||% list()
      for (step in recipe$steps) {
        before_step <- df
        attempt <- tryCatch(sap_guided_apply_action(df, step$action, step$params), error = function(e) e)
        if (inherits(attempt, "error")) {
          errors <- c(errors, sprintf("%s: %s", step$label %||% step$action, conditionMessage(attempt)))
          if (isTRUE(input$guided_recipe_stop_error)) break else next
        }
        df <- attempt$data; ok <- ok + 1L
        applied_steps[[length(applied_steps) + 1L]] <- step
        sheet_history[[length(sheet_history) + 1L]] <- list(
          operation = paste0("recipe_", step$action), data = before_step,
          description = paste0("Cleaning recipe: ", step$label %||% step$action),
          guided_step = step
        )
      }
      sheets[[nm]] <- df; guided_all[[nm]] <- applied_steps
      history_all[[nm]] <- sheet_history; index_all[[nm]] <- length(sheet_history)
      result_rows[[length(result_rows) + 1L]] <- data.frame(Worksheet = nm, Applied = ok, Errors = paste(errors, collapse = " | "), stringsAsFactors = FALSE)
    }
    app_state$workbook_sheets <- sheets
    app_state$guided_cleaning_steps <- guided_all
    app_state$workbook_cleaning_histories <- history_all
    app_state$workbook_history_indices <- index_all
    recipe_message(if (length(result_rows)) do.call(rbind, result_rows) else data.frame())
    showNotification("Recipe processing finished. Review the per-worksheet result.", type = "message")
  })

  output$guided_recipe_result <- renderUI({
    x <- recipe_message(); if (is.null(x) || !nrow(x)) return(NULL)
    tags$table(class = "table table-sm mt-3",
      tags$thead(tags$tr(tags$th("Worksheet"), tags$th("Steps applied"), tags$th("Notes"))),
      tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(tags$td(x$Worksheet[i]), tags$td(x$Applied[i]), tags$td(if (nzchar(x$Errors[i])) x$Errors[i] else "Complete"))))
    )
  })

  output$guided_download_recipe <- downloadHandler(
    filename = function() paste0(janitor::make_clean_names(input$guided_recipe_name %||% "cleaning_recipe"), ".R"),
    content = function(file) {
      steps <- get_guided_steps()
      lines <- c(
        "# Survey Analytics Platform - Guided Cleaning Recipe",
        sprintf("# Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        "# Start with: df <- raw_data",
        ""
      )
      for (i in seq_along(steps)) lines <- c(lines, sprintf("# Step %d: %s", i, steps[[i]]$label %||% steps[[i]]$action), steps[[i]]$code %||% "# See application audit log", "")
      writeLines(lines, file, useBytes = TRUE)
    }
  )

  output$guided_merge_sheets_ui <- renderUI({
    sheets <- current_workbook(); if (length(sheets) < 2L) return(div(class = "alert alert-info", "Multiple worksheets are required for workbook merging."))
    checkboxGroupInput(ns("guided_merge_sheets"), "Worksheets", choices = names(sheets), selected = names(sheets))
  })

  selected_merge_sheets <- reactive({
    sheets <- current_workbook(); chosen <- input$guided_merge_sheets %||% names(sheets)
    sheets[intersect(chosen, names(sheets))]
  })

  output$guided_merge_key_ui <- renderUI({
    sheets <- selected_merge_sheets(); if (length(sheets) < 2L) return(NULL)
    common <- Reduce(intersect, lapply(sheets, names))
    selectInput(ns("guided_merge_keys"), "Join key variable(s)", choices = common, multiple = TRUE)
  })

  calculate_readiness <- function() {
    sheets <- selected_merge_sheets();
    key <- if (identical(input$guided_merge_strategy, "join")) input$guided_merge_keys else NULL
    sap_workbook_merge_readiness(sheets, key = key)
  }

  observeEvent(input$guided_check_merge, {
    merge_readiness(calculate_readiness())
    showNotification("Merge readiness check completed.", type = "message")
  })

  output$guided_merge_readiness <- DT::renderDT({
    x <- merge_readiness(); if (is.null(x)) x <- calculate_readiness()
    DT::datatable(x, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$guided_merge_readiness_summary <- renderUI({
    x <- merge_readiness(); if (is.null(x)) x <- calculate_readiness()
    if (is.null(x) || !nrow(x)) return(div(class = "text-muted", "Choose at least two worksheets."))
    mismatch_n <- sum(nzchar(x$type_mismatches)) + sum(nzchar(x$missing_vs_first)) + sum(nzchar(x$extra_vs_first))
    key_dup_n <- sum(x$key_duplicates, na.rm = TRUE)
    div(class = if (mismatch_n == 0L && key_dup_n == 0L) "alert alert-success" else "alert alert-warning",
      strong(if (mismatch_n == 0L && key_dup_n == 0L) "Ready with no structural warnings." else "Review before merging."),
      sprintf(" Structural differences: %d. Duplicate join keys: %d. Append mode can harmonize compatible numeric/integer fields and converts conflicting types to character instead of guessing.", mismatch_n, key_dup_n)
    )
  })

  observeEvent(input$guided_preview_merge, {
    tryCatch({
      sheets <- selected_merge_sheets();
      result <- sap_merge_workbook_sheets(
        sheets,
        strategy = input$guided_merge_strategy %||% "append",
        key = input$guided_merge_keys,
        add_source = isTRUE(input$guided_merge_source),
        join_type = input$guided_join_type %||% "full"
      )
      merge_readiness(calculate_readiness())
      merge_preview(result)
      showNotification(sprintf("Merge preview ready: %s rows x %s columns.", format(nrow(result), big.mark = ","), ncol(result)), type = "message")
    }, error = function(e) {
      merge_preview(NULL); showNotification(conditionMessage(e), type = "error")
    })
  })

  output$guided_merge_preview <- DT::renderDT({
    df <- merge_preview(); req(is.data.frame(df))
    DT::datatable(head(df, 250), rownames = FALSE, filter = "top", options = list(pageLength = 8, scrollX = TRUE))
  })

  observeEvent(input$guided_apply_merge, {
    df <- merge_preview()
    if (!is.data.frame(df)) { showNotification("Preview the merge first.", type = "warning"); return() }
    sync_current_to_workbook()
    merged_name <- paste0("Merged - ", app_state$workbook_name %||% "workbook")
    app_state$active_sheet <- ""
    app_state$raw_data <- df
    cleaning_state$original_data <- df
    cleaning_state$current_data <- df
    cleaning_state$history <- list()
    cleaning_state$history_index <- 0L
    app_state$processed_data <- df
    app_state$dataset_name <- tools::file_path_sans_ext(merged_name)
    app_state$last_action <- sprintf("Merged %d worksheets", length(selected_merge_sheets()))
    mh <- isolate(app_state$workbook_merge_history %||% list())
    mh[[length(mh) + 1L]] <- list(
      timestamp = Sys.time(), worksheets = names(selected_merge_sheets()),
      strategy = input$guided_merge_strategy %||% "append",
      keys = input$guided_merge_keys %||% character(),
      join_type = input$guided_join_type %||% "full",
      rows = nrow(df), columns = ncol(df)
    )
    app_state$workbook_merge_history <- mh
    if (!is.null(services)) {
      try(services$data$set(df, kind = "processed", module = "guided_cleaning_studio", description = "Merged cleaned worksheets", name = app_state$dataset_name), silent = TRUE)
    }
    showNotification("Merged dataset is now the active dataset. The individual cleaned worksheets remain in the workbook workspace.", type = "message")
  })

  output$guided_download_merged_csv <- downloadHandler(
    filename = function() paste0(janitor::make_clean_names(app_state$workbook_name %||% "merged_workbook"), "_merged.csv"),
    content = function(file) {
      df <- merge_preview(); req(is.data.frame(df)); readr::write_csv(df, file, na = "")
    }
  )
  output$guided_download_merged_xlsx <- downloadHandler(
    filename = function() paste0(janitor::make_clean_names(app_state$workbook_name %||% "merged_workbook"), "_merged.xlsx"),
    content = function(file) {
      df <- merge_preview(); req(is.data.frame(df)); openxlsx::write.xlsx(df, file, overwrite = TRUE)
    }
  )

  # The handbook catalog is internal-only; it is not rendered in the cleaning UI.

}

message("[SAP] Visual Data Cleaning Workbench v5.2.0 loaded")
