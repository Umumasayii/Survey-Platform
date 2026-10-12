# =============================================================================
# MODULE 33: BI STUDIO CORE
# =============================================================================
# Pure helpers for the multi-table semantic model, repeatable transformation
# pipelines, visual specifications, dashboard pages, safe remote ingestion and
# serialisable workspace documents. UI modules consume this file; keeping the
# engine here makes the critical behaviour testable without a Shiny session.
# =============================================================================

sap_bi_empty_relationships <- function() {
  data.frame(
    id = character(), from_table = character(), from_key = character(),
    to_table = character(), to_key = character(), cardinality = character(),
    cross_filter = character(), active = logical(), created_at = character(),
    stringsAsFactors = FALSE
  )
}

sap_bi_empty_measures <- function() {
  data.frame(
    id = character(), name = character(), table = character(),
    column = character(), aggregation = character(), format = character(),
    description = character(), created_at = character(),
    stringsAsFactors = FALSE
  )
}

sap_bi_default_state <- function() {
  list(
    tables = list(),
    active_table = NULL,
    relationships = sap_bi_empty_relationships(),
    measures = sap_bi_empty_measures(),
    hierarchies = list(),
    sources = list(),
    pipelines = list(),
    pipeline_log = list(),
    global_filters = list(),
    security_rules = list(),
    refresh_schedules = list(),
    dashboard = list(
      pages = list(list(id = "page_overview", name = "Overview", visual_ids = character())),
      active_page = "page_overview",
      visuals = list(),
      theme = "nexus",
      updated_at = as.character(Sys.time())
    ),
    publications = list(),
    refresh_runs = list()
  )
}

sap_bi_ensure_state <- function(app_state) {
  # Modules call this once during server initialization, before an observer or
  # reactive expression exists. isolate() permits that intentional bootstrap
  # read while preserving normal reactive invalidation everywhere else.
  current <- shiny::isolate(app_state$bi)
  defaults <- sap_bi_default_state()
  if (!is.list(current)) {
    app_state$bi <- defaults
    return(invisible(defaults))
  }
  for (nm in names(defaults)) {
    if (is.null(current[[nm]])) current[[nm]] <- defaults[[nm]]
  }
  if (!is.data.frame(current$relationships)) current$relationships <- sap_bi_empty_relationships()
  if (!is.data.frame(current$measures)) current$measures <- sap_bi_empty_measures()
  if (!is.list(current$dashboard)) current$dashboard <- defaults$dashboard
  app_state$bi <- current
  invisible(current)
}

sap_bi_id <- function(prefix = "item") {
  raw <- paste(prefix, format(Sys.time(), "%Y%m%d%H%M%OS6"), runif(1), sep = "-")
  paste0(prefix, "_", substr(digest::digest(raw, algo = "xxhash64"), 1L, 12L))
}

sap_bi_first <- function(x, default = NULL) {
  if (is.null(x) || !length(x)) default else x[[1]]
}

sap_bi_has_role <- function(app_state, required = "viewer") {
  ranks <- c(viewer = 1L, reviewer = 2L, editor = 3L, owner = 4L, admin = 5L)
  profile <- app_state$current_user_profile %||% list(role = "viewer")
  role <- tolower(as.character(profile$role %||% "viewer"))[[1]]
  current_rank <- unname(ranks[role])
  required_rank <- unname(ranks[tolower(as.character(required)[[1]])])
  if (!length(current_rank) || is.na(current_rank[[1]])) current_rank <- 0L else current_rank <- current_rank[[1]]
  if (!length(required_rank) || is.na(required_rank[[1]])) required_rank <- 99L else required_rank <- required_rank[[1]]
  current_rank >= required_rank
}

sap_bi_require_role <- function(app_state, required = "editor", action = "change this workspace") {
  if (!sap_bi_has_role(app_state, required)) {
    stop(sprintf("The %s role is required to %s.", required, action), call. = FALSE)
  }
  invisible(TRUE)
}

sap_bi_clean_name <- function(x, fallback = "Table") {
  x <- trimws(as.character(x %||% ""))
  x <- gsub("[^[:alnum:] _.-]+", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  if (!nzchar(x)) fallback else substr(x, 1L, 80L)
}

sap_bi_unique_name <- function(name, existing) {
  name <- sap_bi_clean_name(name)
  if (!name %in% existing) return(name)
  i <- 2L
  while (paste0(name, " ", i) %in% existing) i <- i + 1L
  paste0(name, " ", i)
}

sap_bi_table_names <- function(app_state) {
  sap_bi_ensure_state(app_state)
  names(app_state$bi$tables) %||% character()
}

sap_bi_apply_rls <- function(app_state, table_name, df) {
  if (!is.data.frame(df)) return(df)
  profile <- app_state$current_user_profile %||% list(role = "owner", username = "local")
  role <- tolower(as.character(profile$role %||% "owner"))
  username <- tolower(as.character(profile$username %||% "local"))
  if (role %in% c("admin", "owner")) return(df)
  rules <- app_state$bi$security_rules %||% list()
  rules <- Filter(function(x) {
    if (!isTRUE(x$enabled %||% TRUE) || !identical(x$table, table_name)) return(FALSE)
    type <- x$subject_type %||% "all"
    target <- tolower(as.character(x$subject_value %||% ""))
    identical(type, "all") || (identical(type, "role") && identical(target, role)) ||
      (identical(type, "username") && identical(target, username))
  }, rules)
  if (!length(rules)) return(df)
  fields <- unique(vapply(rules, function(x) x$field, character(1)))
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields)) {
    # A transformation that removes an RLS key must fail closed. Skipping the
    # rule would expose aggregates derived from rows the viewer cannot access.
    out <- df[FALSE, , drop = FALSE]
    attr(out, "sap_rls_error") <- paste(
      "Row-security field unavailable:", paste(missing_fields, collapse = ", ")
    )
    return(out)
  }
  for (field in fields) {
    allowed <- unique(unlist(lapply(Filter(function(x) identical(x$field, field), rules), function(x) x$values), use.names = FALSE))
    df <- df[as.character(df[[field]]) %in% as.character(allowed), , drop = FALSE]
  }
  df
}

sap_bi_get_table <- function(app_state, name = NULL, original = FALSE,
                             secured = TRUE) {
  sap_bi_ensure_state(app_state)
  name <- name %||% app_state$bi$active_table
  if (is.null(name) || !name %in% names(app_state$bi$tables)) return(NULL)
  item <- app_state$bi$tables[[name]]
  df <- if (isTRUE(original)) item$source_data else item$data
  if (isTRUE(secured)) sap_bi_apply_rls(app_state, name, df) else df
}

sap_bi_schema <- function(df) {
  if (!is.data.frame(df)) return(data.frame())
  data.frame(
    field = names(df),
    type = vapply(df, function(x) class(x)[[1]], character(1)),
    role = vapply(df, function(x) {
      if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) "Date"
      else if (is.numeric(x)) "Measure"
      else "Dimension"
    }, character(1)),
    missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    unique = vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1)),
    stringsAsFactors = FALSE
  )
}

sap_bi_redact_url <- function(url) {
  url <- as.character(url %||% "")[[1]]
  if (!nzchar(url) || !requireNamespace("httr", quietly = TRUE)) return(url)
  parsed <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  if (is.null(parsed)) return(url)
  parsed$username <- NULL
  parsed$password <- NULL
  if (length(parsed$query)) {
    secret_query <- grepl("token|secret|password|pass|key|signature|credential|auth",
                          names(parsed$query), ignore.case = TRUE)
    parsed$query[secret_query] <- lapply(parsed$query[secret_query], function(x) "[redacted]")
  }
  tryCatch(httr::build_url(parsed), error = function(e) url)
}

sap_bi_redact_config <- function(config) {
  if (!is.list(config)) return(list())
  secret <- grepl("password|secret|token|api.?key|credential", names(config), ignore.case = TRUE)
  config[secret] <- lapply(config[secret], function(x) "[stored outside application]")
  url_fields <- grepl("url|uri|endpoint", names(config), ignore.case = TRUE) & !secret
  config[url_fields] <- lapply(config[url_fields], sap_bi_redact_url)
  nested <- vapply(config, is.list, logical(1)) & !secret
  config[nested] <- lapply(config[nested], sap_bi_redact_config)
  config
}

sap_bi_add_table <- function(app_state, name, data, source_type = "memory",
                             source_config = list(), set_active = TRUE,
                             replace = FALSE) {
  if (!is.data.frame(data)) data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!ncol(data)) stop("A model table must contain at least one field.", call. = FALSE)
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  clean <- sap_bi_clean_name(name)
  if (!isTRUE(replace)) clean <- sap_bi_unique_name(clean, names(model$tables))
  if (isTRUE(replace) && !clean %in% names(model$tables)) replace <- FALSE
  now <- as.character(Sys.time())
  created <- if (isTRUE(replace)) model$tables[[clean]]$created_at %||% now else now
  model$tables[[clean]] <- list(
    name = clean,
    data = data,
    source_data = data,
    schema = sap_bi_schema(data),
    source_type = as.character(source_type),
    source_config = sap_bi_redact_config(source_config),
    rows = nrow(data), cols = ncol(data),
    created_at = created, updated_at = now
  )
  if (isTRUE(set_active) || is.null(model$active_table)) model$active_table <- clean
  if (is.null(model$pipelines[[clean]])) model$pipelines[[clean]] <- list()
  app_state$bi <- model
  if (identical(model$active_table, clean)) sap_bi_sync_active(app_state)
  clean
}

sap_bi_sync_active <- function(app_state) {
  sap_bi_ensure_state(app_state)
  table_name <- app_state$bi$active_table
  df <- sap_bi_get_table(app_state, table_name)
  if (!is.data.frame(df)) return(invisible(FALSE))
  # Keep the compatibility bridge inside the same row-security boundary as
  # Studio visuals. Classic modules must never receive an unfiltered source.
  app_state$raw_data <- sap_bi_get_table(app_state, table_name,
                                         original = TRUE, secured = TRUE)
  app_state$processed_data <- df
  app_state$data_imported <- TRUE
  app_state$dataset_name <- table_name
  app_state$variable_types <- tryCatch(detect_variable_types(df), error = function(e) sap_bi_schema(df))
  app_state$last_action <- paste("Active model table:", table_name)
  invisible(TRUE)
}

sap_bi_set_active <- function(app_state, name) {
  sap_bi_ensure_state(app_state)
  if (!name %in% names(app_state$bi$tables)) stop("Unknown model table.", call. = FALSE)
  model <- app_state$bi
  model$active_table <- name
  app_state$bi <- model
  sap_bi_sync_active(app_state)
  invisible(name)
}

sap_bi_remove_table <- function(app_state, name) {
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  if (!name %in% names(model$tables)) return(invisible(FALSE))
  model$tables[[name]] <- NULL
  model$pipelines[[name]] <- NULL
  if (nrow(model$relationships)) {
    model$relationships <- model$relationships[
      model$relationships$from_table != name & model$relationships$to_table != name,
      , drop = FALSE
    ]
  }
  if (nrow(model$measures)) model$measures <- model$measures[model$measures$table != name, , drop = FALSE]
  if (identical(model$active_table, name)) model$active_table <- sap_bi_first(names(model$tables), NULL)
  app_state$bi <- model
  if (!is.null(model$active_table)) {
    sap_bi_sync_active(app_state)
  } else {
    app_state$raw_data <- NULL
    app_state$processed_data <- NULL
    app_state$data_imported <- FALSE
    app_state$dataset_name <- NULL
  }
  invisible(TRUE)
}

sap_bi_validate_relationship <- function(from_df, from_key, to_df, to_key,
                                         cardinality = "many-to-one") {
  issues <- character()
  if (!is.data.frame(from_df) || !from_key %in% names(from_df)) issues <- c(issues, "The source key is unavailable.")
  if (!is.data.frame(to_df) || !to_key %in% names(to_df)) issues <- c(issues, "The target key is unavailable.")
  if (length(issues)) return(list(valid = FALSE, issues = issues, matches = 0L, unmatched = NA_integer_))
  left <- from_df[[from_key]]
  right <- to_df[[to_key]]
  if (anyNA(left)) issues <- c(issues, "The source key contains missing values.")
  if (anyNA(right)) issues <- c(issues, "The target key contains missing values.")
  left_unique <- !anyDuplicated(left[!is.na(left)])
  right_unique <- !anyDuplicated(right[!is.na(right)])
  if (cardinality %in% c("one-to-one", "one-to-many") && !left_unique) {
    issues <- c(issues, "The source key is not unique for the selected cardinality.")
  }
  if (cardinality %in% c("one-to-one", "many-to-one") && !right_unique) {
    issues <- c(issues, "The target key is not unique for the selected cardinality.")
  }
  common <- intersect(unique(left[!is.na(left)]), unique(right[!is.na(right)]))
  unmatched <- sum(!is.na(left) & !left %in% common)
  list(
    valid = !length(issues), issues = issues, matches = length(common),
    unmatched = unmatched, left_unique = left_unique, right_unique = right_unique
  )
}

sap_bi_add_relationship <- function(app_state, from_table, from_key, to_table,
                                    to_key, cardinality = "many-to-one",
                                    cross_filter = "single") {
  sap_bi_ensure_state(app_state)
  if (identical(from_table, to_table)) stop("A relationship requires two different tables.", call. = FALSE)
  left <- sap_bi_get_table(app_state, from_table)
  right <- sap_bi_get_table(app_state, to_table)
  validation <- sap_bi_validate_relationship(left, from_key, right, to_key, cardinality)
  if (!isTRUE(validation$valid)) stop(paste(validation$issues, collapse = " "), call. = FALSE)
  model <- app_state$bi
  # Avoid tidy-evaluation ambiguity in base data-frame expressions.
  duplicate <- model$relationships$from_table == from_table &
    model$relationships$from_key == from_key &
    model$relationships$to_table == to_table &
    model$relationships$to_key == to_key
  if (any(duplicate)) stop("This relationship already exists.", call. = FALSE)
  row <- data.frame(
    id = sap_bi_id("rel"), from_table = from_table, from_key = from_key,
    to_table = to_table, to_key = to_key, cardinality = cardinality,
    cross_filter = cross_filter, active = TRUE,
    created_at = as.character(Sys.time()), stringsAsFactors = FALSE
  )
  model$relationships <- rbind(model$relationships, row)
  app_state$bi <- model
  row$id[[1]]
}

sap_bi_remove_relationship <- function(app_state, relationship_id) {
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  model$relationships <- model$relationships[model$relationships$id != relationship_id, , drop = FALSE]
  app_state$bi <- model
  invisible(TRUE)
}

sap_bi_add_measure <- function(app_state, name, table, column = "",
                               aggregation = "count", format = "number",
                               description = "") {
  sap_bi_ensure_state(app_state)
  df <- sap_bi_get_table(app_state, table)
  if (!is.data.frame(df)) stop("Select a valid table.", call. = FALSE)
  if (!aggregation %in% c("count", "distinct_count") && !column %in% names(df)) {
    stop("Select a valid measure field.", call. = FALSE)
  }
  if (aggregation %in% c("sum", "mean", "median", "min", "max") && !is.numeric(df[[column]])) {
    stop("This aggregation requires a numeric field.", call. = FALSE)
  }
  model <- app_state$bi
  clean <- sap_bi_clean_name(name, "Measure")
  if (tolower(clean) %in% tolower(model$measures$name)) stop("Measure names must be unique.", call. = FALSE)
  row <- data.frame(
    id = sap_bi_id("measure"), name = clean, table = table,
    column = column %||% "", aggregation = aggregation, format = format,
    description = description %||% "", created_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  model$measures <- rbind(model$measures, row)
  app_state$bi <- model
  row$id[[1]]
}

sap_bi_measure_value <- function(df, aggregation = "count", column = "") {
  if (!is.data.frame(df)) return(NA_real_)
  if (identical(aggregation, "count")) return(nrow(df))
  if (!column %in% names(df)) return(NA_real_)
  x <- df[[column]]
  switch(
    aggregation,
    distinct_count = length(unique(x[!is.na(x)])),
    sum = sum(as.numeric(x), na.rm = TRUE),
    mean = mean(as.numeric(x), na.rm = TRUE),
    median = stats::median(as.numeric(x), na.rm = TRUE),
    min = suppressWarnings(min(as.numeric(x), na.rm = TRUE)),
    max = suppressWarnings(max(as.numeric(x), na.rm = TRUE)),
    nrow(df)
  )
}

sap_bi_filter_vector <- function(x, operator, value = NULL) {
  operator <- operator %||% "equals"
  if (operator == "is_missing") return(is.na(x))
  if (operator == "not_missing") return(!is.na(x))
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    value <- tryCatch(as.Date(value), error = function(e) value)
  } else if (is.numeric(x)) {
    value <- suppressWarnings(as.numeric(value))
  } else {
    value <- as.character(value)
  }
  out <- switch(
    operator,
    equals = x == value,
    not_equals = x != value,
    greater = x > value,
    greater_equal = x >= value,
    less = x < value,
    less_equal = x <= value,
    contains = grepl(as.character(value), as.character(x), fixed = TRUE, ignore.case = TRUE),
    starts_with = startsWith(tolower(as.character(x)), tolower(as.character(value))),
    ends_with = endsWith(tolower(as.character(x)), tolower(as.character(value))),
    `in` = as.character(x) %in% as.character(value),
    rep(TRUE, length(x))
  )
  out[is.na(out)] <- FALSE
  out
}

sap_bi_apply_step <- function(df, step, tables = list()) {
  op <- step$operation %||% ""
  p <- step$params %||% list()
  if (!isTRUE(step$enabled %||% TRUE)) return(df)
  if (op == "filter") {
    stopifnot(p$field %in% names(df))
    return(df[sap_bi_filter_vector(df[[p$field]], p$operator, p$value), , drop = FALSE])
  }
  if (op == "select") {
    cols <- intersect(as.character(p$columns), names(df))
    if (!length(cols)) stop("The select step removed every field.", call. = FALSE)
    return(df[, cols, drop = FALSE])
  }
  if (op == "rename") {
    stopifnot(p$field %in% names(df))
    candidate <- make.names(sap_bi_clean_name(p$new_name, p$field))
    existing <- names(df)[names(df) != p$field]
    new_name <- tail(make.unique(c(existing, candidate), sep = "_"), 1L)
    names(df)[match(p$field, names(df))] <- new_name
    return(df)
  }
  if (op == "replace_missing") {
    stopifnot(p$field %in% names(df))
    x <- df[[p$field]]
    value <- p$value
    if (is.numeric(x)) value <- suppressWarnings(as.numeric(value))
    if (inherits(x, "Date")) value <- as.Date(value)
    x[is.na(x)] <- value
    df[[p$field]] <- x
    return(df)
  }
  if (op == "convert") {
    stopifnot(p$field %in% names(df))
    df[[p$field]] <- switch(
      p$type,
      numeric = suppressWarnings(as.numeric(as.character(df[[p$field]]))),
      integer = suppressWarnings(as.integer(as.character(df[[p$field]]))),
      character = as.character(df[[p$field]]),
      factor = as.factor(df[[p$field]]),
      logical = as.logical(df[[p$field]]),
      date = suppressWarnings(as.Date(df[[p$field]])),
      df[[p$field]]
    )
    return(df)
  }
  if (op == "sort") {
    stopifnot(p$field %in% names(df))
    ord <- order(df[[p$field]], na.last = TRUE, decreasing = identical(p$direction, "descending"))
    return(df[ord, , drop = FALSE])
  }
  if (op == "deduplicate") {
    cols <- intersect(as.character(p$columns), names(df))
    if (!length(cols)) cols <- names(df)
    return(df[!duplicated(df[, cols, drop = FALSE]), , drop = FALSE])
  }
  if (op == "calculate") {
    stopifnot(p$left %in% names(df))
    left <- df[[p$left]]
    right <- if (!is.null(p$right) && p$right %in% names(df)) df[[p$right]] else p$constant
    selected_fields <- intersect(as.character(p$fields %||% character()), names(df))
    if (!length(selected_fields)) selected_fields <- unique(c(p$left, if (!is.null(p$right)) p$right else character()))
    selected_fields <- intersect(selected_fields, names(df))
    numeric_matrix <- function(fields) {
      if (!length(fields)) stop("Choose one or more source fields.", call. = FALSE)
      as.matrix(data.frame(lapply(df[fields], function(x) suppressWarnings(as.numeric(as.character(x)))), check.names = FALSE))
    }
    num <- function(x) suppressWarnings(as.numeric(as.character(x)))
    value <- switch(
      p$calculation,
      add = num(left) + num(right),
      subtract = num(left) - num(right),
      multiply = num(left) * num(right),
      divide = ifelse(num(right) == 0, NA_real_, num(left) / num(right)),
      percentage = ifelse(num(right) == 0, NA_real_, 100 * num(left) / num(right)),
      average = rowMeans(cbind(num(left), num(right)), na.rm = TRUE),
      concat = paste(as.character(left), as.character(right), sep = p$separator %||% " "),
      coalesce = ifelse(is.na(left) | as.character(left) == "", right, left),
      row_sum = { m <- numeric_matrix(selected_fields); z <- rowSums(m, na.rm = TRUE); z[rowSums(!is.na(m)) == 0] <- NA_real_; z },
      row_mean = { m <- numeric_matrix(selected_fields); z <- rowMeans(m, na.rm = TRUE); z[rowSums(!is.na(m)) == 0] <- NA_real_; z },
      row_min = { m <- numeric_matrix(selected_fields); apply(m, 1L, function(z) if (all(is.na(z))) NA_real_ else min(z, na.rm = TRUE)) },
      row_max = { m <- numeric_matrix(selected_fields); apply(m, 1L, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)) },
      count_non_missing = { m <- df[selected_fields]; rowSums(vapply(m, function(z) !is.na(z) & nzchar(trimws(as.character(z))), logical(nrow(df)))) },
      concat_many = apply(df[selected_fields], 1L, function(z) paste(as.character(z[!is.na(z) & nzchar(trimws(as.character(z)))]), collapse = p$separator %||% " ")),
      stop("Unsupported safe calculation.", call. = FALSE)
    )
    if (identical(p$calculation, "average")) value[!is.finite(value)] <- NA_real_
    candidate <- make.names(sap_bi_clean_name(p$new_field, "Calculated field"))
    new_name <- tail(make.unique(c(names(df), candidate), sep = "_"), 1L)
    df[[new_name]] <- value
    return(df)
  }
  if (op == "join") {
    other <- tables[[p$with_table]]$data %||% tables[[p$with_table]]
    if (!is.data.frame(other)) stop("The joined table is unavailable.", call. = FALSE)
    stopifnot(p$left_key %in% names(df), p$right_key %in% names(other))
    type <- p$join_type %||% "left"
    return(merge(
      df, other, by.x = p$left_key, by.y = p$right_key,
      all.x = type %in% c("left", "full"),
      all.y = type %in% c("right", "full"), sort = FALSE,
      suffixes = c("", paste0("_", make.names(p$with_table)))
    ))
  }
  if (op == "union") {
    other <- tables[[p$with_table]]$data %||% tables[[p$with_table]]
    if (!is.data.frame(other)) stop("The appended table is unavailable.", call. = FALSE)
    common <- intersect(names(df), names(other))
    if (!length(common)) stop("Union requires at least one shared field.", call. = FALSE)
    return(rbind(df[, common, drop = FALSE], other[, common, drop = FALSE]))
  }
  if (op == "pivot_longer") {
    cols <- intersect(as.character(p$columns), names(df))
    if (!length(cols)) stop("Choose fields to pivot.", call. = FALSE)
    return(tidyr::pivot_longer(
      df, cols = dplyr::all_of(cols),
      names_to = p$names_to %||% "variable", values_to = p$values_to %||% "value"
    ))
  }
  if (op == "pivot_wider") {
    stopifnot(p$names_from %in% names(df), p$values_from %in% names(df))
    return(tidyr::pivot_wider(df, names_from = dplyr::all_of(p$names_from), values_from = dplyr::all_of(p$values_from)))
  }
  if (op == "aggregate") {
    groups <- intersect(as.character(p$group_by), names(df))
    if (!length(groups)) stop("Aggregation requires at least one grouping field.", call. = FALSE)
    if (identical(p$aggregation, "count")) {
      out <- stats::aggregate(rep(1, nrow(df)), df[groups], sum)
    } else {
      stopifnot(p$measure %in% names(df))
      fun <- switch(p$aggregation, sum = sum, mean = mean, median = stats::median, min = min, max = max, sum)
      out <- stats::aggregate(df[[p$measure]], df[groups], function(x) fun(x, na.rm = TRUE))
    }
    candidate <- make.names(sap_bi_clean_name(p$new_field, p$aggregation))
    names(out)[ncol(out)] <- tail(make.unique(c(names(out)[-ncol(out)], candidate), sep = "_"), 1L)
    return(out)
  }
  stop("Unknown transformation operation: ", op, call. = FALSE)
}

sap_bi_run_pipeline <- function(source_data, steps, tables = list()) {
  if (!is.data.frame(source_data)) stop("Pipeline source data are unavailable.", call. = FALSE)
  data <- source_data
  log <- vector("list", length(steps))
  if (!length(steps)) return(list(data = data, log = list(), success = TRUE))
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    before <- c(nrow(data), ncol(data))
    result <- tryCatch(
      list(data = sap_bi_apply_step(data, step, tables), error = NULL),
      error = function(e) list(data = data, error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      log[[i]] <- list(id = step$id, status = "failed", message = result$error,
                       rows_before = before[[1]], rows_after = before[[1]])
      return(list(data = data, log = log[seq_len(i)], success = FALSE,
                  failed_step = step$id, error = result$error))
    }
    data <- as.data.frame(result$data, stringsAsFactors = FALSE)
    log[[i]] <- list(id = step$id, status = if (isTRUE(step$enabled %||% TRUE)) "completed" else "skipped",
                     message = "", rows_before = before[[1]], rows_after = nrow(data),
                     cols_before = before[[2]], cols_after = ncol(data))
  }
  list(data = data, log = log, success = TRUE)
}

sap_bi_apply_pipeline <- function(app_state, table_name) {
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  if (!table_name %in% names(model$tables)) stop("Unknown pipeline table.", call. = FALSE)
  steps <- model$pipelines[[table_name]] %||% list()
  result <- sap_bi_run_pipeline(model$tables[[table_name]]$source_data, steps, model$tables)
  if (isTRUE(result$success)) {
    model$tables[[table_name]]$data <- result$data
    model$tables[[table_name]]$schema <- sap_bi_schema(result$data)
    model$tables[[table_name]]$rows <- nrow(result$data)
    model$tables[[table_name]]$cols <- ncol(result$data)
    model$tables[[table_name]]$updated_at <- as.character(Sys.time())
    model$pipeline_log[[table_name]] <- result$log
    app_state$bi <- model
    if (identical(model$active_table, table_name)) sap_bi_sync_active(app_state)
  }
  result
}

sap_bi_add_step <- function(app_state, table_name, operation, label, params) {
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  if (!table_name %in% names(model$tables)) stop("Select a model table first.", call. = FALSE)
  step <- list(
    id = sap_bi_id("step"), operation = operation,
    label = label %||% tools::toTitleCase(gsub("_", " ", operation)),
    params = params %||% list(), enabled = TRUE,
    created_at = as.character(Sys.time())
  )
  model$pipelines[[table_name]] <- c(model$pipelines[[table_name]] %||% list(), list(step))
  app_state$bi <- model
  step$id
}

sap_bi_remove_step <- function(app_state, table_name, step_id) {
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  steps <- model$pipelines[[table_name]] %||% list()
  model$pipelines[[table_name]] <- Filter(function(x) !identical(x$id, step_id), steps)
  app_state$bi <- model
  invisible(TRUE)
}

sap_bi_move_step <- function(app_state, table_name, step_id, direction = -1L) {
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  steps <- model$pipelines[[table_name]] %||% list()
  ids <- vapply(steps, function(x) x$id, character(1))
  pos <- match(step_id, ids)
  target <- pos + as.integer(direction)
  if (is.na(pos) || target < 1L || target > length(steps)) return(invisible(FALSE))
  steps[c(pos, target)] <- steps[c(target, pos)]
  model$pipelines[[table_name]] <- steps
  app_state$bi <- model
  invisible(TRUE)
}

sap_bi_apply_global_filters <- function(app_state, table_name, df = NULL) {
  sap_bi_ensure_state(app_state)
  df <- df %||% sap_bi_get_table(app_state, table_name)
  if (!is.data.frame(df)) return(df)
  filters <- app_state$bi$global_filters %||% list()
  for (flt in filters) {
    if (!identical(flt$table, table_name) || !flt$field %in% names(df)) next
    values <- flt$values %||% character()
    if (length(values)) df <- df[as.character(df[[flt$field]]) %in% as.character(values), , drop = FALSE]
  }
  df
}

sap_bi_summarise_visual <- function(df, x = "", y = "", color = "",
                                    aggregation = "count", top_n = 20L) {
  if (!is.data.frame(df)) return(data.frame())
  groups <- unique(c(x, color))
  groups <- groups[nzchar(groups) & groups %in% names(df)]
  if (!length(groups)) {
    return(data.frame(.sap_value = sap_bi_measure_value(df, aggregation, y)))
  }
  if (identical(aggregation, "count")) {
    result <- stats::aggregate(rep(1, nrow(df)), df[groups], sum)
  } else if (identical(aggregation, "distinct_count")) {
    if (!y %in% names(df)) return(data.frame())
    result <- stats::aggregate(df[[y]], df[groups], function(z) length(unique(z[!is.na(z)])))
  } else {
    if (!y %in% names(df)) return(data.frame())
    fn <- switch(aggregation, sum = sum, mean = mean, median = stats::median,
                 min = min, max = max, sum)
    result <- stats::aggregate(df[[y]], df[groups], function(z) fn(as.numeric(z), na.rm = TRUE))
  }
  names(result)[ncol(result)] <- ".sap_value"
  result <- result[order(result$.sap_value, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  if (is.finite(top_n) && nrow(result) > top_n) result <- head(result, top_n)
  result
}

sap_bi_format_value <- function(x, format = "number", decimals = 1L) {
  if (!length(x) || is.na(x)) return("—")
  switch(
    format,
    percent = paste0(formatC(100 * as.numeric(x), format = "f", digits = decimals, big.mark = ","), "%"),
    currency = paste0("$", formatC(as.numeric(x), format = "f", digits = decimals, big.mark = ",")),
    integer = format(round(as.numeric(x)), big.mark = ",", scientific = FALSE),
    formatC(as.numeric(x), format = "f", digits = decimals, big.mark = ",")
  )
}

sap_bi_model_manifest <- function(app_state) {
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  tables <- lapply(model$tables, function(x) list(
    name = x$name, rows = x$rows, cols = x$cols, schema = x$schema,
    source_type = x$source_type, source_config = sap_bi_redact_config(x$source_config),
    created_at = x$created_at, updated_at = x$updated_at
  ))
  list(
    active_table = model$active_table,
    tables = tables,
    relationships = model$relationships,
    measures = model$measures,
    hierarchies = model$hierarchies,
    pipelines = model$pipelines,
    global_filters = model$global_filters,
    security_rules = model$security_rules,
    refresh_schedules = model$refresh_schedules,
    dashboard = model$dashboard,
    publications = model$publications,
    refresh_runs = utils::head(model$refresh_runs, 100L),
    saved_at = as.character(Sys.time())
  )
}

sap_bi_restore_manifest <- function(app_state, manifest) {
  if (!is.list(manifest)) return(invisible(FALSE))
  sap_bi_ensure_state(app_state)
  model <- app_state$bi
  for (nm in c("relationships", "measures", "hierarchies", "pipelines",
               "global_filters", "security_rules", "refresh_schedules",
               "dashboard", "publications", "refresh_runs")) {
    if (!is.null(manifest[[nm]])) model[[nm]] <- manifest[[nm]]
  }
  if (!is.null(manifest$active_table) && manifest$active_table %in% names(model$tables)) {
    model$active_table <- manifest$active_table
  }
  app_state$bi <- model
  invisible(TRUE)
}

sap_bi_is_private_ip <- function(ip) {
  ip <- trimws(as.character(ip))
  grepl("^(127\\.|10\\.|0\\.|169\\.254\\.|192\\.168\\.|::1$|fc|fd|fe80)", ip, ignore.case = TRUE) |
    grepl("^172\\.(1[6-9]|2[0-9]|3[01])\\.", ip)
}

sap_bi_validate_remote_url <- function(url, allow_http = FALSE, allowed_hosts = NULL) {
  url <- trimws(as.character(url %||% ""))
  parsed <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$scheme) || is.null(parsed$hostname)) {
    stop("Enter a complete HTTPS URL.", call. = FALSE)
  }
  allowed <- if (isTRUE(allow_http)) c("https", "http") else "https"
  if (!tolower(parsed$scheme) %in% allowed) stop("Only HTTPS data sources are allowed.", call. = FALSE)
  host <- tolower(parsed$hostname)
  allow_private <- tolower(Sys.getenv("SAP_ALLOW_PRIVATE_REMOTE_URLS", "false")) %in% c("true", "1", "yes")
  if (is.null(allowed_hosts)) {
    allowed_hosts <- unlist(strsplit(tolower(Sys.getenv("SAP_ALLOWED_REMOTE_HOSTS", "")), ",", fixed = TRUE))
  }
  allowed_hosts <- trimws(tolower(as.character(allowed_hosts)))
  allowed_hosts <- allowed_hosts[nzchar(allowed_hosts)]
  if (length(allowed_hosts) && !any(host == allowed_hosts | vapply(allowed_hosts, function(x) endsWith(host, paste0(".", x)), logical(1)))) {
    stop("The remote host is not present in SAP_ALLOWED_REMOTE_HOSTS.", call. = FALSE)
  }
  if (!allow_private && (host %in% c("localhost", "localhost.localdomain") || endsWith(host, ".local") || sap_bi_is_private_ip(host))) {
    stop("Private-network and loopback destinations are blocked.", call. = FALSE)
  }
  if (!allow_private && requireNamespace("curl", quietly = TRUE)) {
    resolved <- tryCatch(curl::nslookup(host), error = function(e) character())
    if (length(resolved) && any(sap_bi_is_private_ip(resolved))) {
      stop("The destination resolves to a private-network address.", call. = FALSE)
    }
  }
  url
}

sap_bi_download_remote <- function(url, max_mb = 100, timeout_seconds = 30) {
  url <- sap_bi_validate_remote_url(url, allow_http = identical(Sys.getenv("SAP_ALLOW_HTTP_IMPORT", "false"), "true"))
  ext <- tools::file_ext(httr::parse_url(url)$path %||% "")
  tmp <- tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else "")
  handle <- curl::new_handle()
  configured_limit <- suppressWarnings(as.numeric(Sys.getenv("SAP_REMOTE_MAX_MB", "100")))
  if (!is.finite(configured_limit) || configured_limit < 1) configured_limit <- 100
  requested_limit <- suppressWarnings(as.numeric(max_mb))
  if (!is.finite(requested_limit) || requested_limit < 1) requested_limit <- configured_limit
  max_mb <- min(requested_limit, configured_limit)
  timeout_seconds <- suppressWarnings(as.numeric(timeout_seconds))
  if (!is.finite(timeout_seconds) || timeout_seconds < 1) timeout_seconds <- 30
  curl::handle_setopt(
    handle, timeout = timeout_seconds, connecttimeout = 10,
    followlocation = FALSE,
    useragent = "Survey-Intelligence-Studio/4.0"
  )
  limit <- as.numeric(max_mb) * 1024^2
  received <- 0
  con <- file(tmp, open = "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  stream_error <- NULL
  response <- tryCatch(
    curl::curl_fetch_stream(url, function(chunk) {
      received <<- received + length(chunk)
      if (received > limit) {
        stream_error <<- "Remote file exceeds the configured import limit."
        return(FALSE)
      }
      writeBin(chunk, con)
      TRUE
    }, handle = handle),
    error = function(e) {
      if (is.null(stream_error)) stream_error <<- conditionMessage(e)
      NULL
    }
  )
  close(con)
  con <- NULL
  if (!is.null(stream_error)) {
    unlink(tmp)
    stop(stream_error, call. = FALSE)
  }
  if (!is.null(response$status_code) && response$status_code >= 300L && response$status_code < 400L) {
    unlink(tmp)
    stop("Remote redirects are blocked. Use the final direct HTTPS file URL.", call. = FALSE)
  }
  if (!is.null(response$status_code) && response$status_code >= 400L) {
    unlink(tmp)
    stop(sprintf("Remote source returned HTTP %s.", response$status_code), call. = FALSE)
  }
  tmp
}

sap_bi_read_file <- function(path, display_name = basename(path), sheet = NULL) {
  ext <- tolower(tools::file_ext(display_name))
  out <- switch(
    ext,
    csv = utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    tsv = utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE),
    txt = utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE),
    xls = readxl::read_excel(path, sheet = sheet %||% 1),
    xlsx = readxl::read_excel(path, sheet = sheet %||% 1),
    sav = haven::read_sav(path),
    dta = haven::read_dta(path),
    sas7bdat = haven::read_sas(path),
    json = jsonlite::fromJSON(path, flatten = TRUE),
    parquet = {
      sap_optional_call(
        "arrow", "read_parquet", path, as_data_frame = TRUE,
        feature = "Parquet import"
      )
    },
    stop("Unsupported file type: .", ext, call. = FALSE)
  )
  if (!is.data.frame(out)) out <- as.data.frame(out, stringsAsFactors = FALSE)
  out
}

sap_bi_read_sql <- function(query, host, port, database, user, password,
                            sslmode = "require", max_rows = 500000L) {
  query <- trimws(query %||% "")
  if (!grepl("^(SELECT|WITH)\\b", query, ignore.case = TRUE) ||
      grepl(";|\\b(INSERT|UPDATE|DELETE|DROP|ALTER|TRUNCATE|COPY|GRANT|REVOKE)\\b", query, ignore.case = TRUE)) {
    stop("Only a single read-only SELECT or WITH query is allowed.", call. = FALSE)
  }
  driver <- sap_optional_call("RPostgres", "Postgres", feature = "PostgreSQL data connections")
  con <- sap_optional_call(
    "DBI", "dbConnect", driver, host = host, port = as.integer(port), dbname = database,
    user = user, password = password, sslmode = sslmode,
    connect_timeout = 10, options = "-c statement_timeout=120000",
    feature = "PostgreSQL data connections"
  )
  on.exit(sap_optional_call("DBI", "dbDisconnect", con), add = TRUE)
  wrapped <- sprintf("SELECT * FROM (%s) sap_source_query LIMIT %d", query, as.integer(max_rows))
  sap_optional_call("DBI", "dbGetQuery", con, wrapped, feature = "PostgreSQL data connections")
}

message("[SAP] BI Studio core v4.0.0 loaded")
