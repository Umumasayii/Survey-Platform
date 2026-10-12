# =============================================================================
# MODULE 17: VARIABLE MANAGER
# =============================================================================
# A centralized interface for managing every variable's metadata in one place —
# the "define variables" screen you'd find in SPSS. It SURFACES the existing
# Metadata Engine (MetadataService + variable_dictionary); it does not duplicate
# storage. Everything persists through services$metadata$update_variable(), so
# the dictionary, every module that reads labels, and the project stay in sync.
#
# Capabilities (all no-code, through the interface):
#   - Search and filter the variable list
#   - Edit label, description, units inline
#   - Change measurement level (nominal / ordinal / interval / ratio)
#   - Assign a survey role (outcome / predictor / id / weight / none)
#   - Per-variable editor for value labels (code -> label) and missing codes
#   - Refresh types/counts from the current dataset (keeps user labels)
#
# Note on "data type": the underlying R type is derived from the data and shown
# read-only here. Storage-type recasting remains in Data Cleaning / Transformation.
# This module does provide the requested no-code Create Variable / Custom Column
# workflow and persists the resulting processed dataset through DatasetService.
# =============================================================================

MEASUREMENT_LEVELS <- c("nominal", "ordinal", "interval", "ratio")
SURVEY_ROLES       <- c("none", "outcome", "predictor", "id", "weight", "group", "date")

# ---- UI ---------------------------------------------------------------------

mod_variable_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex justify-content-between align-items-center mb-3",
        h4(class = "mb-0", tagList(icon("table-list"), " Variable Manager")),
        div(class = "d-flex gap-2",
            actionButton(ns("refresh"), "Refresh from dataset",
                         class = "btn-sm btn-outline-secondary", icon = icon("rotate")),
            report_button(ns, "add_var_manager", "Add to Report"))),

    p(class = "text-muted small",
      "Define and edit every variable's metadata in one place. Changes save to the project dictionary and are used by all analysis modules."),

    # Filter / search row
    div(class = "card mb-3", div(class = "card-body py-2",
      fluidRow(
        column(5, textInput(ns("search"), NULL, placeholder = "Search variable or label...",
                            width = "100%")),
        column(4, selectInput(ns("filter_level"), NULL,
                              choices = c("All measurement levels" = "", MEASUREMENT_LEVELS),
                              width = "100%")),
        column(3, selectInput(ns("filter_role"), NULL,
                              choices = c("All roles" = "", SURVEY_ROLES),
                              width = "100%"))
      )
    )),

    # Summary line
    uiOutput(ns("summary")),

    # No-code custom column workflow
    div(class = "card mb-3", div(class = "card-body",
      h6(tagList(icon("square-plus"), " Create Variable / Custom Column")),
      p(class = "text-muted small", "Create a new variable from existing variables using selectable methods. No R formula is required."),
      fluidRow(
        column(4, textInput(ns("custom_name"), "New variable name", placeholder = "e.g. total_score")),
        column(4, selectizeInput(ns("custom_fields"), "Source variables", choices = character(), multiple = TRUE, options = list(plugins = list("remove_button")))),
        column(4, selectInput(ns("custom_method"), "Calculation / transformation method", choices = c(
          "Add" = "add", "Subtract" = "subtract", "Multiply" = "multiply", "Divide" = "divide",
          "Percentage" = "percentage", "Average / mean" = "average", "Concatenate text" = "concat",
          "Coalesce / first non-missing value" = "coalesce", "Row sum" = "row_sum", "Row mean" = "row_mean",
          "Row minimum" = "row_min", "Row maximum" = "row_max", "Count non-missing fields" = "count_non_missing",
          "Concatenate several variables" = "concat_many"
        )))
      ),
      fluidRow(
        column(8, textInput(ns("custom_separator"), "Text separator", value = " ")),
        column(4, div(style = "padding-top:31px;", actionButton(ns("create_custom"), "Create variable", icon = icon("plus"), class = "btn-primary w-100")))
      )
    )),

    # The editable variable grid
    div(class = "card", div(class = "card-body",
      withSpinner(DT::DTOutput(ns("grid")), type = 8, color = "#0d6efd")
    )),

    # Per-variable detail editor (value labels + missing codes)
    div(class = "card mt-3", div(class = "card-body",
      h6(tagList(icon("tags"), " Value labels & missing codes")),
      p(class = "text-muted small",
        "Select a variable in the table above, then define what its codes mean (e.g. 1 = Male, 2 = Female) and which codes represent missing data."),
      uiOutput(ns("detail_editor"))
    ))
  )
}

# ---- Server -----------------------------------------------------------------

mod_variable_manager_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive pull of the dictionary. Depends on the metadata tick so it
    # refreshes whenever any module updates a variable.
    dict <- reactive({
      if (is.null(services) || is.null(services$metadata)) return(data.frame())
      # subscribe to the metadata version so edits anywhere refresh this view
      services$metadata$version()
      d <- tryCatch(services$metadata$get(), error = function(e) data.frame())
      if (!is.data.frame(d)) data.frame() else d
    })

    # Apply search + filters.
    filtered <- reactive({
      d <- dict()
      if (!nrow(d)) return(d)
      q <- trimws(tolower(input$search %||% ""))
      if (nzchar(q)) {
        hay <- tolower(paste(d$variable %||% "", d$label %||% ""))
        d <- d[grepl(q, hay, fixed = TRUE), , drop = FALSE]
      }
      if (!is.null(input$filter_level) && nzchar(input$filter_level))
        d <- d[!is.na(d$measurement_level) & d$measurement_level == input$filter_level, , drop = FALSE]
      if (!is.null(input$filter_role) && nzchar(input$filter_role))
        d <- d[!is.na(d$survey_role) & d$survey_role == input$filter_role, , drop = FALSE]
      d
    })

    output$summary <- renderUI({
      d <- dict(); f <- filtered()
      if (!nrow(d)) {
        return(div(class = "alert alert-info",
          icon("circle-info"),
          " No variables catalogued yet. Import a dataset, then click 'Refresh from dataset' to populate the dictionary."))
      }
      div(class = "small text-muted mb-2",
          sprintf("Showing %d of %d variables", nrow(f), nrow(d)))
    })

    # The editable columns we present (subset of the dictionary, in a friendly
    # order). data_type is read-only (derived from the data).
    display_cols <- c("variable", "label", "data_type", "measurement_level",
                      "survey_role", "units", "description", "n_missing", "n_unique")

    output$grid <- DT::renderDT({
      d <- filtered()
      if (!nrow(d)) {
        return(DT::datatable(
          data.frame(Message = "No variables match the current filters."),
          rownames = FALSE, options = list(dom = "t")))
      }
      show <- d[, intersect(display_cols, names(d)), drop = FALSE]
      # Friendly headers
      headers <- c(variable = "Variable", label = "Label", data_type = "Type",
                   measurement_level = "Level", survey_role = "Role",
                   units = "Units", description = "Description",
                   n_missing = "Missing", n_unique = "Unique")
      colnames(show) <- headers[names(show)] %||% names(show)

      DT::datatable(
        show, rownames = FALSE, selection = "single",
        editable = list(target = "cell", disable = list(
          columns = which(colnames(show) %in% c("Variable", "Type", "Missing", "Unique")) - 1)),
        options = list(pageLength = 15, scrollX = TRUE,
                       columnDefs = list(list(className = "dt-left", targets = "_all"))),
        class = "stripe hover compact")
    }, server = FALSE)

    # Persist an inline grid edit.
    observeEvent(input$grid_cell_edit, {
      info <- input$grid_cell_edit
      d <- filtered(); req(nrow(d) > 0)
      # map the display column index back to the dictionary field name
      shown <- d[, intersect(display_cols, names(d)), drop = FALSE]
      field <- names(shown)[info$col + 1]
      # never edit the identity or derived columns
      if (field %in% c("variable", "data_type", "n_missing", "n_unique")) {
        if (!is.null(services$notify)) services$notify$warning("That column can't be edited here.")
        return(invisible(NULL))
      }
      var_name <- shown$variable[info$row]
      new_val  <- trimws(info$value)

      # validate constrained fields
      if (field == "measurement_level" && nzchar(new_val) &&
          !new_val %in% MEASUREMENT_LEVELS) {
        if (!is.null(services$notify))
          services$notify$error(sprintf("Level must be one of: %s",
                                        paste(MEASUREMENT_LEVELS, collapse = ", ")))
        return(invisible(NULL))
      }
      if (field == "survey_role" && nzchar(new_val) && !new_val %in% SURVEY_ROLES) {
        if (!is.null(services$notify))
          services$notify$error(sprintf("Role must be one of: %s",
                                        paste(SURVEY_ROLES, collapse = ", ")))
        return(invisible(NULL))
      }

      fields <- list(); fields[[field]] <- new_val
      ok <- services$metadata$update_variable(var_name, fields)
      if (!is.null(services$notify)) {
        if (ok) services$notify$success(sprintf("Updated %s for '%s'", field, var_name))
        else    services$notify$error(sprintf("Could not update '%s'", var_name))
      }
    }, ignoreInit = TRUE)

    # Refresh types/counts from the current dataset (keeps user labels).
    observeEvent(input$refresh, {
      if (is.null(services) || is.null(services$metadata)) return(invisible(NULL))
      df <- if (!is.null(services$data)) services$data$get("processed") else NULL
      if (!is.data.frame(df) || !ncol(df)) {
        if (!is.null(services$notify))
          services$notify$warning("No dataset loaded to catalogue.")
        return(invisible(NULL))
      }
      n <- services$metadata$populate(df)
      if (!is.null(services$notify))
        services$notify$success(sprintf("Catalogued %d variables.", n))
    })

    observeEvent(app_state$processed_data, {
      df <- if (!is.null(services) && !is.null(services$data)) services$data$get("processed") else app_state$processed_data
      fields <- if (is.data.frame(df)) names(df) else character()
      updateSelectizeInput(session, "custom_fields", choices = fields, selected = intersect(input$custom_fields %||% character(), fields), server = TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$create_custom, {
      if (is.null(services) || is.null(services$data)) return(invisible(NULL))
      df <- services$data$get("processed")
      if (!is.data.frame(df) || !nrow(df)) return(services$notify$warning("Import data before creating a variable."))
      fields <- intersect(as.character(input$custom_fields %||% character()), names(df))
      if (!length(fields)) return(services$notify$warning("Choose at least one source variable."))
      method <- input$custom_method %||% "add"
      binary <- c("add","subtract","multiply","divide","percentage")
      if (method %in% binary && length(fields) < 2L) return(services$notify$warning("This method requires two source variables."))
      requested_name <- trimws(input$custom_name %||% "")
      if (!nzchar(requested_name)) return(services$notify$warning("Enter a new variable name."))
      new_name <- make.names(requested_name)
      if (new_name %in% names(df)) return(services$notify$warning("That variable already exists. Choose another name."))
      sep <- input$custom_separator %||% " "
      num <- function(x) suppressWarnings(as.numeric(as.character(x)))
      mat <- function(fs) as.matrix(data.frame(lapply(df[fs], num), check.names = FALSE))
      first <- df[[fields[[1]]]]
      second <- if (length(fields) >= 2L) df[[fields[[2]]]] else NULL
      value <- tryCatch(switch(method,
        add = num(first) + num(second),
        subtract = num(first) - num(second),
        multiply = num(first) * num(second),
        divide = ifelse(num(second) == 0, NA_real_, num(first) / num(second)),
        percentage = ifelse(num(second) == 0, NA_real_, 100 * num(first) / num(second)),
        average = { m <- mat(fields); z <- rowMeans(m, na.rm = TRUE); z[rowSums(!is.na(m)) == 0] <- NA_real_; z },
        concat = paste(as.character(first), as.character(second %||% ""), sep = sep),
        coalesce = apply(df[fields], 1L, function(z) { z <- as.character(z); z <- z[!is.na(z) & nzchar(trimws(z))]; if (length(z)) z[[1]] else NA_character_ }),
        row_sum = { m <- mat(fields); z <- rowSums(m, na.rm = TRUE); z[rowSums(!is.na(m)) == 0] <- NA_real_; z },
        row_mean = { m <- mat(fields); z <- rowMeans(m, na.rm = TRUE); z[rowSums(!is.na(m)) == 0] <- NA_real_; z },
        row_min = { m <- mat(fields); apply(m, 1L, function(z) if (all(is.na(z))) NA_real_ else min(z, na.rm = TRUE)) },
        row_max = { m <- mat(fields); apply(m, 1L, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)) },
        count_non_missing = rowSums(vapply(df[fields], function(z) !is.na(z) & nzchar(trimws(as.character(z))), logical(nrow(df)))),
        concat_many = apply(df[fields], 1L, function(z) { z <- as.character(z); paste(z[!is.na(z) & nzchar(trimws(z))], collapse = sep) }),
        stop("Unsupported custom-column method.", call. = FALSE)
      ), error = function(e) e)
      if (inherits(value, "error")) return(services$notify$error(conditionMessage(value)))
      df[[new_name]] <- value
      services$data$set(df, kind = "processed", module = "variable_manager", description = paste("Created custom variable", new_name))
      if (!is.null(services$metadata)) try(services$metadata$populate(df), silent = TRUE)

      # Keep the Advanced BI table synchronized when it mirrors the active legacy dataset.
      if (exists("sap_bi_ensure_state", mode = "function")) {
        try({
          sap_bi_ensure_state(app_state)
          active <- app_state$bi$active_table %||% ""
          if (nzchar(active) && active %in% names(app_state$bi$tables) && identical(app_state$bi$tables[[active]]$source_type %||% "", "legacy_import")) {
            model <- app_state$bi
            model$tables[[active]]$source_data <- df
            model$tables[[active]]$data <- df
            model$tables[[active]]$schema <- sap_bi_schema(df)
            model$tables[[active]]$rows <- nrow(df); model$tables[[active]]$cols <- ncol(df)
            app_state$bi <- model
          }
        }, silent = TRUE)
      }
      updateTextInput(session, "custom_name", value = "")
      services$notify$success(sprintf("Created variable '%s' using %s.", new_name, gsub("_", " ", method)))
    }, ignoreInit = TRUE)

    # ---- Per-variable detail editor -----------------------------------------
    selected_var <- reactive({
      sel <- input$grid_rows_selected
      d <- filtered()
      if (is.null(sel) || !nrow(d)) return(NULL)
      d$variable[sel]
    })

    output$detail_editor <- renderUI({
      v <- selected_var()
      if (is.null(v)) {
        return(div(class = "text-muted small",
                   "Select a variable above to edit its value labels and missing codes."))
      }
      row <- dict()[dict()$variable == v, , drop = FALSE]
      vl  <- if (nrow(row) && !is.na(row$value_labels[1])) row$value_labels[1] else ""
      mc  <- if (nrow(row) && !is.na(row$missing_codes[1])) row$missing_codes[1] else ""
      tagList(
        div(class = "mb-2", tags$strong("Editing: "), tags$code(v)),
        fluidRow(
          column(6,
            tags$label(class = "form-label small", "Value labels (one per line, code = label)"),
            tags$textarea(id = ns("vl_text"), class = "form-control", rows = 5,
                          placeholder = "1 = Male\n2 = Female\n9 = Refused",
                          .value_labels_to_text(vl))),
          column(6,
            tags$label(class = "form-label small", "Missing codes (comma-separated)"),
            textInput(ns("mc_text"), NULL, value = .missing_codes_to_text(mc),
                      placeholder = "e.g. 99, -1, NA"),
            div(class = "small text-muted mt-1",
                "Codes listed here are treated as missing in analyses."))
        ),
        div(class = "mt-2",
            actionButton(ns("save_detail"), "Save value labels & missing codes",
                         class = "btn-sm btn-primary", icon = icon("floppy-disk")))
      )
    })

    observeEvent(input$save_detail, {
      v <- selected_var(); req(!is.null(v))
      # parse value labels ("1 = Male" per line) into a named list
      vl_parsed <- .parse_value_labels(input$vl_text %||% "")
      mc_parsed <- .parse_missing_codes(input$mc_text %||% "")
      fields <- list(
        value_labels  = vl_parsed,      # named list -> JSON by db layer
        missing_codes = mc_parsed       # vector -> JSON by db layer
      )
      ok <- services$metadata$update_variable(v, fields)
      if (!is.null(services$notify)) {
        if (ok) services$notify$success(sprintf("Saved codes for '%s'.", v))
        else    services$notify$error(sprintf("Could not save codes for '%s'.", v))
      }
    })

    # ---- Report -------------------------------------------------------------
    register_report_button(
      input, services, "add_var_manager", kind = "table",
      title   = function() "Variable dictionary",
      payload = function() {
        d <- dict()
        if (!nrow(d)) return(NULL)
        d[, intersect(display_cols, names(d)), drop = FALSE]
      },
      code = function() "# Variable dictionary exported from the Variable Manager")
  })
}

# ---- helpers (module-level, pure) -------------------------------------------

#' JSON value-labels string -> "code = label" text block for editing.
.value_labels_to_text <- function(json_str) {
  if (is.null(json_str) || !nzchar(json_str)) return("")
  parsed <- tryCatch(jsonlite::fromJSON(json_str), error = function(e) NULL)
  if (is.null(parsed) || !length(parsed)) return("")
  paste(sprintf("%s = %s", names(parsed), unlist(parsed)), collapse = "\n")
}

#' "code = label" text block -> named list for storage.
.parse_value_labels <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  out <- list()
  for (ln in lines) {
    ln <- trimws(ln)
    if (!nzchar(ln) || !grepl("=", ln, fixed = TRUE)) next
    parts <- strsplit(ln, "=", fixed = TRUE)[[1]]
    code  <- trimws(parts[1])
    lab   <- trimws(paste(parts[-1], collapse = "="))
    if (nzchar(code)) out[[code]] <- lab
  }
  if (length(out)) out else NULL
}

#' JSON missing-codes -> comma text.
.missing_codes_to_text <- function(json_str) {
  if (is.null(json_str) || !nzchar(json_str)) return("")
  parsed <- tryCatch(jsonlite::fromJSON(json_str), error = function(e) NULL)
  if (is.null(parsed) || !length(parsed)) return("")
  paste(unlist(parsed), collapse = ", ")
}

#' comma text -> character vector for storage.
.parse_missing_codes <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  codes <- trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  codes <- codes[nzchar(codes)]
  if (length(codes)) codes else NULL
}

message("[SAP] Variable Manager v5.4.0 loaded")  # SAP_MODULE_BANNER

# =============================================================================
# END OF MODULE 17
# =============================================================================
