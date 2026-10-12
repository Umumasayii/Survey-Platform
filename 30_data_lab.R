# =============================================================================
# MODULE 30: DATA LAB
# =============================================================================
# A safe learning and prototyping workspace for:
#   * every dataset shipped with base R's {datasets} package
#   * the optional palmerpenguins::penguins teaching dataset
#   * configurable synthetic survey-shaped data
# =============================================================================

.sap_dataset_catalog <- function() {
  info <- tryCatch(utils::data(package = "datasets")$results,
                   error = function(e) NULL)
  if (is.null(info) || !nrow(info)) {
    return(data.frame(
      source = character(), object = character(), load_key = character(),
      title = character(), stringsAsFactors = FALSE
    ))
  }

  item <- as.character(info[, "Item"])
  has_loader <- grepl("\\([^()]+\\)\\s*$", item)
  object <- trimws(sub("\\s*\\([^()]+\\)\\s*$", "", item))
  load_key <- item
  load_key[has_loader] <- sub("^.*\\(([^()]*)\\)\\s*$", "\\1", item[has_loader])

  out <- data.frame(
    source = "datasets",
    object = object,
    load_key = trimws(load_key),
    title = as.character(info[, "Title"]),
    stringsAsFactors = FALSE
  )
  out <- out[!duplicated(out[c("source", "object", "load_key")]), , drop = FALSE]

  if (sap_optional_available("palmerpenguins")) {
    out <- rbind(
      data.frame(
        source = "palmerpenguins", object = "penguins", load_key = "penguins",
        title = "Palmer Archipelago penguin measurements",
        stringsAsFactors = FALSE
      ),
      out
    )
  }

  rownames(out) <- NULL
  out$id <- as.character(seq_len(nrow(out)))
  out
}

.sap_dataset_to_frame <- function(x, object_name = "dataset") {
  add_row_names <- function(df, original) {
    rn <- rownames(original)
    if (!is.null(rn) && length(rn) == nrow(df) &&
        !identical(rn, as.character(seq_len(nrow(df))))) {
      df <- data.frame(.row = rn, df, stringsAsFactors = FALSE, check.names = FALSE)
    }
    rownames(df) <- NULL
    df
  }

  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (inherits(x, "dist")) {
    return(add_row_names(as.data.frame(as.matrix(x), check.names = FALSE), x))
  }
  if (inherits(x, "ts")) {
    values <- as.matrix(x)
    if (is.null(colnames(values))) colnames(values) <- "value"
    return(data.frame(
      time = as.numeric(stats::time(x)),
      as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE),
      check.names = FALSE
    ))
  }
  if (is.table(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (is.matrix(x) || is.array(x)) {
    if (length(dim(x)) > 2L) {
      return(as.data.frame(as.table(x), stringsAsFactors = FALSE))
    }
    return(add_row_names(as.data.frame(x, stringsAsFactors = FALSE,
                                       check.names = FALSE), x))
  }
  if (is.atomic(x) && is.null(dim(x))) {
    values <- as.vector(x)
    labels <- names(x)
    if (!is.null(labels) && length(labels) == length(values) && any(nzchar(labels))) {
      return(data.frame(name = labels, value = values, stringsAsFactors = FALSE))
    }
    return(data.frame(value = values, stringsAsFactors = FALSE))
  }
  if (is.list(x)) {
    lens <- lengths(x)
    if (length(lens) && length(unique(lens)) == 1L &&
        all(vapply(x, function(v) is.atomic(v) && is.null(dim(v)), logical(1)))) {
      return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
    }

    # Several base-R teaching objects are covariance/correlation bundles such
    # as ability.cov and Harman*.cor. Use their principal matrix as the active
    # rectangular dataset instead of excluding them from the library.
    matrix_parts <- names(x)[vapply(x, function(v) is.matrix(v) || is.table(v), logical(1))]
    if (length(matrix_parts)) {
      chosen <- if ("cov" %in% matrix_parts) "cov" else matrix_parts[[1]]
      return(add_row_names(
        as.data.frame(as.matrix(x[[chosen]]), stringsAsFactors = FALSE,
                      check.names = FALSE),
        x[[chosen]]
      ))
    }
  }

  stop(sprintf("'%s' cannot be represented safely as a rectangular dataset.",
               object_name), call. = FALSE)
}

.sap_load_catalog_dataset <- function(catalog_row) {
  source <- catalog_row$source[[1]]
  object_name <- catalog_row$object[[1]]
  load_key <- catalog_row$load_key[[1]]

  if (identical(source, "palmerpenguins")) {
    x <- getExportedValue("palmerpenguins", object_name)
  } else {
    env <- new.env(parent = baseenv())
    utils::data(list = load_key, package = source, envir = env)
    available <- ls(env, all.names = TRUE)
    if (!length(available)) stop("The selected dataset did not load.", call. = FALSE)
    chosen <- if (object_name %in% available) object_name else available[[1]]
    x <- get(chosen, envir = env, inherits = FALSE)
  }

  .sap_dataset_to_frame(x, object_name)
}

mod_data_lab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "mb-4",
      h2(icon("flask"), "Data Lab"),
      p(
        "Learn the platform with trusted teaching datasets or generate synthetic data before importing a real survey.",
        class = "text-muted mb-0"
      )
    ),

    navset_card_tab(
      nav_panel(
        "Built-in R datasets",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card h-100",
            card_header(icon("book-open"), " Dataset library"),
            card_body(
              selectizeInput(
                ns("r_dataset"), "Choose a dataset", choices = NULL,
                options = list(placeholder = "Search iris, mtcars, AirPassengers…")
              ),
              uiOutput(ns("r_dataset_details")),
              actionButton(
                ns("load_r_dataset"), "Use this dataset",
                icon = icon("play"), class = "btn btn-primary w-100 mt-3"
              ),
              div(
                class = "small text-muted mt-3",
                icon("circle-info"),
                " The library is generated dynamically from the installed R datasets package. Non-tabular objects are converted to analysis-ready data frames when possible."
              )
            )
          ),
          card(
            class = "sap-card h-100",
            card_header(icon("table"), " Preview"),
            card_body(
              uiOutput(ns("r_preview_status")),
              DT::DTOutput(ns("r_preview"))
            )
          )
        )
      ),

      nav_panel(
        "Random data generator",
        layout_columns(
          col_widths = c(4, 8),
          card(
            class = "sap-card h-100",
            card_header(icon("dice"), " Generator settings"),
            card_body(
              numericInput(ns("gen_rows"), "Rows", value = 500, min = 10, max = 100000, step = 10),
              sliderInput(ns("gen_numeric"), "Numeric variables", min = 0, max = 20, value = 4),
              sliderInput(ns("gen_categorical"), "Categorical variables", min = 0, max = 20, value = 3),
              sliderInput(ns("gen_likert"), "Likert items", min = 0, max = 30, value = 5),
              sliderInput(ns("gen_missing"), "Missing values (%)", min = 0, max = 40, value = 5),
              numericInput(ns("gen_seed"), "Random seed", value = 20260731, min = 1, max = 2147483646),
              checkboxInput(ns("gen_date"), "Include survey date", value = TRUE),
              actionButton(ns("generate"), "Generate preview", icon = icon("wand-magic-sparkles"),
                           class = "btn btn-primary w-100"),
              actionButton(ns("load_generated"), "Use generated data", icon = icon("play"),
                           class = "btn btn-outline-primary w-100 mt-2"),
              downloadButton(ns("download_generated"), "Download CSV",
                             class = "btn btn-outline-secondary w-100 mt-2")
            )
          ),
          card(
            class = "sap-card h-100",
            card_header(icon("table-cells"), " Generated preview"),
            card_body(
              uiOutput(ns("generated_status")),
              DT::DTOutput(ns("generated_preview"))
            )
          )
        )
      )
    )
  )
}

mod_data_lab_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    catalog <- .sap_dataset_catalog()
    selected_data <- reactiveVal(NULL)
    generated_data <- reactiveVal(NULL)

    activate_dataset <- function(df, dataset_name, description) {
      stopifnot(is.data.frame(df))
      variable_types <- tryCatch(detect_variable_types(df), error = function(e) NULL)
      app_state$dataset_name <- dataset_name
      if (!is.null(services) && !is.null(services$data)) {
        if (!is.null(services$project) && !is.null(app_state$current_user) &&
            is.null(services$project$current_id())) {
          services$project$create(
            name = paste(dataset_name, "analysis"),
            description = "Auto-created from Data Lab",
            persist_current = FALSE
          )
        }
        services$data$set(
          df, kind = "raw", module = "data_lab",
          description = paste("Stored original:", description),
          variable_types = variable_types, name = dataset_name
        )
        services$data$set(
          df, kind = "processed", module = "data_lab",
          description = description, variable_types = variable_types,
          name = dataset_name
        )
      } else {
        app_state$raw_data <- df
        app_state$processed_data <- df
        app_state$data_imported <- TRUE
        app_state$variable_types <- variable_types
      }
      if (is.null(app_state$current_project))
        app_state$current_project_name <- "Data Lab"
      app_state$quality_score <- tryCatch(calculate_quality_score(df)$score,
                                          error = function(e) NA_real_)
      app_state$transformation_count <- 0L
      app_state$last_action <- description
      showNotification(
        sprintf("%s is active: %s rows × %s columns",
                dataset_name, format(nrow(df), big.mark = ","), ncol(df)),
        type = "message", duration = 5
      )
    }

    if (nrow(catalog)) {
      labels <- sprintf("%s — %s", catalog$object, catalog$title)
      if (any(catalog$source != "datasets")) {
        labels[catalog$source != "datasets"] <- sprintf(
          "%s — %s [%s]", catalog$object[catalog$source != "datasets"],
          catalog$title[catalog$source != "datasets"],
          catalog$source[catalog$source != "datasets"]
        )
      }
      updateSelectizeInput(
        session, "r_dataset", choices = stats::setNames(catalog$id, labels),
        selected = { idx <- match("iris", catalog$object); if (is.na(idx)) catalog$id[[1]] else catalog$id[[idx]] },
        server = TRUE
      )
    }

    selected_row <- reactive({
      req(input$r_dataset)
      row <- catalog[catalog$id == input$r_dataset, , drop = FALSE]
      shiny::validate(shiny::need(nrow(row) == 1L, "Choose a dataset."))
      row
    })

    observeEvent(selected_row(), {
      row <- selected_row()
      df <- tryCatch(
        .sap_load_catalog_dataset(row),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
      selected_data(df)
    }, ignoreInit = FALSE)

    output$r_dataset_details <- renderUI({
      row <- selected_row()
      df <- selected_data()
      tagList(
        div(class = "small fw-semibold", row$object[[1]]),
        div(class = "small text-muted", row$title[[1]]),
        if (!is.null(df)) div(
          class = "d-flex gap-2 flex-wrap mt-2",
          span(class = "badge text-bg-light", sprintf("%s rows", format(nrow(df), big.mark = ","))),
          span(class = "badge text-bg-light", sprintf("%s columns", ncol(df))),
          span(class = "badge text-bg-light", paste("Source:", row$source[[1]]))
        )
      )
    })

    output$r_preview_status <- renderUI({
      if (is.null(selected_data())) {
        div(class = "alert alert-light", "Choose a supported dataset to preview it.")
      } else NULL
    })

    output$r_preview <- DT::renderDT({
      req(selected_data())
      DT::datatable(
        head(selected_data(), 250), rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, pageLength = 10, lengthChange = FALSE)
      )
    })

    observeEvent(input$load_r_dataset, {
      req(selected_data())
      row <- selected_row()
      activate_dataset(
        selected_data(),
        sprintf("R dataset: %s", row$object[[1]]),
        sprintf("Loaded teaching dataset %s from %s", row$object[[1]], row$source[[1]])
      )
    }, ignoreInit = TRUE)

    build_generated <- function() {
      n <- as.integer(input$gen_rows)
      seed <- as.integer(input$gen_seed)
      set.seed(seed)

      df <- data.frame(respondent_id = seq_len(n), stringsAsFactors = FALSE)
      for (i in seq_len(as.integer(input$gen_numeric))) {
        df[[sprintf("numeric_%02d", i)]] <- round(
          stats::rnorm(n, mean = i * 10, sd = max(1, i * 2)), 2
        )
      }
      for (i in seq_len(as.integer(input$gen_categorical))) {
        df[[sprintf("category_%02d", i)]] <- sample(
          c("A", "B", "C", "D"), n, replace = TRUE,
          prob = c(0.4, 0.3, 0.2, 0.1)
        )
      }
      for (i in seq_len(as.integer(input$gen_likert))) {
        df[[sprintf("likert_%02d", i)]] <- sample(
          1:5, n, replace = TRUE, prob = c(0.08, 0.17, 0.35, 0.27, 0.13)
        )
      }
      if (isTRUE(input$gen_date)) {
        df$survey_date <- as.Date("2025-01-01") + sample(0:730, n, replace = TRUE)
      }

      miss <- as.numeric(input$gen_missing) / 100
      candidate <- setdiff(names(df), "respondent_id")
      if (miss > 0 && length(candidate)) {
        for (nm in candidate) {
          flag <- stats::runif(n) < miss
          df[[nm]][flag] <- NA
        }
      }
      df
    }

    observeEvent(input$generate, {
      generated_data(build_generated())
    }, ignoreInit = TRUE)

    output$generated_status <- renderUI({
      df <- generated_data()
      if (is.null(df)) {
        return(div(class = "alert alert-light", "Choose settings and generate a preview."))
      }
      div(
        class = "alert alert-success py-2",
        icon("circle-check"),
        sprintf(" Generated %s rows × %s columns. This is synthetic test data, not real respondents.",
                format(nrow(df), big.mark = ","), ncol(df))
      )
    })

    output$generated_preview <- DT::renderDT({
      req(generated_data())
      DT::datatable(
        head(generated_data(), 250), rownames = FALSE, filter = "top",
        options = list(scrollX = TRUE, pageLength = 10, lengthChange = FALSE)
      )
    })

    observeEvent(input$load_generated, {
      req(generated_data())
      activate_dataset(
        generated_data(), "Synthetic Survey Data",
        sprintf("Generated synthetic dataset with seed %s", input$gen_seed)
      )
    }, ignoreInit = TRUE)

    output$download_generated <- downloadHandler(
      filename = function() sprintf("synthetic_survey_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        req(generated_data())
        safe <- if (!is.null(services) && !is.null(services$privacy)) {
          services$privacy$prepare_export(generated_data(), "synthetic Data Lab export")
        } else generated_data()
        utils::write.csv(safe, file, row.names = FALSE, na = "")
      }
    )
  })
}

message("[SAP] Data Lab v1.0-20260731 loaded")  # SAP_MODULE_BANNER
