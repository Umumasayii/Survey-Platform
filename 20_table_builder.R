# =============================================================================
# MODULE 20: PROFESSIONAL TABLE BUILDER
# =============================================================================

mod_table_builder_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Professional Table Builder",
      "Build survey crosstabs with bases, percentages, weighting controls, suppression, and report-ready layouts.",
      "table-cells"),
    uiOutput(ns("body"))
  )
}

mod_table_builder_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    table_setting <- function(key, fallback) {
      if (!is.null(services) && !is.null(services$settings)) {
        return(services$settings$get(key, fallback))
      }
      fallback
    }
    observeEvent(app_state$processed_data, {
      vars <- sap_safe_names(app_state)
      updateSelectizeInput(session, "rows", choices = vars, server = TRUE)
      updateSelectizeInput(session, "banners", choices = vars, server = TRUE)
      updateSelectInput(session, "weight", choices = c("None" = "", sap_numeric_names(app_state)))
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      layout_columns(
        col_widths = c(4, 8),
        card(class = "sap-card",
          card_header(icon("wand-magic-sparkles"), " Table specification"),
          card_body(
            selectizeInput(session$ns("rows"), "Row questions", choices = sap_safe_names(app_state), multiple = TRUE,
                           options = list(placeholder = "Select one or more row variables")),
            selectizeInput(session$ns("banners"), "Banner / column questions", choices = sap_safe_names(app_state), multiple = TRUE,
                           options = list(placeholder = "Select banner variables")),
            selectInput(session$ns("weight"), "Weight", choices = c("None" = "", sap_numeric_names(app_state))),
            checkboxGroupInput(session$ns("statistics"), "Display statistics",
              choices = c("Count" = "count", "Row %" = "row_pct", "Column %" = "col_pct", "Total %" = "total_pct", "Mean" = "mean"),
              selected = c("count", "row_pct")),
            checkboxInput(session$ns("include_missing"), "Include missing as a category", FALSE),
            checkboxInput(session$ns("significance"), "Column significance letters", TRUE),
            checkboxInput(session$ns("weighted_base"), "Show weighted and unweighted bases", TRUE),
            numericInput(session$ns("min_base"), "Minimum base warning", value = 30, min = 1, step = 1),
            numericInput(session$ns("suppress"), "Suppress cells below", value = 5, min = 0, step = 1),
            selectInput(session$ns("decimals"), "Decimal places", choices = 0:3,
                        selected = min(3L, max(0L, as.integer(table_setting("decimal_places", 1L))))),
            div(class = "d-grid gap-2",
                actionButton(session$ns("build"), "Build publication table", icon = icon("play"), class = "btn-primary"),
                actionButton(session$ns("save_spec"), "Save table specification", icon = icon("floppy-disk"), class = "btn-outline-primary"),
                div(class = "d-flex gap-2",
                    actionButton(session$ns("add_dashboard"), NULL, icon = icon("gauge-high"), class = "btn-outline-primary flex-fill", title = "Send to Dashboard", `aria-label` = "Send to Dashboard"),
                    actionButton(session$ns("add_report"), NULL, icon = icon("file-arrow-up"), class = "btn-outline-secondary flex-fill", title = "Send to Report", `aria-label` = "Send to Report")),
                div(class = "d-flex gap-2",
                    downloadButton(session$ns("download_csv"), "CSV", class = "btn btn-outline-secondary btn-sm flex-fill"),
                    downloadButton(session$ns("download_xlsx"), "Excel", class = "btn btn-outline-secondary btn-sm flex-fill")))
          )
        ),
        div(
          uiOutput(session$ns("status")),
          card(class = "sap-card",
               card_header(icon("table"), " Publication table"),
               card_body(DTOutput(session$ns("preview")))),
          card(class = "sap-card mt-3",
               card_header(icon("list-check"), " Publication checks"),
               card_body(uiOutput(session$ns("checks"))))
        )
      )
    })

    table_data <- eventReactive(input$build, {
      df <- app_state$processed_data
      req(is.data.frame(df), length(input$rows) >= 1, length(input$statistics) >= 1)
      keep_missing <- isTRUE(input$include_missing)
      decimals <- as.integer(input$decimals %||% 1L)
      alpha <- suppressWarnings(as.numeric(table_setting("significance_level", .05)))
      if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- .05
      w <- rep(1, nrow(df))
      if (nzchar(input$weight %||% "") && input$weight %in% names(df)) {
        w <- suppressWarnings(as.numeric(df[[input$weight]])); w[!is.finite(w) | w < 0] <- NA_real_
      }
      design_spec <- app_state$survey_design %||% list(enabled = FALSE)
      if (nzchar(input$weight %||% "")) { design_spec$enabled <- TRUE; design_spec$weight <- input$weight }
      use_survey <- isTRUE(design_spec$enabled) && !is.null(services) && !is.null(services$survey) && services$survey$is_available()
      policy <- app_state$governance_policy %||% list()
      suppress_n <- max(as.numeric(input$suppress %||% 0), as.numeric(policy$small_cell %||% 0), na.rm = TRUE)
      min_base_n <- max(as.numeric(input$min_base %||% 30), as.numeric(policy$min_base %||% 0), na.rm = TRUE)
      banners <- input$banners %||% character()
      if (!length(banners)) banners <- NA_character_

      as_category <- function(x) {
        out <- as.character(x)
        if (keep_missing) out[is.na(out) | !nzchar(trimws(out))] <- "(Missing)"
        factor(out)
      }
      significance_letters <- function(unweighted) {
        marks <- matrix("", nrow(unweighted), ncol(unweighted), dimnames = dimnames(unweighted))
        if (!isTRUE(input$significance) || ncol(unweighted) < 2L || ncol(unweighted) > 26L) return(marks)
        bases <- colSums(unweighted)
        for (i in seq_len(nrow(unweighted))) {
          comparisons <- list(); keys <- character()
          for (a in seq_len(ncol(unweighted) - 1L)) for (b in (a + 1L):ncol(unweighted)) {
            if (bases[[a]] <= 0 || bases[[b]] <= 0) next
            test <- tryCatch(stats::prop.test(c(unweighted[i, a], unweighted[i, b]), c(bases[[a]], bases[[b]]), correct = FALSE), error = function(e) NULL)
            if (!is.null(test)) {
              keys <- c(keys, paste(a, b, sep = ":")); comparisons[[length(comparisons) + 1L]] <- test$p.value
            }
          }
          if (!length(comparisons)) next
          adjusted <- stats::p.adjust(unlist(comparisons), method = "holm")
          for (k in which(adjusted < alpha)) {
            pair <- as.integer(strsplit(keys[[k]], ":", fixed = TRUE)[[1]])
            a <- pair[[1]]; b <- pair[[2]]
            pa <- unweighted[i, a] / bases[[a]]; pb <- unweighted[i, b] / bases[[b]]
            if (pa > pb) marks[i, a] <- paste0(marks[i, a], LETTERS[[b]])
            if (pb > pa) marks[i, b] <- paste0(marks[i, b], LETTERS[[a]])
          }
        }
        marks
      }

      build_block <- function(row_var, banner_var) {
        r <- as_category(df[[row_var]])
        cvar <- if (is.na(banner_var)) factor(rep("Overall", nrow(df))) else as_category(df[[banner_var]])
        valid <- !is.na(r) & !is.na(cvar) & !is.na(w)
        if (!any(valid)) return(NULL)
        r <- droplevels(r[valid]); cvar <- droplevels(cvar[valid]); ww <- w[valid]
        if (nlevels(r) > 250L || nlevels(cvar) > 100L) stop(paste("Table dimensions are too large for", row_var, "— reduce high-cardinality fields."), call. = FALSE)
        weighted <- stats::xtabs(ww ~ r + cvar, drop.unused.levels = TRUE)
        unweighted <- stats::xtabs(~ r + cvar, drop.unused.levels = TRUE)
        if (use_survey) {
          survey_df <- df; survey_df[[row_var]] <- as_category(survey_df[[row_var]])
          if (!is.na(banner_var)) survey_df[[banner_var]] <- as_category(survey_df[[banner_var]])
          survey_tab <- tryCatch(services$survey$table(survey_df, row_var, if (is.na(banner_var)) NULL else banner_var, spec = design_spec), error = function(e) NULL)
          if (!is.null(survey_tab) && is.na(banner_var) && length(dim(survey_tab)) == 1L) {
            survey_tab <- matrix(as.numeric(survey_tab), ncol = 1L,
                                 dimnames = list(names(survey_tab), "Overall"))
          }
          if (!is.null(survey_tab) && all(dim(survey_tab) == dim(weighted))) weighted <- survey_tab
        }
        row_total <- rowSums(weighted); col_total <- colSums(weighted); grand <- sum(weighted)
        col_base <- colSums(unweighted); letters <- significance_letters(unweighted)
        cells <- expand.grid(row_index = seq_len(nrow(weighted)), col_index = seq_len(ncol(weighted)))
        out <- data.frame(
          Row_question = row_var,
          Category = rownames(weighted)[cells$row_index],
          Banner_question = if (is.na(banner_var)) "Overall" else banner_var,
          Segment = colnames(weighted)[cells$col_index],
          Weighted_Count = as.numeric(weighted[cbind(cells$row_index, cells$col_index)]),
          Unweighted_Count = as.numeric(unweighted[cbind(cells$row_index, cells$col_index)]),
          Row_Percent = 100 * as.numeric(weighted[cbind(cells$row_index, cells$col_index)]) / row_total[cells$row_index],
          Column_Percent = 100 * as.numeric(weighted[cbind(cells$row_index, cells$col_index)]) / col_total[cells$col_index],
          Total_Percent = 100 * as.numeric(weighted[cbind(cells$row_index, cells$col_index)]) / grand,
          Significance = letters[cbind(cells$row_index, cells$col_index)],
          Weighted_Base = col_total[cells$col_index],
          Unweighted_Base = col_base[cells$col_index],
          Mean = NA_real_, Status = "OK", stringsAsFactors = FALSE
        )
        if ("mean" %in% input$statistics && is.numeric(df[[row_var]])) {
          raw_y <- df[[row_var]][valid]
          mean_rows <- lapply(seq_len(nlevels(cvar)), function(j) {
            idx <- cvar == levels(cvar)[[j]] & is.finite(raw_y) & is.finite(ww)
            data.frame(
              Row_question = row_var, Category = "[Mean]",
              Banner_question = if (is.na(banner_var)) "Overall" else banner_var,
              Segment = levels(cvar)[[j]], Weighted_Count = sum(ww[idx]),
              Unweighted_Count = sum(idx), Row_Percent = NA_real_, Column_Percent = NA_real_, Total_Percent = NA_real_,
              Significance = "", Weighted_Base = sum(ww[cvar == levels(cvar)[[j]]]),
              Unweighted_Base = sum(cvar == levels(cvar)[[j]]),
              Mean = if (any(idx)) stats::weighted.mean(raw_y[idx], ww[idx]) else NA_real_, Status = "OK",
              stringsAsFactors = FALSE
            )
          })
          out <- rbind(out, do.call(rbind, mean_rows))
        }
        suppress <- out$Unweighted_Count > 0 & out$Unweighted_Count < suppress_n
        low_base <- out$Unweighted_Base > 0 & out$Unweighted_Base < min_base_n
        out$Status[low_base] <- "Low base"
        out$Status[suppress] <- "Suppressed"
        protected <- c("Weighted_Count", "Unweighted_Count", "Row_Percent", "Column_Percent", "Total_Percent", "Mean", "Significance")
        for (nm in protected) out[[nm]][suppress] <- if (is.character(out[[nm]])) "" else NA
        out
      }

      blocks <- list()
      for (row_var in input$rows) for (banner_var in banners) {
        blocks[[length(blocks) + 1L]] <- build_block(row_var, banner_var)
      }
      out <- do.call(rbind, Filter(Negate(is.null), blocks)); rownames(out) <- NULL
      requested <- c("Row_question", "Category", "Banner_question", "Segment")
      stats_map <- c(count = "Weighted_Count", row_pct = "Row_Percent", col_pct = "Column_Percent", total_pct = "Total_Percent", mean = "Mean")
      requested <- c(requested, unname(stats_map[intersect(names(stats_map), input$statistics)]))
      if (isTRUE(input$weighted_base)) requested <- c(requested, "Unweighted_Count", "Weighted_Base", "Unweighted_Base")
      if (isTRUE(input$significance)) requested <- c(requested, "Significance")
      requested <- unique(c(requested, "Status"))
      out <- out[, requested, drop = FALSE]
      numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
      out[numeric_cols] <- lapply(out[numeric_cols], round, digits = decimals)
      attr(out, "suppression_threshold") <- suppress_n
      attr(out, "minimum_base") <- min_base_n
      attr(out, "survey_aware") <- use_survey
      attr(out, "row_var") <- paste(input$rows, collapse = ", ")
      attr(out, "col_var") <- if (all(is.na(banners))) "Overall" else paste(stats::na.omit(banners), collapse = ", ")
      out
    }, ignoreInit = TRUE)

    output$preview <- renderDT({
      out <- table_data()
      if (is.null(out)) return(datatable(data.frame(Instruction = "Choose row variables and select Build preview."), options = list(dom = "t"), rownames = FALSE))
      datatable(out, filter = "top", options = list(scrollX = TRUE, pageLength = 20, dom = "tip"), rownames = FALSE)
    })

    output$status <- renderUI({
      req(is.data.frame(app_state$processed_data))
      rows <- length(input$rows %||% character(0)); banners <- length(input$banners %||% character(0))
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Row questions", rows, "list"),
          sap_kpi("Banner questions", banners, "columns"),
          sap_kpi("Weighting", if (!is.null(input$weight) && nzchar(input$weight)) "On" else "Off", "weight-hanging"),
          sap_kpi("Minimum base", input$min_base %||% 30, "triangle-exclamation"))
    })

    output$checks <- renderUI({
      out <- table_data()
      if (is.null(out)) return(p(class = "text-muted mb-0", "Build a preview to run publication checks."))
      minimum_base <- attr(out, "minimum_base") %||% (input$min_base %||% 30)
      small <- if ("Status" %in% names(out)) sum(out$Status == "Low base", na.rm = TRUE) else 0
      suppressed <- if ("Status" %in% names(out)) sum(out$Status == "Suppressed", na.rm = TRUE) else 0
      tagList(
        div(class = "d-flex flex-wrap gap-2 mb-2",
            sap_status_pill("Publication table generated", "success"),
            sap_status_pill(if (small > 0) paste(small, "small-base rows") else "Bases acceptable", if (small > 0) "warning" else "success"),
            sap_status_pill(paste(suppressed, "suppressed cells"), if (suppressed > 0) "warning" else "success"),
            sap_status_pill(if (isTRUE(input$significance))
              sprintf("Holm-adjusted letters (alpha %.3f)", as.numeric(table_setting("significance_level", .05)))
              else "No significance test", "neutral")),
        p(class = "small text-muted mb-0", if (isTRUE(attr(out, "survey_aware")))
          "Complex-design weighted estimates are active. Column letters use Holm-adjusted pairwise proportion tests; suppression and minimum-base rules use the stricter of table and governance settings."
        else "Model weights, Holm-adjusted column comparisons, small-cell suppression, and minimum-base flags are active. Save a Survey Design to use complex-design weighted estimates.")
      )
    })

    export_authorized <- function(content, purpose) {
      tryCatch({
        if (exists("sap_export_assert_content", mode = "function")) {
          sap_export_assert_content(services, content, purpose)
        }
        TRUE
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 9)
        FALSE
      })
    }

    output$download_csv <- downloadHandler(
      filename = function() paste0("survey-table-", Sys.Date(), ".csv"),
      content = function(file) {
        out <- table_data(); req(is.data.frame(out))
        shiny::req(export_authorized(out, "table builder CSV export"))
        utils::write.csv(out, file, row.names = FALSE, na = "")
      }
    )
    output$download_xlsx <- downloadHandler(
      filename = function() paste0("survey-table-", Sys.Date(), ".xlsx"),
      content = function(file) {
        out <- table_data(); req(is.data.frame(out))
        if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' is required for Excel export.", call. = FALSE)
        shiny::req(export_authorized(out, "table builder Excel export"))
        wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Table")
        openxlsx::writeDataTable(wb, "Table", out, tableStyle = "TableStyleMedium2")
        openxlsx::freezePane(wb, "Table", firstRow = TRUE, firstCol = TRUE)
        openxlsx::setColWidths(wb, "Table", cols = seq_len(ncol(out)), widths = "auto")
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    observeEvent(input$save_spec, {
      app_state$table_builder_spec <- list(rows = input$rows, banners = input$banners, weight = input$weight,
        statistics = input$statistics, include_missing = input$include_missing, significance = input$significance,
        min_base = input$min_base, suppress = input$suppress, decimals = input$decimals, updated_at = Sys.time())
      sap_frontend_record(app_state, "Table Builder", "Table specification saved", app_state$table_builder_spec)
      showNotification("Table specification saved.", type = "message")
    })

    observeEvent(input$add_dashboard, {
      out <- table_data(); req(is.data.frame(out))
      ttl <- paste("Crosstab:", attr(out, "row_var"), "by", attr(out, "col_var") %||% "Total")
      if (exists(".sap_capture_for_dashboard", mode = "function")) {
        .sap_capture_for_dashboard(services, "table", ttl, out)
      } else {
        showNotification("Dashboard integration is not available.", type = "error")
      }
    })

    observeEvent(input$add_report, {
      out <- table_data(); req(is.data.frame(out))
      entry <- list(type = "table", title = paste("Crosstab:", attr(out, "row_var"), "by", attr(out, "col_var") %||% "Total"), data = out, created_at = Sys.time())
      app_state$report_frontend_blocks <- c(app_state$report_frontend_blocks %||% list(), list(entry))
      sap_frontend_record(app_state, "Table Builder", "Table preview added to frontend report queue")
      showNotification("Table preview added to the report queue.", type = "message")
    })
  })
}

message("[SAP] Table Builder engine v4.0.0 loaded")
