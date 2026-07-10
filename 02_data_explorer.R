# =============================================================================
# MODULE: Data Explorer
# =============================================================================
# 
# Complete dataset exploration module with:
# 1. Dataset Summary (statistics & quality metrics)
# 2. Variable List (searchable, filterable)
# 3. Variable Inspector (detailed stats & visualizations)
# 4. Interactive Data Table (search, sort, filter, export)
# 5. Missing Value Analysis (heatmap visualization)
#
# Dependencies: plotly, DT, dplyr, ggplot2
#
# =============================================================================

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_data_explorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("magnifying-glass"), "Data Explorer"),
      p("Explore your dataset: summaries, distributions, and detailed analysis", 
        class = "text-muted")
    ),
    
    # Tab structure
    navset_tab(
      id = ns("explorer_tabs"),
      
      # TAB 1: OVERVIEW
      nav_panel(
        "Overview",
        icon = icon("gauge"),
        div(class = "p-4",
          uiOutput(ns("dataset_summary"))
        )
      ),
      
      # TAB 2: VARIABLES
      nav_panel(
        "Variables",
        icon = icon("list"),
        div(
          class = "p-4",
          div(
            class = "row",
            div(
              class = "col-md-3",
              # Variable list
              div(
                class = "card",
                div(
                  class = "card-header",
                  h5("Variables")
                ),
                div(
                  class = "card-body",
                  style = "max-height: 600px; overflow-y: auto;",
                  
                  # Search box
                  textInput(ns("var_search"), "Search:", placeholder = "Variable name..."),
                  
                  # Type filter
                  checkboxGroupInput(
                    ns("var_type_filter"),
                    "Filter by type:",
                    choices = c("Numeric" = "numeric", 
                               "Integer" = "integer",
                               "Character" = "character",
                               "Factor" = "factor",
                               "Date" = "date",
                               "Logical" = "logical"),
                    selected = c("numeric", "integer", "character", "factor", "date", "logical")
                  ),
                  
                  hr(),
                  
                  # Variable list
                  uiOutput(ns("variable_list"))
                )
              )
            ),
            
            div(
              class = "col-md-9",
              # Variable inspector
              uiOutput(ns("variable_inspector"))
            )
          )
        )
      ),
      
      # TAB 3: DATA TABLE
      nav_panel(
        "Data Table",
        icon = icon("table"),
        div(
          class = "p-4",
          
          # Controls
          div(
            class = "row mb-3",
            div(
              class = "col-md-6",
              textInput(ns("table_search"), "Search table:", placeholder = "Type to filter...")
            ),
            div(
              class = "col-md-3",
              numericInput(ns("table_rows"), "Rows per page:", value = 10, min = 5, max = 100)
            ),
            div(
              class = "col-md-3",
              downloadButton(ns("export_table"), "Export (CSV)", class = "btn-primary btn-sm")
            )
          ),
          
          hr(),
          
          # Data table
          div(
            class = "table-responsive",
            style = "max-height: 600px; overflow: auto;",
            DT::dataTableOutput(ns("data_table"), height = "100%"),
            div(class = "mt-2 text-end", report_button(ns, "add_dataset_overview", "Add dataset overview"))
          )
        )
      ),
      
      # TAB 4: MISSING VALUES
      nav_panel(
        "Missing Values",
        icon = icon("triangle-exclamation"),
        div(
          class = "p-4",
          
          # Missing value summary
          div(
            class = "row mb-4",
            lapply(1:3, function(i) {
              div(
                class = "col-md-4",
                div(
                  class = "card",
                  div(
                    class = "card-body",
                    id = ns(paste0("missing_card_", i)),
                    uiOutput(ns(paste0("missing_stat_", i)))
                  )
                )
              )
            })
          ),
          
          # Missing value table
          div(
            class = "card",
            div(
              class = "card-header",
              h5("Missing Values by Variable")
            ),
            div(
              class = "card-body",
              div(
                class = "table-responsive",
                DT::dataTableOutput(ns("missing_table")),
                div(class = "mt-2 text-end", report_button(ns, "add_missing_table", "Add to Report"))
              )
            )
          ),
          
          hr(),
          
          # Visualization placeholder
          div(
            class = "card mt-4",
            div(
              class = "card-header",
              h5("Missing Values Heatmap (by Observation)")
            ),
            div(
              class = "card-body",
              plotly::plotlyOutput(ns("missing_heatmap"), height = "420px"),
              div(class = "mt-2 text-end", report_button(ns, "add_missing_heatmap", "Add to Report"))
            )
          )
        )
      )
    )
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_data_explorer_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # Holds the ggplot mirror of the (native plotly) missing-value heatmap so
    # "Add to Report" can rasterize it. See plot_capture() in 00_services.R.
    pc <- plot_capture()

    # Dataset overview as a report-ready table.
    dataset_overview_df <- reactive({
      df <- df_data()
      req(df)
      data.frame(
        Property = c("Rows", "Columns", "Numeric variables",
                     "Categorical variables", "Total missing values",
                     "Complete rows"),
        Value = c(
          nrow(df), ncol(df),
          sum(vapply(df, is.numeric, logical(1))),
          sum(vapply(df, function(x) is.factor(x) || is.character(x), logical(1))),
          sum(is.na(df)),
          sum(stats::complete.cases(df))
        ),
        stringsAsFactors = FALSE
      )
    })
    
    # =========================================================================
    # REACTIVE: PROCESSED DATA (via DatasetService, with app_state fallback)
    # =========================================================================
    # Reads route through services$data (the backend single source of truth)
    # when wired; fall back to direct app_state reads so the module still runs
    # standalone. Reads stay inside reactive() -> reactive context preserved.

    df_data <- reactive({
      if (!is.null(services) && !is.null(services$data)) {
        if (!services$data$is_loaded()) return(NULL)
        return(services$data$get("processed"))
      }
      if (!isTRUE(app_state$data_imported)) return(NULL)
      app_state$processed_data
    })

    df_raw <- reactive({
      if (!is.null(services) && !is.null(services$data)) {
        if (!services$data$is_loaded()) return(NULL)
        return(services$data$get("raw"))
      }
      if (!isTRUE(app_state$data_imported)) return(NULL)
      app_state$raw_data
    })

    var_types <- reactive({
      if (!is.null(services) && !is.null(services$data)) {
        if (!services$data$is_loaded()) return(NULL)
        return(services$data$variable_types())
      }
      if (!isTRUE(app_state$data_imported)) return(NULL)
      app_state$variable_types
    })
    
    # =========================================================================
    # REACTIVE: FILTERED VARIABLES
    # =========================================================================
    
    # Single source of truth for variable metadata: derived from the ACTUAL
    # active dataset columns, so names always match df_data(). This fixes the
    # "Variable not found" bug that occurred when the stored variable_types
    # metadata (original names) diverged from the processed data (snake_cased
    # by clean_names at import).
    explorer_var_table <- reactive({
      df <- df_data()
      req(df)
      n <- nrow(df)
      classify <- function(x) {
        if (inherits(x, "Date") || inherits(x, "POSIXct")) "date"
        else if (is.factor(x))                             "factor"
        else if (is.logical(x))                            "logical"
        else if (is.integer(x))                            "integer"
        else if (is.numeric(x))                            "numeric"
        else                                               "character"
      }
      data.frame(
        variable      = names(df),
        type          = vapply(df, classify, character(1)),
        missing_count = as.integer(colSums(is.na(df))),
        missing_pct   = round(colSums(is.na(df)) / max(1, n) * 100, 1),
        unique_values = vapply(df, function(x) length(unique(x)), integer(1)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })

    filtered_variables <- reactive({
      var_df <- explorer_var_table()
      if (is.null(var_df)) return(NULL)
      
      # Search filter
      search_term <- tolower(input$var_search %||% "")
      if (search_term != "") {
        var_df <- var_df %>%
          filter(grepl(search_term, tolower(variable), fixed = TRUE))
      }
      
      # Type filter
      type_filter <- input$var_type_filter
      if (length(type_filter) > 0) {
        var_df <- var_df %>%
          filter(type %in% type_filter)
      }
      
      return(var_df)
    })
    
    # Selected variable
    selected_variable <- reactive({
      input$var_selection
    })
    
    # =========================================================================
    # TAB 1: DATASET SUMMARY
    # =========================================================================
    
    output$dataset_summary <- renderUI({
      df <- df_data()
      
      # Handle no data imported
      if (is.null(df)) {
        return(empty_state(
          icon = "inbox",
          title = "No Data Imported",
          message = "Go to Data > Import to load your dataset",
          action_text = "← Go to Import"
        ))
      }
      var_df <- var_types()
      
      # Calculate statistics
      n_rows <- nrow(df)
      n_cols <- ncol(df)
      n_numeric <- sum(var_df$type %in% c("numeric", "integer"))
      n_categorical <- sum(var_df$type %in% c("factor", "categorical_character", "logical"))
      n_character <- sum(var_df$type == "character")
      n_date <- sum(var_df$type %in% c("date", "datetime"))
      pct_missing <- round(sum(is.na(df)) / (n_rows * n_cols) * 100, 2)
      n_duplicates <- sum(duplicated(df))
      n_dup_cols <- length(names(df)[duplicated(names(df))])
      mem_size <- format(object.size(df), units = "auto")
      
      # Calculate complete cases
      n_complete <- sum(complete.cases(df))
      pct_complete <- round(n_complete / n_rows * 100, 2)
      
      tagList(
        # Main KPI Cards
        div(
          class = "row mb-4",
          div(class = "col-md-3", 
            kpi_card("Rows", format(n_rows, big.mark = ","), 
                    subtitle = "observations", color = "primary")),
          div(class = "col-md-3",
            kpi_card("Columns", n_cols, 
                    subtitle = "variables", color = "success")),
          div(class = "col-md-3",
            kpi_card("Complete", paste0(pct_complete, "%"), 
                    subtitle = sprintf("%s cases", format(n_complete, big.mark = ",")), 
                    color = "info")),
          div(class = "col-md-3",
            kpi_card("Memory", mem_size, 
                    subtitle = "estimated usage", color = "warning"))
        ),
        
        # Variable Type Distribution
        div(
          class = "row mb-4",
          div(class = "col-md-6",
            div(
              class = "card",
              div(class = "card-header", h5("Variables by Type")),
              div(class = "card-body",
                div(
                  class = "row",
                  div(class = "col-md-6",
                    div(
                      class = "text-center mb-3",
                      h6("Numeric"),
                      h3(class = "text-primary", n_numeric)
                    )
                  ),
                  div(class = "col-md-6",
                    div(
                      class = "text-center mb-3",
                      h6("Categorical"),
                      h3(class = "text-success", n_categorical)
                    )
                  ),
                  div(class = "col-md-6",
                    div(
                      class = "text-center mb-3",
                      h6("Character"),
                      h3(class = "text-info", n_character)
                    )
                  ),
                  div(class = "col-md-6",
                    div(
                      class = "text-center mb-3",
                      h6("Date/Time"),
                      h3(class = "text-warning", n_date)
                    )
                  )
                )
              )
            )
          ),
          
          # Data Quality
          div(class = "col-md-6",
            div(
              class = "card",
              div(class = "card-header", h5("Data Quality")),
              div(class = "card-body",
                div(class = "mb-3",
                  div(class = "d-flex justify-content-between",
                    span("Missing Values:"),
                    strong(paste0(pct_missing, "%"))
                  ),
                  div(class = "progress",
                    div(class = "progress-bar bg-warning", 
                        role = "progressbar",
                        style = paste0("width: ", pct_missing, "%"))
                  )
                ),
                
                div(class = "mb-3",
                  div(class = "d-flex justify-content-between",
                    span("Duplicate Rows:"),
                    strong(format(n_duplicates, big.mark = ","))
                  )
                ),
                
                div(class = "mb-3",
                  div(class = "d-flex justify-content-between",
                    span("Duplicate Columns:"),
                    strong(n_dup_cols)
                  )
                ),
                
                if (pct_missing > 20 || n_duplicates > 0) {
                  validation_box(
                    type = "warning",
                    title = "Data Quality Issues",
                    message = "Consider using Data Cleaning module to address issues."
                  )
                }
              )
            )
          )
        ),
        
        # Detailed summary table
        div(
          class = "card",
          div(class = "card-header", h5("Variable Summary")),
          div(
            class = "card-body",
            div(
              class = "table-responsive",
              style = "max-height: 400px; overflow-y: auto;",
              DT::renderDataTable({
                var_df %>%
                  select(variable, type, missing_count, missing_pct, unique_values) %>%
                  setNames(c("Variable", "Type", "Missing", "Missing %", "Unique")) %>%
                  DT::datatable(
                    options = list(
                      pageLength = 10,
                      lengthChange = FALSE,
                      searching = TRUE,
                      ordering = TRUE,
                      dom = "ltp",
                      columnDefs = list(
                        list(className = "dt-right", targets = c(2, 3, 4))
                      )
                    ),
                    rownames = FALSE,
                    filter = "top"
                  )
              })
            )
          )
        )
      )
    })
    
    # =========================================================================
    # TAB 2: VARIABLES - LIST & INSPECTOR
    # =========================================================================
    
    # Render variable list
    output$variable_list <- renderUI({
      req(df_data())  # Validate data first
      
      vars <- filtered_variables()
      
      if (is.null(vars) || nrow(vars) == 0) {
        return(div(class = "alert alert-info", "No variables match filters"))
      }
      
      div(
        lapply(1:nrow(vars), function(i) {
          var_name <- vars$variable[i]
          var_type <- vars$type[i]
          var_missing <- vars$missing_pct[i]
          
          # Color code based on type
          type_badge <- switch(var_type,
            "numeric" = "bg-primary",
            "integer" = "bg-primary",
            "character" = "bg-info",
            "categorical_character" = "bg-success",
            "factor" = "bg-success",
            "date" = "bg-warning",
            "datetime" = "bg-warning",
            "logical" = "bg-secondary",
            "bg-dark"
          )
          
          actionButton(
            ns(paste0("var_", i)),
            label = NULL,
            class = "btn btn-outline-primary w-100 text-start mb-2",
            onclick = paste0("Shiny.setInputValue('", ns("var_selection"), "', '", var_name, "')"),
            div(
              class = "d-flex justify-content-between align-items-center w-100",
              div(
                strong(var_name),
                br(),
                span(class = "text-muted", var_type)
              ),
              span(class = paste("badge", type_badge), 
                   paste0(var_missing, "% NA"))
            )
          )
        })
      )
    })
    
    # Render variable inspector
    output$variable_inspector <- renderUI({
      var_name <- selected_variable()
      
      if (is.null(var_name)) {
        return(div(
          class = "alert alert-info",
          "Select a variable to inspect"
        ))
      }
      
      req(df_data())
      df <- df_data()
      
      if (!var_name %in% names(df)) {
        return(div(
          class = "alert alert-danger",
          "Variable not found"
        ))
      }
      
      # Get variable
      col_data <- df[[var_name]]
      
      # Get variable info (from the data-derived table, names guaranteed to match)
      var_info <- explorer_var_table() %>%
        filter(variable == var_name)
      
      var_type <- var_info$type[1]
      n_missing <- var_info$missing_count[1]
      pct_missing <- var_info$missing_pct[1]
      n_unique <- var_info$unique_values[1]
      
      # Build inspector based on type
      if (var_type %in% c("numeric", "integer")) {
        render_numeric_inspector(ns, var_name, col_data, var_type)
      } else if (var_type %in% c("factor", "categorical_character", "logical")) {
        render_categorical_inspector(ns, var_name, col_data, var_type)
      } else if (var_type %in% c("date", "datetime")) {
        render_date_inspector(ns, var_name, col_data, var_type)
      } else if (var_type == "character") {
        render_character_inspector(ns, var_name, col_data, var_type)
      } else {
        render_unknown_inspector(ns, var_name, col_data, var_type)
      }
    })
    
    # =========================================================================
    # TAB 3: DATA TABLE
    # =========================================================================
    
    output$data_table <- DT::renderDataTable({
      df <- df_data()
      
      # Return empty table if no data
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          data.frame(),
          options = list(dom = 't'),
          class = "stripe hover"
        ))
      }
      
      DT::datatable(
        df,
        options = list(
          pageLength = input$table_rows,
          lengthChange = FALSE,
          searching = TRUE,
          searchHighlight = TRUE,
          ordering = TRUE,
          dom = "ltp",
          columnDefs = list(
            list(className = "dt-right", targets = "_all")
          ),
          scrollX = TRUE,
          scrollY = "400px"
        ),
        rownames = FALSE,
        filter = "top",
        selection = "none"
      )
    })
    
    # Export table
    output$export_table <- downloadHandler(
      filename = function() {
        paste0("data_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(df_data(), file, row.names = FALSE)
      }
    )
    
    # =========================================================================
    # TAB 4: MISSING VALUES
    # =========================================================================
    
    # Missing value statistics
    output$missing_stat_1 <- renderUI({
      df <- df_data()
      
      if (is.null(df)) {
        return(div(h6("No Data"), h3("—")))
      }
      n_missing_total <- sum(is.na(df))
      n_cells <- nrow(df) * ncol(df)
      pct_missing <- round(n_missing_total / n_cells * 100, 2)
      
      div(
        h6("Total Missing Values"),
        h3(class = "text-warning", format(n_missing_total, big.mark = ",")),
        span(paste0(pct_missing, "% of all cells"))
      )
    })
    
    output$missing_stat_2 <- renderUI({
      df <- df_data()
      
      if (is.null(df)) {
        return(div(h6("No Data"), h3("—")))
      }
      n_vars_missing <- sum(colSums(is.na(df)) > 0)
      n_vars_total <- ncol(df)
      
      div(
        h6("Variables with Missing"),
        h3(class = "text-info", n_vars_missing),
        span(paste0("out of ", n_vars_total, " variables"))
      )
    })
    
    output$missing_stat_3 <- renderUI({
      df <- df_data()
      
      if (is.null(df)) {
        return(div(h6("No Data"), h3("—")))
      }
      n_rows_complete <- sum(complete.cases(df))
      pct_complete <- round(n_rows_complete / nrow(df) * 100, 2)
      
      div(
        h6("Complete Cases"),
        h3(class = "text-success", format(n_rows_complete, big.mark = ",")),
        span(paste0(pct_complete, "% of rows"))
      )
    })
    
    # Missing value table
    # Single source for the render AND the report button.
    missing_df <- reactive({
      df <- df_data()
      req(df)
      out <- data.frame(
        Variable = names(df),
        Missing = colSums(is.na(df)),
        "Missing %" = round(colSums(is.na(df)) / nrow(df) * 100, 2),
        Type = vapply(df, function(x) class(x)[1], character(1)),
        check.names = FALSE
      ) %>%
        filter(Missing > 0) %>%
        arrange(desc(Missing))

      if (nrow(out) == 0) {
        out <- data.frame(
          Variable = "No missing values",
          Missing = 0,
          "Missing %" = 0,
          Type = "",
          check.names = FALSE
        )
      }
      out
    })

    output$missing_table <- DT::renderDataTable({
      df <- df_data()

      # Return empty table if no data
      if (is.null(df)) {
        return(DT::datatable(
          data.frame(Message = "No data imported"),
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        missing_df(),
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          searching = FALSE,
          ordering = TRUE,
          dom = "ltp"
        ),
        rownames = FALSE
      )
    })
    
    # Missing-values heatmap: variables (x) by observation (y), red = missing.
    # Rows are sampled when the dataset is large so the plot stays responsive.
    output$missing_heatmap <- plotly::renderPlotly({
      df <- df_data()
      req(df)

      na_mat <- is.na(df)
      total_na <- sum(na_mat)

      if (total_na == 0) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>%
            plotly::layout(
              title = list(
                text = "No missing values \u2014 dataset is complete",
                font = list(size = 14)
              )
            )
        )
      }

      n <- nrow(df)
      max_rows <- 500
      has_na  <- which(rowSums(na_mat) > 0)
      others  <- setdiff(seq_len(n), has_na)

      if (n <= max_rows) {
        take <- seq_len(n)
      } else if (length(has_na) >= max_rows) {
        # More NA-rows than we can show: sample among them (never a negative
        # size). This is the case that crashed on large, gappy datasets.
        take <- sort(has_na[sample.int(length(has_na), max_rows)])
      } else {
        # Keep all NA-rows, fill the rest with a sample of complete rows.
        n_fill <- min(max_rows - length(has_na), length(others))
        fill   <- if (n_fill > 0)
                    others[sample.int(length(others), n_fill)] else integer(0)
        take   <- sort(c(has_na, fill))
      }

      z <- matrix(as.integer(na_mat[take, , drop = FALSE]),
                  nrow = length(take))

      # Native plot_ly cannot be rasterized without kaleido/webshot2, so mirror
      # it as a ggplot purely for the report PNG path. Display stays plotly.
      pc$keep("missing_heatmap",
        ggplot2::ggplot(
          data.frame(
            Variable = factor(rep(names(df), each = length(take)),
                              levels = names(df)),
            Row      = rep(take, times = ncol(df)),
            Missing  = as.vector(z) == 1L
          ),
          ggplot2::aes(x = Variable, y = Row, fill = Missing)) +
          ggplot2::geom_raster() +
          ggplot2::scale_fill_manual(values = c(`FALSE` = "#e9ecef",
                                                `TRUE`  = "#dc3545"),
                                     guide = "none") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = 0)) +
          ggplot2::labs(title = "Missing values",
                        x = NULL,
                        y = sprintf("Observation (%d of %d shown)", length(take), n)))

      plotly::plot_ly(
        x = names(df),
        y = take,
        z = z,
        type = "heatmap",
        colorscale = list(c(0, "#e9ecef"), c(1, "#dc3545")),
        showscale = FALSE,
        hovertemplate = "Variable: %{x}<br>Row: %{y}<br>%{z}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = sprintf("Observation (%d of %d shown)",
                                       length(take), n))
        )
    })

    # =========================================================================
    # ADD TO REPORT
    # =========================================================================

    register_report_button(
      input, services, "add_dataset_overview", kind = "table",
      title   = "Dataset overview",
      payload = function() dataset_overview_df(),
      code    = "dim(df)\ncolSums(is.na(df))\nsum(complete.cases(df))")

    register_report_button(
      input, services, "add_missing_table", kind = "table",
      title   = "Missing values by variable",
      payload = function() missing_df(),
      code    = "colSums(is.na(df)) %>%\n  sort(decreasing = TRUE)")

    register_report_button(
      input, services, "add_missing_heatmap", kind = "plot",
      title   = "Missing value pattern",
      payload = pc$get("missing_heatmap"),
      code    = "image(is.na(df))   # or naniar::vis_miss(df)")

  })
}

# =============================================================================
# HELPER FUNCTIONS - VARIABLE INSPECTORS
# =============================================================================

# Numeric variable inspector
render_numeric_inspector <- function(ns, var_name, col_data, var_type) {
  
  col_clean <- col_data[!is.na(col_data)]
  
  # Calculate statistics
  n_missing <- sum(is.na(col_data))
  n_obs <- length(col_clean)
  mean_val <- mean(col_clean)
  median_val <- median(col_clean)
  sd_val <- sd(col_clean)
  min_val <- min(col_clean)
  max_val <- max(col_clean)
  q1 <- quantile(col_clean, 0.25)
  q3 <- quantile(col_clean, 0.75)
  iqr_val <- q3 - q1
  
  # Detect outliers (IQR method)
  lower_bound <- q1 - 1.5 * iqr_val
  upper_bound <- q3 + 1.5 * iqr_val
  n_outliers <- sum(col_clean < lower_bound | col_clean > upper_bound)
  
  div(
    class = "card",
    div(
      class = "card-header",
      h5(icon("chart-line"), " ", var_name),
      span(class = "text-muted", paste("Type:", var_type))
    ),
    div(
      class = "card-body",
      
      # Statistics table
      div(
        class = "row mb-4",
        div(
          class = "col-md-6",
          tags$table(
            class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td("N (non-missing):"), tags$td(strong(format(n_obs, big.mark = ",")))),
              tags$tr(tags$td("N (missing):"), tags$td(strong(n_missing))),
              tags$tr(tags$td("Mean:"), tags$td(strong(round(mean_val, 3)))),
              tags$tr(tags$td("Median:"), tags$td(strong(round(median_val, 3)))),
              tags$tr(tags$td("Std Dev:"), tags$td(strong(round(sd_val, 3))))
            )
          )
        ),
        div(
          class = "col-md-6",
          tags$table(
            class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td("Min:"), tags$td(strong(round(min_val, 3)))),
              tags$tr(tags$td("Max:"), tags$td(strong(round(max_val, 3)))),
              tags$tr(tags$td("Q1 (25%):"), tags$td(strong(round(q1, 3)))),
              tags$tr(tags$td("Q3 (75%):"), tags$td(strong(round(q3, 3)))),
              tags$tr(tags$td("IQR:"), tags$td(strong(round(iqr_val, 3))))
            )
          )
        )
      ),
      
      # Outliers
      if (n_outliers > 0) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " ", n_outliers, " outliers detected (using IQR method)"
        )
      },
      
      # Histogram
      div(
        class = "mt-4",
        h6("Distribution"),
        renderPlotly({
          plot_ly(
            x = ~col_clean,
            type = "histogram",
            nbinsx = 30,
            marker = list(color = "#0d6efd"),
            hovertemplate = "%{x}: %{y}<extra></extra>"
          ) %>%
            layout(
              xaxis = list(title = var_name),
              yaxis = list(title = "Frequency"),
              margin = list(l = 50, r = 50, t = 50, b = 50),
              hovermode = "x unified"
            )
        })
      ),
      
      # Box plot
      div(
        class = "mt-4",
        renderPlotly({
          plot_ly(
            y = ~col_clean,
            type = "box",
            marker = list(color = "#198754"),
            name = var_name,
            hovertemplate = "Value: %{y}<extra></extra>"
          ) %>%
            layout(
              yaxis = list(title = var_name),
              margin = list(l = 50, r = 50, t = 50, b = 50),
              showlegend = FALSE
            )
        })
      )
    )
  )
}

# Categorical variable inspector
render_categorical_inspector <- function(ns, var_name, col_data, var_type) {
  
  col_clean <- col_data[!is.na(col_data)]
  n_missing <- sum(is.na(col_data))
  n_obs <- length(col_clean)
  n_unique <- n_distinct(col_clean)
  
  # Frequency table
  freq_table <- data.frame(
    Category = names(table(col_clean)),
    Frequency = as.numeric(table(col_clean)),
    Percent = round(as.numeric(prop.table(table(col_clean))) * 100, 2),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(Frequency))
  
  div(
    class = "card",
    div(
      class = "card-header",
      h5(icon("chart-bar"), " ", var_name),
      span(class = "text-muted", paste("Type:", var_type))
    ),
    div(
      class = "card-body",
      
      # Statistics
      div(
        class = "row mb-4",
        div(
          class = "col-md-6",
          tags$table(
            class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td("N (non-missing):"), tags$td(strong(format(n_obs, big.mark = ",")))),
              tags$tr(tags$td("N (missing):"), tags$td(strong(n_missing))),
              tags$tr(tags$td("Unique values:"), tags$td(strong(n_unique)))
            )
          )
        )
      ),
      
      # Bar chart
      div(
        class = "mt-4",
        h6("Frequency Distribution"),
        renderPlotly({
          # Limit to top 15 categories for readability
          top_freq <- freq_table %>% slice(1:min(15, nrow(freq_table)))
          
          plot_ly(
            data = top_freq,
            x = ~reorder(Category, Frequency),
            y = ~Frequency,
            type = "bar",
            marker = list(color = "#fd7e14"),
            text = ~paste0(Frequency, " (", Percent, "%)"),
            textposition = "outside",
            hovertemplate = "%{x}: %{y}<extra></extra>"
          ) %>%
            layout(
              xaxis = list(title = "Category"),
              yaxis = list(title = "Frequency"),
              margin = list(l = 50, r = 50, t = 50, b = 50),
              hovermode = "x unified"
            )
        })
      ),
      
      # Frequency table (top 10)
      div(
        class = "mt-4",
        h6("Frequency Table (Top 10)"),
        div(
          class = "table-responsive",
          style = "max-height: 300px; overflow-y: auto;",
          DT::renderDataTable({
            top_table <- freq_table %>% slice(1:min(10, nrow(freq_table)))
            
            DT::datatable(
              top_table,
              options = list(
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE,
                ordering = FALSE,
                dom = "t"
              ),
              rownames = FALSE
            )
          })
        )
      )
    )
  )
}

# Date variable inspector
render_date_inspector <- function(ns, var_name, col_data, var_type) {
  
  col_clean <- col_data[!is.na(col_data)]
  n_missing <- sum(is.na(col_data))
  n_obs <- length(col_clean)
  
  # Date statistics
  min_date <- min(col_clean)
  max_date <- max(col_clean)
  date_range <- as.numeric(max_date - min_date)
  
  div(
    class = "card",
    div(
      class = "card-header",
      h5(icon("calendar"), " ", var_name),
      span(class = "text-muted", paste("Type:", var_type))
    ),
    div(
      class = "card-body",
      
      # Statistics
      div(
        class = "row mb-4",
        div(
          class = "col-md-6",
          tags$table(
            class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td("N (non-missing):"), tags$td(strong(format(n_obs, big.mark = ",")))),
              tags$tr(tags$td("N (missing):"), tags$td(strong(n_missing))),
              tags$tr(tags$td("Earliest:"), tags$td(strong(format(min_date, "%Y-%m-%d")))),
              tags$tr(tags$td("Latest:"), tags$td(strong(format(max_date, "%Y-%m-%d")))),
              tags$tr(tags$td("Range (days):"), tags$td(strong(date_range)))
            )
          )
        )
      ),
      
      # Timeline
      div(
        class = "mt-4",
        h6("Timeline"),
        renderPlotly({
          date_freq <- data.frame(
            date = col_clean,
            count = 1
          ) %>%
            group_by(date) %>%
            summarise(count = n(), .groups = "drop") %>%
            arrange(date)
          
          plot_ly(
            data = date_freq,
            x = ~date,
            y = ~count,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#0dcaf0"),
            marker = list(color = "#0dcaf0", size = 5),
            hovertemplate = "%{x|%Y-%m-%d}: %{y} observations<extra></extra>"
          ) %>%
            layout(
              xaxis = list(title = "Date"),
              yaxis = list(title = "Observations"),
              margin = list(l = 50, r = 50, t = 50, b = 50),
              hovermode = "x unified"
            )
        })
      )
    )
  )
}

# Character variable inspector
render_character_inspector <- function(ns, var_name, col_data, var_type) {
  
  col_clean <- col_data[!is.na(col_data)]
  n_missing <- sum(is.na(col_data))
  n_obs <- length(col_clean)
  n_unique <- n_distinct(col_clean)
  
  # String statistics
  str_lengths <- nchar(col_clean)
  mean_length <- mean(str_lengths)
  max_length <- max(str_lengths)
  
  div(
    class = "card",
    div(
      class = "card-header",
      h5(icon("font"), " ", var_name),
      span(class = "text-muted", paste("Type:", var_type))
    ),
    div(
      class = "card-body",
      
      # Statistics
      div(
        class = "row mb-4",
        div(
          class = "col-md-6",
          tags$table(
            class = "table table-sm table-borderless",
            tags$tbody(
              tags$tr(tags$td("N (non-missing):"), tags$td(strong(format(n_obs, big.mark = ",")))),
              tags$tr(tags$td("N (missing):"), tags$td(strong(n_missing))),
              tags$tr(tags$td("Unique values:"), tags$td(strong(n_unique))),
              tags$tr(tags$td("Mean length:"), tags$td(strong(round(mean_length, 2)))),
              tags$tr(tags$td("Max length:"), tags$td(strong(max_length)))
            )
          )
        )
      ),
      
      # Sample values
      div(
        class = "mt-4",
        h6("Sample Values (first 10 unique)"),
        div(
          class = "card bg-light",
          div(
            class = "card-body",
            style = "max-height: 300px; overflow-y: auto;",
            tags$ul(
              lapply(unique(col_clean)[1:min(10, n_unique)], function(val) {
                tags$li(code(as.character(val)))
              })
            )
          )
        )
      )
    )
  )
}

# Unknown type inspector
render_unknown_inspector <- function(ns, var_name, col_data, var_type) {
  
  div(
    class = "card",
    div(
      class = "card-header",
      h5(var_name),
      span(class = "text-muted", paste("Type:", var_type))
    ),
    div(
      class = "card-body",
      div(
        class = "alert alert-info",
        "Detailed inspection not available for this variable type"
      )
    )
  )
}

# =============================================================================
# =============================================================================

message("[SAP] Data Explorer v2.4-20260708 loaded")  # SAP_MODULE_BANNER
