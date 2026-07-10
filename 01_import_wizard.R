# =============================================================================
# MODULE: Import Wizard
# =============================================================================
# 
# Complete 6-step data import wizard with:
# 1. Data Source Selection (CSV, Excel, Clipboard, URL)
# 2. Import Options (delimiter, encoding, missing values, cleaning)
# 3. Variable Detection (auto-detect types: numeric, categorical, date, etc.)
# 4. Value Label Wizard (label categorical variables)
# 5. Validation (check for duplicates, empty rows, mixed types, etc.)
# 6. Summary & Confirmation (show what was imported)
#
# =============================================================================

# =============================================================================
# UI FUNCTION
# =============================================================================

mod_import_wizard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header
    div(
      class = "mb-4",
      h2(icon("upload"), "Data Import Wizard"),
      p("Import your dataset in 6 easy steps", class = "text-muted")
    ),
    
    # Main content area
    div(
      class = "card",
      div(
        class = "card-body",
        
        # Step indicator
        uiOutput(ns("step_indicator")),
        hr(),
        
        # Step content (changes based on current step)
        uiOutput(ns("step_content")),
        
        hr(),
        
        # Navigation buttons
        div(
          class = "d-flex justify-content-between mt-4",
          actionButton(ns("btn_prev"), "← Previous", class = "btn btn-secondary", disabled = NA),
          uiOutput(ns("next_button_ui"))
        )
      )
    )
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================

mod_import_wizard_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # =========================================================================
    # REACTIVE STATE FOR WIZARD
    # =========================================================================
    
    wizard_state <- reactiveValues(
      current_step = 1,
      raw_data = NULL,
      file_info = NULL,
      # Excel worksheet selection
      file_path = NULL,
      file_ext = NULL,
      sheets = NULL,
      sheet = NULL,
      import_options = list(
        header = TRUE,
        delimiter = ",",
        decimal = ".",
        encoding = "UTF-8",
        clean_names = TRUE,
        trim_whitespace = TRUE,
        remove_empty_rows = TRUE,
        remove_empty_cols = TRUE,
        auto_detect_dates = TRUE,
        missing_codes = c("", "NA", "N/A", "NULL", "99", "999", "-99")
      ),
      variable_types = NULL,
      variable_labels = NULL,
      validation_results = NULL,
      import_log = data.frame()
    )
    
    # =========================================================================
    # STEP INDICATOR
    # =========================================================================
    
    output$step_indicator <- renderUI({
      steps <- list(
        list(num = 1, title = "Data Source", icon = "file"),
        list(num = 2, title = "Options", icon = "sliders"),
        list(num = 3, title = "Variables", icon = "table"),
        list(num = 4, title = "Labels", icon = "tags"),
        list(num = 5, title = "Validate", icon = "check"),
        list(num = 6, title = "Summary", icon = "flag-checkered")
      )
      
      div(
        class = "progress mb-4",
        style = "height: 50px;",
        lapply(steps, function(step) {
          is_active <- wizard_state$current_step == step$num
          is_complete <- wizard_state$current_step > step$num
          
          step_class <- if (is_active) {
            "step-active"
          } else if (is_complete) {
            "step-complete"
          } else {
            "step-inactive"
          }
          
          div(
            class = paste("progress-bar text-dark", step_class),
            role = "progressbar",
            style = "width: 16.666%; border: 1px solid #ddd;",
            div(
              style = "padding: 8px; text-align: center; font-size: 12px;",
              icon(step$icon),
              br(),
              strong(step$num, ". ", step$title)
            )
          )
        })
      )
    })
    
    # =========================================================================
    # STEP CONTENT - MAIN UI RENDERER
    # =========================================================================
    
    output$step_content <- renderUI({
      switch(as.character(wizard_state$current_step),
        "1" = step_1_data_source_ui(ns),
        "2" = step_2_import_options_ui(ns),
        "3" = step_3_variable_detection_ui(ns),
        "4" = step_4_value_labels_ui(ns),
        "5" = step_5_validation_ui(ns),
        "6" = step_6_summary_ui(ns),
        div(class = "alert alert-danger", "Unknown step")
      )
    })
    
    # =========================================================================
    # STEP 1: DATA SOURCE SELECTION
    # =========================================================================
    
    step_1_data_source_ui <- function(ns) {
      tagList(
        h4("Step 1: Select Data Source"),
        p("Choose how you want to import your data", class = "text-muted"),
        
        navset_tab(
          nav_panel(
            "Upload File",
            div(
              class = "mt-3",
              fileInput(
                ns("upload_file"),
                "Choose file (CSV or Excel)",
                accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv")
              ),
              span(
                class = "small text-muted d-block mb-2",
                sprintf("Maximum upload size: %s. Raise with options(sap.max_upload_mb = N) before startup.",
                        sap_max_upload_label())
              ),
              # Worksheet picker: rendered only for Excel workbooks.
              uiOutput(ns("excel_sheet_ui")),
              div(
                id = ns("file_preview_box"),
                uiOutput(ns("file_preview"))
              )
            )
          ),
          
          nav_panel(
            "Paste Data",
            div(
              class = "mt-3",
              p("Paste your data directly below (comma or tab-separated)"),
              textAreaInput(
                ns("paste_data"),
                "Paste data:",
                height = "250px",
                placeholder = "col1,col2,col3\nval1,val2,val3\n..."
              ),
              actionButton(ns("btn_parse_paste"), "Parse Pasted Data", class = "btn-primary")
            )
          ),
          
          nav_panel(
            "URL",
            div(
              class = "mt-3",
              textInput(
                ns("url_input"),
                "CSV or Excel file URL",
                placeholder = "https://example.com/data.csv"
              ),
              actionButton(ns("btn_load_url"), "Load from URL", class = "btn-primary")
            )
          )
        ),
        
        # File info display
        uiOutput(ns("file_info_display"))
      )
    }
    
    # Worksheet picker. Only rendered for Excel workbooks; a single-sheet
    # workbook still shows it (read-only information about what was loaded).
    output$excel_sheet_ui <- renderUI({
      sheets <- wizard_state$sheets
      if (is.null(sheets) || !length(sheets)) return(NULL)

      div(
        class = "mb-3 p-2 border rounded bg-light",
        selectInput(
          ns("excel_sheet"),
          label = tagList(icon("table-list"), " Worksheet"),
          choices  = sheets,
          selected = wizard_state$sheet %||% sheets[1],
          width = "100%"
        ),
        span(
          class = "small text-muted",
          sprintf("%d worksheet%s in this workbook",
                  length(sheets), if (length(sheets) == 1L) "" else "s")
        )
      )
    })

    # Handle file upload
    observeEvent(input$upload_file, {
      req(input$upload_file)
      
      file_path <- input$upload_file$datapath
      file_name <- input$upload_file$name
      file_size <- input$upload_file$size
      
      tryCatch({
        # Determine file type
        ext <- tolower(tools::file_ext(file_name))

        if (!ext %in% c("csv", "txt", "tsv", "xlsx", "xls")) {
          showNotification("Unsupported file format. Please use CSV, TSV, TXT or Excel.",
                           type = "error")
          return()
        }

        # For Excel, enumerate worksheets and load the first one. The user can
        # switch sheets afterwards via input$excel_sheet.
        # Locals only -- never read wizard_state back in the same observer.
        sheets <- if (ext %in% c("xlsx", "xls")) excel_sheet_names(file_path)
                  else character(0)
        first_sheet <- if (length(sheets)) sheets[1] else NULL

        # Read the FULL file (no row/column limit), auto-detecting delimiter
        # and encoding. See read_any_file() in the helpers section.
        # Large files take a while: give the user something to look at.
        df <- withProgress(
          message = sprintf("Reading %s", file_name),
          detail  = "this can take a moment for large files",
          value   = 0.4,
          read_any_file(file_path, ext, sheet = first_sheet))

        if (is.null(df) || nrow(df) == 0) {
          showNotification("File appears to be empty or unreadable.", type = "error")
          return()
        }

        wizard_state$file_path <- file_path
        wizard_state$file_ext  <- ext
        wizard_state$sheets    <- if (length(sheets)) sheets else NULL
        wizard_state$sheet     <- first_sheet
        wizard_state$raw_data  <- df

        wizard_state$file_info <- list(
          name = file_name,
          size = file_size,
          path = file_path,
          ext = ext,
          sheet = first_sheet,
          n_sheets = length(sheets),
          nrows = nrow(df),
          ncols = ncol(df)
        )

        showNotification(
          if (length(sheets) > 1L)
            sprintf("Loaded worksheet '%s' (%d sheets available)", first_sheet,
                    length(sheets))
          else "File loaded successfully!",
          type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    })

    # Re-read the workbook when the user picks a different worksheet.
    observeEvent(input$excel_sheet, {
      path <- isolate(wizard_state$file_path)
      ext  <- isolate(wizard_state$file_ext)
      req(path, ext, ext %in% c("xlsx", "xls"))

      sel <- input$excel_sheet
      req(nzchar(sel))
      if (identical(sel, isolate(wizard_state$sheet))) return(invisible(NULL))

      tryCatch({
        df <- read_any_file(path, ext, sheet = sel)
        if (is.null(df) || nrow(df) == 0) {
          showNotification(sprintf("Worksheet '%s' is empty.", sel), type = "warning")
          return()
        }

        # Rebuild file_info from an isolated copy: reading + writing the same
        # nested reactiveValues in one observer self-invalidates.
        fi <- isolate(wizard_state$file_info)
        if (is.null(fi)) fi <- list()
        fi$sheet <- sel
        fi$nrows <- nrow(df)
        fi$ncols <- ncol(df)

        wizard_state$sheet     <- sel
        wizard_state$raw_data  <- df
        wizard_state$file_info <- fi

        # Downstream steps were derived from the old sheet.
        wizard_state$variable_types  <- NULL
        wizard_state$variable_labels <- NULL

        showNotification(sprintf("Loaded worksheet '%s' (%d x %d)",
                                 sel, nrow(df), ncol(df)), type = "message")
      }, error = function(e) {
        showNotification(paste("Could not read worksheet:", e$message),
                         type = "error")
      })
    }, ignoreInit = TRUE)
    
    # File preview
    output$file_preview <- renderUI({
      if (is.null(wizard_state$raw_data)) {
        return(NULL)
      }
      
      div(
        class = "alert alert-info mt-3",
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          h5("Preview (first 5 rows)"),
          div(
            class = "btn-group",
            actionButton(ns("btn_preview_modal"), "\U0001F4CA View Full Dataset",
                        class = "btn btn-sm btn-primary"),
            actionButton(ns("btn_edit_modal"), "\u270F\uFE0F Edit Dataset",
                        class = "btn btn-sm btn-warning")
          )
        ),
        renderTable(
          sanitize_encoding(head(wizard_state$raw_data, 5)),
          striped = TRUE,
          hover = TRUE,
          bordered = TRUE,
          width = "100%"
        )
      )
    })
    
    # Preview modal - show full dataset with navigation
    observeEvent(input$btn_preview_modal, {
      req(wizard_state$raw_data)
      showModal(
        modalDialog(
          title = "Dataset Preview - Full Data",
          size = "xl",
          easyClose = TRUE,
          
          div(
            # Dataset info
            div(
              class = "row mb-3",
              div(class = "col-md-4",
                strong("Rows: "), format(nrow(wizard_state$raw_data), big.mark = ",")),
              div(class = "col-md-4",
                strong("Columns: "), ncol(wizard_state$raw_data)),
              div(class = "col-md-4",
                strong("Memory: "),
                paste0(round(as.numeric(object.size(wizard_state$raw_data)) /
                             1024 / 1024, 2), " MB"))
            ),
            hr(),
            # Paginated, searchable, horizontally-scrolling table (DT handles
            # large data far better than a static renderTable).
            DT::DTOutput(ns("full_dataset_dt"))
          ),
          
          footer = modalButton("Close")
        )
      )
    })

    output$full_dataset_dt <- DT::renderDT({
      req(wizard_state$raw_data)
      DT::datatable(
        wizard_state$raw_data,
        rownames = FALSE,
        filter   = "top",
        options  = list(
          scrollX     = TRUE,
          pageLength  = 15,
          lengthMenu  = c(10, 15, 25, 50, 100),
          searchHighlight = TRUE
        )
      )
    })

    # ---- Edit Dataset -----------------------------------------------------
    # Opens an editable table; cell edits are staged and applied to the raw
    # data on Save, so the user can fix values before importing.
    edit_buffer <- reactiveVal(NULL)

    observeEvent(input$btn_edit_modal, {
      req(wizard_state$raw_data)
      edit_buffer(wizard_state$raw_data)     # working copy
      showModal(
        modalDialog(
          title = "Edit Dataset",
          size = "xl",
          easyClose = FALSE,
          div(
            class = "alert alert-warning small",
            "Double-click a cell to edit. Changes apply to the dataset only ",
            "when you click ", strong("Save changes"), "."
          ),
          DT::DTOutput(ns("edit_dataset_dt")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("btn_save_edits"), "Save changes",
                         class = "btn btn-success")
          )
        )
      )
    })

    output$edit_dataset_dt <- DT::renderDT({
      req(edit_buffer())
      DT::datatable(
        edit_buffer(),
        rownames  = FALSE,
        editable  = "cell",
        options   = list(scrollX = TRUE, pageLength = 15,
                         lengthMenu = c(10, 15, 25, 50))
      )
    })

    # Stage each cell edit into the working copy.
    observeEvent(input$edit_dataset_dt_cell_edit, {
      info <- input$edit_dataset_dt_cell_edit
      buf  <- edit_buffer()
      req(buf)
      # DT sends 0-based column index with rownames = FALSE
      j <- info$col + 1L
      buf[info$row, j] <- DT::coerceValue(info$value, buf[info$row, j])
      edit_buffer(buf)
    })

    observeEvent(input$btn_save_edits, {
      req(edit_buffer())
      wizard_state$raw_data <- edit_buffer()
      removeModal()
      msg <- "Dataset updated. Re-run detection/validation if needed."
      if (!is.null(services) && !is.null(services$notify)) {
        services$notify$success(msg, module = "import_wizard")
      } else {
        showNotification(msg, type = "message")
      }
    })

    # File info display
    output$file_info_display <- renderUI({
      if (is.null(wizard_state$file_info)) {
        return(NULL)
      }
      
      info <- wizard_state$file_info
      
      div(
        class = "alert alert-success mt-4",
        h5(icon("check-circle"), "File Information"),
        div(
          class = "row",
          div(class = "col-md-6", strong("File:"), br(), info$name),
          div(class = "col-md-6", strong("Size:"), br(), 
              paste(round(info$size / 1024, 1), "KB"))
        ),
        div(
          class = "row mt-2",
          div(class = "col-md-6", strong("Rows:"), br(), info$nrows),
          div(class = "col-md-6", strong("Columns:"), br(), info$ncols)
        ),
        if (!is.null(info$sheet)) div(
          class = "row mt-2",
          div(class = "col-md-6", strong("Worksheet:"), br(), info$sheet),
          div(class = "col-md-6", strong("Sheets in file:"), br(),
              info$n_sheets %||% 1L)
        )
      )
    })
    
    # =========================================================================
    # STEP 2: IMPORT OPTIONS
    # =========================================================================
    
    step_2_import_options_ui <- function(ns) {
      tagList(
        h4("Step 2: Import Options"),
        p("Configure how your data should be imported", class = "text-muted"),
        
        div(
          class = "row",
          div(
            class = "col-md-6",
            h5("Format Options"),
            checkboxInput(ns("opt_header"), "First row contains headers", 
                         value = wizard_state$import_options$header),
            selectInput(
              ns("opt_delimiter"),
              "Delimiter:",
              choices = list("Comma (,)" = ",", "Semicolon (;)" = ";", 
                           "Tab" = "\t", "Space" = " "),
              selected = wizard_state$import_options$delimiter
            ),
            selectInput(
              ns("opt_decimal"),
              "Decimal separator:",
              choices = list("Period (.)" = ".", "Comma (,)" = ","),
              selected = wizard_state$import_options$decimal
            ),
            selectInput(
              ns("opt_encoding"),
              "Encoding:",
              choices = list("UTF-8" = "UTF-8", "Latin-1" = "Latin-1", 
                           "ISO-8859-1" = "ISO-8859-1", "ASCII" = "ASCII"),
              selected = wizard_state$import_options$encoding
            )
          ),
          
          div(
            class = "col-md-6",
            h5("Data Cleaning"),
            checkboxInput(ns("opt_clean_names"), "Clean variable names (snake_case)",
                         value = wizard_state$import_options$clean_names),
            checkboxInput(ns("opt_trim"), "Trim whitespace",
                         value = wizard_state$import_options$trim_whitespace),
            checkboxInput(ns("opt_remove_empty_rows"), "Remove empty rows",
                         value = wizard_state$import_options$remove_empty_rows),
            checkboxInput(ns("opt_remove_empty_cols"), "Remove empty columns",
                         value = wizard_state$import_options$remove_empty_cols),
            checkboxInput(ns("opt_auto_dates"), "Auto-detect dates",
                         value = wizard_state$import_options$auto_detect_dates)
          )
        ),
        
        hr(),
        
        h5("Missing Value Codes"),
        p("Which values should be treated as missing?", class = "text-muted"),
        
        div(
          class = "bg-light p-3 rounded",
          checkboxGroupInput(
            ns("missing_codes"),
            "Select missing value codes:",
            choices = c(
              "Empty string (\"\")" = "",
              "NA" = "NA",
              "N/A" = "N/A",
              "NULL" = "NULL",
              "99" = "99",
              "999" = "999",
              "-99" = "-99"
            ),
            selected = wizard_state$import_options$missing_codes,
            inline = FALSE
          ),
          textInput(ns("custom_missing"), "Add custom missing code:", 
                   placeholder = "e.g., NaN, .")
        )
      )
    }
    
    # Sync Step 2 inputs into wizard_state.
    # Use observeEvent(ignoreInit = TRUE): the handler runs ONLY when a step-2
    # input actually changes (i.e. after that UI is rendered), never during
    # module construction. This avoids reading inputs that don't exist yet and
    # keeps every read inside a proper reactive context.
    observeEvent(
      list(
        input$opt_header, input$opt_delimiter, input$opt_decimal,
        input$opt_encoding, input$opt_clean_names, input$opt_trim,
        input$opt_remove_empty_rows, input$opt_remove_empty_cols,
        input$opt_auto_dates, input$missing_codes, input$custom_missing
      ),
      {
        opts <- isolate(wizard_state$import_options)

        opts$header            <- input$opt_header            %||% opts$header
        opts$delimiter         <- input$opt_delimiter         %||% opts$delimiter
        opts$decimal           <- input$opt_decimal           %||% opts$decimal
        opts$encoding          <- input$opt_encoding          %||% opts$encoding
        opts$clean_names       <- input$opt_clean_names       %||% opts$clean_names
        opts$trim_whitespace   <- input$opt_trim              %||% opts$trim_whitespace
        opts$remove_empty_rows <- input$opt_remove_empty_rows %||% opts$remove_empty_rows
        opts$remove_empty_cols <- input$opt_remove_empty_cols %||% opts$remove_empty_cols
        opts$auto_detect_dates <- input$opt_auto_dates        %||% opts$auto_detect_dates

        if (!is.null(input$missing_codes) || !is.null(input$custom_missing)) {
          opts$missing_codes <- c(input$missing_codes, input$custom_missing)
        }

        wizard_state$import_options <- opts
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )
    
    # =========================================================================
    # STEP 3: VARIABLE DETECTION
    # =========================================================================
    
    step_3_variable_detection_ui <- function(ns) {
      tagList(
        h4("Step 3: Variable Detection"),
        p("Review detected variable types. Click to change type.", class = "text-muted"),
        
        div(
          id = ns("var_detection_box"),
          uiOutput(ns("variable_detection_table"))
        )
      )
    }
    
    # Detect variable types when moving to step 3.
    # Trigger on step + variable_types so a reset (types -> NULL) while on
    # step 3 re-detects, matching the original observe semantics.
    shiny::observeEvent(
      list(wizard_state$current_step, wizard_state$variable_types),
      {
        if (wizard_state$current_step == 3 && is.null(wizard_state$variable_types)) {
          tryCatch({
            # Re-load data with current options
            df <- load_data_with_options(wizard_state$raw_data, wizard_state$import_options)
            wizard_state$variable_types <- detect_variable_types_extended(df)
          }, error = function(e) {
            showNotification(paste("Error detecting types:", e$message), type = "error")
            # Fall back to a basic type read straight off the raw frame so the
            # step resolves (the spinner would otherwise spin forever, since it
            # waits on variable_types). The user can still correct types by hand.
            fallback <- tryCatch(
              detect_variable_types(wizard_state$raw_data),
              error = function(e2) {
                data.frame(
                  variable = names(wizard_state$raw_data),
                  type = vapply(wizard_state$raw_data, function(x) class(x)[1],
                                character(1)),
                  stringsAsFactors = FALSE)
              })
            wizard_state$variable_types <- fallback
          })
        }
      },
      ignoreInit = FALSE
    )
    
    output$variable_detection_table <- renderUI({
      if (is.null(wizard_state$variable_types)) {
        return(div(class = "spinner-border", role = "status"))
      }
      
      var_table <- wizard_state$variable_types
      
      div(
        div(
          class = "table-responsive",
          style = "max-height: 500px; overflow-y: auto;",
          tags$table(
            class = "table table-sm table-hover",
            tags$thead(
              tags$tr(
                tags$th("Variable"),
                tags$th("Detected Type"),
                tags$th("Missing"),
                tags$th("Unique"),
                tags$th("Change Type?")
              )
            ),
            tags$tbody(
              lapply(1:nrow(var_table), function(i) {
                var_name <- var_table$variable[i]
                var_type <- var_table$type[i]
                
                tags$tr(
                  tags$td(strong(var_name)),
                  tags$td(span(class = "badge bg-info", var_type)),
                  tags$td(paste0(var_table$missing_count[i], " (", var_table$missing_pct[i], "%)")),
                  tags$td(var_table$unique_values[i]),
                  tags$td(
                    selectInput(
                      ns(paste0("type_", i)),
                      NULL,
                      choices = c("numeric", "integer", "character", "factor", "date", "logical"),
                      selected = var_type,
                      width = "150px"
                    )
                  )
                )
              })
            )
          )
        ),
        
        div(
          class = "mt-3 p-3 bg-light rounded",
          strong("Summary:"),
          br(),
          paste("Total variables:", nrow(var_table)),
          br(),
          paste("Missing values detected in", sum(var_table$missing_count > 0), "variables")
        )
      )
    })
    
    # =========================================================================
    # STEP 4: VALUE LABELS
    # =========================================================================
    
    step_4_value_labels_ui <- function(ns) {
      tagList(
        h4("Step 4: Value Labels (Optional)"),
        p("Label categorical variables for clarity", class = "text-muted"),
        
        div(
          uiOutput(ns("categorical_labeling"))
        )
      )
    }
    
    output$categorical_labeling <- renderUI({
      if (is.null(wizard_state$variable_types)) {
        return(div(class = "text-muted", "No variables to label. Go to previous step."))
      }
      
      # Get categorical variables
      var_table <- wizard_state$variable_types
      cat_vars <- var_table$variable[var_table$type %in% c("factor", "categorical_character", "integer")]
      
      if (length(cat_vars) == 0) {
        return(div(class = "alert alert-info", "No categorical variables detected."))
      }
      
      tagList(
        div(
          class = "alert alert-info",
          "Found ", strong(length(cat_vars)), " categorical variable(s). ",
          "Select only the ones you want to label \u2014 labels are optional."
        ),
        # Let the user CHOOSE which variables to label (instead of showing all).
        shinyWidgets::pickerInput(
          ns("vars_to_label"),
          "Variables to label:",
          choices  = cat_vars,
          multiple = TRUE,
          options  = shinyWidgets::pickerOptions(
            actionsBox        = TRUE,
            liveSearch        = TRUE,
            noneSelectedText  = "Choose variables to label...",
            selectedTextFormat = "count > 2"
          )
        ),
        # Label cards render only for the selected variables.
        uiOutput(ns("label_cards"))
      )
    })

    # Render value-label cards ONLY for the variables the user selected.
    output$label_cards <- renderUI({
      selected <- input$vars_to_label
      if (is.null(selected) || length(selected) == 0) {
        return(div(class = "text-muted small",
                   "No variables selected. Pick one or more above to add labels, ",
                   "or continue \u2014 labelling is optional."))
      }

      tagList(
        lapply(selected, function(var_name) {
          col_data <- wizard_state$raw_data[[var_name]]
          unique_vals <- unique(na.omit(col_data))
          
          if (length(unique_vals) > 20) {
            return(div(
              class = "card mb-3",
              div(class = "card-header",
                  strong(var_name),
                  span(class = "badge bg-warning float-end",
                       paste(length(unique_vals), "unique"))),
              div(class = "card-body text-muted small",
                  "Too many unique values (>20) to label individually.")
            ))
          }
          
          div(
            class = "card mb-3",
            div(
              class = "card-header",
              strong(var_name),
              span(class = "badge bg-secondary float-end", paste(length(unique_vals), "unique"))
            ),
            div(
              class = "card-body",
              lapply(unique_vals, function(val) {
                label_input_id <- ns(paste0("label_", var_name, "_", val))
                div(
                  class = "row mb-2",
                  div(class = "col-md-3", code(as.character(val))),
                  div(class = "col-md-9",
                    textInput(
                      label_input_id,
                      NULL,
                      placeholder = "Enter label..."
                    )
                  )
                )
              })
            )
          )
        })
      )
    })
    
    # Summary preview table — driven by the "Rows to view" selector so the
    # user chooses how many rows to see (5/10/15/20/25/50/100).
    output$summary_preview_table <- renderTable({
      req(wizard_state$raw_data)
      n <- as.integer(input$summary_n_rows %||% 10)
      df <- load_data_with_options(wizard_state$raw_data, wizard_state$import_options)
      sanitize_encoding(head(df, n))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # =========================================================================
    # STEP 5: VALIDATION
    # =========================================================================
    
    step_5_validation_ui <- function(ns) {
      tagList(
        h4("Step 5: Data Validation"),
        p("Checking data quality before import", class = "text-muted"),
        
        uiOutput(ns("validation_report"))
      )
    }
    
    output$validation_report <- renderUI({
      if (wizard_state$current_step == 5) {
        tryCatch({
          df <- load_data_with_options(wizard_state$raw_data, wizard_state$import_options)
          wizard_state$validation_results <- validate_data_comprehensive(df)
        }, error = function(e) {
          showNotification(paste("Validation error:", e$message), type = "error")
        })
      }
      
      if (is.null(wizard_state$validation_results)) {
        return(div(class = "spinner-border"))
      }
      
      results <- wizard_state$validation_results
      
      tagList(
        # Summary
        div(
          class = "row mb-4",
          div(
            class = "col-md-3",
            div(
              class = "card",
              div(
                class = "card-body text-center",
                h3(class = "text-success", icon("check-circle"), " ", nrow(results$checks_passed)),
                "Checks Passed"
              )
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "card",
              div(
                class = "card-body text-center",
                h3(class = if (results$warnings > 0) "text-warning" else "text-success",
                   icon("exclamation-triangle"), " ", results$warnings),
                "Warnings"
              )
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "card",
              div(
                class = "card-body text-center",
                h3(class = if (results$errors > 0) "text-danger" else "text-success",
                   icon("times-circle"), " ", results$errors),
                "Errors"
              )
            )
          )
        ),
        
        # Detailed report
        div(
          id = ns("validation_details"),
          lapply(results$checks_passed$check, function(check) {
            div(
              class = "alert alert-success",
              icon("check"), " ", check
            )
          }),
          
          lapply(results$warnings_list, function(warning) {
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"), " ", warning
            )
          }),
          
          lapply(results$errors_list, function(error) {
            div(
              class = "alert alert-danger",
              icon("times-circle"), " ", error
            )
          })
        )
      )
    })
    
    # =========================================================================
    # STEP 6: SUMMARY & CONFIRMATION
    # =========================================================================
    
    step_6_summary_ui <- function(ns) {
      tagList(
        h4("Step 6: Import Summary"),
        p("Review and confirm your import", class = "text-muted"),
        
        uiOutput(ns("import_summary")),
        
        div(
          class = "mt-4",
          actionButton(ns("btn_confirm_import"), "✓ Confirm & Import Data", 
                      class = "btn btn-lg btn-success"),
          actionButton(ns("btn_cancel_import"), "Cancel", 
                      class = "btn btn-lg btn-secondary ms-2")
        )
      )
    }
    
    output$import_summary <- renderUI({
      if (is.null(wizard_state$raw_data)) {
        return(div(class = "alert alert-danger", "No data loaded"))
      }
      
      df <- load_data_with_options(wizard_state$raw_data, wizard_state$import_options)
      
      tagList(
        div(
          class = "row mb-4",
          div(
            class = "col-md-4",
            kpi_card("Rows", format(nrow(df), big.mark = ","), 
                    subtitle = "observations", color = "primary")
          ),
          div(
            class = "col-md-4",
            kpi_card("Columns", ncol(df), 
                    subtitle = "variables", color = "success")
          ),
          div(
            class = "col-md-4",
            kpi_card("Memory", 
                    format(object.size(df), units = "Mb"),
                    subtitle = "estimated", color = "info")
          )
        ),
        
        div(
          class = "card",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            strong("Rows Preview"),
            div(
              class = "d-flex align-items-center",
              tags$label("Rows to view:", class = "me-2 mb-0 small text-muted"),
              selectInput(
                ns("summary_n_rows"), NULL,
                choices  = c(5, 10, 15, 20, 25, 50, 100),
                selected = 10,
                width    = "100px"
              )
            )
          ),
          div(
            class = "card-body",
            div(
              class = "table-responsive",
              style = "max-height: 400px; overflow: auto;",
              tableOutput(ns("summary_preview_table"))
            )
          )
        ),
        
        div(
          class = "card mt-3",
          div(
            class = "card-header",
            strong("Variable Summary")
          ),
          div(
            class = "card-body",
            renderTable(
              sanitize_encoding(wizard_state$variable_types[, c("variable", "type", "missing_count", "unique_values")]),
              striped = TRUE,
              hover = TRUE
            )
          )
        )
      )
    })
    
    # =========================================================================
    # NAVIGATION BUTTONS
    # =========================================================================

    # Next button is hidden on the final step (Summary, where "Confirm & Import"
    # is the action) and once the dataset has been imported successfully.
    output$next_button_ui <- renderUI({
      if (wizard_state$current_step >= 6 || isTRUE(wizard_state$imported)) {
        return(NULL)
      }
      actionButton(ns("btn_next"), "Next →", class = "btn btn-primary")
    })

    observeEvent(input$btn_next, {
      # Validation before moving to next step
      if (wizard_state$current_step == 1) {
        if (is.null(wizard_state$raw_data)) {
          showNotification("Please load a file first", type = "error")
          return()
        }
      }
      
      if (wizard_state$current_step < 6) {
        wizard_state$current_step <- wizard_state$current_step + 1
      }
    })
    
    observeEvent(input$btn_prev, {
      if (wizard_state$current_step > 1) {
        wizard_state$current_step <- wizard_state$current_step - 1
      }
    })
    
    # =========================================================================
    # CONFIRM IMPORT
    # =========================================================================
    
    observeEvent(input$btn_confirm_import, {
      tryCatch({
        # Load and process data
        df <- load_data_with_options(wizard_state$raw_data, wizard_state$import_options)
        
        # Handle missing codes
        df <- handle_missing_codes(df, wizard_state$import_options$missing_codes)
        
        # Clean names if requested
        if (wizard_state$import_options$clean_names) {
          df <- clean_names(df, case = "snake")
        }
        
        # Commit the dataset. Prefer the backend service (single source of
        # truth: bumps data version, invalidates cache, logs, persists via
        # DAL). Fall back to direct app_state writes if services not wired,
        # so the module still works standalone during incremental migration.
        if (!is.null(services)) {
          # Ensure a project exists BEFORE the dataset write, so persistence
          # has somewhere to land. On a fresh start there is no current
          # project, so DatasetService$set()'s DAL write would be a no-op and
          # the import would vanish on reopen. Auto-creating a project here is
          # what makes "import -> close -> reopen -> still there" work without
          # the user having to think about projects at all.
          if (!is.null(services$project) &&
              is.null(services$project$current_id())) {
            base <- tools::file_path_sans_ext(
              wizard_state$file_info$name %||% "Imported dataset")
            services$project$create(
              name = sprintf("%s", base),
              description = "Auto-created on first import",
              persist_current = FALSE)
          }

          # raw stays in app_state (service governs the *active* dataset)
          app_state$raw_data <- wizard_state$raw_data
          services$data$set(
            df,
            kind           = "processed",
            module         = "import_wizard",
            description    = sprintf("Imported %s",
                                     wizard_state$file_info$name %||% "dataset"),
            variable_types = wizard_state$variable_types
          )
        } else {
          app_state$raw_data       <- wizard_state$raw_data
          app_state$processed_data <- df
          app_state$data_imported  <- TRUE
          app_state$variable_types <- wizard_state$variable_types
        }
        
        # Create import log entry
        log_entry <- data.frame(
          timestamp = Sys.time(),
          file_name = wizard_state$file_info$name,
          rows = nrow(df),
          columns = ncol(df),
          file_size_kb = round(wizard_state$file_info$size / 1024, 1)
        )
        
        app_state$import_log <- rbind(app_state$import_log, log_entry)
        
        # Success notification (via service when available, else direct)
        msg <- sprintf("Import successful: %d rows x %d columns loaded",
                       nrow(df), ncol(df))
        if (!is.null(services)) {
          services$notify$success(msg, module = "import_wizard")
        } else {
          showNotification(
            HTML(paste0("<strong>✓ Import Successful!</strong><br>",
                        nrow(df), " rows × ", ncol(df), " columns loaded")),
            type = "message", duration = 5
          )
        }
        
        # Mark as imported. Stay on the Summary step (do not reset to step 1)
        # so the user sees the success state with View / Edit actions, and the
        # Next button stays hidden.
        wizard_state$imported <- TRUE
        
      }, error = function(e) {
        if (!is.null(services)) {
          services$notify$error(paste("Import failed:", e$message),
                                module = "import_wizard")
        } else {
          showNotification(paste("Import failed:", e$message), type = "error")
        }
      })
    })
    
    observeEvent(input$btn_cancel_import, {
      # Reset wizard
      wizard_state$current_step <- 1
      wizard_state$raw_data <- NULL
      wizard_state$file_info <- NULL
      wizard_state$imported <- FALSE
      wizard_state$file_path <- NULL
      wizard_state$file_ext <- NULL
      wizard_state$sheets <- NULL
      wizard_state$sheet <- NULL
      showNotification("Import cancelled", type = "info")
    })
    
    # =========================================================================
    # HELPER FUNCTIONS (Module-specific)
    # =========================================================================

    # Make a data frame safe for renderTable()/xtable::sanitize(), which calls
    # gsub() in the session locale and errors on bytes invalid for that locale
    # (common with xlsx text on a non-UTF-8 Windows locale). Converting
    # character/factor columns to UTF-8-marked strings makes gsub treat them as
    # UTF-8 regardless of locale. Invalid bytes are replaced rather than fatal.
    sanitize_encoding <- function(df) {
      if (is.null(df) || ncol(df) == 0) return(df)
      is_txt <- vapply(df, function(x) is.character(x) || is.factor(x),
                       logical(1))
      df[is_txt] <- lapply(df[is_txt], function(x) {
        enc2utf8(iconv(as.character(x), from = "", to = "UTF-8", sub = "byte"))
      })
      # column names too
      names(df) <- enc2utf8(iconv(names(df), from = "", to = "UTF-8",
                                  sub = "byte"))
      df
    }

    # Read a delimited-text or Excel file into a data.frame with NO row/column
    # limit, auto-detecting delimiter and encoding, and inferring column types
    # from many rows. Handles large files and non-UTF-8 (Latin-1 / extended
    # ASCII) CSVs. Prefers {readr} when available, falls back to base R.
    # Worksheet names of an Excel workbook; character(0) on any failure so the
    # caller can fall back to a plain single-sheet read.
    excel_sheet_names <- function(path) {
      tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
    }

    read_any_file <- function(path, ext, sheet = NULL) {
      # --- Excel -----------------------------------------------------------
      # `sheet` may be a worksheet name or a 1-based index; NULL = first sheet
      # (readxl's own default).
      if (ext %in% c("xlsx", "xls")) {
        args <- list(path = path)
        if (!is.null(sheet) && nzchar(as.character(sheet))) args$sheet <- sheet
        return(as.data.frame(do.call(readxl::read_excel, args),
                             stringsAsFactors = FALSE, check.names = FALSE))
      }

      # --- Delimited text: detect delimiter from the header line -----------
      line1 <- tryCatch(
        readLines(path, n = 1, warn = FALSE), error = function(e) "")
      if (!length(line1) || is.na(line1)) line1 <- ""
      counts <- c(
        ","  = lengths(regmatches(line1, gregexpr(",",  line1))),
        "\t" = lengths(regmatches(line1, gregexpr("\t", line1))),
        ";"  = lengths(regmatches(line1, gregexpr(";",  line1))),
        "|"  = lengths(regmatches(line1, gregexpr("\\|", line1)))
      )
      delim <- if (ext == "tsv") "\t" else names(which.max(counts))
      if (is.null(delim) || !nzchar(delim)) delim <- ","

      # --- Preferred path: readr (fast, encoding-aware, no row limit) ------
      if (requireNamespace("readr", quietly = TRUE)) {
        enc <- tryCatch({
          g <- readr::guess_encoding(path, n_max = 10000)
          if (nrow(g) && g$confidence[1] >= 0.5) g$encoding[1] else "UTF-8"
        }, error = function(e) "UTF-8")

        df <- tryCatch(
          readr::read_delim(
            path, delim = delim,
            locale = readr::locale(encoding = enc),
            guess_max = 1e6,          # infer types from up to 1M rows
            show_col_types = FALSE,
            name_repair = "minimal",
            progress = FALSE
          ),
          error = function(e) NULL
        )
        if (!is.null(df) && ncol(df) > 0) {
          return(as.data.frame(df, stringsAsFactors = FALSE,
                              check.names = FALSE))
        }
      }

      # --- Base R fallback: try common encodings until one parses ----------
      for (enc in c("UTF-8", "latin1", "")) {
        df <- tryCatch(
          utils::read.table(
            path, sep = delim, header = TRUE, stringsAsFactors = FALSE,
            check.names = FALSE, quote = "\"", comment.char = "",
            fill = TRUE, fileEncoding = if (nzchar(enc)) enc else ""
          ),
          error = function(e) NULL, warning = function(w) NULL
        )
        if (!is.null(df) && ncol(df) > 0 && nrow(df) > 0) return(df)
      }

      stop("Could not read the file with common delimiters/encodings.")
    }

    # Load data with specified options
    load_data_with_options <- function(raw_df, options) {
      df <- raw_df
      
      # Handle missing codes
      df <- handle_missing_codes(df, options$missing_codes)
      
      # Trim whitespace on genuine character columns only.
      #
      # `across(where(is.character), str_trim)` aborts the whole pipeline if any
      # selected column isn't a plain atomic character vector (e.g. a list-column
      # from a messy read, or a labelled/haven column that reports as character
      # but carries attributes str_trim chokes on). dplyr rethrows it as the
      # opaque "In argument: across(where(is.character), str_trim)" toast seen in
      # variable detection. Trim each column defensively instead, skipping any
      # that can't be coerced, so one odd column never blocks the import.
      if (isTRUE(options$trim_whitespace)) {
        chr_cols <- names(df)[vapply(df, function(x)
          is.character(x) && is.null(dim(x)), logical(1))]
        for (cn in chr_cols) {
          df[[cn]] <- tryCatch(
            trimws(df[[cn]]),
            error = function(e) df[[cn]])
        }
      }
      
      # Remove empty rows
      if (options$remove_empty_rows) {
        df <- df %>%
          filter(!if_all(everything(), is.na))
      }
      
      # Remove empty columns
      if (options$remove_empty_cols) {
        df <- df %>%
          select(where(~!all(is.na(.x))))
      }
      
      return(df)
    }
    
    # Extended variable type detection
    detect_variable_types_extended <- function(df) {
      result <- detect_variable_types(df)
      result$label <- rep("", nrow(result))
      return(result)
    }
    
    # Comprehensive data validation
    validate_data_comprehensive <- function(df) {
      checks_passed <- data.frame(check = character())
      warnings_list <- list()
      errors_list <- list()
      
      # Check 1: No duplicate columns
      if (length(names(df)) == length(unique(names(df)))) {
        checks_passed <- rbind(checks_passed, data.frame(check = "No duplicate columns"))
      } else {
        warnings_list[[1]] <- "Duplicate column names detected"
      }
      
      # Check 2: No completely empty rows
      empty_rows <- sum(rowSums(is.na(df)) == ncol(df))
      if (empty_rows == 0) {
        checks_passed <- rbind(checks_passed, data.frame(check = "No completely empty rows"))
      } else {
        warnings_list[[length(warnings_list) + 1]] <- 
          paste(empty_rows, "completely empty rows detected")
      }
      
      # Check 3: No completely empty columns
      empty_cols <- sum(colSums(is.na(df)) == nrow(df))
      if (empty_cols == 0) {
        checks_passed <- rbind(checks_passed, data.frame(check = "No completely empty columns"))
      } else {
        warnings_list[[length(warnings_list) + 1]] <- 
          paste(empty_cols, "completely empty columns detected")
      }
      
      # Check 4: Check for duplicates
      dup_rows <- sum(duplicated(df))
      if (dup_rows == 0) {
        checks_passed <- rbind(checks_passed, data.frame(check = "No duplicate rows"))
      } else {
        warnings_list[[length(warnings_list) + 1]] <- 
          paste(dup_rows, "duplicate rows detected")
      }
      
      # Summary
      list(
        checks_passed = checks_passed,
        warnings = length(warnings_list),
        errors = length(errors_list),
        warnings_list = warnings_list,
        errors_list = errors_list
      )
    }
    
  })
}

# =============================================================================
# =============================================================================

message("[SAP] Import Wizard v2.8-20260709 loaded")  # SAP_MODULE_BANNER
