# =============================================================================
# MODULE 26: OPEN-ENDED TEXT ANALYSIS
# =============================================================================

mod_text_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("Open-Ended Text Analysis",
      "Explore verbatim responses, build a coding framework, review keywords, and prepare human-approved qualitative summaries.",
      "comments"),
    uiOutput(ns("body"))
  )
}

mod_text_analysis_server <- function(id, app_state, services = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(app_state$processed_data, {
      chars <- sap_character_names(app_state)
      updateSelectInput(session, "text_var", choices = chars)
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      navset_card_tab(
        nav_panel("Explore responses",
          layout_columns(col_widths = c(4, 8),
            card(class = "sap-card", card_header(icon("sliders"), " Text preparation"), card_body(
              selectInput(session$ns("text_var"), "Open-ended response variable", choices = sap_character_names(app_state)),
              checkboxGroupInput(session$ns("cleaning"), "Cleaning", choices = c(
                "Lowercase" = "lower", "Remove punctuation" = "punct",
                "Remove numbers" = "numbers", "Remove common stopwords" = "stop",
                "Redact emails, phones, URLs and IPs in previews" = "pii"
              ), selected = c("lower", "punct", "stop", "pii")),
              numericInput(session$ns("min_length"), "Minimum word length", 3, min = 1, max = 20),
              numericInput(session$ns("top_n"), "Top terms", 25, min = 5, max = 100),
              numericInput(session$ns("topic_count"), "Theme clusters", 5, min = 2, max = 15),
              textInput(session$ns("search"), "Search responses", placeholder = "Keyword or phrase"),
              actionButton(session$ns("analyze"), "Analyze text", icon = icon("play"), class = "btn-primary w-100")
            )),
            div(
              uiOutput(session$ns("text_kpis")),
              card(class = "sap-card", card_header(icon("chart-column"), " Frequent terms"), card_body(plotlyOutput(session$ns("term_plot"), height = "330px"))),
              card(class = "sap-card mt-3", card_header(icon("shapes"), " Unsupervised theme clusters"), card_body(DTOutput(session$ns("topic_table")))),
              card(class = "sap-card mt-3", card_header(icon("quote-left"), " Response browser"), card_body(DTOutput(session$ns("response_table"))))
            )
          )
        ),
        nav_panel("Coding framework",
          layout_columns(col_widths = c(4, 8),
            card(class = "sap-card", card_header(icon("tags"), " Add code"), card_body(
              textInput(session$ns("code_name"), "Code name"),
              textAreaInput(session$ns("code_definition"), "Definition", rows = 3),
              textAreaInput(session$ns("include_terms"), "Include when", rows = 2),
              textAreaInput(session$ns("exclude_terms"), "Exclude when", rows = 2),
              textInput(session$ns("example"), "Example response"),
              actionButton(session$ns("add_code"), "Add to codebook", icon = icon("plus"), class = "btn-primary w-100"),
              actionButton(session$ns("auto_code"), "Apply codebook to responses", icon = icon("wand-magic-sparkles"), class = "btn-outline-primary w-100 mt-2"),
              downloadButton(session$ns("download_coding"), "Download coded responses", class = "btn btn-outline-secondary btn-sm w-100 mt-2")
            )),
            div(
              card(class = "sap-card", card_header(icon("book"), " Qualitative codebook"), card_body(DTOutput(session$ns("codebook_table")))),
              card(class = "sap-card mt-3", card_header(icon("tags"), " Applied coding"), card_body(DTOutput(session$ns("coding_table"))))
            )
          )
        )
      )
    })

    redact_pii <- function(x) {
      x <- gsub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}", "[EMAIL]", x, ignore.case = TRUE, perl = TRUE)
      x <- gsub("https?://[^[:space:]]+|www\\.[^[:space:]]+", "[URL]", x, ignore.case = TRUE, perl = TRUE)
      x <- gsub("(?<![[:alnum:]])(?:\\+?[0-9][0-9 .()/-]{7,}[0-9])(?![[:alnum:]])", "[PHONE]", x, perl = TRUE)
      gsub("(?<![0-9])(?:[0-9]{1,3}\\.){3}[0-9]{1,3}(?![0-9])", "[IP]", x, perl = TRUE)
    }

    text_result <- eventReactive(input$analyze, {
      df <- app_state$processed_data; req(is.data.frame(df), nzchar(input$text_var %||% ""))
      raw <- as.character(df[[input$text_var]]); raw[is.na(raw)] <- ""
      display <- if ("pii" %in% input$cleaning) redact_pii(raw) else raw
      txt <- display
      if ("lower" %in% input$cleaning) txt <- tolower(txt)
      if ("punct" %in% input$cleaning) txt <- gsub("[[:punct:]]+", " ", txt)
      if ("numbers" %in% input$cleaning) txt <- gsub("[[:digit:]]+", " ", txt)
      words <- unlist(strsplit(txt, "\\s+"), use.names = FALSE)
      words <- words[nchar(words) >= (input$min_length %||% 3)]
      stop <- c("the","and","for","that","with","this","was","are","from","have","has","had","but","not","you","your","our","they","their","there","what","when","where","which","would","could","should","very","also","into","about")
      if ("stop" %in% input$cleaning) words <- words[!tolower(words) %in% stop]
      counts <- sort(table(words[nzchar(words)]), decreasing = TRUE)
      top <- head(counts, input$top_n %||% 25)
      search <- trimws(input$search %||% "")
      keep <- if (nzchar(search)) grepl(search, raw, ignore.case = TRUE, fixed = TRUE) else nzchar(raw)
      topic_rows <- data.frame()
      nonempty_idx <- which(nzchar(trimws(txt)))
      vocabulary <- names(head(counts, 500L))
      if (length(nonempty_idx) >= 4L && length(vocabulary) >= 3L) {
        docs <- strsplit(txt[nonempty_idx], "\\s+")
        dtm <- vapply(vocabulary, function(term) vapply(docs, function(tokens) sum(tokens == term), numeric(1)), numeric(length(docs)))
        if (is.null(dim(dtm))) dtm <- base::matrix(dtm, ncol = 1L, dimnames = list(NULL, vocabulary[[1]]))
        doc_frequency <- colSums(dtm > 0)
        tfidf <- dtm * base::matrix(log((nrow(dtm) + 1) / (doc_frequency + 1)) + 1, nrow(dtm), ncol(dtm), byrow = TRUE)
        keep_terms <- colSums(tfidf) > 0
        tfidf <- tfidf[, keep_terms, drop = FALSE]
        k <- min(as.integer(input$topic_count %||% 5L), nrow(tfidf) - 1L)
        if (k >= 2L && ncol(tfidf) >= 2L) {
          set.seed(20260815L)
          clusters <- tryCatch(stats::kmeans(tfidf, centers = k, iter.max = 50, nstart = 10), error = function(e) NULL)
          if (!is.null(clusters)) topic_rows <- do.call(rbind, lapply(seq_len(k), function(cluster) {
            idx <- which(clusters$cluster == cluster)
            importance <- if (length(idx)) sort(colMeans(tfidf[idx, , drop = FALSE]), decreasing = TRUE) else numeric()
            data.frame(Theme = paste("Theme", cluster), Responses = length(idx),
                       Top_terms = paste(names(head(importance, 8L)), collapse = ", "), stringsAsFactors = FALSE)
          }))
        }
      }
      list(raw = raw, display = display, top = data.frame(term = names(top), frequency = as.numeric(top), stringsAsFactors = FALSE),
           topics = topic_rows, responses = data.frame(Row = which(keep), Response = display[keep], stringsAsFactors = FALSE))
    }, ignoreInit = TRUE)

    output$text_kpis <- renderUI({
      res <- text_result()
      if (is.null(res)) return(div(class = "alert alert-info", "Choose an open-text variable and run the analysis."))
      nonempty <- nzchar(trimws(res$raw))
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Responses", format(sum(nonempty), big.mark = ","), "comments"),
          sap_kpi("Blank responses", format(sum(!nonempty), big.mark = ","), "comment-slash"),
          sap_kpi("Unique responses", format(length(unique(res$raw[nonempty])), big.mark = ","), "fingerprint"),
          sap_kpi("Displayed terms", nrow(res$top), "font"))
    })

    output$term_plot <- renderPlotly({
      res <- text_result(); req(!is.null(res), nrow(res$top) > 0)
      d <- res$top; d$term <- factor(d$term, levels = rev(d$term))
      p <- ggplot(d, aes(x = frequency, y = term)) + geom_col() + labs(x = "Frequency", y = NULL) + theme_minimal()
      sap_ggplotly(p)
    })

    output$response_table <- renderDT({
      res <- text_result()
      if (is.null(res)) return(datatable(data.frame(Instruction = "Run text analysis to browse responses."), options = list(dom = "t"), rownames = FALSE))
      datatable(res$responses, options = list(pageLength = 10, dom = "ftip", scrollX = TRUE), rownames = FALSE)
    })

    output$topic_table <- renderDT({
      res <- text_result()
      if (is.null(res) || !nrow(res$topics)) return(datatable(data.frame(Instruction = "At least four non-empty responses and several recurring terms are required."), options = list(dom = "t"), rownames = FALSE))
      datatable(res$topics, rownames = FALSE, options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    })

    observeEvent(input$add_code, {
      req(nzchar(input$code_name %||% ""))
      row <- data.frame(Code = input$code_name, Definition = input$code_definition %||% "", Include_when = input$include_terms %||% "",
                        Exclude_when = input$exclude_terms %||% "", Example = input$example %||% "", stringsAsFactors = FALSE)
      cb <- app_state$text_codebook; app_state$text_codebook <- if (is.data.frame(cb)) rbind(cb, row) else row
      sap_frontend_record(app_state, "Text Analysis", paste("Text code added:", input$code_name))
      showNotification("Code added to the qualitative codebook.", type = "message")
    })

    output$codebook_table <- renderDT({
      cb <- app_state$text_codebook
      if (!is.data.frame(cb) || nrow(cb) == 0) return(datatable(data.frame(Instruction = "Define a coding framework for open-ended responses."), options = list(dom = "t"), rownames = FALSE))
      datatable(cb, options = list(pageLength = 12, dom = "tip", scrollX = TRUE), rownames = FALSE)
    })

    coding_result <- eventReactive(input$auto_code, {
      res <- text_result(); req(!is.null(res))
      cb <- app_state$text_codebook; req(is.data.frame(cb), nrow(cb) > 0)
      coded <- list()
      split_terms <- function(x) {
        terms <- trimws(unlist(strsplit(x %||% "", "[,;\\n]", perl = TRUE)))
        terms[nzchar(terms)]
      }
      for (i in seq_len(nrow(cb))) {
        include <- split_terms(cb$Include_when[[i]]); exclude <- split_terms(cb$Exclude_when[[i]])
        if (!length(include)) next
        include_hit <- vapply(res$raw, function(x) any(vapply(include, grepl, logical(1), x = x, ignore.case = TRUE, fixed = TRUE)), logical(1))
        exclude_hit <- if (length(exclude)) vapply(res$raw, function(x) any(vapply(exclude, grepl, logical(1), x = x, ignore.case = TRUE, fixed = TRUE)), logical(1)) else rep(FALSE, length(res$raw))
        hit <- include_hit & !exclude_hit & nzchar(trimws(res$raw))
        if (any(hit)) coded[[length(coded) + 1L]] <- data.frame(
          Row = which(hit), Code = cb$Code[[i]], Response = res$display[hit],
          Match_method = "Dictionary", stringsAsFactors = FALSE
        )
      }
      out <- if (length(coded)) do.call(rbind, coded) else data.frame(Row = integer(), Code = character(), Response = character(), Match_method = character())
      app_state$text_coding_results <- out
      out
    }, ignoreInit = TRUE)

    output$coding_table <- renderDT({
      d <- coding_result()
      if (is.null(d) || !nrow(d)) return(datatable(data.frame(Instruction = "Add include terms to the codebook, analyze text, then apply the codebook."), options = list(dom = "t"), rownames = FALSE))
      datatable(d, rownames = FALSE, filter = "top", options = list(dom = "tip", pageLength = 12, scrollX = TRUE))
    })
    output$download_coding <- downloadHandler(
      filename = function() paste0("coded-responses-", Sys.Date(), ".csv"),
      content = function(file) {
        d <- coding_result(); req(is.data.frame(d), nrow(d) > 0)
        if (exists("sap_export_assert_content", mode = "function")) sap_export_assert_content(services, d, "coded text export")
        utils::write.csv(d, file, row.names = FALSE, na = "")
      }
    )
  })
}

message("[SAP] Text Analysis engine v4.0.0 loaded")
