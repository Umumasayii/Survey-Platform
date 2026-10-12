# =============================================================================
# MODULE 15: AI ASSISTANT (interpretation UI component)
# =============================================================================
# A reusable dropdown + modal that any analytical module drops next to an
# output. The module supplies a PAYLOAD FUNCTION returning the structured
# result (analysis_type, variables, results, assumptions, n, metadata) — never
# a raw dataset. The dropdown maps to AIService$run_task(); the modal shows a
# loading state, the interpretation, and actions: copy, insert into the report,
# retry.
#
# Usage in a module UI:
#   ai_assistant_ui(ns("ai_ttest"))
#
# Usage in a module server:
#   ai_assistant_server("ai_ttest", services,
#     payload = function() list(
#       analysis_type = "Independent t-test",
#       variables = c(input$t_var, input$t_group),
#       results   = stat_result_df(t_test_result()),
#       n = nrow(df_data())))
# =============================================================================

AI_TASKS <- list(
  list(id = "interpret",       label = "Interpret results"),
  list(id = "beginner",        label = "Explain for beginners"),
  list(id = "publication",     label = "Write for publication"),
  list(id = "executive",       label = "Generate executive summary"),
  list(id = "next_analysis",   label = "Suggest next analysis"),
  list(id = "recommend_viz",   label = "Recommend better charts"),
  list(id = "assumptions",     label = "Explain assumptions"),
  list(id = "issues",          label = "Identify potential issues"),
  list(id = "conclusions",     label = "Draft conclusions"),
  list(id = "recommendations", label = "Draft recommendations")
)

#' The AI Assistant dropdown button.
ai_assistant_ui <- function(id, label = "AI Assistant") {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "btn-group dropup",
    shiny::tags$button(
      class = "btn btn-sm btn-outline-primary dropdown-toggle sap-ai-btn",
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      type = "button", title = label, `aria-label` = label,
      shiny::icon("wand-magic-sparkles")
    ),
    shiny::tags$ul(
      class = "dropdown-menu",
      lapply(AI_TASKS, function(t) {
        shiny::tags$li(
          shiny::actionLink(ns(paste0("task_", t$id)), t$label,
                            class = "dropdown-item")
        )
      })
    )
  )
}

mod_ai_assistant_server <- function(id, services, payload,
                                    default_style = "Research Report") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ai <- if (!is.null(services)) services$ai else NULL

    state <- shiny::reactiveValues(busy = FALSE, result = NULL, task = NULL,
                                   style = default_style)

    resolve_payload <- function() {
      payload_error <- NULL
      p <- tryCatch(
        if (is.function(payload)) payload() else payload,
        error = function(e) {
          payload_error <<- conditionMessage(e)
          NULL
        }
      )
      list(
        value = if (!is.null(p) && is.list(p)) p else NULL,
        error = payload_error
      )
    }

    run_task <- function(task) {
      if (is.null(ai)) {
        if (!is.null(services$notify)) services$notify$error("AI service unavailable.")
        return(invisible(NULL))
      }
      resolved <- resolve_payload()
      p <- resolved$value
      if (is.null(p)) {
        if (!is.null(services$notify)) {
          if (!is.null(resolved$error) && nzchar(resolved$error)) {
            services$notify$error(paste0(
              "The analysis is available, but its AI payload could not be prepared: ",
              resolved$error
            ))
          } else {
            services$notify$warning(
              "No completed result is available for this AI action. Run or refresh the analysis first."
            )
          }
        }
        return(invisible(NULL))
      }
      state$task  <- task
      state$busy  <- TRUE
      state$result <- NULL
      # Snapshot the style NOW, while we're still inside the observer that
      # called run_task. The onFlushed callback below runs outside any reactive
      # consumer, so reading state$style there would error ("can't access
      # reactive value outside a reactive consumer").
      style_now <- state$style
      show_modal()

      # Defer the (potentially slow) call so the modal + spinner paint first.
      # onFlushed() runs outside Shiny's normal reactive consumer. AIService may
      # legitimately read reactive-backed services (for example PrivacyService),
      # so execute the call inside isolate() to provide a safe non-tracking
      # reactive context. Without this, privacy_register can raise:
      # "Can't access reactive value ... outside of reactive consumer."
      session$onFlushed(function() {
        res <- shiny::isolate(tryCatch(
          ai$run_task(task, p, style_now),
          error = function(e) list(ok = FALSE, error = conditionMessage(e))
        ))
        state$busy <- FALSE
        state$result <- res
      }, once = TRUE)
    }

    # one observer per task id
    lapply(AI_TASKS, function(t) {
      shiny::observeEvent(input[[paste0("task_", t$id)]], {
        run_task(t$id)
      }, ignoreInit = TRUE)
    })

    show_modal <- function() {
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("wand-magic-sparkles"), " AI interpretation"),
        size = "l", easyClose = TRUE,
        shiny::div(
          shiny::selectInput(ns("style"), "Interpretation style",
            choices = c("Beginner Explanation", "Research Report",
                        "Academic Publication", "Executive Summary",
                        "Technical Interpretation", "Plain Language Summary"),
            selected = state$style, width = "100%"),
          shiny::uiOutput(ns("out"))
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("retry"), "Regenerate",
                              icon = shiny::icon("rotate")),
          shiny::actionButton(ns("insert"), NULL,
                              icon = shiny::icon("file-arrow-up"),
                              class = "btn-outline-secondary", title = "Send to Report", `aria-label` = "Send to Report"),
          shiny::modalButton("Close")
        )
      ))
    }

    output$out <- shiny::renderUI({
      if (isTRUE(state$busy)) {
        return(shiny::div(class = "text-center text-muted py-4",
          shiny::tags$div(class = "spinner-border text-primary", role = "status"),
          shiny::tags$p(class = "mt-2", "Generating interpretation…")))
      }
      res <- state$result
      if (is.null(res)) return(NULL)
      if (!isTRUE(res$ok)) {
        return(shiny::div(class = "alert alert-danger",
          shiny::icon("triangle-exclamation"), " ",
          res$error %||% "The AI request failed."))
      }
      token_bits <- character(0)
      if (!is.na(res$prompt_tokens %||% NA_integer_)) {
        token_bits <- c(token_bits, sprintf("%s input tokens", res$prompt_tokens))
      }
      if (!is.na(res$completion_tokens %||% NA_integer_)) {
        token_bits <- c(token_bits, sprintf("%s output tokens", res$completion_tokens))
      }
      meta_bits <- c(
        res$provider %||% "?",
        res$model %||% "?",
        if (!is.null(res$seconds)) sprintf("%.1fs", res$seconds) else NULL,
        token_bits
      )

      shiny::tagList(
        shiny::div(class = "alert alert-warning py-2 small",
          shiny::icon("triangle-exclamation"), " ",
          "AI-generated draft. Verify all claims against the displayed statistics before using it."),
        shiny::tags$div(
          class = "sap-ai-output",
          style = "white-space:pre-wrap;background:var(--bs-tertiary-bg,#f8f9fa);border:1px solid var(--bs-border-color,#dee2e6);border-radius:8px;padding:16px;max-height:52vh;overflow:auto;",
          res$text
        ),
        shiny::tags$div(class = "small text-muted mt-2",
          paste(Filter(nzchar, meta_bits), collapse = " · ")),
        shiny::checkboxInput(ns("reviewed"),
          "I reviewed this draft against the computed results.", value = FALSE),
        shiny::tags$div(class = "mt-2",
          shiny::tags$textarea(id = ns("copybox"), class = "form-control",
            rows = 1, style = "position:absolute;left:-9999px;", res$text),
          shiny::tags$button(class = "btn btn-sm btn-outline-secondary",
            onclick = sprintf(
              "navigator.clipboard.writeText(document.getElementById('%s').value);this.textContent='Copied';",
              ns("copybox")),
            shiny::icon("copy"), " Copy"))
      )
    })

    # Style change re-runs the current task with the new style.
    shiny::observeEvent(input$style, {
      state$style <- input$style
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$retry, {
      if (!is.null(state$task)) run_task(state$task)
    })

    # Insert the interpretation into the report as a text block.
    shiny::observeEvent(input$insert, {
      res <- state$result
      if (is.null(res) || !isTRUE(res$ok) || is.null(services$report)) {
        if (!is.null(services$notify)) services$notify$warning("Nothing to insert yet.")
        return(invisible(NULL))
      }
      if (!isTRUE(input$reviewed)) {
        if (!is.null(services$notify)) {
          services$notify$warning(
            "Review the AI draft and confirm the checkbox before inserting it."
          )
        }
        return(invisible(NULL))
      }
      resolved <- resolve_payload()
      p <- resolved$value
      if (is.null(p)) {
        if (!is.null(services$notify)) {
          services$notify$error(
            resolved$error %||% "The analysis payload is no longer available."
          )
        }
        return(invisible(NULL))
      }
      title <- sprintf("AI interpretation — %s",
                       (p$analysis_type %||% "analysis"))
      blk <- services$report$add_block(kind = "text", title = title,
                                       payload = res$text,
                                       code = "")
      if (!is.null(services$notify)) {
        if (is.null(blk)) services$notify$warning("Could not add to report.")
        else services$notify$success("AI interpretation added to report.")
      }
    })
  })
}

message("[SAP] AI Assistant v1.1.1-20260731 loaded")  # SAP_MODULE_BANNER

# =============================================================================
# END OF MODULE 15
# =============================================================================
