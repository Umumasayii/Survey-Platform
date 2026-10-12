# =============================================================================
# NEXUS STUDIO 4.0 — OLLAMA PREFLIGHT
# =============================================================================
# Run from the project directory:
#   source("preflight_ollama.R")
#
# Supported environment variables:
#   OLLAMA_BASE_URL   Local: http://localhost:11434
#                     Remote/cloud: reachable HTTPS endpoint
#   OLLAMA_MODEL      Exact model tag to test (optional)
#   OLLAMA_API_KEY    Bearer token for Ollama Cloud/protected endpoint (optional)
# =============================================================================

if (file.exists("sap_environment.R")) {
  source("sap_environment.R", local = FALSE)
  sap_load_project_env(getwd(), quiet = TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

required <- c("httr", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required package(s): ", paste(missing, collapse = ", "), call. = FALSE)
}

hosted <- nzchar(Sys.getenv("POSIT_PRODUCT", "")) ||
  nzchar(Sys.getenv("CONNECT_SERVER", "")) ||
  tolower(Sys.getenv("R_CONFIG_ACTIVE", "")) %in% c("shinyapps", "connect_cloud")
base <- trimws(Sys.getenv(
  "OLLAMA_BASE_URL",
  if (hosted) "" else "http://localhost:11434"
))
base <- sub("/+$", "", base)
base <- sub("/api$", "", base, ignore.case = TRUE)
api_key <- trimws(Sys.getenv("OLLAMA_API_KEY", ""))

if (!nzchar(base)) {
  stop(
    "No Ollama endpoint is configured. Set OLLAMA_BASE_URL. ",
    "For a local session use http://localhost:11434; for a published app use a reachable HTTPS endpoint.",
    call. = FALSE
  )
}

loopback <- grepl("^https?://(localhost|127\\.0\\.0\\.1)(:|/|$)", base, ignore.case = TRUE)
if (hosted && loopback) {
  stop(
    "This is a published runtime. localhost refers to the hosting server, not your computer. ",
    "Set OLLAMA_BASE_URL to a reachable remote/private endpoint or https://ollama.com/api.",
    call. = FALSE
  )
}

headers <- if (nzchar(api_key)) {
  httr::add_headers(Authorization = paste("Bearer", api_key))
} else NULL

read_json <- function(resp) {
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                     simplifyVector = FALSE)
}

request <- function(method, url, ...) {
  args <- list(url = url)
  if (!is.null(headers)) args <- append(args, list(headers))
  args <- c(args, list(...))
  tryCatch(do.call(method, args), error = function(e) e)
}

get_json <- function(path, required = TRUE) {
  resp <- request(httr::GET, paste0(base, path), httr::timeout(15))
  if (inherits(resp, "error")) {
    if (!required) return(NULL)
    stop("Could not reach Ollama at ", base, ": ", conditionMessage(resp), call. = FALSE)
  }
  if (httr::status_code(resp) != 200L) {
    if (!required) return(NULL)
    details <- tryCatch(read_json(resp)$error, error = function(e) NULL)
    stop("Ollama returned HTTP ", httr::status_code(resp), " for ", path,
         if (!is.null(details)) paste0(": ", details) else "", call. = FALSE)
  }
  read_json(resp)
}

models_raw <- get_json("/api/tags")$models %||% list()
models <- if (length(models_raw)) {
  unique(Filter(nzchar, vapply(models_raw, function(x) {
    as.character(x$model %||% x$name %||% "")
  }, character(1))))
} else character(0)

version_obj <- get_json("/api/version", required = FALSE)
version <- version_obj$version %||%
  if (grepl("ollama\\.com", base, ignore.case = TRUE)) "cloud" else "remote"

cat("✓ Ollama endpoint reachable:", base, "\n")
cat("✓ Endpoint version/mode:", version, "\n")

if (!length(models)) {
  stop("The Ollama endpoint is reachable, but no models are available.", call. = FALSE)
}
cat("✓ Available models:", paste(models, collapse = ", "), "\n")

model <- trimws(Sys.getenv("OLLAMA_MODEL", ""))
if (!nzchar(model)) model <- models[[1]]
if (!model %in% models) {
  stop("Requested model '", model, "' is unavailable. Available: ",
       paste(models, collapse = ", "), call. = FALSE)
}

body <- list(
  model = model,
  messages = list(
    list(role = "system", content = "Return exactly the requested text."),
    list(role = "user", content = "Reply with exactly: OLLAMA READY")
  ),
  stream = FALSE,
  options = list(temperature = 0, num_predict = 16)
)

post_headers <- c("content-type" = "application/json")
if (nzchar(api_key)) post_headers <- c(post_headers, Authorization = paste("Bearer", api_key))
resp <- tryCatch(
  httr::POST(
    paste0(base, "/api/chat"),
    httr::add_headers(.headers = post_headers),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(180)
  ),
  error = function(e) e
)
if (inherits(resp, "error")) {
  stop("Ollama generation request failed: ", conditionMessage(resp), call. = FALSE)
}
if (httr::status_code(resp) != 200L) {
  details <- tryCatch(read_json(resp)$error, error = function(e) NULL)
  stop("Ollama generation returned HTTP ", httr::status_code(resp),
       if (!is.null(details)) paste0(": ", details) else "", call. = FALSE)
}
parsed <- read_json(resp)
text <- trimws(parsed$message$content %||% "")
if (!nzchar(text)) stop("Ollama returned an empty test response.", call. = FALSE)

cat("✓ Generation test succeeded with model:", parsed$model %||% model, "\n")
cat("  Response:", text, "\n")
cat("\nOllama is ready for Survey Analytics Platform.\n")
