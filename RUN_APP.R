# Start the application after running INSTALL_DEPENDENCIES.R once.
if (!file.exists("app.R")) {
  stop("Open the extracted Survey Analytics project folder before running this file.", call. = FALSE)
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Run source('INSTALL_DEPENDENCIES.R') before starting the application.", call. = FALSE)
}
shiny::runApp(appDir = getwd(), launch.browser = interactive())
