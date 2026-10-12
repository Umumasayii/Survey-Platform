# =============================================================================
# MODULE 27: GEOSPATIAL SURVEY ANALYSIS
# =============================================================================
# Dataset-driven GIS analysis with accessible selectors, multiple raster
# basemaps, point/bubble/density rendering, descriptive statistics, and
# Dashboard / Report / AI actions.
# =============================================================================

.sap_geo_basemaps <- c(
  "Road / street map" = "road",
  "Grayscale map" = "grayscale",
  "Satellite imagery" = "satellite",
  "Terrain / topographic map" = "terrain",
  "Dark map" = "dark"
)

.sap_geo_tile_url <- function(type = "road") {
  switch(
    type,
    grayscale = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
    satellite = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
    terrain = "https://tile.opentopomap.org/{z}/{x}/{y}.png",
    dark = "https://basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
    "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
  )
}

.sap_geo_numeric_summary <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  valid <- x_num[is.finite(x_num)]
  n_total <- length(x_num); n_valid <- length(valid); n_missing <- n_total - n_valid
  q <- if (n_valid) stats::quantile(valid, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE) else rep(NA_real_, 3L)
  data.frame(
    Statistic = c("Valid values", "Missing values", "Missing percentage", "Distinct values",
                  "Mean", "Median", "Standard deviation", "Variance", "Minimum", "Q1",
                  "Q3", "Interquartile range", "Maximum", "Sum"),
    Value = c(
      n_valid, n_missing, if (n_total) sprintf("%.2f%%", 100 * n_missing / n_total) else "0.00%",
      length(unique(valid)),
      if (n_valid) mean(valid) else NA_real_, if (n_valid) stats::median(valid) else NA_real_,
      if (n_valid > 1L) stats::sd(valid) else NA_real_, if (n_valid > 1L) stats::var(valid) else NA_real_,
      if (n_valid) min(valid) else NA_real_, q[[1]], q[[3]], if (n_valid) q[[3]] - q[[1]] else NA_real_,
      if (n_valid) max(valid) else NA_real_, if (n_valid) sum(valid) else NA_real_
    ), stringsAsFactors = FALSE
  )
}

.sap_geo_categorical_summary <- function(x) {
  raw <- as.character(x)
  missing <- is.na(x)
  blank <- !missing & !nzchar(trimws(raw))
  usable <- raw[!missing & !blank]
  freq <- sort(table(usable, useNA = "no"), decreasing = TRUE)
  top_value <- if (length(freq)) names(freq)[[1]] else NA_character_
  top_count <- if (length(freq)) as.integer(freq[[1]]) else 0L
  total <- length(raw)
  data.frame(
    Statistic = c("Total count", "Missing values", "Blank values", "Distinct values",
                  "Most frequent value", "Most frequent count", "Most frequent percentage"),
    Value = c(total, sum(missing), sum(blank), length(freq), top_value, top_count,
              if (length(usable)) sprintf("%.2f%%", 100 * top_count / length(usable)) else "0.00%"),
    stringsAsFactors = FALSE
  )
}

.sap_geo_frequency <- function(x, limit = 100L) {
  raw <- as.character(x)
  raw[is.na(x)] <- "(Missing)"
  raw[!is.na(x) & !nzchar(trimws(raw))] <- "(Blank)"
  freq <- sort(table(raw, useNA = "no"), decreasing = TRUE)
  if (!length(freq)) return(data.frame())
  out <- data.frame(Value = names(freq), Count = as.integer(freq), stringsAsFactors = FALSE)
  out$Percent <- round(100 * out$Count / sum(out$Count), 2)
  utils::head(out, limit)
}

sap_geospatial_plotly <- function(coords, basemap = "road", style = "points", variable_numeric = FALSE) {
  if (!is.data.frame(coords) || !nrow(coords)) return(NULL)
  tile <- .sap_geo_tile_url(basemap)
  mapbox <- list(
    style = "white-bg",
    layers = list(list(sourcetype = "raster", source = list(tile), below = "traces")),
    center = list(lat = mean(coords$lat, na.rm = TRUE), lon = mean(coords$lon, na.rm = TRUE)),
    zoom = 4
  )

  if (identical(style, "heat")) {
    z <- if (isTRUE(variable_numeric)) coords$numeric_value else rep(1, nrow(coords))
    p <- plotly::plot_ly(coords, type = "densitymapbox", lat = ~lat, lon = ~lon, z = z,
                         radius = 18, text = ~label, hoverinfo = "text")
  } else {
    marker <- list(opacity = .72)
    if (identical(style, "bubble")) {
      vals <- abs(coords$numeric_value)
      vals[!is.finite(vals)] <- 0
      mx <- max(vals, na.rm = TRUE)
      marker$size <- if (is.finite(mx) && mx > 0) 7 + 25 * sqrt(vals / mx) else rep(9, nrow(coords))
      marker$sizemode <- "diameter"
    } else marker$size <- 8
    if (isTRUE(variable_numeric)) {
      marker$color <- coords$numeric_value
      marker$colorscale <- "Viridis"
      marker$showscale <- TRUE
    }
    p <- plotly::plot_ly(coords, type = "scattermapbox", mode = "markers", lat = ~lat, lon = ~lon,
                         text = ~label, hoverinfo = "text", marker = marker)
  }
  p |> plotly::layout(mapbox = mapbox, margin = list(l = 0, r = 0, t = 0, b = 0)) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE)
}

sap_geo_read_vector <- function(path, name = "boundary") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Vector boundary import requires the optional 'sf' package.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(name))
  source_path <- path
  cleanup <- NULL
  if (ext == "zip") {
    cleanup <- tempfile("sap-geo-"); dir.create(cleanup)
    utils::unzip(path, exdir = cleanup)
    shp <- list.files(cleanup, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
    if (!length(shp)) stop("The ZIP file does not contain a shapefile (.shp).", call. = FALSE)
    source_path <- shp[[1]]
  }
  on.exit(if (!is.null(cleanup)) unlink(cleanup, recursive = TRUE, force = TRUE), add = TRUE)
  obj <- suppressWarnings(sf::st_read(source_path, quiet = TRUE, stringsAsFactors = FALSE))
  if (!inherits(obj, "sf") || !nrow(obj)) stop("No vector features were found.", call. = FALSE)
  crs <- sf::st_crs(obj)
  list(
    data = obj,
    summary = data.frame(
      Metric = c("Features", "Geometry type", "CRS", "Attributes"),
      Value = c(nrow(obj), paste(unique(as.character(sf::st_geometry_type(obj))), collapse = ", "),
                if (is.na(crs$epsg)) crs$input %||% "Unknown" else paste0("EPSG:", crs$epsg),
                ncol(sf::st_drop_geometry(obj))), stringsAsFactors = FALSE
    )
  )
}

mod_geospatial_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sap_module_header("GIS / Geospatial Analysis",
      "Create interactive maps from coordinate fields, inspect variable statistics, and send governed outputs to Dashboard, Report, or AI.",
      "map-location-dot"),
    uiOutput(ns("body"))
  )
}

mod_geospatial_analysis_server <- function(id, app_state, services = NULL, pool = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(app_state$processed_data, {
      df <- app_state$processed_data
      vars <- if (is.data.frame(df)) names(df) else character()
      nums <- if (is.data.frame(df)) vars[vapply(df, is.numeric, logical(1))] else character()
      updateSelectInput(session, "region", choices = c("None" = "", vars))
      updateSelectInput(session, "latitude", choices = c("Choose latitude" = "", nums))
      updateSelectInput(session, "longitude", choices = c("Choose longitude" = "", nums))
      updateSelectInput(session, "mapped_variable", choices = c("Respondent count" = "", vars))
      updateSelectInput(session, "weight", choices = c("None" = "", nums))
    }, ignoreInit = FALSE)

    output$body <- renderUI({
      if (!isTRUE(app_state$data_imported)) return(sap_no_data())
      layout_columns(col_widths = c(4, 8),
        card(class = "sap-card", card_header(icon("sliders"), " Map specification"), card_body(
          selectInput(session$ns("latitude"), "Latitude", choices = c("Choose latitude" = "", sap_numeric_names(app_state))),
          selectInput(session$ns("longitude"), "Longitude", choices = c("Choose longitude" = "", sap_numeric_names(app_state))),
          selectInput(session$ns("mapped_variable"), "Variable to visualize", choices = c("Respondent count" = "", sap_safe_names(app_state))),
          selectInput(session$ns("map_style"), "Map visualization", choices = c("Points" = "points", "Proportional / bubble symbols" = "bubble", "Density / heat" = "heat")),
          selectInput(session$ns("basemap"), "Map background", choices = .sap_geo_basemaps),
          selectInput(session$ns("region"), "Region / administrative area", choices = c("None" = "", sap_safe_names(app_state))),
          selectInput(session$ns("weight"), "Weight", choices = c("None" = "", sap_numeric_names(app_state))),
          numericInput(session$ns("min_base"), "Minimum publishable base", 30, min = 1),
          checkboxInput(session$ns("jitter"), "Jitter point locations for privacy", TRUE),
          fileInput(session$ns("boundary_file"), "Optional vector boundary",
                    accept = c(".geojson", ".json", ".gpkg", ".kml", ".zip")),
          helpText("GeoJSON, GeoPackage, KML or zipped Shapefile. Requires optional sf package."),
          uiOutput(session$ns("boundary_status")),
          div(class = "d-grid gap-2",
              actionButton(session$ns("preview"), "Preview geography", icon = icon("map"), class = "btn-primary"),
              actionButton(session$ns("save"), "Save map specification", icon = icon("floppy-disk"), class = "btn-outline-primary")),
          hr(),
          div(class = "d-flex gap-2 justify-content-end",
              actionButton(session$ns("send_dashboard"), NULL, icon = icon("gauge-high"), class = "btn btn-sm btn-outline-primary", title = "Send to Dashboard", `aria-label` = "Send to Dashboard"),
              actionButton(session$ns("send_report"), NULL, icon = icon("file-arrow-up"), class = "btn btn-sm btn-success", title = "Send to Report", `aria-label` = "Send to Report"),
              ai_assistant_ui(session$ns("ai_geo"), "Interpret with AI"))
        )),
        div(
          uiOutput(session$ns("geo_kpis")),
          card(class = "sap-card", card_header(icon("map"), " Interactive map"), card_body(
            plotlyOutput(session$ns("map_preview"), height = "430px"),
            tags$small(class = "text-muted", "Basemap attribution: © OpenStreetMap contributors / Carto / Esri / OpenTopoMap as applicable."))),
          card(class = "sap-card mt-3", card_header(icon("draw-polygon"), " Vector boundary inspection"), card_body(DTOutput(session$ns("boundary_summary")))) ,
          layout_columns(col_widths = c(6, 6),
            card(class = "sap-card mt-3", card_header(icon("chart-simple"), " Variable statistics"), card_body(DTOutput(session$ns("variable_stats")))),
            card(class = "sap-card mt-3", card_header(icon("list-ol"), " Value frequency"), card_body(DTOutput(session$ns("value_frequency"))))
          ),
          card(class = "sap-card mt-3", card_header(icon("table-list"), " Geographic coverage"), card_body(DTOutput(session$ns("coverage_table"))))
        )
      )
    })

    boundary_result <- reactive({
      info <- input$boundary_file
      if (is.null(info) || !nrow(info)) return(NULL)
      tryCatch(sap_geo_read_vector(info$datapath[[1]], info$name[[1]]), error = function(e) list(error = conditionMessage(e)))
    })

    output$boundary_status <- renderUI({
      b <- boundary_result()
      if (is.null(b)) return(NULL)
      if (!is.null(b$error)) return(div(class = "alert alert-warning py-2", icon("triangle-exclamation"), " ", b$error))
      div(class = "alert alert-success py-2", icon("circle-check"), " Vector boundary loaded for inspection.")
    })
    output$boundary_summary <- renderDT({
      b <- boundary_result()
      if (is.null(b)) return(datatable(data.frame(Instruction = "Upload an optional boundary to inspect its CRS and geometry."), options = list(dom = "t"), rownames = FALSE))
      if (!is.null(b$error)) return(datatable(data.frame(Status = b$error), options = list(dom = "t"), rownames = FALSE))
      datatable(b$summary, options = list(dom = "t"), rownames = FALSE)
    })

    geo_result <- eventReactive(input$preview, {
      df <- app_state$processed_data; req(is.data.frame(df))
      req(nzchar(input$latitude %||% ""), nzchar(input$longitude %||% ""))
      req(input$latitude %in% names(df), input$longitude %in% names(df))
      w <- rep(1, nrow(df))
      if (nzchar(input$weight %||% "") && input$weight %in% names(df)) {
        w <- suppressWarnings(as.numeric(df[[input$weight]])); w[!is.finite(w) | w <= 0] <- NA_real_
      }
      selected <- input$mapped_variable %||% ""
      variable <- if (nzchar(selected) && selected %in% names(df)) df[[selected]] else rep(1, nrow(df))
      variable_numeric <- nzchar(selected) && is.numeric(df[[selected]])
      region_values <- if (nzchar(input$region %||% "") && input$region %in% names(df)) as.character(df[[input$region]]) else rep("All respondents", nrow(df))
      region_values[is.na(region_values) | !nzchar(region_values)] <- "Missing"
      policy <- app_state$governance_policy %||% list()
      min_base <- max(as.numeric(input$min_base %||% 30), as.numeric(policy$min_base %||% 0), na.rm = TRUE)

      coords <- data.frame(
        row_id = seq_len(nrow(df)),
        lat = suppressWarnings(as.numeric(df[[input$latitude]])),
        lon = suppressWarnings(as.numeric(df[[input$longitude]])),
        region = region_values,
        value = as.character(variable),
        numeric_value = if (variable_numeric) suppressWarnings(as.numeric(variable)) else 1,
        stringsAsFactors = FALSE
      )
      coords <- coords[is.finite(coords$lat) & is.finite(coords$lon) & abs(coords$lat) <= 90 & abs(coords$lon) <= 180, , drop = FALSE]
      approval_required <- isTRUE(policy$require_approval) && !isTRUE(app_state$governance_export_approved)
      jitter_applied <- isTRUE(input$jitter) || approval_required
      if (jitter_applied && nrow(coords)) { set.seed(20260815L); coords$lat <- jitter(coords$lat, amount = .002); coords$lon <- jitter(coords$lon, amount = .002) }
      variable_label <- if (nzchar(selected)) selected else "Respondent"
      coords$label <- paste0("Region: ", coords$region, "<br>", variable_label, ": ", coords$value)
      coords <- utils::head(coords, 15000L)
      attr(coords, "jitter_applied") <- jitter_applied

      coverage <- NULL
      if (nzchar(input$region %||% "")) {
        valid <- !is.na(w)
        pieces <- split(seq_len(nrow(df))[valid], region_values[valid])
        coverage <- do.call(rbind, lapply(names(pieces), function(region_name) {
          idx <- pieces[[region_name]]
          estimate <- if (variable_numeric) stats::weighted.mean(suppressWarnings(as.numeric(variable[idx])), w[idx], na.rm = TRUE) else sum(w[idx], na.rm = TRUE)
          data.frame(Region = region_name, Unweighted_n = length(idx), Weighted_n = sum(w[idx], na.rm = TRUE),
                     Estimate = estimate, Publishable = length(idx) >= min_base, stringsAsFactors = FALSE)
        }))
        coverage$Estimate[!coverage$Publishable] <- NA_real_
        coverage$Disclosure_status <- ifelse(coverage$Publishable, "Publishable", "Suppressed — low base")
        coverage <- coverage[order(coverage$Unweighted_n, decreasing = TRUE), , drop = FALSE]
      }
      list(coords = coords, coverage = coverage, minimum_base = min_base,
           variable = variable, variable_name = selected, variable_numeric = variable_numeric,
           basemap = input$basemap, map_style = input$map_style)
    }, ignoreInit = TRUE)

    output$geo_kpis <- renderUI({
      res <- geo_result()
      if (is.null(res)) return(div(class = "alert alert-info", "Choose latitude, longitude, and a variable, then preview the geography."))
      regions <- if (is.data.frame(res$coverage)) nrow(res$coverage) else 0
      suppressed <- if (is.data.frame(res$coverage)) sum(!res$coverage$Publishable) else 0
      div(class = "sap-kpi-grid mb-3",
          sap_kpi("Valid coordinates", if (is.data.frame(res$coords)) format(nrow(res$coords), big.mark = ",") else "0", "location-dot"),
          sap_kpi("Regions", regions, "map"),
          sap_kpi("Suppressed regions", suppressed, "eye-slash"),
          sap_kpi("Variable type", if (isTRUE(res$variable_numeric)) "Numeric" else "Categorical", "chart-simple"))
    })

    output$map_preview <- renderPlotly({
      res <- geo_result()
      if (is.null(res) || !is.data.frame(res$coords) || !nrow(res$coords)) return(NULL)
      sap_geospatial_plotly(res$coords, res$basemap, res$map_style, res$variable_numeric)
    })

    output$variable_stats <- renderDT({
      res <- geo_result()
      if (is.null(res)) return(datatable(data.frame(Instruction = "Preview the map to calculate variable statistics."), options = list(dom = "t"), rownames = FALSE))
      d <- if (isTRUE(res$variable_numeric)) .sap_geo_numeric_summary(res$variable) else .sap_geo_categorical_summary(res$variable)
      datatable(d, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$value_frequency <- renderDT({
      res <- geo_result()
      if (is.null(res)) return(datatable(data.frame(Instruction = "Preview the map to inspect value frequencies."), options = list(dom = "t"), rownames = FALSE))
      if (isTRUE(res$variable_numeric)) return(datatable(data.frame(Instruction = "Frequency table is shown for categorical/text variables."), options = list(dom = "t"), rownames = FALSE))
      d <- .sap_geo_frequency(res$variable)
      if (!nrow(d)) return(datatable(data.frame(Instruction = "No values available."), options = list(dom = "t"), rownames = FALSE))
      datatable(d, rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
    })

    output$coverage_table <- renderDT({
      res <- geo_result()
      if (is.null(res) || !is.data.frame(res$coverage)) return(datatable(data.frame(Instruction = "Select a region variable to review coverage and suppression."), options = list(dom = "t"), rownames = FALSE))
      datatable(res$coverage, options = list(pageLength = 15, dom = "tip"), rownames = FALSE)
    })

    observeEvent(input$save, {
      app_state$geospatial_spec <- list(basemap = input$basemap, map_style = input$map_style, region = input$region,
        latitude = input$latitude, longitude = input$longitude, mapped_variable = input$mapped_variable, weight = input$weight,
        min_base = input$min_base, jitter = input$jitter, updated_at = Sys.time())
      sap_frontend_record(app_state, "Geospatial", "Geospatial analysis specification saved", app_state$geospatial_spec)
      if (!is.null(pool) && !is.null(app_state$current_project) && exists("db_upsert_studio_document", mode = "function")) {
        try(db_upsert_studio_document(pool, app_state$current_project, "analysis_spec", "GIS / Geospatial Analysis", app_state$geospatial_spec, app_state$current_user), silent = TRUE)
      }
      showNotification("Geospatial specification saved.", type = "message")
    })

    observeEvent(input$send_dashboard, {
      res <- geo_result(); req(!is.null(res), is.data.frame(res$coords), nrow(res$coords) > 0)
      if (exists("sap_bi_ensure_state", mode = "function")) sap_bi_ensure_state(app_state)
      if (exists("sap_bi_has_role", mode = "function") && !isTRUE(sap_bi_has_role(app_state, "editor"))) {
        return(showNotification("The editor role is required to send GIS outputs to the Dashboard.", type = "warning"))
      }
      if (is.null(app_state$bi) || !is.list(app_state$bi$dashboard)) return(showNotification("Dashboard Studio is not available.", type = "error"))
      model <- app_state$bi
      id <- if (exists("sap_bi_id", mode = "function")) sap_bi_id("visual") else paste0("gis_", as.integer(Sys.time()))
      spec <- list(id = id, title = paste("GIS —", input$mapped_variable %||% "Respondents"), chart = "map",
                   table = "__processed_data__", latitude = input$latitude, longitude = input$longitude,
                   mapped_variable = input$mapped_variable %||% "", basemap = input$basemap, map_style = input$map_style,
                   jitter = isTRUE(input$jitter), note = "", created_at = as.character(Sys.time()), updated_at = as.character(Sys.time()))
      model$dashboard$visuals[[id]] <- spec
      page_index <- match(model$dashboard$active_page, vapply(model$dashboard$pages, function(x) x$id, character(1)))
      if (is.na(page_index)) page_index <- 1L
      model$dashboard$pages[[page_index]]$visual_ids <- unique(c(model$dashboard$pages[[page_index]]$visual_ids, id))
      model$dashboard$updated_at <- as.character(Sys.time())
      app_state$bi <- model
      if (!is.null(pool) && !is.null(app_state$current_project) && exists("db_upsert_studio_document", mode = "function")) {
        try(db_upsert_studio_document(pool, app_state$current_project, "dashboard", "Dashboard Studio", app_state$bi$dashboard, app_state$current_user), silent = TRUE)
      }
      showNotification("GIS map sent to Dashboard Studio.", type = "message")
    })

    if (exists("register_report_button", mode = "function")) register_report_button(
      input, services, "send_report", kind = "table", title = function() paste("GIS summary —", input$mapped_variable %||% "Respondents"),
      payload = function() { res <- geo_result(); if (is.null(res)) return(NULL); if (isTRUE(res$variable_numeric)) .sap_geo_numeric_summary(res$variable) else .sap_geo_categorical_summary(res$variable) }
    )

    if (exists("mod_ai_assistant_server", mode = "function")) mod_ai_assistant_server(
      "ai_geo", services,
      payload = function() {
        res <- geo_result(); if (is.null(res)) return(NULL)
        list(title = paste("GIS analysis", res$variable_name %||% "Respondents"),
             summary = if (isTRUE(res$variable_numeric)) .sap_geo_numeric_summary(res$variable) else .sap_geo_categorical_summary(res$variable),
             coverage = res$coverage, map_style = res$map_style, basemap = res$basemap)
      }
    )
  })
}

message("[SAP] Geospatial Analysis engine v5.4.0 loaded")
