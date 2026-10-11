library(shiny)
library(bslib)
library(readxl)
library(DT)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(scales)
library(temperature)

# =========================================================
# THEME
# =========================================================

app_theme <- bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = "#2c3e50",
  secondary    = "#7f8c8d",
  success      = "#3d7a5e",
  warning      = "#a9762f",
  danger       = "#a13d3d",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter", wght = 600),
  "border-radius"     = "0.35rem",
  "card-border-color" = "#e3e6e8",
  "navbar-bg"         = "#2c3e50"
) |>
  bs_add_rules(
    "
    body { color: #2c3e50; }
    .card { box-shadow: none; border: 1px solid #e3e6e8; }
    .card-header { background-color: #f8f9fa; font-weight: 600; font-size: .95rem; border-bottom: 1px solid #e3e6e8; }
    .bslib-value-box .value-box-value { font-weight: 600; font-size: 1.4rem; }
    .bslib-value-box .value-box-title { font-size: .8rem; text-transform: uppercase; letter-spacing: .03em; opacity: .75; }
    .step-row { display:flex; align-items:center; gap:.6rem; padding:.3rem 0; font-size: .92rem; }
    .step-icon { width: 1.2rem; text-align:center; }
    .status-pill { border-radius: 0.3rem; padding: .3rem .7rem; font-size:.85rem; display:inline-flex; align-items:center; gap:.4rem; font-weight: 500; }
    .app-hint { font-size: .8rem; color: #7f8c8d; }
    .btn { font-weight: 500; }
    "
  )

# =========================================================
# UI HELPERS
# =========================================================

# Small coloured status pill used across tabs
status_pill <- function(text, status = c("idle", "warn", "ok", "error")) {
  status <- match.arg(status)
  cls <- switch(status,
                idle  = "bg-light text-dark border",
                warn  = "bg-warning-subtle text-dark border",
                ok    = "bg-success-subtle text-dark border",
                error = "bg-danger-subtle text-dark border"
  )
  ic <- switch(status,
               idle  = "circle",
               warn  = "clock",
               ok    = "check",
               error = "triangle-exclamation"
  )
  span(class = paste("status-pill", cls), icon(ic, class = "small"), text)
}

# One row in a step checklist (e.g. Model tab pipeline)
step_row <- function(label, done) {
  div(
    class = "step-row",
    span(
      class = "step-icon",
      if (done) icon("check", class = "text-success") else icon("circle", class = "text-muted")
    ),
    span(if (done) label else tags$span(label, class = "text-muted"))
  )
}

# =========================================================
# UI
# =========================================================

ui <- page_navbar(
  title = "Climate Risk",
  theme = app_theme,
  fillable = TRUE,
  header = useShinyjs(),
  
  # -------------------------------------------------------
  # DATA
  # -------------------------------------------------------
  nav_panel(
    "Data",
    icon = icon("table"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        h5(icon("upload"), " Upload Data"),
        
        fileInput(
          "file",
          "Temperature dataset",
          accept = c(".xlsx", ".xls", ".csv"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        
        helpText("Accepted formats: Excel (.xlsx, .xls) and CSV (.csv). Expects a numeric temperature column."),
        
        hr(),
        
        disabled(
          actionButton(
            "fit_model",
            "Fit Temperature Model",
            class = "btn-primary w-100",
            icon = icon("chart-line")
          )
        ),
        
        div(class = "app-hint mt-2", "Enabled once a dataset has been uploaded."),
        
        hr(),
        
        uiOutput("data_step_summary")
      ),
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        row_heights = "110px",
        value_box(
          title = "Rows",
          value = textOutput("vb_rows"),
          showcase = icon("table-list"),
          theme = "light",
          class = "border"
        ),
        value_box(
          title = "Columns",
          value = textOutput("vb_cols"),
          showcase = icon("columns"),
          theme = "light",
          class = "border"
        ),
        value_box(
          title = "Mean Temp",
          value = textOutput("vb_mean"),
          showcase = icon("temperature-half"),
          theme = "light",
          class = "border"
        ),
        value_box(
          title = "Range",
          value = textOutput("vb_range"),
          showcase = icon("arrows-left-right"),
          theme = "light",
          class = "border"
        )
      ),
      
      card(
        card_header(icon("magnifying-glass"), " Historical Temperature Data"),
        card_body(
          uiOutput("data_info"),
          withSpinner(
            DTOutput("data_preview"),
            color = "#2c3e50", type = 4, size = 0.6
          )
        )
      )
    )
  ),
  
  # -------------------------------------------------------
  # MODEL
  # -------------------------------------------------------
  nav_panel(
    "Model",
    icon = icon("wave-square"),
    
    layout_columns(
      col_widths = c(8, 4),
      
      card(
        card_header(icon("chart-line"), " Temperature Model"),
        card_body(
          withSpinner(
            plotOutput("model_plot", height = "500px"),
            color = "#2c3e50", type = 4, size = 0.6
          )
        ),
        card_footer(class = "app-hint", "Historical temperature series used to fit the anomaly and volatility models.")
      ),
      
      card(
        card_header(icon("list-check"), " Model Status"),
        card_body(
          uiOutput("model_status"),
          hr(),
          h6("Pipeline"),
          uiOutput("model_pipeline")
        )
      )
    )
  ),
  
  # -------------------------------------------------------
  # SIMULATION
  # -------------------------------------------------------
  nav_panel(
    "Simulation",
    icon = icon("dice"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        h5(icon("sliders"), " Simulation Parameters"),
        
        sliderInput(
          "nb_years",
          "Simulation horizon (years)",
          value = 1, min = 1, max = 100, step = 1
        ),
        
        sliderInput(
          "nbr_scenarios",
          "Number of scenarios",
          value = 100, min = 10, max = 10000, step = 10
        ),
        
        disabled(
          actionButton(
            "simulate",
            "Run Simulation",
            class = "btn-success w-100",
            icon = icon("play")
          )
        ),
        
        div(class = "app-hint mt-2", "Enabled once the temperature model has been fitted."),
        
        hr(),
        
        uiOutput("simulate_download_ui")
      ),
      
      card(
        card_header(icon("chart-area"), " Simulated Temperature Scenarios"),
        card_body(
          withSpinner(
            plotOutput("scenario_plot", height = "600px"),
            color = "#2c3e50", type = 4, size = 0.6
          )
        ),
        card_footer(class = "app-hint", textOutput("scenario_footer", inline = TRUE))
      )
    )
  ),
  
  # -------------------------------------------------------
  # CLIMATE VAR
  # -------------------------------------------------------
  nav_panel(
    "Climate VaR",
    icon = icon("triangle-exclamation"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        h5(icon("calculator"), " Climate VaR Parameters"),
        
        sliderInput(
          "alpha",
          "VaR level (\u03b1)",
          value = 0.05, min = 0.001, max = 0.5, step = 0.001,
          post = ""
        ),
        
        div(class = "app-hint mb-2", textOutput("alpha_label", inline = TRUE)),
        
        disabled(
          actionButton(
            "calculate_var",
            "Compute Climate VaR",
            class = "btn-warning w-100",
            icon = icon("calculator")
          )
        ),
        
        div(class = "app-hint mt-2", "Enabled once a simulation has been run."),
        
        hr(),
        
        uiOutput("var_status"),
        
        hr(),
        
        uiOutput("var_download_ui")
      ),
      
      layout_columns(
        col_widths = c(8, 4),
        
        card(
          card_header(icon("chart-column"), " Temperature Distribution"),
          card_body(
            withSpinner(
              plotOutput("var_plot", height = "500px"),
              color = "#2c3e50", type = 4, size = 0.6
            )
          )
        ),
        
        card(
          card_header(icon("table"), " VaR Results"),
          card_body(
            DTOutput("var_table")
          )
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(uiOutput("pipeline_badge"))
)


# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  values <- reactiveValues(
    data             = NULL,
    fourier_basis    = NULL,
    model_mt         = NULL,
    model_variance   = NULL,
    anomalies_model  = NULL,
    model_variance   = NULL,
    sigma_model      = NULL,
    scenarios        = NULL,
    var_results      = NULL
  )
  
  # ---------------------------------------------------------
  # READ FILE
  # ---------------------------------------------------------
  
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      extension <- tools::file_ext(input$file$name)
      
      if (extension %in% c("xlsx", "xls")) {
        values$data <- readxl::read_excel(input$file$datapath)
      } else if (extension == "csv") {
        values$data <- read.csv(input$file$datapath,
                 sep = ";",
                 dec = ",",
                 stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file format.")
      }
      values$data <- temperature::prepare_data(values$data)
      # reset downstream state on new upload
      
      values$fourier_basis   <- NULL
      values$model_mt        <- NULL
      values$model_variance  <- NULL
      values$anomalies_model <- NULL
      values$sigma_model     <- NULL
      values$scenarios       <- NULL
      values$var_results     <- NULL
      
      showNotification("Data uploaded successfully.", type = "message")
    },
    error = function(e) {
      showNotification(paste("Could not read file:", e$message), type = "error", duration = NULL)
    })
  })
  
  # enable/disable buttons based on pipeline state
  observe({
    toggleState("fit_model",    condition = !is.null(values$data))
    toggleState("simulate",     condition = !is.null(values$anomalies_model) && !is.null(values$sigma_model))
    toggleState("calculate_var", condition = !is.null(values$scenarios))
  })
  
  # ---------------------------------------------------------
  # DATA SUMMARY (value boxes + status)
  # ---------------------------------------------------------
  
  temp_col <- reactive({
    req(values$data)
    if ("Temp" %in% names(values$data)) "Temp" else NULL
  })
  
  output$vb_rows  <- renderText({ if (is.null(values$data)) "\u2013" else format(nrow(values$data), big.mark = ",") })
  output$vb_cols  <- renderText({ if (is.null(values$data)) "\u2013" else ncol(values$data) })
  output$vb_mean  <- renderText({
    tc <- temp_col()
    if (is.null(values$data) || is.null(tc)) "\u2013" else paste0(round(mean(values$data[[tc]], na.rm = TRUE), 1), "\u00b0")
  })
  output$vb_range <- renderText({
    tc <- temp_col()
    if (is.null(values$data) || is.null(tc)) "\u2013" else {
      rng <- range(values$data[[tc]], na.rm = TRUE)
      paste0(round(rng[1], 1), "\u00b0 \u2013 ", round(rng[2], 1), "\u00b0")
    }
  })
  
  output$data_info <- renderUI({
    req(values$data)
    div(
      class = "mb-3",
      span(class = "badge bg-success me-2", paste(nrow(values$data), "rows")),
      span(class = "badge bg-secondary", paste(ncol(values$data), "columns"))
    )
  })
  
  output$data_step_summary <- renderUI({
    if (is.null(values$data)) {
      status_pill("Awaiting data", "idle")
    } else {
      status_pill("Data loaded", "ok")
    }
  })
  
  # ---------------------------------------------------------
  # DATA PREVIEW
  # ---------------------------------------------------------
  
  output$data_preview <- renderDT({
    req(values$data)
    datatable(
      values$data,
      options = list(pageLength = 10, scrollX = TRUE),
      class = "compact stripe hover"
    )
  })
  
  # ---------------------------------------------------------
  # FIT MODEL
  # ---------------------------------------------------------
  
  observeEvent(input$fit_model, {
    req(values$data)
    
    tryCatch({
      values$fourier_basis <- temperature::construct_fourier_basis(values$data)
      
      values$model_mt <- temperature::average_model(values$fourier_basis)
      
      variance_model_result <- temperature::variance_model(values$fourier_basis, values$model_mt)
      values$model_variance <- variance_model_result$model_variance
      values$fourier_basis <- variance_model_result$fourier_basis
      
      values$anomalies_model <- temperature::anomaly_model(values$fourier_basis, eps=0.001)
      
      variance_result <- temperature::sigm_model(values$fourier_basis, values$anomalies_model, eps=0.001)
      
      values$sigma_model   <- variance_result$sigma_model
      values$fourier_basis <- variance_result$fourier_basis
      
      showNotification("Temperature model fitted successfully.", type = "message")
    },
    error = function(e) {
      showNotification(paste("Model fitting failed:", e$message), type = "error", duration = NULL)
    })
  })
  
  # ---------------------------------------------------------
  # MODEL STATUS + PIPELINE
  # ---------------------------------------------------------
  
  output$model_status <- renderUI({
    if (is.null(values$data)) {
      status_pill("Upload temperature data first", "idle")
    } else if (is.null(values$anomalies_model)) {
      status_pill("Data uploaded \u2013 model not fitted yet", "warn")
    } else {
      status_pill("Model successfully fitted", "ok")
    }
  })
  
  output$model_pipeline <- renderUI({
    tagList(
      step_row("Data loaded",        !is.null(values$data)),
      step_row("Fourier basis built", !is.null(values$fourier_basis)),
      step_row("Average model is fitted", !is.null(values$model_mt)),
      step_row("Variance model is fitted", !is.null(values$model_variance)),
      step_row("Anomaly model fitted", !is.null(values$anomalies_model)),
      step_row("Sigma (volatility) model fitted", !is.null(values$sigma_model))
    )
  })
  
  # ---------------------------------------------------------
  # MODEL PLOT
  # ---------------------------------------------------------
  
  output$model_plot <- renderPlot({
    req(values$data)
    tc <- if ("Temp" %in% names(values$data)) "Temp" else names(values$data)[1]
    
    df <- data.frame(idx = seq_len(nrow(values$data)), y = values$data[[tc]])
    
    ggplot(df, aes(x = idx, y = y)) +
      geom_line(color = "#2c3e50", linewidth = 0.4) +
      labs(title = "Historical Temperature", x = "Time", y = "Temperature") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "plain", size = 13, colour = "#2c3e50"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e9ecef"),
        axis.text        = element_text(colour = "#7f8c8d"),
        axis.title       = element_text(colour = "#495057")
      )
  })
  
  # ---------------------------------------------------------
  # SIMULATION
  # ---------------------------------------------------------
  
  observeEvent(input$simulate, {
    req(values$fourier_basis, values$anomalies_model, values$sigma_model)
    
    tryCatch({
      values$scenarios <- temperature::generate_scenarios(
        nbr_scenarios   = input$nbr_scenarios,
        nb_years        = input$nb_years,
        model_mt        = values$model_mt,
        model_variance  = values$model_variance,
        sigma_model     = values$sigma_model,
        fourier_basis   = values$fourier_basis,
        anomalies_model = values$anomalies_model
      )
      
      values$var_results <- NULL # invalidate downstream VaR
      showNotification("Simulation completed successfully.", type = "message")
    },
    error = function(e) {
      showNotification(paste("Simulation failed:", e$message), type = "error", duration = NULL)
    })
  })
  
  output$scenario_footer <- renderText({
    if (is.null(values$scenarios)) {
      "Run a simulation to see scenario paths here."
    } else {
      paste0(input$nbr_scenarios, " scenarios over ", input$nb_years,
             ifelse(input$nb_years == 1, " year.", " years."))
    }
  })
  
  # ---------------------------------------------------------
  # SCENARIO PLOT
  # ---------------------------------------------------------
  
  output$scenario_plot <- renderPlot({
    req(values$scenarios)
    
    mat <- as.matrix(values$scenarios)
    df <- as.data.frame(mat)
    df$time <- seq_len(nrow(df))
    long <- reshape(df, direction = "long", varying = names(df)[names(df) != "time"],
                    v.names = "value", timevar = "scenario", idvar = "time",
                    times = names(df)[names(df) != "time"])
    long$mean_line <- ave(long$value, long$time)
    
    ggplot(long, aes(x = time, y = value, group = scenario)) +
      geom_line(color = "#2c3e50", alpha = 0.06, linewidth = 0.3) +
      geom_line(aes(y = mean_line), color = "#a13d3d", linewidth = 0.8) +
      labs(title = "Simulated Temperature Scenarios", x = "Time", y = "Temperature") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "plain", size = 13, colour = "#2c3e50"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e9ecef"),
        axis.text        = element_text(colour = "#7f8c8d"),
        axis.title       = element_text(colour = "#495057")
      )
  })
  
  output$simulate_download_ui <- renderUI({
    req(values$scenarios)
    downloadButton("download_scenarios", "Download scenarios (.csv)", class = "btn-outline-secondary w-100")
  })
  
  output$download_scenarios <- downloadHandler(
    filename = function() paste0("scenarios_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(values$scenarios, file, row.names = FALSE)
  )
  
  # ---------------------------------------------------------
  # CLIMATE VAR
  # ---------------------------------------------------------
  
  output$alpha_label <- renderText({
    paste0("Current level: ", percent(input$alpha, accuracy = 0.1))
  })
  
  observeEvent(input$calculate_var, {
    req(values$data, values$scenarios)
    
    tryCatch({
      values$var_results <- temperature::climate_var(
        alpha            = input$alpha,
        historical_data  = values$data,
        df_scenarios     = values$scenarios
      )
      
      showNotification("Climate VaR calculated successfully.", type = "message")
    },
    error = function(e) {
      showNotification(paste("VaR calculation failed:", e$message), type = "error", duration = NULL)
    })
  })
  
  # ---------------------------------------------------------
  # VAR STATUS
  # ---------------------------------------------------------
  
  output$var_status <- renderUI({
    if (is.null(values$scenarios)) {
      status_pill("Run a simulation first", "idle")
    } else if (is.null(values$var_results)) {
      status_pill("Ready to calculate Climate VaR", "warn")
    } else {
      status_pill("Climate VaR calculated", "ok")
    }
  })
  
  # ---------------------------------------------------------
  # VAR TABLE
  # ---------------------------------------------------------
  
  output$var_table <- renderDT({
    req(values$var_results)
    
    df <- data.frame(
      Type = c("Cold", "Hot"),
      `Monte Carlo VaR` = round(c(
        values$var_results$cold_monte_carlo_VaR,
        values$var_results$hot_monte_carlo_VaR
      ), 2),
      check.names = FALSE
    )
    
    datatable(
      df,
      options = list(dom = "t", ordering = FALSE),
      rownames = FALSE,
      class = "compact stripe"
    )
  })
  
  # ---------------------------------------------------------
  # VAR PLOT
  # ---------------------------------------------------------
  
  output$var_plot <- renderPlot({
    req(values$data, values$var_results)
    tc <- if ("Temp" %in% names(values$data)) "Temp" else names(values$data)[1]
    
    df <- data.frame(temp = values$data[[tc]])
    vlines <- data.frame(
      value = c(values$var_results$cold_monte_carlo_VaR, values$var_results$hot_monte_carlo_VaR),
      label = c("Cold Monte Carlo VaR", "Hot Monte Carlo VaR")
    )
    
    ggplot(df, aes(x = temp)) +
      geom_histogram(bins = 50, fill = "#a8b4bd", colour = "#ffffff", linewidth = 0.15) +
      geom_vline(data = vlines, aes(xintercept = value, colour = label),
                 linewidth = 0.8, linetype = "dashed") +
      scale_colour_manual(values = c("Cold Monte Carlo VaR" = "#2c6e91", "Hot Monte Carlo VaR" = "#a13d3d"), name = NULL) +
      labs(title = "Historical Temperature Distribution", x = "Temperature", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "plain", size = 13, colour = "#2c3e50"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e9ecef"),
        axis.text        = element_text(colour = "#7f8c8d"),
        axis.title       = element_text(colour = "#495057"),
        legend.position  = "top",
        legend.text      = element_text(colour = "#495057")
      )
  })
  
  output$var_download_ui <- renderUI({
    req(values$var_results)
    downloadButton("download_var", "Download VaR results (.csv)", class = "btn-outline-secondary w-100")
  })
  
  output$download_var <- downloadHandler(
    filename = function() paste0("climate_var_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- data.frame(
        Type = c("Cold", "Hot"),
        Monte_Carlo_VaR = c(values$var_results$cold_monte_carlo_VaR, values$var_results$hot_monte_carlo_VaR)
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ---------------------------------------------------------
  # GLOBAL PIPELINE BADGE (top-right of navbar)
  # ---------------------------------------------------------
  
  output$pipeline_badge <- renderUI({
    step <- if (is.null(values$data)) {
      1
    } else if (is.null(values$anomalies_model)) {
      2
    } else if (is.null(values$scenarios)) {
      3
    } else if (is.null(values$var_results)) {
      4
    } else {
      5
    }
    labels <- c("Upload data", "Fit model", "Run simulation", "Compute VaR", "Complete")
    status <- if (step == 5) "ok" else "warn"
    status_pill(paste0("Step ", min(step, 5), "/5 \u2013 ", labels[min(step, 5)]), status)
  })
}

shinyApp(ui = ui, server = server)