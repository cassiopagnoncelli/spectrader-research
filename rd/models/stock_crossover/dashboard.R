library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Trading Strategy Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Position Exits", tabName = "exits", icon = icon("chart-line")),
      menuItem("Kelly Criterion", tabName = "kelly", icon = icon("balance-scale")),
      menuItem("Returns Analysis", tabName = "returns", icon = icon("chart-area")),
      menuItem("Captures", tabName = "accuracy", icon = icon("bullseye")),
      menuItem("Captures Breakdown", tabName = "captures_breakdown", icon = icon("chart-pie")),
      menuItem("Concurrency", tabName = "concurrency", icon = icon("layer-group")),
      menuItem("Signals & Returns", tabName = "signals_returns", icon = icon("table")),
      hr(),
      menuItem("Options Returns", tabName = "options_returns", icon = icon("dollar-sign")),
      menuItem("Options Kelly", tabName = "options_kelly", icon = icon("calculator")),
      menuItem("Options Surface", tabName = "options_surface", icon = icon("cube")),
      hr(),
      menuItem("Signal Model", tabName = "models", icon = icon("chart-bar")),
      hr(),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            title = "Trading Strategy Performance Summary",
            status = "primary",
            solidHeader = TRUE,
            h4("Key Metrics"),
            htmlOutput("overview_metrics")
          )
        ),
        fluidRow(
          valueBoxOutput("total_trades", width = 3),
          valueBoxOutput("win_rate", width = 3),
          valueBoxOutput("avg_return", width = 3),
          valueBoxOutput("kelly_fraction", width = 3)
        )
      ),
      
      # Position Exits Tab
      tabItem(
        tabName = "exits",
        fluidRow(
          box(
            width = 12,
            title = "Position Exit Analysis",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "position_filter",
              "Position Filter:",
              choices = c("All Positions" = "all", 
                         "Captured" = "captured", 
                         "Uncaptured" = "uncaptured"),
              selected = "all"
            ),
            actionButton("refresh_samples", "Refresh Samples", icon = icon("refresh"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Captured Position Exits (Quantile Regression)",
            status = "info",
            fluidRow(
              column(
                width = 12,
                div(
                  style = "text-align: center; margin-bottom: 15px;",
                  actionButton("prev_chart", "Previous", icon = icon("arrow-left"), style = "margin-right: 10px;"),
                  htmlOutput("chart_counter", inline = TRUE),
                  actionButton("next_chart", "Next", icon = icon("arrow-right"), style = "margin-left: 10px;")
                )
              )
            ),
            plotOutput("current_exit_plot", height = 400)
          )
        )
      ),
      
      # Kelly Criterion Tab
      tabItem(
        tabName = "kelly",
        fluidRow(
          box(
            width = 12,
            title = "Kelly Criterion Analysis",
            status = "primary",
            solidHeader = TRUE,
            numericInput("kelly_tau", "Kelly Quantile Tau:", value = 0.32, min = 0.01, max = 0.99, step = 0.01),
            checkboxInput("kelly_log_transform", "Log Transform", value = FALSE)
          )
        ),
        fluidRow(
          box(
            width = 8,
            title = "Kelly Portfolio Growth",
            status = "info",
            plotOutput("kelly_plot", height = 500)
          ),
          box(
            width = 4,
            title = "Kelly Statistics",
            status = "info",
            htmlOutput("kelly_stats")
          )
        )
      ),
      
      # Returns Analysis Tab
      tabItem(
        tabName = "returns",
        fluidRow(
          box(
            width = 12,
            title = "Returns Distribution Analysis",
            status = "primary",
            solidHeader = TRUE,
            numericInput("bins", "Number of bins:", value = 30, min = 10, max = 100, step = 5)
          )
        ),
        fluidRow(
          box(
            width = 8,
            title = "Returns Distribution",
            status = "info",
            plotOutput("returns_distribution", height = 500)
          ),
          box(
            width = 4,
            title = "Distribution Statistics",
            status = "info",
            htmlOutput("returns_stats")
          )
        )
      ),
      
      # Captures Tab
      tabItem(
        tabName = "accuracy",
        fluidRow(
          box(
            width = 12,
            title = "Captures",
            status = "primary",
            solidHeader = TRUE,
            selectInput("side", "Trading Side:", choices = c("long", "short"), selected = "long")
          )
        ),
        fluidRow(
          box(
            width = 4,
            title = "Captured Positions",
            status = "success",
            solidHeader = TRUE,
            htmlOutput("accuracy_captured_metrics"),
            hr(),
            htmlOutput("accuracy_captured_dist")
          ),
          box(
            width = 4,
            title = "Uncaptured Positions",
            status = "warning",
            solidHeader = TRUE,
            htmlOutput("accuracy_uncaptured_metrics"),
            hr(),
            htmlOutput("accuracy_uncaptured_dist")
          ),
          box(
            width = 4,
            title = "All Positions",
            status = "info",
            solidHeader = TRUE,
            htmlOutput("accuracy_all_metrics"),
            hr(),
            htmlOutput("accuracy_all_dist")
          )
        )
      ),
      
      # Captures Breakdown Tab
      tabItem(
        tabName = "captures_breakdown",
        fluidRow(
          box(
            width = 12,
            title = "Captures Breakdown",
            status = "primary",
            solidHeader = TRUE,
            selectInput("breakdown_side", "Trading Side:", choices = c("long", "short"), selected = "long"),
            selectInput("breakdown_category", "Category:", 
                       choices = c("Overall", "Captured", "Uncaptured"), 
                       selected = "Overall")
          )
        ),
        fluidRow(
          box(
            width = 4,
            title = "Overall Results",
            status = "primary",
            solidHeader = TRUE,
            htmlOutput("breakdown_overall")
          ),
          box(
            width = 4,
            title = "g1 (R <= 0)",
            status = "danger",
            solidHeader = TRUE,
            htmlOutput("breakdown_g1")
          ),
          box(
            width = 4,
            title = "g2 (R > 0)",
            status = "success",
            solidHeader = TRUE,
            htmlOutput("breakdown_g2")
          )
        )
      ),
      
      # Concurrency Tab
      tabItem(
        tabName = "concurrency",
        fluidRow(
          box(
            width = 12,
            title = "Trade Concurrency Analysis",
            status = "primary",
            solidHeader = TRUE,
            p("Analysis of overlapping trades and concurrent positions over time.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Concurrency Summary Statistics",
            status = "info",
            solidHeader = TRUE,
            htmlOutput("concurrency_summary_stats")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Concurrent Trades Over Time",
            status = "info",
            plotOutput("concurrency_over_time", height = 400)
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Trade-to-Trade Overlap Matrix",
            status = "info",
            plotOutput("concurrency_overlap_matrix", height = 500)
          ),
          box(
            width = 6,
            title = "Distribution of Overlap Counts",
            status = "info",
            plotOutput("concurrency_distribution", height = 500)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Trade Timeline (Gantt View)",
            status = "info",
            plotOutput("concurrency_waterfall", height = 500)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Weekly Trade Concurrency (Punchcard View)",
            status = "info",
            plotOutput("concurrency_punchcard", height = 400)
          )
        )
      ),
      
      # Signals, Returns Tab
      tabItem(
        tabName = "signals_returns",
        fluidRow(
          box(
            width = 12,
            title = "Signals and Returns Data Table",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("signals_returns_table")
          )
        )
      ),
      
      # Options Returns Tab
      tabItem(
        tabName = "options_returns",
        fluidRow(
          box(
            width = 12,
            title = "American Options Returns Analysis",
            status = "primary",
            solidHeader = TRUE,
            numericInput("options_K", "Strike Multiplier (K):", value = 1.25, min = 0.1, max = 2.0, step = 0.05),
            numericInput("options_tm", "Days to Maturity (tm):", value = 25, min = 1, max = 100, step = 1)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Options Returns Table",
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("options_returns_table")
          )
        )
      ),
      
      # Options Surface Tab
      tabItem(
        tabName = "options_surface",
        fluidRow(
          box(
            width = 12,
            title = "Options Surface Optimization",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                h4("Parameters"),
                numericInput("surface_max_position_days", "Feature Days:", value = 22, min = 1, max = 100, step = 1),
                numericInput("surface_vol_0", "Entry Volatility (σ₀):", value = NA, min = 0.05, max = 3.0, step = 0.05),
                numericInput("surface_vol_t", "Exit Volatility (σₜ):", value = NA, min = 0.05, max = 3.0, step = 0.05),
                selectInput("surface_goal", "Goal:",
                           choices = c("log-portfolio" = "log-portfolio",
                                     "log-portfolio-kelly" = "log-portfolio-kelly",
                                     "sharpe" = "sharpe"),
                           selected = "log-portfolio"),
                conditionalPanel(
                  condition = "input.surface_goal == 'log-portfolio-kelly'",
                  numericInput("surface_kelly_q", "Kelly Quantile (q):", value = 0.3, min = 0.01, max = 0.99, step = 0.01),
                  numericInput("surface_kelly_cap", "Kelly Cap:", value = 0.4, min = 0.01, max = 1.0, step = 0.01)
                ),
                conditionalPanel(
                  condition = "input.surface_goal == 'log-portfolio'",
                  numericInput("surface_wager", "Wager:", value = 0.08, min = 0.01, max = 1.0, step = 0.01)
                ),
                actionButton("compute_surface", "Compute Surface", icon = icon("play"), class = "btn-primary")
              ),
              column(
                width = 6,
                h4("Maximum"),
                htmlOutput("surface_maximum")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "3D Surface Plot",
            status = "info",
            solidHeader = TRUE,
            plotly::plotlyOutput("surface_plot", height = "800px")
          )
        )
      ),
      
      # Options Kelly Tab
      tabItem(
        tabName = "options_kelly",
        fluidRow(
          box(
            width = 12,
            title = "Options Kelly Analysis",
            status = "primary",
            solidHeader = TRUE,
            p(
              style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
              strong("Note: "), "American Option Pricing is non-deterministic when volatilities (vol_0, vol_t) are not explicitly set. ",
              "The model will estimate volatilities from the data, which can produce slightly different results on each calculation."
            ),
            fluidRow(
              column(
                width = 6,
                h4("Parameters"),
                numericInput("options_kelly_K", "Strike Multiplier (K):", value = 1.25, min = 0.1, max = 2.0, step = 0.05),
                numericInput("options_kelly_tm", "Days to Maturity (tm):", value = 21, min = 1, max = 100, step = 1),
                numericInput("options_kelly_tau", "quantile τ:", value = 0.3, min = 0.01, max = 0.99, step = 0.01),
                numericInput("options_kelly_cap", "Kelly cap:", value = 0.4, min = 0.01, max = 1.0, step = 0.01),
                numericInput("options_kelly_vol_0", "Entry Volatility (σ₀):", value = NA, min = 0.05, max = 3.0, step = 0.05),
                numericInput("options_kelly_vol_t", "Exit Volatility (σₜ):", value = NA, min = 0.05, max = 3.0, step = 0.05)
              ),
              column(
                width = 6,
                h4("Values"),
                htmlOutput("options_kelly_values")
              )
            )
          )
        )
      ),
      
      # Models Tab
      tabItem(
        tabName = "models",
        fluidRow(
          box(
            width = 12,
            title = "Model Analysis",
            status = "primary",
            solidHeader = TRUE,
            p("Signal prediction model diagnostics and performance analysis.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Model Plots",
            status = "info",
            fluidRow(
              column(
                width = 12,
                div(
                  style = "text-align: center; margin-bottom: 15px;",
                  actionButton("prev_model_plot", "Previous", icon = icon("arrow-left"), style = "margin-right: 10px;"),
                  htmlOutput("model_plot_counter", inline = TRUE),
                  actionButton("next_model_plot", "Next", icon = icon("arrow-right"), style = "margin-left: 10px;")
                )
              )
            ),
            plotOutput("current_model_plot", height = 500)
          )
        )
      ),
      
      # Settings Tab
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            width = 12,
            title = "Dashboard Settings",
            status = "primary",
            solidHeader = TRUE,
            p("This dashboard displays trading strategy performance metrics and visualizations."),
            h4("Data Source"),
            p("The dashboard uses data from the trade simulation analysis stored in the global environment."),
            h4("Required Objects"),
            tags$ul(
              tags$li(strong("dfsr:"), "Data frame with signals and returns"),
              tags$li(strong("posl:"), "List of position cohorts"),
              tags$li(strong("f_star:"), "Optimal Kelly fraction")
            ),
            hr(),
            h4("Refresh Data"),
            p("To refresh the dashboard with new data, reload the trade simulation script and restart the dashboard."),
            actionButton("check_data", "Check Data Availability", icon = icon("check-circle"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Data Status",
            status = "info",
            verbatimTextOutput("data_status")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(
    dfsr = NULL,
    posl = NULL,
    f_star = NULL,
    accuracy = NULL,
    sample_trades = NULL,
    current_chart_index = 1,
    model_signal = NULL,
    current_model_plot_index = 1
  )
  
  # Define model plot names
  model_plot_names <- c(
    "Metrics Comparison",
    "Feature Importance (Top 20)",
    "All Predictions",
    "Predictions vs Actuals (Test)",
    "Residuals (Test)",
    "Residual Distribution (Test)",
    "Predictions vs Actuals (Validation)",
    "Residuals (Validation)",
    "Predictions vs Actuals (Train)",
    "Residuals (Train)"
  )
  
  # Load data from global environment
  observe({
    if (exists("dfsr", envir = .GlobalEnv)) {
      rv$dfsr <- get("dfsr", envir = .GlobalEnv)
    }
    if (exists("posl", envir = .GlobalEnv)) {
      rv$posl <- get("posl", envir = .GlobalEnv)
    }
    if (exists("f_star", envir = .GlobalEnv)) {
      rv$f_star <- get("f_star", envir = .GlobalEnv)
    }
    if (exists("model_signal", envir = .GlobalEnv)) {
      rv$model_signal <- get("model_signal", envir = .GlobalEnv)
    }
  })
  
  # Calculate accuracy when data or side changes
  observe({
    req(rv$dfsr)
    rv$accuracy <- exit_accuracy(rv$dfsr, side = input$side)
  })
  
  # Generate sample trades based on position filter
  observeEvent(c(input$refresh_samples, input$position_filter), {
    req(rv$dfsr)
    
    # Filter positions based on selection
    if (input$position_filter == "captured") {
      # Captured positions only (have an exit method)
      dfsr_filtered <- rv$dfsr %>% filter(!is.na(exit_method))
    } else if (input$position_filter == "uncaptured") {
      # Uncaptured positions only (no exit method)
      dfsr_filtered <- rv$dfsr %>% filter(is.na(exit_method))
    } else {
      # All positions
      dfsr_filtered <- rv$dfsr
    }
    
    # Get all trade IDs (no limit)
    if (nrow(dfsr_filtered) > 0) {
      rv$sample_trades <- dfsr_filtered %>%
        pull(trade)
    } else {
      rv$sample_trades <- NULL
    }
    
    # Reset to first chart when samples are refreshed
    rv$current_chart_index <- 1
  })
  
  # Carousel Navigation - Previous Button
  observeEvent(input$prev_chart, {
    req(rv$sample_trades)
    if (rv$current_chart_index > 1) {
      rv$current_chart_index <- rv$current_chart_index - 1
    }
  })
  
  # Carousel Navigation - Next Button
  observeEvent(input$next_chart, {
    req(rv$sample_trades)
    if (rv$current_chart_index < length(rv$sample_trades)) {
      rv$current_chart_index <- rv$current_chart_index + 1
    }
  })
  
  # Carousel - Chart Counter Display
  output$chart_counter <- renderUI({
    req(rv$sample_trades)
    
    total_charts <- length(rv$sample_trades)
    current_idx <- rv$current_chart_index
    current_trade_id <- rv$sample_trades[current_idx]
    
    HTML(sprintf(
      "<strong style='font-size: 16px;'>Chart %d of %d</strong> <span style='color: #777;'>(Trade ID: %s)</span>",
      current_idx, total_charts, current_trade_id
    ))
  })
  
  # Carousel - Render Current Chart
  output$current_exit_plot <- renderPlot({
    req(rv$sample_trades, rv$posl)
    
    current_idx <- rv$current_chart_index
    trade_id <- rv$sample_trades[current_idx]
    
    plot_position_cohort_exit_art(rv$posl[[trade_id]], ylim = c(.7, 1.7))
  })
  
  # Overview Metrics
  output$overview_metrics <- renderUI({
    req(rv$dfsr)
    
    n_trades <- nrow(rv$dfsr)
    n_captured <- sum(!is.na(rv$dfsr$exit_method))
    n_uncaptured <- n_trades - n_captured
    
    HTML(sprintf(
      "<table class='table table-bordered'>
        <tr><td><strong>Total Signals:</strong></td><td>%d</td></tr>
        <tr><td><strong>Captured Positions:</strong></td><td>%d</td></tr>
        <tr><td><strong>Uncaptured Positions:</strong></td><td>%d</td></tr>
      </table>",
      n_trades, n_captured, n_uncaptured
    ))
  })
  
  # Value Boxes
  output$total_trades <- renderValueBox({
    req(rv$dfsr)
    valueBox(
      nrow(rv$dfsr),
      "Total Trades",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$win_rate <- renderValueBox({
    req(rv$dfsr)
    win_rate <- mean(rv$dfsr$R > 0, na.rm = TRUE) * 100
    valueBox(
      sprintf("%.1f%%", win_rate),
      "Win Rate",
      icon = icon("trophy"),
      color = if (win_rate > 50) "green" else "red"
    )
  })
  
  output$avg_return <- renderValueBox({
    req(rv$dfsr)
    avg_return <- mean(rv$dfsr$R, na.rm = TRUE) * 100
    valueBox(
      sprintf("%.2f%%", avg_return),
      "Avg Return",
      icon = icon("percent"),
      color = if (avg_return > 0) "green" else "red"
    )
  })
  
  output$kelly_fraction <- renderValueBox({
    req(rv$f_star)
    valueBox(
      sprintf("%.3f", rv$f_star),
      "Kelly Fraction",
      icon = icon("balance-scale"),
      color = "purple"
    )
  })
  
  # Overview plots
  output$overview_distribution <- renderPlot({
    req(rv$dfsr)
    plot_distribution(na.omit(rv$dfsr$R), title = "Returns Distribution")
  })
  
  output$overview_kelly <- renderPlot({
    req(rv$dfsr, rv$f_star)
    plot_kelly_trades(rv$dfsr$R, rv$f_star, log.transform = FALSE)
  })
  
  # Position Exit Plots
  output$exit_plots <- renderUI({
    req(rv$sample_trades, rv$posl)
    
    plot_output_list <- lapply(seq_along(rv$sample_trades), function(i) {
      trade_id <- rv$sample_trades[i]
      plotname <- paste0("exit_plot_", i)
      plotOutput(plotname, height = 300)
    })
    
    do.call(tagList, plot_output_list)
  })
  
  observe({
    req(rv$sample_trades, rv$posl)
    
    lapply(seq_along(rv$sample_trades), function(i) {
      trade_id <- rv$sample_trades[i]
      plotname <- paste0("exit_plot_", i)
      
      output[[plotname]] <- renderPlot({
        plot_position_cohort_exit_art(rv$posl[[trade_id]], ylim = c(.7, 1.7))
      })
    })
  })
  
  # Kelly Criterion
  output$kelly_plot <- renderPlot({
    req(rv$dfsr)
    f_star <- kelly_quantile(log(1 + rv$dfsr$R), tau = input$kelly_tau)
    plot_kelly_trades(rv$dfsr$R, f_star, log.transform = input$kelly_log_transform)
  })
  
  output$kelly_stats <- renderUI({
    req(rv$dfsr)
    
    returns <- rv$dfsr$R
    f_classical <- kelly_fraction(returns)
    f_quantile <- kelly_quantile(log(1 + returns), tau = input$kelly_tau)
    
    # Calculate portfolio growth
    portfolio_classical <- prod(1 + returns * f_classical)
    portfolio_quantile <- prod(1 + returns * f_quantile)
    
    HTML(sprintf(
      "<h4>Kelly Fractions</h4>
      <table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>Classical Kelly:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Quantile Kelly (τ=%.2f):</strong></td><td style='text-align: right;'>%.4f</td></tr>
      </table>
      <h4>Portfolio Growth</h4>
      <table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>Classical:</strong></td><td style='text-align: right;'>%.2fx</td></tr>
        <tr><td><strong>Quantile:</strong></td><td style='text-align: right;'>%.2fx</td></tr>
      </table>
      <h4>Return Statistics</h4>
      <table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>Mean Return:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Median Return:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Std Dev:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Sharpe Ratio:</strong></td><td style='text-align: right;'>%.4f</td></tr>
      </table>",
      f_classical, input$kelly_tau, f_quantile,
      portfolio_classical, portfolio_quantile,
      mean(returns, na.rm = TRUE),
      median(returns, na.rm = TRUE),
      sd(returns, na.rm = TRUE),
      sharpe_ratio(returns, na.rm = TRUE)
    ))
  })
  
  # Returns Distribution
  output$returns_distribution <- renderPlot({
    req(rv$dfsr)
    plot_distribution(na.omit(rv$dfsr$R), bins = input$bins, title = "Returns Distribution")
  })
  
  output$returns_stats <- renderUI({
    req(rv$dfsr)
    
    returns <- na.omit(rv$dfsr$R)
    
    HTML(sprintf(
      "<h4>Summary Statistics</h4>
      <table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
        <tr><td><strong>Mean:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Median:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Std Dev:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Min:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Max:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Q0.05:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Q0.25:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Q0.75:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Q0.95:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Skewness:</strong></td><td style='text-align: right;'>%.4f</td></tr>
      </table>",
      length(returns),
      mean(returns),
      median(returns),
      sd(returns),
      min(returns),
      max(returns),
      quantile(returns, 0.05),
      quantile(returns, 0.25),
      quantile(returns, 0.75),
      quantile(returns, 0.95),
      moments::skewness(returns)
    ))
  })
  
  # Signal Accuracy - Captured
  output$accuracy_captured_metrics <- renderUI({
    req(rv$accuracy)
    
    accuracy_captured <- rv$accuracy %>% filter(!is.na(exit_method))
    metrics <- exit_metrics(accuracy_captured, input$side)
    
    if (input$side == "long") {
      HTML(sprintf(
        "<h5>Metrics</h5>
        <table class='table table-condensed' style='font-size: 18px;'>
          <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
          <tr><td><strong>RMSE:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>t mean:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>t sd:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>Alpha:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Sharpe:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        </table>",
        metrics$n, metrics$rmse, metrics$t_mean, metrics$t_sd,
        metrics$long_alpha, metrics$long_capture, metrics$long_capture_sd, metrics$long_sharpe
      ))
    } else {
      HTML(sprintf(
        "<h5>Metrics</h5>
        <table class='table table-condensed' style='font-size: 18px;'>
          <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
          <tr><td><strong>RMSE:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>t mean:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>t sd:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>Alpha:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Sharpe:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        </table>",
        metrics$n, metrics$rmse, metrics$t_mean, metrics$t_sd,
        metrics$short_alpha, metrics$short_capture, metrics$short_capture_sd, metrics$short_sharpe
      ))
    }
  })
  
  output$accuracy_captured_dist <- renderUI({
    req(rv$accuracy)
    
    accuracy_captured <- rv$accuracy %>% filter(!is.na(exit_method))
    dist <- analyse_distribution(accuracy_captured$R, groups = c(0))
    
    HTML(sprintf(
      "<h5>Distribution</h5>
      <table class='table table-condensed' style='font-size: 18px;'>
        <tr><td><strong>Expected:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Mean:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Median:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
      </table>",
      dist$overall_results$expected_value,
      dist$overall_results$mean,
      dist$overall_results$median,
      dist$overall_results$sd
    ))
  })
  
  # Signal Accuracy - Uncaptured
  output$accuracy_uncaptured_metrics <- renderUI({
    req(rv$accuracy)
    
    accuracy_uncaptured <- rv$accuracy %>% filter(is.na(exit_method))
    metrics <- exit_metrics(accuracy_uncaptured, input$side)
    
    if (input$side == "long") {
      HTML(sprintf(
        "<h5>Metrics</h5>
        <table class='table table-condensed' style='font-size: 18px;'>
          <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
          <tr><td><strong>RMSE:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>t mean:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>t sd:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Alpha:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Sharpe:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        </table>",
        metrics$n, metrics$rmse, metrics$t_mean, metrics$t_sd,
        metrics$long_alpha, metrics$long_capture, metrics$long_capture_sd, metrics$long_sharpe
      ))
    } else {
      HTML(sprintf(
        "<h5>Metrics</h5>
        <table class='table table-condensed' style='font-size: 18px;'>
          <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
          <tr><td><strong>RMSE:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>t mean:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>t sd:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Alpha:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Sharpe:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        </table>",
        metrics$n, metrics$rmse, metrics$t_mean, metrics$t_sd,
        metrics$short_alpha, metrics$short_capture, metrics$short_capture_sd, metrics$short_sharpe
      ))
    }
  })
  
  output$accuracy_uncaptured_dist <- renderUI({
    req(rv$accuracy)
    
    accuracy_uncaptured <- rv$accuracy %>% filter(is.na(exit_method))
    dist <- analyse_distribution(accuracy_uncaptured$R, groups = c(0))
    
    HTML(sprintf(
      "<h5>Distribution</h5>
      <table class='table table-condensed' style='font-size: 18px;'>
        <tr><td><strong>Expected:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Mean:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Median:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
      </table>",
      dist$overall_results$expected_value,
      dist$overall_results$mean,
      dist$overall_results$median,
      dist$overall_results$sd
    ))
  })
  
  # Signal Accuracy - All Positions
  output$accuracy_all_metrics <- renderUI({
    req(rv$accuracy)
    
    metrics <- exit_metrics(rv$accuracy, input$side)
    
    if (input$side == "long") {
      HTML(sprintf(
        "<h5>Metrics</h5>
        <table class='table table-condensed' style='font-size: 18px;'>
          <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
          <tr><td><strong>RMSE:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>t mean:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>t sd:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>Alpha:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Sharpe:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        </table>",
        metrics$n, metrics$rmse, metrics$t_mean, metrics$t_sd,
        metrics$long_alpha, metrics$long_capture, metrics$long_capture_sd, metrics$long_sharpe
      ))
    } else {
      HTML(sprintf(
        "<h5>Metrics</h5>
        <table class='table table-condensed' style='font-size: 18px;'>
          <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
          <tr><td><strong>RMSE:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>t mean:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>t sd:</strong></td><td style='text-align: right;'>%.2f</td></tr>
          <tr><td><strong>Alpha:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Capture SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Sharpe:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        </table>",
        metrics$n, metrics$rmse, metrics$t_mean, metrics$t_sd,
        metrics$short_alpha, metrics$short_capture, metrics$short_capture_sd, metrics$short_sharpe
      ))
    }
  })
  
  output$accuracy_all_dist <- renderUI({
    req(rv$accuracy)
    
    dist <- analyse_distribution(rv$accuracy$R, groups = c(0))
    
    HTML(sprintf(
      "<h5>Distribution</h5>
      <table class='table table-condensed' style='font-size: 18px;'>
        <tr><td><strong>Expected:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Mean:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Median:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
      </table>",
      dist$overall_results$expected_value,
      dist$overall_results$mean,
      dist$overall_results$median,
      dist$overall_results$sd
    ))
  })
  
  # Captures Breakdown - Get filtered data based on category
  breakdown_data <- reactive({
    req(rv$dfsr, input$breakdown_category, input$breakdown_side)
    
    # Get accuracy data
    accuracy_data <- exit_accuracy(rv$dfsr, side = input$breakdown_side)
    
    # Filter based on category
    if (input$breakdown_category == "Captured") {
      accuracy_data %>% filter(!is.na(exit_method))
    } else if (input$breakdown_category == "Uncaptured") {
      accuracy_data %>% filter(is.na(exit_method))
    } else {
      # Overall
      accuracy_data
    }
  })
  
  # Captures Breakdown - Overall Results
  output$breakdown_overall <- renderUI({
    req(breakdown_data())
    
    dist <- analyse_distribution(breakdown_data()$R, groups = c(0))
    
    HTML(sprintf(
      "<table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>Expected:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Mean:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>Median:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>N:</strong></td><td style='text-align: right;'>%d</td></tr>
      </table>",
      dist$overall_results$expected_value,
      dist$overall_results$mean,
      dist$overall_results$median,
      dist$overall_results$sd,
      dist$overall_results$n
    ))
  })
  
  # Captures Breakdown - g1 (R <= 0)
  output$breakdown_g1 <- renderUI({
    req(breakdown_data())
    
    dist <- analyse_distribution(breakdown_data()$R, groups = c(0))
    
    # Check if g1 exists in group_results
    if (!is.null(dist$group_results) && nrow(dist$group_results) > 0) {
      g1_data <- dist$group_results %>% filter(group == "g1")
      
      if (nrow(g1_data) > 0) {
        HTML(sprintf(
          "<table class='table table-bordered' style='font-size: 18px;'>
            <tr><td><strong>Count:</strong></td><td style='text-align: right;'>%d</td></tr>
            <tr><td><strong>Prob:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Mean:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Expected:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Median:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Min:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Max:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          </table>",
          g1_data$count,
          g1_data$prob,
          g1_data$mean,
          g1_data$expected,
          g1_data$median,
          ifelse(is.na(g1_data$sd), 0, g1_data$sd),
          g1_data$min,
          g1_data$max
        ))
      } else {
        HTML("<p style='text-align: center; padding: 20px;'>No data available for g1</p>")
      }
    } else {
      HTML("<p style='text-align: center; padding: 20px;'>No group data available</p>")
    }
  })
  
  # Captures Breakdown - g2 (R > 0)
  output$breakdown_g2 <- renderUI({
    req(breakdown_data())
    
    dist <- analyse_distribution(breakdown_data()$R, groups = c(0))
    
    # Check if g2 exists in group_results
    if (!is.null(dist$group_results) && nrow(dist$group_results) > 0) {
      g2_data <- dist$group_results %>% filter(group == "g2")
      
      if (nrow(g2_data) > 0) {
        HTML(sprintf(
          "<table class='table table-bordered' style='font-size: 18px;'>
            <tr><td><strong>Count:</strong></td><td style='text-align: right;'>%d</td></tr>
            <tr><td><strong>Prob:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Mean:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Expected:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Median:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>SD:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Min:</strong></td><td style='text-align: right;'>%.4f</td></tr>
            <tr><td><strong>Max:</strong></td><td style='text-align: right;'>%.4f</td></tr>
          </table>",
          g2_data$count,
          g2_data$prob,
          g2_data$mean,
          g2_data$expected,
          g2_data$median,
          ifelse(is.na(g2_data$sd), 0, g2_data$sd),
          g2_data$min,
          g2_data$max
        ))
      } else {
        HTML("<p style='text-align: center; padding: 20px;'>No data available for g2</p>")
      }
    } else {
      HTML("<p style='text-align: center; padding: 20px;'>No group data available</p>")
    }
  })
  
  # Concurrency Analysis - Create df_dates
  df_dates <- reactive({
    req(rv$dfsr)
    
    rv$dfsr %>%
      dplyr::mutate(
        entry = date,
        exit = add_business_days(date, t)
      ) %>%
      select(trade, symbol, entry, exit, R, t)
  })
  
  # Concurrency Summary Statistics
  output$concurrency_summary_stats <- renderUI({
    req(df_dates())
    
    summary <- concurrency_summary(df_dates())
    
    HTML(sprintf(
      "<table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>Mean Concurrent Trades:</strong></td><td style='text-align: right;'>%.2f</td></tr>
        <tr><td><strong>Median Concurrent Trades:</strong></td><td style='text-align: right;'>%.0f</td></tr>
        <tr><td><strong>Max Concurrent Trades:</strong></td><td style='text-align: right;'>%d</td></tr>
        <tr><td><strong>Q0.05:</strong></td><td style='text-align: right;'>%.2f</td></tr>
        <tr><td><strong>Q0.32:</strong></td><td style='text-align: right;'>%.2f</td></tr>
        <tr><td><strong>Q0.68:</strong></td><td style='text-align: right;'>%.2f</td></tr>
        <tr><td><strong>Q0.80:</strong></td><td style='text-align: right;'>%.2f</td></tr>
        <tr><td><strong>Q0.95:</strong></td><td style='text-align: right;'>%.2f</td></tr>
        <tr><td><strong>%% Time with Multiple Trades:</strong></td><td style='text-align: right;'>%.1f%%</td></tr>
      </table>",
      summary$mean_concurrent,
      summary$median_concurrent,
      summary$max_concurrent,
      summary$q.05,
      summary$q.32,
      summary$q.68,
      summary$q.80,
      summary$q.95,
      summary$pct_time_multi * 100
    ))
  })
  
  # Concurrency Plot 1: Concurrency Over Time
  output$concurrency_over_time <- renderPlot({
    req(df_dates())
    plot_concurrency_over_time(df_dates(), plot = FALSE)$plot
  })
  
  # Concurrency Plot 2: Overlap Matrix
  output$concurrency_overlap_matrix <- renderPlot({
    req(df_dates())
    plot_concurrency_overlap_matrix(df_dates(), plot = FALSE)$plot
  })
  
  # Concurrency Plot 3: Distribution
  output$concurrency_distribution <- renderPlot({
    req(df_dates())
    plot_concurrency_distribution(df_dates(), plot = FALSE)$plot
  })
  
  # Concurrency Plot 4: Waterfall (Gantt)
  output$concurrency_waterfall <- renderPlot({
    req(df_dates())
    plot_concurrency_waterfall(df_dates(), plot = FALSE)$plot
  })
  
  # Concurrency Plot 5: Punchcard
  output$concurrency_punchcard <- renderPlot({
    req(df_dates())
    plot_concurrency_punchcard(df_dates(), plot = FALSE)$plot
  })
  
  # Models Carousel Navigation - Previous Button
  observeEvent(input$prev_model_plot, {
    if (rv$current_model_plot_index > 1) {
      rv$current_model_plot_index <- rv$current_model_plot_index - 1
    }
  })
  
  # Models Carousel Navigation - Next Button
  observeEvent(input$next_model_plot, {
    if (rv$current_model_plot_index < length(model_plot_names)) {
      rv$current_model_plot_index <- rv$current_model_plot_index + 1
    }
  })
  
  # Models Carousel - Plot Counter Display
  output$model_plot_counter <- renderUI({
    total_plots <- length(model_plot_names)
    current_idx <- rv$current_model_plot_index
    plot_name <- model_plot_names[current_idx]
    
    HTML(sprintf(
      "<strong style='font-size: 16px;'>Plot %d of %d</strong> <span style='color: #777;'>(%s)</span>",
      current_idx, total_plots, plot_name
    ))
  })
  
  # Models Carousel - Render Current Plot
  output$current_model_plot <- renderPlot({
    req(rv$model_signal)
    
    current_idx <- rv$current_model_plot_index
    
    # Switch between different plots based on index
    switch(current_idx,
      plot_metrics_comparison(rv$model_signal),
      plot_feature_importance(rv$model_signal, top_n = 20),
      plot_all_predictions(rv$model_signal),
      plot_predictions_vs_actuals(rv$model_signal, "test"),
      plot_residuals(rv$model_signal, "test"),
      plot_residual_distribution(rv$model_signal, "test"),
      plot_predictions_vs_actuals(rv$model_signal, "val"),
      plot_residuals(rv$model_signal, "val"),
      plot_predictions_vs_actuals(rv$model_signal, "train"),
      plot_residuals(rv$model_signal, "train")
    )
  })
  
  # Signals, Returns Table
  output$signals_returns_table <- DT::renderDataTable({
    req(rv$dfsr)
    
    DT::datatable(
      rv$dfsr,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        searching = TRUE,
        ordering = TRUE,
        lengthMenu = c(10, 25, 50, 100, 500),
        autoWidth = TRUE
      ),
      filter = "top",
      rownames = FALSE,
      class = "display compact"
    ) %>%
      DT::formatRound(columns = which(sapply(rv$dfsr, is.numeric)), digits = 4)
  })
  
  # Options Returns Table
  output$options_returns_table <- DT::renderDataTable({
    req(rv$dfsr)
    
    # Call american_optprice_returns with user-specified parameters
    options_data <- american_optprice_returns(rv$dfsr, K = input$options_K, tm = input$options_tm)
    
    DT::datatable(
      options_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        searching = TRUE,
        ordering = TRUE,
        lengthMenu = c(10, 25, 50, 100, 500),
        autoWidth = TRUE
      ),
      filter = "top",
      rownames = FALSE,
      class = "display compact"
    ) %>%
      DT::formatRound(columns = which(sapply(options_data, is.numeric)), digits = 4)
  })
  
  # Options Kelly - Reactive expressions for proper dependency management
  options_kelly_opt_R <- reactive({
    req(rv$dfsr, input$options_kelly_K, input$options_kelly_tm)
    
    # Build parameter list (dfsr is first positional argument)
    params <- list(
      rv$dfsr,
      K = input$options_kelly_K,
      tm = input$options_kelly_tm
    )
    
    # Add volatility parameters if they are set (not NA)
    if (!is.na(input$options_kelly_vol_0)) {
      params$vol_0 <- input$options_kelly_vol_0
    }
    if (!is.na(input$options_kelly_vol_t)) {
      params$vol_t <- input$options_kelly_vol_t
    }
    
    # Call american_optprice_returns with parameters
    do.call(american_optprice_returns, params) %>% pull(opt_R)
  })
  
  options_kelly_f_star <- reactive({
    req(options_kelly_opt_R())
    kelly_fraction(options_kelly_opt_R())
  })
  
  options_kelly_f_star_q <- reactive({
    req(options_kelly_opt_R(), input$options_kelly_tau, input$options_kelly_cap)
    kelly_quantile(options_kelly_opt_R(), tau = input$options_kelly_tau, cap = input$options_kelly_cap)
  })
  
  options_kelly_log_portfolio <- reactive({
    req(options_kelly_f_star(), options_kelly_opt_R())
    log(prod(1 + options_kelly_f_star() * options_kelly_opt_R()))
  })
  
  options_kelly_log_portfolio_q <- reactive({
    req(options_kelly_f_star_q(), options_kelly_opt_R())
    log(prod(1 + options_kelly_f_star_q() * options_kelly_opt_R()))
  })
  
  # Options Kelly Values - Display
  output$options_kelly_values <- renderUI({
    req(options_kelly_f_star(), options_kelly_f_star_q(), 
        options_kelly_log_portfolio(), options_kelly_log_portfolio_q())
    
    HTML(sprintf(
      "<table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>F<sup>*</sup>:</strong></td><td style='text-align: right;'>%.6f</td></tr>
        <tr><td><strong>F<sub>q</sub><sup>*</sup>:</strong></td><td style='text-align: right;'>%.6f</td></tr>
        <tr><td><strong>Log P<sub>n</sub>:</strong></td><td style='text-align: right;'>%.6f</td></tr>
        <tr><td><strong>Log P<sub>n,q=τ</sub>:</strong></td><td style='text-align: right;'>%.6f</td></tr>
      </table>",
      options_kelly_f_star(),
      options_kelly_f_star_q(),
      options_kelly_log_portfolio(),
      options_kelly_log_portfolio_q()
    ))
  })
  
  # Options Surface - Reactive values for storing surface data
  surface_data <- reactiveValues(
    grid = NULL,
    matrix = NULL,
    K_values = NULL,
    tm_values = NULL
  )
  
  # Options Surface - Compute on button click
  observeEvent(input$compute_surface, {
    req(rv$dfsr, input$surface_max_position_days)
    
    # Define K and tm values
    K_values <- seq(0.6, 1.4, by = 0.01)
    
    tm_values <- tibble(x = c(4, 14, 21, 28, 35, 42, 49)) %>%
      dplyr::filter(x > ceiling(365/252 * input$surface_max_position_days)) %>%
      dplyr::pull(x)
    
    # Build goal list based on selection
    goal <- if (input$surface_goal == "sharpe") {
      list(method = "sharpe")
    } else if (input$surface_goal == "log-portfolio-kelly") {
      list(method = "log-portfolio-kelly", q = input$surface_kelly_q, cap = input$surface_kelly_cap)
    } else {
      list(method = "log-portfolio", wager = input$surface_wager)
    }
    
    # Build volatility parameters
    vol_0 <- if (is.na(input$surface_vol_0)) NA else input$surface_vol_0
    vol_t <- if (is.na(input$surface_vol_t)) NA else input$surface_vol_t
    
    # Compute surface grid
    withProgress(message = 'Computing surface...', value = 0, {
      grid <- options_optim_surface_grid(
        data = rv$dfsr,
        K_values = K_values,
        tm_values = tm_values,
        goal = goal,
        vol_0 = vol_0,
        vol_t = vol_t
      )
      
      incProgress(0.7)
      
      # Compute surface matrix
      matrix <- options_optim_surface_matrix(
        grid,
        length(K_values),
        length(tm_values)
      )
      
      incProgress(0.3)
      
      # Store in reactive values
      surface_data$grid <- grid
      surface_data$matrix <- matrix
      surface_data$K_values <- K_values
      surface_data$tm_values <- tm_values
    })
  })
  
  # Options Surface - Display maximum
  output$surface_maximum <- renderUI({
    req(surface_data$grid)
    
    maximum <- options_optim_maximal(surface_data$grid)
    
    HTML(sprintf(
      "<table class='table table-bordered' style='font-size: 18px;'>
        <tr><td><strong>K:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        <tr><td><strong>tm:</strong></td><td style='text-align: right;'>%d</td></tr>
        <tr><td><strong>Result:</strong></td><td style='text-align: right;'>%.6f</td></tr>
      </table>",
      maximum$K,
      maximum$tm,
      maximum$result
    ))
  })
  
  # Options Surface - Render 3D plot
  output$surface_plot <- plotly::renderPlotly({
    req(surface_data$matrix, surface_data$K_values, surface_data$tm_values, surface_data$grid)
    
    plotly::plot_ly() %>%
      plotly::add_surface(
        x = surface_data$K_values,
        y = surface_data$tm_values,
        z = surface_data$matrix,
        colorscale = "Viridis"
      ) %>%
      plotly::layout(
        title = NULL,
        scene = list(
          xaxis = list(title = "Strike Multiplier (K)"),
          yaxis = list(title = "Days to Maturity (tm)"),
          zaxis = list(
            title = "Portfolio Goal",
            range = c(
              min(surface_data$grid$result, na.rm = TRUE, 0),
              max(surface_data$grid$result, na.rm = TRUE)
            )
          )
        )
      )
  })
  
  # Data Status Check
  observeEvent(input$check_data, {
    output$data_status <- renderPrint({
      cat("Checking data availability in global environment...\n\n")
      
      if (exists("dfsr", envir = .GlobalEnv)) {
        dfsr <- get("dfsr", envir = .GlobalEnv)
        cat("✓ dfsr found:", nrow(dfsr), "rows,", ncol(dfsr), "columns\n")
      } else {
        cat("✗ dfsr not found\n")
      }
      
      if (exists("posl", envir = .GlobalEnv)) {
        posl <- get("posl", envir = .GlobalEnv)
        cat("✓ posl found:", length(posl), "positions\n")
      } else {
        cat("✗ posl not found\n")
      }
      
      if (exists("f_star", envir = .GlobalEnv)) {
        f_star <- get("f_star", envir = .GlobalEnv)
        cat("✓ f_star found:", f_star, "\n")
      } else {
        cat("✗ f_star not found\n")
      }
      
      cat("\nNote: Run the trade_simulation.R script to generate the required data objects.")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
