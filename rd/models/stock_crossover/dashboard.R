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
      menuItem("Position Cohort", tabName = "position_cohort", icon = icon("chart-area")),
      menuItem("Kelly Criterion", tabName = "kelly", icon = icon("balance-scale")),
      menuItem("Returns Analysis", tabName = "returns", icon = icon("chart-area")),
      menuItem("Captures", tabName = "accuracy", icon = icon("bullseye")),
      menuItem("Captures Inspect", tabName = "exits", icon = icon("chart-line")),
      menuItem("Captures Breakdown", tabName = "captures_breakdown", icon = icon("chart-pie")),
      menuItem("Capture Methods", tabName = "capture_methods", icon = icon("table")),
      menuItem("Concurrency", tabName = "concurrency", icon = icon("layer-group")),
      menuItem("Signals & Returns", tabName = "signals_returns", icon = icon("table")),
      hr(),
      menuItem("Options Kelly", tabName = "options_kelly", icon = icon("calculator")),
      menuItem("Options Surface", tabName = "options_surface", icon = icon("cube")),
      menuItem("Options Returns", tabName = "options_returns", icon = icon("dollar-sign")),
      hr(),
      menuItem("Signal Model", tabName = "models", icon = icon("chart-bar")),
      menuItem("DQR models", tabName = "dqr_models", icon = icon("chart-line")),
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
          valueBoxOutput("sharpe_ratio", width = 3)
        ),
        fluidRow(
          valueBoxOutput("profit_factor", width = 3),
          valueBoxOutput("capture_rate", width = 3),
          valueBoxOutput("avg_holding_period", width = 3),
          valueBoxOutput("max_drawdown", width = 3)
        )
      ),
      
      # Position Exits Tab
      tabItem(
        tabName = "exits",
        fluidRow(
          box(
            width = 12,
            title = "Captures Inspect",
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
            title = "Position Exits — Inspect",
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
            plotOutput("current_exit_plot", height = 670)
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
      
      # Capture Methods Tab
      tabItem(
        tabName = "capture_methods",
        fluidRow(
          box(
            width = 12,
            title = "Exit Methods Summary",
            status = "primary",
            solidHeader = TRUE,
            p("Summary statistics for different exit methods. Uncaptured positions have no exit method (NA), while captured positions exited via specific strategies.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Uncaptured Positions",
            status = "warning",
            solidHeader = TRUE,
            uiOutput("capture_methods_uncaptured_cards")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Captured Positions (by Exit Method)",
            status = "success",
            solidHeader = TRUE,
            uiOutput("capture_methods_captured_cards")
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
      
      # Position Cohort Tab
      tabItem(
        tabName = "position_cohort",
        fluidRow(
          box(
            width = 12,
            title = "Position Cohort Entry Profiler",
            status = "primary",
            solidHeader = TRUE,
            numericInput("cohort_lookback", "Lookback:", value = 5, min = 1, max = 50, step = 1),
            p("Entry profiler analysis showing aggregated position behavior after entry across all trades.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Entry Profiler Chart",
            status = "info",
            solidHeader = TRUE,
            plotly::plotlyOutput("position_cohort_plot", height = "600px")
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
                numericInput(
                  "surface_max_position_days", "Max Position Days (min tm):",
                  value = 22,
                  min = 1,
                  max = 100,
                  step = 1
                ),
                numericInput("surface_vol_0", "Entry Volatility (σ₀):", value = NA, min = 0.05, max = 3.0, step = 0.05),
                numericInput("surface_vol_t", "Exit Volatility (σₜ):", value = NA, min = 0.05, max = 3.0, step = 0.05),
                selectInput(
                  "surface_goal",
                  "Goal:",
                  choices = c(
                    "log-portfolio" = "log-portfolio",
                    "log-portfolio-kelly" = "log-portfolio-kelly",
                    "sharpe" = "sharpe"
                  ),
                  selected = "log-portfolio"
                ),
                conditionalPanel(
                  condition = "input.surface_goal == 'log-portfolio-kelly'",
                  numericInput(
                    "surface_kelly_q",
                    "Kelly Quantile (q):",
                    value = 0.3,
                    min = 0.01,
                    max = 0.99,
                    step = 0.01
                  ),
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
      
      # DQR Models Tab
      tabItem(
        tabName = "dqr_models",
        fluidRow(
          box(
            width = 12,
            title = "DQR Models Analysis",
            status = "primary",
            solidHeader = TRUE,
            p("Decaying Quantile Regression models for risk management. Each model represents a fitted quantile regression for different risk thresholds."),
            selectInput("dqr_model_select", "Select Model:", choices = NULL)
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = textOutput("dqr_model_title"),
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("dqr_model_summary")
          ),
          box(
            width = 6,
            title = "Model Diagnostics",
            status = "info",
            solidHeader = TRUE,
            htmlOutput("dqr_model_diagnostics")
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
    current_model_plot_index = 1,
    dqr_fits = NULL
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
    if (exists("dqr_fits", envir = .GlobalEnv)) {
      rv$dqr_fits <- get("dqr_fits", envir = .GlobalEnv)
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
    
    # Use unified plot function
    plot_position_cohort_exit(rv$posl[[trade_id]], ylim = c(.7, 1.7))
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
  
  output$sharpe_ratio <- renderValueBox({
    req(rv$dfsr)
    sharpe <- sharpe_ratio(rv$dfsr$R, na.rm = TRUE)
    valueBox(
      sprintf("%.2f", sharpe),
      "Sharpe Ratio",
      icon = icon("chart-line"),
      color = if (sharpe > 1) "green" else if (sharpe > 0) "yellow" else "red"
    )
  })
  
  output$profit_factor <- renderValueBox({
    req(rv$dfsr)
    gross_profit <- sum(rv$dfsr$R[rv$dfsr$R > 0], na.rm = TRUE)
    gross_loss <- abs(sum(rv$dfsr$R[rv$dfsr$R <= 0], na.rm = TRUE))
    pf <- if (gross_loss == 0) Inf else gross_profit / gross_loss
    valueBox(
      sprintf("%.2f", pf),
      "Profit Factor",
      icon = icon("scale-balanced"),
      color = if (pf > 1.5) "green" else if (pf > 1) "yellow" else "red"
    )
  })
  
  output$capture_rate <- renderValueBox({
    req(rv$dfsr)
    capture_rate <- mean(!is.na(rv$dfsr$exit_method), na.rm = TRUE) * 100
    valueBox(
      sprintf("%.1f%%", capture_rate),
      "Capture Rate",
      icon = icon("bullseye"),
      color = if (capture_rate > 70) "green" else if (capture_rate > 50) "yellow" else "red"
    )
  })
  
  output$avg_holding_period <- renderValueBox({
    req(rv$dfsr)
    avg_days <- mean(rv$dfsr$t, na.rm = TRUE)
    valueBox(
      sprintf("%.1f days", avg_days),
      "Avg Holding Period",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$max_drawdown <- renderValueBox({
    req(rv$dfsr)
    dd <- max_drawdown(rv$dfsr$R, na.rm = TRUE)
    valueBox(
      sprintf("%.2f%%", dd * 100),
      "Max Drawdown",
      icon = icon("arrow-down"),
      color = if (dd > -0.10) "green" else if (dd > -0.20) "yellow" else "red"
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
        plot_position_cohort_exit_trifecta(rv$posl[[trade_id]], ylim = c(.7, 1.7))
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
  
  # Position Cohort - Render Entry Profiler Plot
  output$position_cohort_plot <- plotly::renderPlotly({
    req(rv$posl, input$cohort_lookback)
    
    # Load posl_raw from global environment if it exists
    if (exists("posl_raw", envir = .GlobalEnv)) {
      posl_raw <- get("posl_raw", envir = .GlobalEnv)
      
      # Compute position matrix with specified lookback
      posm <- entry_profiler_posm(posl_raw, lookback = input$cohort_lookback)
      
      # Compute metrics and density
      posm_metrics <- entry_profiler_metrics(posm)
      posm_density <- entry_profiler_density(posm)
      
      # Generate the plot
      plot_entry_profiler(posm_metrics, posm_density, lookback = input$cohort_lookback)
    } else {
      # If posl_raw doesn't exist, show a message
      plotly::plot_ly() %>%
        plotly::layout(
          title = "posl_raw object not found in global environment",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "Please ensure 'posl_raw' exists in the global environment.\nRun the trade simulation script to generate this data.",
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              showarrow = FALSE,
              font = list(size = 16, color = "red")
            )
          )
        )
    }
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
  
  # Capture Methods Cards - Uncaptured
  output$capture_methods_uncaptured_cards <- renderUI({
    req(rv$dfsr)
    
    summary_data <- exit_methods_summary(rv$dfsr) %>%
      filter(is.na(exit_method))
    
    if (nrow(summary_data) == 0) {
      return(HTML("<p style='text-align: center; padding: 20px;'>No uncaptured positions found</p>"))
    }
    
    # Create card for uncaptured
    row <- summary_data[1, ]
    div(
      style = "background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 8px; padding: 20px; margin-bottom: 15px;",
      h4(style = "margin-top: 0; color: #d9534f;", "Uncaptured (NA)"),
      fluidRow(
        column(4,
          h5("Count & Probability"),
          tags$table(style = "width: 100%; font-size: 16px;",
            tags$tr(tags$td(strong("Count:")), tags$td(style = "text-align: right;", sprintf("%d", row$n))),
            tags$tr(tags$td(strong("Overall Prob:")), tags$td(style = "text-align: right;", sprintf("%.2f%%", row$overall_probability * 100))),
            tags$tr(tags$td(strong("Capture Prob:")), tags$td(style = "text-align: right;", sprintf("%.2f%%", row$capture_probability * 100)))
          )
        ),
        column(4,
          h5("Returns"),
          tags$table(style = "width: 100%; font-size: 16px;",
            tags$tr(tags$td(strong("Mean R:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$mean_R))),
            tags$tr(tags$td(strong("Median R:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$median_R))),
            tags$tr(tags$td(strong("SD:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$sd_R)))
          )
        ),
        column(4,
          h5("Expected Values"),
          tags$table(style = "width: 100%; font-size: 16px;",
            tags$tr(tags$td(strong("Expected:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$expected))),
            tags$tr(tags$td(strong("Expected|Capture:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$expected_given_capture))),
            tags$tr(tags$td(strong("Min/Max:")), tags$td(style = "text-align: right;", sprintf("%.3f / %.3f", row$min_R, row$max_R)))
          )
        )
      )
    )
  })
  
  # Capture Methods Cards - Captured
  output$capture_methods_captured_cards <- renderUI({
    req(rv$dfsr)
    
    summary_data <- exit_methods_summary(rv$dfsr) %>%
      filter(!is.na(exit_method)) %>%
      arrange(desc(expected_given_capture))
    
    if (nrow(summary_data) == 0) {
      return(HTML("<p style='text-align: center; padding: 20px;'>No captured positions found</p>"))
    }
    
    # Create cards for each exit method
    cards <- lapply(1:nrow(summary_data), function(i) {
      row <- summary_data[i, ]
      
      # Color based on mean return
      card_color <- if (row$mean_R > 0) "#d4edda" else "#f8d7da"
      text_color <- if (row$mean_R > 0) "#155724" else "#721c24"
      
      div(
        style = sprintf("background-color: %s; border: 1px solid #ddd; border-radius: 8px; padding: 20px; margin-bottom: 15px;", card_color),
        h4(style = sprintf("margin-top: 0; color: %s;", text_color), as.character(row$exit_method)),
        fluidRow(
          column(3,
            h5("Count & Probability"),
            tags$table(style = "width: 100%; font-size: 16px;",
              tags$tr(tags$td(strong("Count:")), tags$td(style = "text-align: right;", sprintf("%d", row$n))),
              tags$tr(tags$td(strong("Overall Prob:")), tags$td(style = "text-align: right;", sprintf("%.2f%%", row$overall_probability * 100))),
              tags$tr(tags$td(strong("Capture Prob:")), tags$td(style = "text-align: right;", sprintf("%.2f%%", row$capture_probability * 100)))
            )
          ),
          column(3,
            h5("Returns"),
            tags$table(style = "width: 100%; font-size: 16px;",
              tags$tr(tags$td(strong("Mean R:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$mean_R))),
              tags$tr(tags$td(strong("Median R:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$median_R))),
              tags$tr(tags$td(strong("SD:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$sd_R))),
              tags$tr(tags$td(strong("SE:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$se_R)))
            )
          ),
          column(3,
            h5("Expected Values"),
            tags$table(style = "width: 100%; font-size: 16px;",
              tags$tr(tags$td(strong("Expected:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$expected))),
              tags$tr(tags$td(strong("Expected|Capt:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$expected_given_capture))),
              tags$tr(tags$td(strong("IQR:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$iqr_R)))
            )
          ),
          column(3,
            h5("Quantiles"),
            tags$table(style = "width: 100%; font-size: 16px;",
              tags$tr(tags$td(strong("Q05:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$q05_R))),
              tags$tr(tags$td(strong("Q32:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$q32_R))),
              tags$tr(tags$td(strong("Q68:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$q68_R))),
              tags$tr(tags$td(strong("Q95:")), tags$td(style = "text-align: right;", sprintf("%.4f", row$q95_R)))
            )
          )
        )
      )
    })
    
    do.call(tagList, cards)
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
  
  # DQR Models - Update dropdown choices when dqr_fits is loaded
  observe({
    if (!is.null(rv$dqr_fits) && is.list(rv$dqr_fits) && length(rv$dqr_fits) > 0) {
      model_names <- names(rv$dqr_fits)
      if (is.null(model_names)) {
        model_names <- paste0("Model ", seq_along(rv$dqr_fits))
        names(rv$dqr_fits) <- model_names
      }
      
      updateSelectInput(session, "dqr_model_select", 
                       choices = model_names,
                       selected = model_names[1])
    }
  })
  
  # DQR Models - Render model title
  output$dqr_model_title <- renderText({
    req(input$dqr_model_select)
    input$dqr_model_select
  })
  
  # DQR Models - Render model summary
  output$dqr_model_summary <- renderPrint({
    req(rv$dqr_fits, input$dqr_model_select)
    
    if (!is.list(rv$dqr_fits) || length(rv$dqr_fits) == 0) {
      cat("No DQR models found in dqr_fits")
      return(invisible(NULL))
    }
    
    model_names <- names(rv$dqr_fits)
    if (is.null(model_names)) {
      model_names <- paste0("Model ", seq_along(rv$dqr_fits))
    }
    
    # Find the selected model
    selected_idx <- which(model_names == input$dqr_model_select)
    if (length(selected_idx) == 0) {
      cat("Selected model not found")
      return(invisible(NULL))
    }
    
    # Display summary
    summary(rv$dqr_fits[[selected_idx[1]]])
  })
  
  # DQR Models - Render diagnostics
  output$dqr_model_diagnostics <- renderUI({
    req(rv$dqr_fits, input$dqr_model_select)
    
    if (!is.list(rv$dqr_fits) || length(rv$dqr_fits) == 0) {
      return(HTML("<p style='text-align: center; padding: 20px;'>No DQR models found</p>"))
    }
    
    model_names <- names(rv$dqr_fits)
    if (is.null(model_names)) {
      model_names <- paste0("Model ", seq_along(rv$dqr_fits))
    }
    
    selected_idx <- which(model_names == input$dqr_model_select)
    if (length(selected_idx) == 0) {
      return(HTML("<p style='text-align: center; padding: 20px;'>Selected model not found</p>"))
    }
    
    model <- rv$dqr_fits[[selected_idx[1]]]
    
    # Try to compute diagnostics with detailed error handling
    tryCatch({
      # Get actual, predicted, and tau as per user specifications
      actual <- model$y
      predicted <- predict(model)
      tau <- exit_dqr_extract_quantiles(rv$dqr_fits)[selected_idx[1]]
      
      # Validate data
      if (is.null(actual) || is.null(predicted)) {
        stop("Model does not contain y component or predict() returned NULL")
      }
      
      if (length(actual) != length(predicted)) {
        stop(sprintf("Length mismatch: actual (%d) vs predicted (%d)", length(actual), length(predicted)))
      }
      
      # Compute DQR metrics using the new functions
      pinball_loss <- exit_dqr_pinball_loss(actual, predicted, tau)
      pseudo_r2 <- exit_dqr_pseudo_r2(actual, predicted, tau)
      coverage <- exit_dqr_coverage(actual, predicted)
    
      HTML(sprintf(
        "<h4>Diagnostics (τ = %.2f)</h4>
        <table class='table table-bordered' style='font-size: 18px;'>
          <tr><td><strong>Pseudo-R² (Koenker–Machado):</strong></td><td style='text-align: right;'>%.4f</td></tr>
          <tr><td><strong>Pinball Loss:</strong></td><td style='text-align: right;'>%.6f</td></tr>
          <tr><td><strong>Coverage:</strong></td><td style='text-align: right;'>%.4f</td></tr>
        </table>
        <p style='margin-top: 15px; font-size: 14px; color: #666;'>
          <strong>Coverage:</strong> Proportion of actual values ≤ predicted quantile (should be close to τ)<br>
          <strong>Pinball Loss:</strong> Lower is better (quantile-specific loss function)<br>
          <strong>Pseudo-R²:</strong> Higher is better (1 = perfect, 0 = no better than constant)
        </p>",
        tau, pseudo_r2, pinball_loss, coverage
      ))
    }, error = function(e) {
      HTML(sprintf(
        "<div style='padding: 20px;'>
          <h4>Diagnostics</h4>
          <div style='background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 15px; margin-top: 10px;'>
            <p style='color: #721c24; margin: 0;'><strong>⚠ Error:</strong> %s</p>
          </div>
          <div style='margin-top: 15px; padding: 10px; background-color: #f0f0f0; border-radius: 4px;'>
            <p style='margin: 0; font-size: 14px;'><strong>Troubleshooting:</strong></p>
            <ul style='margin-bottom: 0; font-size: 13px;'>
              <li>Ensure <code>dqr_fits</code> contains fitted quantreg models with y component</li>
              <li>Check that <code>exit_dqr_*</code> functions are loaded from R/dqr.R</li>
              <li>Verify model structure: Model class: <code>%s</code></li>
            </ul>
          </div>
        </div>",
        e$message, paste(class(model), collapse = ", ")
      ))
    })
  })
  
  # DQR Models - Render main path plot
  output$dqr_main_path_plot <- renderPlot({
    req(rv$dqr_fits, input$dqr_model_select)
    
    if (!is.list(rv$dqr_fits) || length(rv$dqr_fits) == 0) {
      return(NULL)
    }
    
    # Load df_train if available
    if (!exists("df_train", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    train_df <- get("df_train", envir = .GlobalEnv)
    
    model_names <- names(rv$dqr_fits)
    if (is.null(model_names)) {
      model_names <- paste0("Model ", seq_along(rv$dqr_fits))
    }
    
    selected_idx <- which(model_names == input$dqr_model_select)
    if (length(selected_idx) == 0) {
      return(NULL)
    }
    
    model <- rv$dqr_fits[[selected_idx[1]]]
    tau <- model$tau
    
    tryCatch({
      # Enrich df_train with feature engineering
      train_df_enriched <- train_df
      
      # Compute predictions and add to train_df
      train_df_plot <- train_df_enriched %>%
        mutate(
          qhat = predict(model, train_df_enriched),
          exit_flag = S >= qhat
        )
      
      ggplot(train_df_plot, aes(x = t, y = S)) +
        geom_line(aes(color = factor(position_id)), linewidth = 0.7, alpha = 0.6) +
        geom_line(aes(y = qhat), color = "black", linewidth = 0.9, linetype = "dashed") +
        geom_point(data = subset(train_df_plot, exit_flag),
                   color = "magenta", size = 1.8, alpha = 0.8) +
        labs(
          title = paste0("Quantile Regression Upper Envelope (τ = ", tau, ")"),
          subtitle = "Dashed = fitted quantile; magenta dots = exit triggers",
          x = "Time (t)", y = "Normalised Price (S)"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Error generating plot:\n", e$message),
                 size = 5, color = "red") +
        theme_void()
    })
  })
  
  # DQR Models - Render calibration plot
  output$dqr_calibration_plot <- renderPlot({
    req(rv$dqr_fits, input$dqr_model_select)
    
    if (!is.list(rv$dqr_fits) || length(rv$dqr_fits) == 0) {
      return(NULL)
    }
    
    # Load df_train if available
    if (!exists("df_train", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    train_df <- get("df_train", envir = .GlobalEnv)
    
    model_names <- names(rv$dqr_fits)
    if (is.null(model_names)) {
      model_names <- paste0("Model ", seq_along(rv$dqr_fits))
    }
    
    selected_idx <- which(model_names == input$dqr_model_select)
    if (length(selected_idx) == 0) {
      return(NULL)
    }
    
    model <- rv$dqr_fits[[selected_idx[1]]]
    tau <- model$tau
    
    tryCatch({
      # Enrich df_train with feature engineering
      train_df_enriched <- train_df
      
      # Compute predictions
      pred <- predict(model, train_df_enriched)
      actual <- train_df_enriched$S
      
      ggplot(data.frame(pred = pred, actual = actual), aes(x = pred, y = actual)) +
        geom_point(alpha = 0.25, color = "gray40") +
        geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8) +
        labs(
          title = paste0("Predicted vs Actual (τ = ", tau, ")"),
          subtitle = "Red line = perfect calibration",
          x = "Predicted Quantile", y = "Actual S"
        ) +
        theme_minimal(base_size = 12)
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Error generating plot:\n", e$message),
                 size = 5, color = "red") +
        theme_void()
    })
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
      
      if (exists("dqr_fits", envir = .GlobalEnv)) {
        dqr_fits <- get("dqr_fits", envir = .GlobalEnv)
        cat("✓ dqr_fits found:", length(dqr_fits), "models\n")
      } else {
        cat("✗ dqr_fits not found\n")
      }
      
      cat("\nNote: Run the trade_simulation.R script to generate the required data objects.")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
