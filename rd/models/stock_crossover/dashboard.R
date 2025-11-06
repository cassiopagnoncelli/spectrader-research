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
        ),
        fluidRow(
          box(
            width = 6,
            title = "Returns Distribution Summary",
            status = "info",
            plotOutput("overview_distribution", height = 300)
          ),
          box(
            width = 6,
            title = "Portfolio Growth",
            status = "info",
            plotOutput("overview_kelly", height = 300)
          )
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
                         "Take Profit (Closed)" = "take_profit", 
                         "Open Positions" = "open"),
              selected = "all"
            ),
            actionButton("refresh_samples", "Refresh Samples", icon = icon("refresh"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Take Profit Position Exits (Quantile Regression)",
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
            title = "Take Profit (Closed) Positions",
            status = "success",
            solidHeader = TRUE,
            htmlOutput("accuracy_take_profit_metrics"),
            hr(),
            htmlOutput("accuracy_take_profit_dist")
          ),
          box(
            width = 4,
            title = "Open Positions",
            status = "warning",
            solidHeader = TRUE,
            htmlOutput("accuracy_open_metrics"),
            hr(),
            htmlOutput("accuracy_open_dist")
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
                       choices = c("Overall", "Take Profit", "Open Positions"), 
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
    current_chart_index = 1
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
    if (input$position_filter == "take_profit") {
      # Take profit (closed) positions only
      dfsr_filtered <- rv$dfsr %>% filter(t < max(t, na.rm = TRUE))
    } else if (input$position_filter == "open") {
      # Open positions only
      dfsr_filtered <- rv$dfsr %>% filter(t == max(t, na.rm = TRUE))
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
    n_closed <- sum(rv$dfsr$t < max(rv$dfsr$t, na.rm = TRUE))
    n_open <- n_trades - n_closed
    
    HTML(sprintf(
      "<table class='table table-bordered'>
        <tr><td><strong>Total Signals:</strong></td><td>%d</td></tr>
        <tr><td><strong>Closed Positions:</strong></td><td>%d</td></tr>
        <tr><td><strong>Open Positions:</strong></td><td>%d</td></tr>
      </table>",
      n_trades, n_closed, n_open
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
  
  # Signal Accuracy - Take Profit
  output$accuracy_take_profit_metrics <- renderUI({
    req(rv$accuracy)
    
    accuracy_take_profit <- rv$accuracy %>% filter(t < max(t))
    metrics <- exit_metrics(accuracy_take_profit, input$side)
    
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
  
  output$accuracy_take_profit_dist <- renderUI({
    req(rv$accuracy)
    
    accuracy_take_profit <- rv$accuracy %>% filter(t < max(t))
    dist <- analyse_distribution(accuracy_take_profit$R, groups = c(0))
    
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
  
  # Signal Accuracy - Open Positions
  output$accuracy_open_metrics <- renderUI({
    req(rv$accuracy)
    
    accuracy_open <- rv$accuracy %>% filter(t == max(t))
    metrics <- exit_metrics(accuracy_open, input$side)
    
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
  
  output$accuracy_open_dist <- renderUI({
    req(rv$accuracy)
    
    accuracy_open <- rv$accuracy %>% filter(t == max(t))
    dist <- analyse_distribution(accuracy_open$R, groups = c(0))
    
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
    if (input$breakdown_category == "Take Profit") {
      accuracy_data %>% filter(t < max(t))
    } else if (input$breakdown_category == "Open Positions") {
      accuracy_data %>% filter(t == max(t))
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
