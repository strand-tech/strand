library(ggplot2)
library(strand)
library(dplyr)
library(tidyr)
library(DT)

server <- function(input, output, session) {

  # Store the sim result data in a reactiveValues object.
  values <- reactiveValues()
  
  #adding a reactive value
  ID <- reactive({
    
    input$positionSummaryTable_rows_selected[1]
      
    
  })
 
  observeEvent(values$sim_result, {
    
    output$plot_1 <- renderPlot(
      values$sim_result$plotPerformance()
    )
    
    output$plot_2 <- renderPlot(
      values$sim_result$plotMarketValue()
    )
    
    # TODO dynamically select exposure plot in_vars based on config file
    output$plot_3 <- renderPlot(
      values$sim_result$plotCategoryExposure(in_var = "category_1")
    )

    output$plot_4 <- renderPlot(
      values$sim_result$plotFactorExposure(in_var = c("factor_1", "factor_2", "factor_3", "factor_4"))
    )
    
    output$plot_5 <- renderPlot(
      values$sim_result$plotNumPositions()
    )
    output$overallStatsTable <- renderTable(values$sim_result$overallStatsDf(), align = "lrr")

    summary_data <- values$sim_result$getSingleStrategySummaryDf("joint", include_zero_row = FALSE)
    perf_stats <- summary_data %>%
      transmute(
        Date = .data$sim_date,
        "GMV ($mm)" = round(.data$end_gmv / 1e6),
        "Gross P&L ($)" = round(.data$gross_pnl),
        "Net P&L ($)" = round(.data$net_pnl),
        "Cumulative net return (%)" = round(100 * .data$net_cum_ret, digits = 2),
        "Turnover ($)" = round(.data$market_fill_gmv),
        "Trade Costs ($)" = round(.data$trade_costs),
        "Financing Costs ($)" = round(.data$financing_costs))
    
    output$perfTable <- renderDT(perf_stats,
                                 rownames = FALSE)
    
    market_value_stats <-  summary_data %>%
      transmute(
        Date = .data$sim_date,
        "Start LMV ($mm)" = round(.data$start_lmv / 1e6),
        "Start SMV ($mm)" = round(.data$start_smv / 1e6),
        "End LMV ($mm)" = round(.data$end_lmv / 1e6),
        "End SMV ($mm)" = round(.data$end_smv / 1e6),
        "End NMV ($mm)" = round(.data$end_nmv / 1e6),
        "End GMV ($mm)" = round(.data$end_gmv / 1e6),
        "Num long" = .data$end_num_long,
        "Num short" = .data$end_num_short)
    

    output$marketValueTable <- renderDT(market_value_stats,
                                        rownames = FALSE)

    pos_summary <- values$sim_result$getPositionSummary(strategy_name = "joint") %>%
      left_join(values$sim_obj$getSecurityReference()[c("id","symbol")], by = "id") %>%
      ungroup() %>%
      select(.data$symbol, .data$gross_pnl, .data$net_pnl,
             .data$average_market_value,
             .data$total_trading, .data$trade_costs, .data$financing_costs,
             .data$days_in_portfolio) %>%
      arrange(.data$gross_pnl)
    
    output$positionSummaryTable <- renderDT(pos_summary,
                                            rownames = FALSE, selection = 'single')

  })
  
  observeEvent(input$holdingsDate, {
    
    if (!is.null(values$sim_obj)) {
      
      output$holdingsTable <- renderDT(
        
        left_join(values$sim_result$getSimDetail(as.character(input$holdingsDate), strategy_name = "joint"),
                  values$sim_obj$getSecurityReference(), by = "id") %>%
          select("symbol", "shares", "order_shares", "fill_shares", "end_shares", "end_nmv",
                 "gross_pnl", "trade_costs", "financing_costs") %>%
          mutate(end_nmv = round(end_nmv),
                 gross_pnl = round(gross_pnl, digits = 2),
                 trade_costs = round(trade_costs, digits = 2),
                 financing_costs = round(financing_costs, digits = 2))
      )
    }
  })
  
  
  #making the holdings data table
  output$holdings <- renderDT(
    
    
    #uses similar logic to above dataframe
    values$sim_result$getSimDetail(strategy_name = "joint", security_id = ID()$id)  %>%
      select("sim_date", "shares", "order_shares", "fill_shares", "end_shares", "end_nmv",
                 "gross_pnl", "trade_costs", "financing_costs") %>%
      mutate(end_nmv = round(end_nmv),
      gross_pnl = round(gross_pnl, digits = 2),
      trade_costs = round(trade_costs, digits = 2),
      financing_costs = round(financing_costs, digits = 2),
      symbol = ID()$symbol) 
  )
  
  
  output$selectedrow <- renderText({
    
    paste0(rownames())
    
  })
  

  observeEvent(input$runSim, {
    
    if (input$runSim %in% 0) {
      return(NULL)
    }
    
    # Create a Progress object
    progress <- Progress$new()
    progress$set(message = "Running simulation", value = 0)
    
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }
    
    # Load yaml from config textAreaInput ui element
    config <- yaml::read_yaml(text = input$config)

    # For debugging:
    #
    # config <- yaml::yaml.load_file("strategy_config.yaml")
    # config$from <- as.Date("2019-01-03")
    # config$to <- as.Date("2019-01-07")
    
    # Supplement with other ui settings
    config$from <- input$startDate
    config$to <- input$endDate
    
    # Create and run the sim
    tryCatch({
      
      data(sample_inputs)
      data(sample_pricing)
      data(sample_secref)
      
      sim <- Simulation$new(config,
                            raw_input_data = sample_inputs,
                            raw_pricing_data = sample_pricing,
                            security_reference_data = sample_secref)
      sim$setShinyCallback(updateProgress)
      res <- sim$run()

      values$sim_result <- res
      values$sim_obj <- sim
      
      updateTabsetPanel(session,
                        "top",
                        selected = "Results"
      )
    },
    error = function(e) { showNotification(e$message, type = "error", duration = NULL)}
    )
 
  })
}
