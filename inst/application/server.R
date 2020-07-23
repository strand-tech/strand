library(ggplot2)
library(strand)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)

server <- function(input, output, session) {

  # Store the sim result data in a reactiveValues object.
  values <- reactiveValues()
  
  # used in selectedHoldingRows, and plotAndTable
  # creates a df of the final positions table
  position_summary <- eventReactive(values$sim_result, {
    
    values$sim_result$getPositionSummary(strategy_name = "joint") %>%
      left_join(values$sim_obj$getSecurityReference()[c("id","symbol")], by = "id") %>%
      ungroup() %>%
      select(.data$symbol, .data$gross_pnl, .data$net_pnl,
             .data$average_market_value,
             .data$total_trading, .data$trade_costs, .data$financing_costs,
             .data$days_in_portfolio) %>%
      arrange(.data$gross_pnl)
    
  })
  
  # produces a name value list that stores
  # 1 maximum net pnl, 2 minimum net pnl
  # 3 maximum alpha, 4 minimum alpha, 5 average alpha
  # 6 quartiles of market fill nmv
  # across all positions and dates
  position_max_min <- eventReactive(values$sim_result, {
    
    simulation_summary <- values$sim_result$getSimDetail(strategy_name = "joint") %>%
      group_by(id) %>%
      mutate(net_pnl = cumsum(net_pnl),
             net_pnl = round(net_pnl, digits = 0),
             alpha_1 = round(alpha_1, digits = 2)) %>%
      ungroup() 
    
    trade_summary <- simulation_summary %>%
      filter(market_fill_nmv != 0)
    
    list("holdings_max" = max(simulation_summary$net_pnl), 
         "holdings_min" = min(simulation_summary$net_pnl),
         "alpha_max" = max(simulation_summary$alpha_1),
         "alpha_min" = min(simulation_summary$alpha_1), 
         "alpha_normalized_average" = (mean(simulation_summary$alpha_1) - min(simulation_summary$alpha_1)) / 
           (max(simulation_summary$alpha_1) -  min(simulation_summary$alpha_1)),
         "market_fill_quartile" = quantile(abs(trade_summary$market_fill_nmv)))
  })
  
  # creates a data frame filled with the day by day of selected holdings
  selected_holding_rows <- eventReactive(input$positionSummaryTable_rows_selected, {
    
    # gets the ID's for the selected holdings
    selected_sec_ref <- values$sim_obj$getSecurityReference() %>%
      as.data.frame() %>%
      filter(symbol %in% position_summary()$symbol[input$positionSummaryTable_rows_selected]) %>%
      select("id", "symbol") %>%
      as.data.frame()

    # save_detail_columns: alpha_1 
    selected_holdings <- 
      left_join(values$sim_result$getSimDetail(strategy_name = "joint", 
                                               security_id = selected_sec_ref$id), 
                selected_sec_ref, by = "id") %>%
      select("sim_date", "symbol", "net_pnl", "shares", "alpha_1","order_shares", "fill_shares", 
             "market_fill_nmv", "end_shares", "end_nmv", "gross_pnl", "trade_costs", "financing_costs") %>%
      mutate(end_nmv = round(end_nmv),
             gross_pnl = round(gross_pnl, digits = 2),
             trade_costs = round(trade_costs, digits = 2),
             financing_costs = round(financing_costs, digits = 2),
             net_pnl = cumsum(net_pnl),
             net_pnl = round(net_pnl, digits = 0),
             gross_pnl = cumsum(gross_pnl),
             gross_pnl = round(gross_pnl, digits = 0),
             alpha_1 = round(alpha_1, digits = 2))
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

  
    output$positionSummaryTable <- renderDT(position_summary(),
                                            rownames = FALSE,
                                            selection = 'single')

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
  
  
  # making the seleted holdings data table
  output$selectedHoldings <- renderDT(
    
    selected_holding_rows(),
    rownames = FALSE,
    selection = "none"
    
  )
  
  # change back to renderPlot
  output$selectedHoldingsPlot <- renderPlotly({
    
    # using market fikl nmv currenty
    # can I normalize the shapes automatically somehow
    selection_plot <- selected_holding_rows() %>%
    #   select("sim_date", "symbol" , "fill_shares", "market_fill_nmv", "net_pnl", "alpha_1") 
    # %>%
      mutate(buy_sell = ifelse(fill_shares > 0, 'triangle-up',
                                  ifelse(fill_shares < 0, 'triangle-down', '0')),
             magnitude = trunc(10 + 20 * log(10 * abs(market_fill_nmv / (max(abs(market_fill_nmv)) - min(abs(market_fill_nmv)))) + 1, 10)))
              # round(10 + 10 * log(abs(market_fill_nmv) + 1, 10), digits = 0)) # adjusted truncated constant
  
    holdings_plot <- plot_ly(selection_plot, x = ~sim_date, y = ~net_pnl, 
                                type = 'scatter', mode = 'lines+markers',
                                line = list(
                                  color = 'black',
                                  opacity = 0.2
                                ),
                                marker = list(
                                  opacity = 1,
                                  color = ~alpha_1,
                                  line = list(
                                    color = 'black'
                                  ),
                                  symbol = ~factor(buy_sell),
                                  size = ~magnitude,
                                  cmin = position_max_min()$alpha_min,
                                  cmax = position_max_min()$alpha_max,
                                  colorscale = list(c(0, "rgb(178, 34, 34)"), 
                                                    list(position_max_min()$alpha_normalized_average, "rgb(255, 255, 0)"),
                                                    list(1, "rgb(50, 205, 50)")), 
                                  colorbar=list(
                                    title='Symbol: 
                                           \n▲ Buy      
                                           \n▼ Sell
                                           \nAlpha:'
                                  ),
                                  name = 'buy_sell'),
                                hoverinfo = "text",
                                hoverlabel = list(
                                  bgcolor = 'white'
                                ),
                                text = paste("Date: ", selection_plot$sim_date, 
                                             '<br>P&L: ', selection_plot$net_pnl,
                                             '<br>alpha: ', selection_plot$alpha_1,
                                             '<br>Shares: ', selection_plot$shares,
                                             '<br>Order: ', selection_plot$order_shares,
                                             '<br>Fill: ', selection_plot$fill_shares,
                                             '<br>End Shares: ', selection_plot$end_shares)) %>%
      layout(
        title = list(
          text = paste("Cumulative Profit and Loss of", selection_plot$symbol[1]),
          x = 0.03
        ),
        # xaxis = list(
        #   showline = TRUE, linewidth = 1, linecolor='black', mirror = TRUE
        # ),
        yaxis = list(
          title = "Net P&L",
          range = c(position_max_min()$holdings_min, position_max_min()$holdings_max),
          fixedrange = TRUE 
          # showline = TRUE, linewidth = 1, linecolor='black', mirror = TRUE
        ))
                                
    alpha_plot <- plot_ly(selection_plot, x = ~sim_date, y = ~alpha_1, 
                          type = "scatter", mode = "lines",
                          line = list(
                            color = "0000FF"
                            )) %>%
      layout(
        yaxis = list(
          title = "Alpha",
          range = c(position_max_min()$alpha_min, position_max_min()$alpha_max),
          fixedrange = TRUE, showzeroline = FALSE,
          showline = TRUE, linewidth = 1, linecolor='black', mirror = TRUE
        ),
        xaxis = list(
          title = "Date",
          showline = TRUE, linewidth = 1, linecolor='black', mirror = TRUE
        ),
        showlegend = FALSE) 
    
    multi_layed_plot <- subplot(holdings_plot, alpha_plot, 
                                nrows = 2, shareX = TRUE, titleY = TRUE, heights = c(0.80, 0.20))
      

  })
  
  # outputs the click information from the graph
  # output$clickInfo <- renderText({
  # 
  #   clicked_point <- nearPoints(selected_holding_rows(), input$plot_click,
  #                        xvar = "sim_date", yvar = "net_pnl", maxpoints = 1, threshold = 10)
  #   
  #   if(nrow(clicked_point) == 0){
  #     
  #     paste0("Click on the graph for information: ")
  #     
  #   } else {
  #     
  #     
  #     # change colums by refernce to name
  #     paste0("Selection Information \nSymbol: ", clicked_point$symbol,
  #            "\nDate: ", as.Date(as.numeric(clicked_point$sim_date), origin = "1970-01-01"),
  #            "\nPnL: ", clicked_point$net_pnl, 
  #            "\nAlpha: ", clicked_point$alpha_1, 
  #            "\nShares: ", clicked_point$shares,
  #            "\nOrdered Shares: ", clicked_point$order_shares, 
  #            "\nFilled Shares: ", clicked_point$fill_shares, 
  #            "\nEnd Shares: ", clicked_point$end_shares)
  #   }
  # })
  
  
  observeEvent(position_summary(),{

    output$selectedPlotAndTable <- renderUI({

      if(is.null(input$positionSummaryTable_rows_selected)){
        fluidRow(
          column(
            8,
            align = "center",
            offset = 2,
            p(strong("Select rows for day by day information"))
          )
        )
      } else {
        fluidRow(
          # column(
            # 12,
            # change back to plot output
            # plotlyOutput('selectedHoldingsPlot', click = "plot_click")
            plotlyOutput('selectedHoldingsPlot', height = "800px"),
          # ),
          # column(
          #   2,
          #   verbatimTextOutput("clickInfo")
          # ),
          br(),
          DT::dataTableOutput('selectedHoldings')
        )
      }
    })

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
