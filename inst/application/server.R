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
             magnitude = trunc(1 + log(abs(market_fill_nmv)))) # adjusted truncated constant
    
             # order_size = round(1 + log(abs(fill_shares), 10), digits = 0))

    # moved color to main graph
    # gg_plot <- ggplot(data = selection_plot, aes(x = sim_date, y = net_pnl, color = alpha_1)) +
      # line plot of the net_pnl data
      # ask about scales
      # geom_line() +
      # creates line color gradient based on alpha truncated to -1 to 1 range
      # scale_colour_gradient2(low = "red", mid = "yellow", 
      #                        high = "seagreen", limits = c(-1, 1), oob = scales::squish) +
      # dot plot of the fill data
      # geom_point(data = subset(selection_plot, !is.na(buy_sell)),
      #             aes(shape = factor(buy_sell),
      #                 size = trunc(1 + log(abs(market_fill_nmv))))) +
      # creates the shape of the fill
      # scale_shape_manual(values = c(Buy = 24, Sell = 25)) + 
    #   ylab("Net P&L") + xlab("Date") + ggtitle("Cumulative Profit and Loss") +
    #   theme_light() + 
    #   theme(
    #     plot.background = element_rect(fill = NA, colour = NA),
    #     plot.title = element_text(size = 18),
    #     axis.text = element_text(size = 10),
    #     axis.text.x = element_text(angle = 0),
    #     legend.position = "bottom",
    #     legend.box = "horizontal") +
    #   labs(shape = "Orders", color = "Symbol", size = "Fill Magnitude") + 
    #   guides(fill =FALSE,
    #          shape = guide_legend(order = 1),
    #          color = guide_legend(order = 2),
    #          size = guide_legend(order = 3))
    # 
    # ggplotly(gg_plot)
    
    interactive_plot <- plot_ly(selection_plot, x = ~sim_date, y = ~net_pnl, 
                                type = 'scatter', mode = 'lines', color = "alpha_1",
                                text = ~paste("Date: ", sim_date, 
                                              '<br>P&L: ', net_pnl,
                                              '<br>alpha: ', alpha_1,
                                              '<br>Shares: ', shares,
                                              '<br>Order: ', order_shares,
                                              '<br>Fill: ', fill_shares,
                                              '<br>End Shares: ', end_shares))  %>%
    #   filter(!is.na(buy_sell)) %>%
      add_trace(
        mode = "markers", symbol = ~factor(buy_sell), marker = list( 
          size = ~magnitude + 2)
      )
      

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
            plotlyOutput('selectedHoldingsPlot'),
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
