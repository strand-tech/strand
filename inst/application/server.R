library(ggplot2)
library(strand)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
<<<<<<< HEAD
library(shinyFiles)
=======
>>>>>>> upstream/master

server <- function(input, output, session) {

  # Store the sim result data in a reactiveValues object.
  values <- reactiveValues()
  
<<<<<<< HEAD
  
  # volumes is the root of the directory
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "simDir", roots = volumes, session = session, restrictions = system.file(package = "base"))
  
  # Displays directory path
  output$directory <- renderText({
    parseDirPath(volumes, input$simDir)
  })
  
  
  # Creates a warning message if the user did not set save_detail_columns: in_var in their simulation
  observeEvent(values$sim_obj, {
    if(!config_values()$in_var %in% colnames(values$sim_obj$getSimDetail())) {
    showNotification(p(strong("Warning:"), "no in_var present, some features will be disabled"), type = "warning", duration = 30)
    }
  })
  
  
  
  # Creates a data.frame of the final positions
  position_summary <- eventReactive(values$sim_obj, {
            # result to obj
    values$sim_obj$getPositionSummary(strategy_name = "joint") %>%
=======
  # Creates a data.frame of the final positions
  position_summary <- eventReactive(values$sim_result, {
    
    values$sim_result$getPositionSummary(strategy_name = "joint") %>%
>>>>>>> upstream/master
      left_join(values$sim_obj$getSecurityReference()[c("id","symbol")], by = "id") %>%
      ungroup() %>%
      select(.data$symbol, .data$gross_pnl, .data$net_pnl,
             .data$average_market_value,
             .data$total_trading, .data$trade_costs, .data$financing_costs,
             .data$days_in_portfolio) %>%
      arrange(.data$gross_pnl)
<<<<<<< HEAD
    
  })
  

  # eventReactive that returns the name of the strategy, the in_var, and the factors  
  config_values <- eventReactive(values$sim_obj, {
    # browser()
    strategy_name <- values$sim_obj$getConfig()$getStrategyNames()
    
    # returns the config in_var
    in_var <- values$sim_obj$getConfig()$getStrategyConfig(strategy_name, "in_var")
    
    # creates vectors of the categories and factors to track
    config_category <- values$sim_obj$getConfig()$getConfig("simulator")$calculate_exposures$category_vars %>%
      as.vector()
    config_factors <- values$sim_obj$getConfig()$getConfig("simulator")$calculate_exposures$factor_vars %>%
      as.vector()
    
    list(
      "in_var" = in_var,
      "config_category" = config_category,
      "config_factors" = config_factors
=======
    
  })
  
  # Produces a name value list that stores
  #   maximum alpha (alpha_max)
  #   minimum alpha (alpha_min)
  #   list of markey fill quarties (market_fill_quartile[[%]])
  alpha_range_and_size <- eventReactive(values$sim_result, {
    
    # Creates a data.frame of all positions throughout the simulation
    # Rounds alpha to get full alpha range
    # Includes days when no trades happen to compare alpha throughout simulation
    simulation_summary <- values$sim_result$getSimDetail(strategy_name = "joint") %>%
      group_by(id) %>%
      mutate(alpha_1 = round(alpha_1, digits = 2)) %>%
      ungroup() 
    
    # Creates a data frame of all trades throughout the simulation
    # Removes all days when no trades happen
    trade_summary <- simulation_summary %>%
      filter(market_fill_nmv != 0)
    
    list("alpha_max" = max(simulation_summary$alpha_1),
         "alpha_min" = min(simulation_summary$alpha_1), 
         # Normalizes the alpha, the mean is the average, not necessarily zero 
         # Used for plotly gradient
         "alpha_normalized_average" = (mean(simulation_summary$alpha_1) - min(simulation_summary$alpha_1)) / 
           (max(simulation_summary$alpha_1) -  min(simulation_summary$alpha_1)),
         # Uses trade summary instead of simulation summary for trade quantile
         "market_fill_quartile" = quantile(abs(trade_summary$market_fill_nmv)))
  })
  
  # creates a data frame filled with the day by day of selected position
  selected_holding_row <- eventReactive(input$positionSummaryTable_rows_selected, {
    
    # Gets the ID of the selected holding
    # Returns a data frame of the position's id and symbol
    selected_sec_ref <- values$sim_obj$getSecurityReference() %>%
      as.data.frame() %>%
      filter(symbol %in% position_summary()$symbol[input$positionSummaryTable_rows_selected]) %>%
      select("id", "symbol") %>%
      as.data.frame()

    # Add 'save_detail_columns: alpha_1' under simulation 
    # Uses id and symbol to get simulation details of the position
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
             alpha_1 = round(alpha_1, digits = 2),
             market_fill_nmv = round(market_fill_nmv, digits = 0))
  })
 
  observeEvent(values$sim_result, {
    
    output$plot_1 <- renderPlotly(
      ggplotly(values$sim_result$plotPerformance(), tooltip = FALSE) 
    )
    
    output$plot_2 <- renderPlotly(
      ggplotly(values$sim_result$plotMarketValue(), tooltip = FALSE)
>>>>>>> upstream/master
    )
  })
  
  # Produces a name value list that stores
  #   maximum alpha (in_var_max)
  #   minimum alpha (in_var_min)
  #   list of markey fill quarties (market_fill_quartile[[%]])
  plot_aesthetics <- eventReactive(config_values(), {
    
    
    # Creates a data.frame of all positions throughout the simulation
    # Rounds alpha to get full alpha range
    # Includes days when no trades happen to compare alpha throughout simulation
    in_var_summary <- values$sim_obj$getSimDetail(strategy_name = "joint") %>%
      select(!!config_values()$in_var, market_fill_nmv) %>%
      mutate(
        !!config_values()$in_var := round(get(config_values()$in_var), digits = 2)
      )
    
<<<<<<< HEAD
    # Creates a data frame of all trades throughout the simulation
    # Removes all days when no trades happen
    trade_summary <- in_var_summary %>%
      select(market_fill_nmv) %>%
      filter(market_fill_nmv != 0)
    
    in_var_max <- max(in_var_summary[[config_values()$in_var]])
    in_var_min <- min(in_var_summary[[config_values()$in_var]])
  
    
    list("in_var_max" = in_var_max,
         "in_var_min" = in_var_min, 
         # Normalizes the alpha, the mean is the average, not necessarily zero 
         # Used for plotly gradient
         "in_var_normalized_average" = (mean(in_var_summary[[config_values()$in_var]]) - in_var_min) / 
           (in_var_max - in_var_min),
         # Uses trade summary instead of simulation summary for trade quantile
         "market_fill_quartile" = quantile(abs(trade_summary$market_fill_nmv)))
  })
  
  # creates a data frame filled with the day by day of selected position
  selected_holding_row <- eventReactive(input$positionSummaryTable_rows_selected, {
    # browser()
    # Gets the ID of the selected holding
    # Returns a data frame of the position's id and symbol
    selected_sec_ref <- values$sim_obj$getSecurityReference() %>%
      as.data.frame() %>%
      filter(symbol %in% position_summary()$symbol[input$positionSummaryTable_rows_selected]) %>%
      select("id", "symbol") %>%
      as.data.frame()

    # Add 'save_detail_columns: alpha_1' under simulation 
    # Uses id and symbol to get simulation details of the position
    selected_holdings <- left_join(values$sim_obj$getSimDetail(strategy_name = "joint", 
                                               security_id = selected_sec_ref$id), 
                selected_sec_ref, by = "id") %>%
      select("sim_date", "symbol", "net_pnl", "shares", !!config_values()$in_var, "order_shares", "fill_shares", 
             "market_fill_nmv", "end_shares", "end_nmv", "gross_pnl", "trade_costs", "financing_costs") %>%
      mutate(end_nmv = round(end_nmv),
             gross_pnl = round(gross_pnl, digits = 2),
             trade_costs = round(trade_costs, digits = 2),
             financing_costs = round(financing_costs, digits = 2),
             net_pnl = cumsum(net_pnl),
             net_pnl = round(net_pnl, digits = 0),
             gross_pnl = cumsum(gross_pnl),
             gross_pnl = round(gross_pnl, digits = 0),
           !!config_values()$in_var := round(get(config_values()$in_var), digits = 2),
             market_fill_nmv = round(market_fill_nmv, digits = 0))
  })
 
                      
  # changed from values$sim_obj
  observeEvent(config_values(), {
    output$plot_1 <- renderPlotly(
      ggplotly(values$sim_obj$plotPerformance(), tooltip = FALSE) 
    )

    output$plot_2 <- renderPlotly(
      ggplotly(values$sim_obj$plotMarketValue(), tooltip = FALSE)
    )
    
    output$plot_3s <- renderUI({
      if(!is.null(config_values()$config_category)){
        category_plot_list <- lapply(1:length(config_values()$config_category), function(i) {
          cat_plot_name <- paste("cat_plot_", i, sep = "")
          plotlyOutput(cat_plot_name)
        })
        do.call(tagList, category_plot_list)
      } else {
        fluidRow(
          column(
            8,
            align = "center",
            offset = 2,
            p(strong("You're not currently tracking any category exposures."))
          )
        )
      }
    })
    
    output$factor_exposure <- renderUI({
      if(!is.null(config_values()$config_factors)){
        output$plot_4 <- renderPlotly(
          ggplotly(values$sim_obj$plotFactorExposure(in_var = config_values()$config_factors), tooltip = FALSE)
        )
        plotlyOutput('plot_4')
      } else {
        fluidRow(
          column(
            8,
            align = "center",
            offset = 2,
            p(strong("You're not currently tracking any factor exposures."))
          )
        )
      }
    })
    
    output$plot_5 <- renderPlotly(
      ggplotly(values$sim_obj$plotNumPositions(), tooltip = FALSE)
=======
    # TODO dynamically select exposure plot in_vars based on config file
    output$plot_3 <- renderPlotly(
      ggplotly(values$sim_result$plotCategoryExposure(in_var = "category_1"), tooltip = FALSE)
    )

    output$plot_4 <- renderPlotly(
      ggplotly(values$sim_result$plotFactorExposure(in_var = c("factor_1", "factor_2", "factor_3", "factor_4")), tooltip = FALSE)
    )
    
    output$plot_5 <- renderPlotly(
      ggplotly(values$sim_result$plotNumPositions(), tooltip = FALSE)
>>>>>>> upstream/master
    )
    output$overallStatsTable <- renderTable(values$sim_obj$overallStatsDf(), align = "lrr")
                          
    summary_data <- values$sim_obj$getSingleStrategySummaryDf("joint", include_zero_row = FALSE)
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
  
  # generates multple category exposure plots
  observeEvent(config_values()$config_category, {
      lapply(1:length(config_values()$config_category), function(i){
        output[[paste("cat_plot_", i, sep = "")]] <- renderPlotly({
          ggplotly(values$sim_obj$plotCategoryExposure(in_var = config_values()$config_category[[i]]), tooltip = FALSE)
        })
      })
  })
  
  observeEvent(input$holdingsDate, {
    if (!is.null(values$sim_obj)) {
      output$holdingsTable <- renderDT(
        left_join(values$sim_obj$getSimDetail(as.character(input$holdingsDate), strategy_name = "joint"),
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
  
  
<<<<<<< HEAD
  # makes the seleted holdings data table
  output$selectedHolding <- renderDT(
=======
  # making the seleted holdings data table
  output$selectedHolding <- renderDT(
    
    selected_holding_row(),
    rownames = FALSE,
    selection = "none"
    
  )
  
  # creates the three plotly plots for the selected holding
  # 1st trace is net market value
  # 2nd trace is end nmv
  # 2nd plot is alpha
  # 3rd plot is subplot combination of the both plots
  output$selectedHoldingPlot <- renderPlotly({
    
    # Adds columns to the selected holding df for the buy sell symbol and the size of the trades
    selection_plot <- selected_holding_row() %>%
      mutate(buy_sell = ifelse(fill_shares > 0, 'triangle-up',
                                  ifelse(fill_shares < 0, 'triangle-down', '0')),
             magnitude = ifelse(market_fill_nmv == 0, 5,
                                 ifelse(abs(market_fill_nmv) > alpha_range_and_size()$market_fill_quartile[["50%"]],
                                 ifelse(abs(market_fill_nmv) > alpha_range_and_size()$market_fill_quartile[["75%"]], 30, 25),
                                 ifelse(abs(market_fill_nmv) > alpha_range_and_size()$market_fill_quartile[["25%"]], 20, 15))))
    
    # Creates one plot with two traces for the PNL and NMV
    holdings_plot <- plot_ly() %>%
      # PNL formatting
      add_trace(
        # Visible when plotly is rendered
        visible = TRUE,
        x = selection_plot$sim_date,
        y = selection_plot$net_pnl, 
        type = 'scatter', mode = 'lines+markers',
        line = list(
          color = 'black',
          opacity = 0.2
        ),
        marker = list(
          opacity = 1,
          color = selection_plot$alpha_1,
          line = list(
            color = 'black'
          ),
          symbol = factor(selection_plot$buy_sell),
          size = selection_plot$magnitude,
          # Sets range of alpha colorscale based off all alphas
          cmin = alpha_range_and_size()$alpha_min,
          cmax = alpha_range_and_size()$alpha_max,
          colorscale = list(c(0, "rgb(178, 34, 34)"),
                            # Yellow will be set to normalized average
                            # Colorscale is based on [0, 1] range
                            list(alpha_range_and_size()$alpha_normalized_average, "rgb(255, 255, 0)"),
                            list(1, "rgb(50, 205, 50)")),
          colorbar = list(
            title='Symbol:
                   \n▲ Buy
                   \n▼ Sell
                   \nAlpha:'
          ),
          showlegend =  TRUE),
        # Creates tool tip aesthetics and information
        hoverinfo = "text",
        hoverlabel = list(
          bgcolor = 'white'
        ),
        text = paste('Profit and Loss<br>Date: ', selection_plot$sim_date,
                     '<br>P&L: ', selection_plot$net_pnl,
                     '<br>NMV: ', selection_plot$end_nmv,
                     '<br>Alpha: ', selection_plot$alpha_1,
                     '<br>Shares: ', selection_plot$shares,
                     '<br>Order: ', selection_plot$order_shares,
                     '<br>Fill: ', selection_plot$fill_shares,
                     '<br>End Shares: ', selection_plot$end_shares,
                     '<br>Fill Market Value: ', selection_plot$market_fill_nmv)) %>%
      # NMV Plot
      # Follows same formatting as PNL but with a different tool tip
      add_trace(
       visible = FALSE,
       x = selection_plot$sim_date,
       y = selection_plot$end_nmv, 
       type = 'scatter', mode = 'lines+markers',
       line = list(
         color = 'black',
         opacity = 0.2
       ),
       marker = list(
         opacity = 1,
         color = selection_plot$alpha_1,
         line = list(
           color = 'black'
         ),
         symbol = factor(selection_plot$buy_sell),
         size = selection_plot$magnitude,
         cmin = alpha_range_and_size()$alpha_min,
         cmax = alpha_range_and_size()$alpha_max,
         colorscale = list(c(0, "rgb(178, 34, 34)"),
                           list(alpha_range_and_size()$alpha_normalized_average, "rgb(255, 255, 0)"),
                           list(1, "rgb(50, 205, 50)")),
         colorbar = list(
            title='Symbol:
                   \n▲ Buy
                   \n▼ Sell
                   \nAlpha:'
          ),
         showlegend =  TRUE),
       hoverinfo = "text",
       hoverlabel = list(
         bgcolor = 'white'
       ),
       text = paste('Net Market Value<br>Date: ', selection_plot$sim_date,
                    '<br>P&L: ', selection_plot$net_pnl,
                    '<br>NMV: ', selection_plot$end_nmv,
                    '<br>Alpha: ', selection_plot$alpha_1,
                    '<br>Shares: ', selection_plot$shares,
                    '<br>Order: ', selection_plot$order_shares,
                    '<br>Fill: ', selection_plot$fill_shares,
                    '<br>End Shares: ', selection_plot$end_shares,
                    '<br>Fill Market Value: ', selection_plot$market_fill_nmv)) %>%
      layout(
        title = list(
          # initial title is same as PNL for consistency when the plot renders
          text = paste("Cumulative Profit and Loss of", selection_plot$symbol[1])
        ),
        # Controls graph toggle button
        updatemenus =  list(
          list(
            # PNL is rendered first
            active = 0,
            type = "buttons",
            buttons = list(
              list(
                label = "P&L",
                method = "update",
                # Order of graphs in visible list is PNL, NMV, and ALPHA
                args = list(list(visible = c(TRUE, FALSE, TRUE)),
                            list(title =  paste("Cumulative Profit and Loss of", selection_plot$symbol[1])))
              ),
              list(
                label = "Market Value",
                method = "update",
                args = list(list(visible = c(FALSE, TRUE, TRUE)),
                            list(title = paste("Market Value of", selection_plot$symbol[1])))
              )
            )
          )
        ))
      
    # Creates the alpha sub plot                             
    alpha_plot <- plot_ly(selection_plot, x = ~sim_date, y = ~alpha_1,
                          type = "scatter", mode = "lines",
                          line = list(
                            color = "rgb(0, 102, 204)"
                            ),
                          hoverinfo = "text",
                          hoverlabel = list(
                            bgcolor = 'white'
                          ),
                          text = paste('Alpha<br>Date: ', selection_plot$sim_date,
                                       '<br>P&L: ', selection_plot$net_pnl,
                                       '<br>NMV: ', selection_plot$end_nmv,
                                       '<br>Alpha: ', selection_plot$alpha_1,
                                       '<br>Shares: ', selection_plot$shares,
                                       '<br>Order: ', selection_plot$order_shares,
                                       '<br>Fill: ', selection_plot$fill_shares,
                                       '<br>End Shares: ', selection_plot$end_shares,
                                       '<br>Fill Market Value: ', selection_plot$market_fill_nmv)) %>%
      layout(
        yaxis = list(
          title = "Alpha",
          range = c(alpha_range_and_size()$alpha_min, alpha_range_and_size()$alpha_max),
          # Removes vertical zoom function from alpha plot
          fixedrange = TRUE, showzeroline = FALSE,
          # Controls vertical lines around alpha plot
          showline = TRUE, linewidth = 3, linecolor='black', mirror = TRUE
        ),
        xaxis = list(
          title = "Date",
          # controls horizontal lines around alpha plot
          showline = TRUE, linewidth = 1, linecolor='black', mirror = TRUE
        ),
        showlegend = FALSE)
    
    # Combines holdings plot with alpha plot and links horizontal axis
    multi_layed_plot <- subplot(holdings_plot, alpha_plot,
                                nrows = 2, shareX = TRUE, titleY = TRUE, heights = c(0.80, 0.20))
     
      

  })
  
 # renders the updated UI when a row is clicked
  observeEvent(position_summary(), {
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
            plotlyOutput('selectedHoldingPlot', height = "800px"),
          br(),
          DT::dataTableOutput('selectedHolding')
        )
      }
    })

  })
  
  
  
  observeEvent(input$runSim, {
>>>>>>> upstream/master
    
    selected_holding_row(),
    rownames = FALSE,
    selection = "none"
    
  )
  
  # creates the three plotly plots for the selected holding
  # 1st trace is net market value
  # 2nd trace is end nmv
  # 2nd plot is alpha
  # 3rd plot is subplot combination of the both plots
  output$selectedHoldingPlot <- renderPlotly({
    
    # Adds columns to the selected holding df for the buy sell symbol and the size of the trades
    selection_plot <- selected_holding_row() %>%
      mutate(buy_sell = ifelse(fill_shares > 0, 'triangle-up',
                                  ifelse(fill_shares < 0, 'triangle-down', '0')),
             magnitude = ifelse(market_fill_nmv == 0, 5,
                                 ifelse(abs(market_fill_nmv) > plot_aesthetics()$market_fill_quartile[["50%"]],
                                 ifelse(abs(market_fill_nmv) > plot_aesthetics()$market_fill_quartile[["75%"]], 30, 25),
                                 ifelse(abs(market_fill_nmv) > plot_aesthetics()$market_fill_quartile[["25%"]], 20, 15))))
    
    # Creates one plot with two traces for the PNL and NMV
    holdings_plot <- plot_ly() %>%
      # PNL formatting
      add_trace(
        # Visible when plotly is rendered
        visible = TRUE,
        x = selection_plot$sim_date,
        y = selection_plot$net_pnl, 
        type = 'scatter', mode = 'lines+markers',
        line = list(
          color = 'black',
          opacity = 0.2
        ),
        marker = list(
          opacity = 1,
          color = selection_plot[[config_values()$in_var]],
          line = list(
            color = 'black'
          ),
          symbol = factor(selection_plot$buy_sell),
          size = selection_plot$magnitude,
          # Sets range of alpha colorscale based off all alphas
          cmin = plot_aesthetics()$in_var_min,
          cmax = plot_aesthetics()$in_var_max,
          colorscale = list(c(0, "rgb(178, 34, 34)"),
                            # Yellow will be set to normalized average
                            # Colorscale is based on [0, 1] range
                            list(plot_aesthetics()$in_var_normalized_average, "rgb(255, 255, 0)"),
                            list(1, "rgb(50, 205, 50)")),
          colorbar = list(
            title= paste('Symbol:
                   \n▲ Buy
                   \n▼ Sell
                   \n', config_values()$in_var, ': '),
            thickness = 10
          ),
          showlegend =  TRUE),
        # Creates tool tip aesthetics and information
        hoverinfo = "text",
        hoverlabel = list(
          bgcolor = 'white'
        ),
        text = paste('Profit and Loss<br>Date: ', selection_plot$sim_date,
                     '<br>P&L: ', selection_plot$net_pnl,
                     '<br>NMV: ', selection_plot$end_nmv,
                     '<br>',  config_values()$in_var,': ', selection_plot[[config_values()$in_var]],
                     '<br>Shares: ', selection_plot$shares,
                     '<br>Order: ', selection_plot$order_shares,
                     '<br>Fill: ', selection_plot$fill_shares,
                     '<br>End Shares: ', selection_plot$end_shares,
                     '<br>Fill Market Value: ', selection_plot$market_fill_nmv)) %>%
      # NMV Plot
      # Follows same formatting as PNL but with a different tool tip
      add_trace(
       visible = FALSE,
       x = selection_plot$sim_date,
       y = selection_plot$end_nmv, 
       type = 'scatter', mode = 'lines+markers',
       line = list(
         color = 'black',
         opacity = 0.2
       ),
       marker = list(
         opacity = 1,
         color = selection_plot[[config_values()$in_var]],
         line = list(
           color = 'black'
         ),
         symbol = factor(selection_plot$buy_sell),
         size = selection_plot$magnitude,
         cmin = plot_aesthetics()$in_var_min,
         cmax = plot_aesthetics()$in_var_max,
         colorscale = list(c(0, "rgb(178, 34, 34)"),
                           list(plot_aesthetics()$in_var_normalized_average, "rgb(255, 255, 0)"),
                           list(1, "rgb(50, 205, 50)")),
         colorbar = list(
            title= paste('Symbol:
                   \n▲ Buy
                   \n▼ Sell
                   \n', config_values()$in_var, ': '),
            thickness = 10
          ),
         showlegend =  TRUE),
       hoverinfo = "text",
       hoverlabel = list(
         bgcolor = 'white'
       ),
       text = paste('Net Market Value<br>Date: ', selection_plot$sim_date,
                    '<br>P&L: ', selection_plot$net_pnl,
                    '<br>NMV: ', selection_plot$end_nmv,
                    '<br>',  config_values()$in_var,': ', selection_plot[[config_values()$in_var]],
                    '<br>Shares: ', selection_plot$shares,
                    '<br>Order: ', selection_plot$order_shares,
                    '<br>Fill: ', selection_plot$fill_shares,
                    '<br>End Shares: ', selection_plot$end_shares,
                    '<br>Fill Market Value: ', selection_plot$market_fill_nmv)) %>%
      layout(
        title = list(
          # initial title is same as PNL for consistency when the plot renders
          text = paste("Cumulative Profit and Loss of", selection_plot$symbol[1])
        ),
        # legend = list(
        #   font = list(size = 500)
        # ),
        # Controls graph toggle button
        updatemenus =  list(
          list(
            # PNL is rendered first
            active = 0,
            type = "buttons",
            buttons = list(
              list(
                label = "P&L",
                method = "update",
                # Order of graphs in visible list is PNL, NMV, and ALPHA
                args = list(list(visible = c(TRUE, FALSE, TRUE)),
                            list(title =  paste("Cumulative Profit and Loss of", selection_plot$symbol[1])))
              ),
              list(
                label = "Market Value",
                method = "update",
                args = list(list(visible = c(FALSE, TRUE, TRUE)),
                            list(title = paste("Market Value of", selection_plot$symbol[1])))
              )
            )
          )
        ))
      
    # Creates the alpha sub plot                             
    in_var_plot <- plot_ly(x = selection_plot$sim_date, y = selection_plot[[config_values()$in_var]],
                          type = "scatter", mode = "lines",
                          line = list(
                            color = "rgb(0, 102, 204)"
                            ),
                          hoverinfo = "text",
                          hoverlabel = list(
                            bgcolor = 'white'
                          ),
                          text = paste(config_values()$in_var, '<br>Date: ', selection_plot$sim_date,
                                       '<br>P&L: ', selection_plot$net_pnl,
                                       '<br>NMV: ', selection_plot$end_nmv,
                                       '<br>',  config_values()$in_var,': ', selection_plot[[config_values()$in_var]],
                                       '<br>Shares: ', selection_plot$shares,
                                       '<br>Order: ', selection_plot$order_shares,
                                       '<br>Fill: ', selection_plot$fill_shares,
                                       '<br>End Shares: ', selection_plot$end_shares,
                                       '<br>Fill Market Value: ', selection_plot$market_fill_nmv)) %>%
      layout(
        yaxis = list(
          title = paste(config_values()$in_var),
          range = c(plot_aesthetics()$in_var_min, plot_aesthetics()$in_var_max),
          # Removes vertical zoom function from alpha plot
          fixedrange = TRUE, showzeroline = FALSE,
          # Controls vertical lines around alpha plot
          showline = TRUE, linewidth = 3, linecolor='black', mirror = TRUE
        ),
        xaxis = list(
          title = "Date",
          # controls horizontal lines around alpha plot
          showline = TRUE, linewidth = 1, linecolor='black', mirror = TRUE
        ),
        showlegend = FALSE)
    
    # Combines holdings plot with alpha plot and links horizontal axis
    multi_layed_plot <- subplot(holdings_plot, in_var_plot,
                                nrows = 2, shareX = TRUE, titleY = TRUE, heights = c(0.80, 0.20))
  })
  
 # renders the updated UI when a row is clicked
  observeEvent(position_summary(), {
    output$selectedPlotAndTable <- renderUI({
      # checks to see if there is in in_var column in the position summary
      if(config_values()$in_var %in% colnames(values$sim_obj$getSimDetail())) {
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
            plotlyOutput('selectedHoldingPlot', height = "800px"),
            br(),
            DT::dataTableOutput('selectedHolding')
          )
        }
      } else {
        fluidRow(
          column(
            8,
            align = "center",
            offset = 2,
            p(strong("Add save_detailed_columns: in_var to your simulation configuration to see more."))
          )
        )
      }
    })

  })
  
  
  
  observeEvent(input$runSim | input$loadSim, {
  # observeEvent(input$runSim | input$directory, {
    # checkts to see if BOTH inputs are null
    if (input$runSim %in% 0 & input$loadSim %in% 0) {
    # if (input$runSim %in% 0 & input$directory %in% 0) {
      return(NULL)
    }
    
    
    # Decides what simulation to run
    if (input$runSim & !input$loadSim) {
    # if (input$runSim & !input$directory) {
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
        sim$run()
        
        
        values$sim_obj <- sim
        
        # updateTabsetPanel(session,
        #                   "top",
        #                   selected = "Results"
        # )
      },
      error = function(e) { showNotification(e$message, type = "error", duration = NULL)}
      )
    } else if (!input$runSim & input$loadSim) {
      if(!is.integer(input$simDir)) {
        
        # gets the yaml file from the uploaded directory and converts the list to a yaml
        upload_yaml <- yaml::read_yaml(paste0(parseDirPath(volumes, input$simDir), "/config.yaml")) %>%
          yaml::as.yaml()
        
        # updates displayed fig in the text area
        updateTextAreaInput(session, "config", value = upload_yaml)
        
        # uploads simulation from the directory
        sim <- Simulation$new()
        
        sim$readFeather(parseDirPath(volumes, input$simDir))
        
        values$sim_obj <- sim
        
        # this is used in both if statements, move it outside to optimize flow
        # updateTabsetPanel(session,
        #                   "top",
        #                   selected = "Results")
      } else { 
        return(showNotification(p(strong("Error:"), "select a valid directory"), type = "error"))
      }
    }
    
    
    updateTabsetPanel(session,
                      "top",
                      selected = "Results")
    
  })
}
