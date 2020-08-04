library(ggplot2)
library(strand)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)

server <- function(input, output, session) {

  # Store the sim result data in a reactiveValues object.
  values <- reactiveValues()
  
  # Creates a data.frame of the final positions
                                            # changed sim_result tom sim_obj
  position_summary <- eventReactive(values$sim_obj, {
            # result to obj
    values$sim_obj$getPositionSummary(strategy_name = "joint") %>%
      left_join(values$sim_obj$getSecurityReference()[c("id","symbol")], by = "id") %>%
      ungroup() %>%
      select(.data$symbol, .data$gross_pnl, .data$net_pnl,
             .data$average_market_value,
             .data$total_trading, .data$trade_costs, .data$financing_costs,
             .data$days_in_portfolio) %>%
      arrange(.data$gross_pnl)
    
  })
  
  # Produces a name value list that stores
  #   maximum alpha (alpha_max)
  #   minimum alpha (alpha_min)
  #   list of markey fill quarties (market_fill_quartile[[%]])
  
  
                                                # result to obj
  alpha_range_and_size <- eventReactive(values$sim_obj, {
    
    # Creates a data.frame of all positions throughout the simulation
    # Rounds alpha to get full alpha range
    # Includes days when no trades happen to compare alpha throughout simulation
    
                                # result to obj
    simulation_summary <- values$sim_obj$getSimDetail(strategy_name = "joint") %>%
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
                            # result to obj
      left_join(values$sim_obj$getSimDetail(strategy_name = "joint", 
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
 
                      # result to obj
  observeEvent(values$sim_obj, {
    # result to obj
    output$plot_1 <- renderPlotly(
      ggplotly(values$sim_obj$plotPerformance(), tooltip = FALSE) 
    )
    # result to obj
    output$plot_2 <- renderPlotly(
      ggplotly(values$sim_obj$plotMarketValue(), tooltip = FALSE)
    )
    
    # TODO dynamically select exposure plot in_vars based on config file
            # result to obj
    output$plot_3 <- renderPlotly(
      ggplotly(values$sim_obj$plotCategoryExposure(in_var = "category_1"), tooltip = FALSE)
    )
                # result to obj
    output$plot_4 <- renderPlotly(
      ggplotly(values$sim_obj$plotFactorExposure(in_var = c("factor_1", "factor_2", "factor_3", "factor_4")), tooltip = FALSE)
    )
                   # result to obj
    output$plot_5 <- renderPlotly(
      ggplotly(values$sim_obj$plotNumPositions(), tooltip = FALSE)
    )
    output$overallStatsTable <- renderTable(values$sim_obj$overallStatsDf(), align = "lrr")
                            # result to obj
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
  
  observeEvent(input$holdingsDate, {
    
    if (!is.null(values$sim_obj)) {
      
      output$holdingsTable <- renderDT(
                          # result to obj
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
      sim$run()
      # res <- sim$run()

      # values$sim_result <- res
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
