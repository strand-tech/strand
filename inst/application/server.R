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
    
    #there has to be a nicer way to do this 
    pos_sum <- values$sim_result$getPositionSummary(strategy_name = "joint") %>%
      left_join(values$sim_obj$getSecurityReference()[c("id","symbol")], by = "id") %>%
      ungroup() %>%
      select(.data$symbol, .data$gross_pnl, .data$net_pnl,
             .data$average_market_value,
             .data$total_trading, .data$trade_costs, .data$financing_costs,
             .data$days_in_portfolio) %>%
      arrange(.data$gross_pnl)

    
    #this gets the list of symbols from selected rows
    symbols_selected <- pos_sum[input$positionSummaryTable_rows_selected, 1] %>%
      as.data.frame()
    
    #gets the security info (id) for the selected symbols
    selected_sec_ref <- values$sim_obj$getSecurityReference() %>%
        as.data.frame() %>%
        filter(symbol %in% symbols_selected$symbol) %>%
        select("id", "symbol") %>%
        as.data.frame()
      
    #so now I am adding the whole data, and filtering it by the selected positions
    #doing this here so I don't have to do it twice repetedly below in the holdingsPlot
    #and the holdings datatable
    indiv_holdings <- left_join(values$sim_result$getSimDetail(strategy_name = "joint"), selected_sec_ref, by = "id")  %>%
      na.omit() %>%
      select("sim_date", "symbol" ,"shares", "order_shares", "fill_shares", "end_shares", "end_nmv",
             "gross_pnl", "trade_costs", "financing_costs", "net_pnl") %>%
      group_by(symbol) %>%
      mutate(end_nmv = round(end_nmv),
             gross_pnl = round(gross_pnl, digits = 2),
             trade_costs = round(trade_costs, digits = 2),
             financing_costs = round(financing_costs, digits = 2),
             net_pnl = cumsum(net_pnl),
             net_pnl = round(net_pnl, digits = 0),
             gross_pnl = cumsum(gross_pnl),
             gross_pnl = round(gross_pnl, digits = 0))
    #reorders the columns
    indiv_holdings <- indiv_holdings[c(1,2,11,3,4,5,6,7,8,9,10)]
    
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
                                            rownames = FALSE)

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
  output$selectedHoldings <- renderDT(
    
    ID(),
    rownames = FALSE
    
  )
  
  
  output$holdingsPlot <- renderPlot({

    plot <- ID() %>%
      select("sim_date", "symbol" , "fill_shares", "net_pnl") %>%
      mutate(buy_sell = ifelse(fill_shares > 0, 'Buy',
                                  ifelse(fill_shares < 0, 'Sell', 0)),
             order_size = round(1 + log(abs(fill_shares), 10), digits = 0))
    # create a data subset similar to fill shares that you can remove 0 values from.
    # put that as the data for geom point, so it can get rid of 0 values
    
    #for the dot plot dots
    order_shapes <- c(24, 25)
    names(order_shapes) <- c('Buy', 'Sell')

      ggplot(data = plot, aes(x = sim_date, y = net_pnl, group = symbol)) +
        geom_line(aes(color = symbol)) +
        geom_point(data = filter(plot, fill_shares !=  0),
                   aes(shape = factor(buy_sell), fill = symbol, size = order_size)) +
        scale_shape_manual(values = order_shapes) +
        xlab("Date") + ylab("Net PnL") + ggtitle("Cumulative Profit and Loss") +
        theme_light() + 
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.position = "bottom",
          legend.box = "horizontal") +
        labs(shape = "Orders", color = "Symbol", size = "Fill Magnitude") + 
        guides(fill =FALSE,
               shape = guide_legend(order = 1),
               color = guide_legend(order = 2),
               size = guide_legend(order =3))

  })
  
  output$clickInfo <- renderText({
    #this is correct
   # first_day <- input$startDate %>%
   #   as.Date() %>%
   #   + 1
    
    #ask jeff about this, why is the base date on the graph that?
    x_click <- input$plot_click$x %>%
      as.numeric() %>%
      round(digits = 0) %>%
      as.Date(origin = "1970-01-01")
    
    y_click <- input$plot_click$y %>%
      as.numeric() %>%
      round(digits = 0)
    
    paste0("Date=", x_click, "\nNet PnL=", y_click)
  })
  
  
  
  output$plotAndTable <- renderUI({
    if(nrow(ID()) == 0){
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
        column(
          2,
          verbatimTextOutput("clickInfo")
        ),
        column(
          10,
          plotOutput('holdingsPlot', click = "plot_click")
        ),
        br(),
        DT::dataTableOutput('selectedHoldings')
      )
    }
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
