#' Make Basic Flextable
#' 
#' @description Make a flextable with preferred formatting
#' @param x The data.frame to use for flextable
#' @param title The string to use as the table title
#' @param col_names A character vector of preferred column names for flextable. 
#' Length of character vector must be equal to the number of columns. Defaults 
#' to NULL, in which case the column names of x are used in the flextable.
#' @param hlines The row numbers to draw horizontal lines beneath. Defaults to 
#' "all", can be "all", "none", or a numeric vector.
#' @return A flextable object with the argued formatting
make_ft = function(x, 
                   title = NULL, 
                   col_names = NULL, 
                   hlines = "all") {
  # Create reused values
  brdr <- officer::fp_border(color = "black", width = 1)
  numcols <- ncol(x)
  numrows <- nrow(x)
  
  # set_header_labels takes a named vector
  if (!is.null(col_names)) names(col_names) <- colnames(x)
  
  # Initiate basic flextable 
  ft <- x %>%
    flextable::flextable() %>%
    flextable::set_header_labels(values = col_names) %>%
    flextable::vline(j = 1:(max(numcols-1, 1)), 
                     border = brdr, part = "body") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::bold(part = "header")
  
  
  # Add horizontal lines
  if (is.numeric(hlines)) {
    ft <- flextable::hline(ft, i = hlines, border = brdr, part = "body")
  } else if (hlines == "all") {
    ft <- flextable::hline(ft, border = brdr, part = "body")
  } else if (hlines == "none") {
  } else {
    stop("hlines argument must be 'all', 'none', or a numeric vector")
  }
  
  # add title
  if (!is.null(title)) {
    ft <- ft %>%
      flextable::add_header_row(top = TRUE, 
                                values = title, 
                                colwidths = numcols) %>%
      flextable::fontsize(i = 1, size = 24, part = "header") %>%
      flextable::padding(padding = 20, i = 1, part = "header") %>%
      flextable::padding(padding.left = 0, part = "header")
  }
  ft <- flextable::border(ft, i = numrows, 
                          border.bottom = officer::fp_border(color = "black",
                                                             width = 2))
  return(ft)
}

#' Show Overall Stats Table
#' 
#' @description Build a flextable object showing a Simulation's overall statistics
#' @param sim A Simulation object to show the statistics for
show_stats = function(sim) {
  sim$overallStatsDf() %>%
    make_ft(hlines = c(2, 5, 6, 8)) %>%
    flextable::autofit()
}

#' Show Strategy Configuration
#' 
#' @description Build a flextable object showing a Simulation's configuration
#' @param sim A Simulation object to show the configuration for
show_config = function(sim) {
  # Limiting to a single strategy here
  
  stopifnot(length(sim$getConfig()$getStrategyNames()) %in% 1)
  
  sim$getConfig()$config$strategies[[1]] %>%
    unlist(recursive = FALSE) %>%
    tibble::enframe() %>%
    tidyr::pivot_wider(id_cols = .data$name) %>%
    select(.data$in_var,
           .data$strategy_capital,
           # .data$ideal_long_weight,
           # .data$ideal_short_weight,
           .data$position_limit_pct_lmv, 
           .data$position_limit_pct_smv,
           .data$position_limit_pct_adv,
           .data$trading_limit_pct_adv) %>%
    tidyr::unnest(cols = names(.)) %>%
    make_ft(title = "Strategy Configuration",
            col_names = c("in_var",
                          "Strategy\nCapital",
                          # "Ideal\nLong\nWeight",
                          # "Ideal\nShort\nWeight",
                          "Position\nLimit\n(% LMV)", 
                          "Position\nLimit\n(% SMV)", 
                          "Position\nLimit\n(% ADV)", 
                          "Trading\nLimit\n(% ADV)")) %>%
    flextable::colformat_num(big.mark = ",", digits = 0) %>%
    flextable::autofit()
}

#' Show Strategy Constraints
#' 
#' @description Build a flextable object showing a Simulation's risk constraints
#' @param sim A Simulation object to show the configuration for
show_constraints = function(sim) {
  # Limiting to a single strategy here
  strategy_configs <- sim$getConfig()$config$strategies[[1]]
  if (has_name(strategy_configs, "constraints")) {
    strategy_configs$constraints %>%
      enframe() %>%
      unnest_wider(.data$value) %>%
      make_ft(title = "Strategy Risk Constraints",
              col_names = c("Name", "Type", "in_var", "Upper\nBound",
                            "Lower\nBound")) %>%
      flextable::autofit()
  } 
  else {
    brdr <- officer::fp_border(color = "black", width = 2)
    data.frame(x = "No Strategy Risk Constraints") %>%
      make_ft(title = "Strategy Risk Constraints",
              col_names = " ") %>%
      flextable::align(align = "left") %>%
      flextable::border_remove() %>%
      flextable::border(part = "body", 
                        border.top = brdr, 
                        border.bottom = brdr) %>%
      flextable::autofit()
  }
  
}

#' Show Best/Worst Performers
#' 
#' @description Build a flextable object showing a Simulation's best and worst 
#' performers
#' @param sim A Simulation object to show the best and worst performers for
show_best_worst = function(sim) {
  # Get position data
  pos_summary <- sim$getPositionSummary(strategy_name = "joint") %>%
    left_join(sim$getSecurityReference()[c("id","symbol")], by = "id") %>%
    ungroup() %>%
    select(.data$symbol, .data$gross_pnl, .data$net_pnl,
           .data$average_market_value,
           .data$total_trading, .data$trade_costs, .data$financing_costs,
           .data$days_in_portfolio)
  
  # Assign re-used column titles
  coltitles <- c("Symbol", "Gross PnL", "Net PnL", "Avg.\nMkt.\nValue",
                 "Total\nTrading", "Trade\nCosts", "Financing\nCosts",
                 "Days\nIn\nPortfolio")
  
  # Ten best performers
  ten_best <- pos_summary %>%
    arrange(desc(.data$gross_pnl)) %>%
    head(10) %>%
    make_ft(title = "Top 10 Performers",
            col_names = coltitles) %>%
    flextable::colformat_num(big.mark = ",", digits = 0) %>%
    flextable::autofit()
  
  # Ten worst performers
  ten_worst <- pos_summary %>%
    arrange(.data$gross_pnl) %>%
    head(10) %>%
    make_ft(title = "Bottom 10 Performers",
            col_names = coltitles) %>%
    flextable::colformat_num(big.mark = ",", digits = 0) %>%
    flextable::autofit()
  
  return(list("ten_best" = ten_best,
              "ten_worst" = ten_worst))
}

