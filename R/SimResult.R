#' SimResult class
#' 
#' @description
#' Class for storing simulation results.
#'
#' @details
#' The \code{SimResult} class wraps simulation results and provides methods for
#' accessing, summarizing, and plotting those results.
#'
#' @export
SimResult <- R6Class(
  "SimResult",
  private = list(
    
    # _list objects are lists whose elements are result data for single
    # periods.
    
    # High-level summary data, including period return, number of positions,
    # etc.
    sim_summary_list = NULL,
    
    # Position-level simulation data
    sim_detail_list = NULL,
    
    # Input data statistics, such as number of NAs and period-over-period
    # correlations.
    input_stats_list = NULL,
    
    # Information on any loosened constraints.
    loosening_info_list = NULL,
    
    # Optimization summary data.
    optimization_summary_list = NULL,
    
    # Exposures data.
    exposures_list = NULL,
    
    # Data for stocks removed due to to delisting.
    delistings_list = NULL,
    
    # The config object stores the list of config information used to run the
    # simulation.
    config = NULL
  ),
  
  public = list(
    
    #' @description Create a new \code{SimResult} object.
    #' @param config The list of configuration information used to run the
    #'   simulation.
    #' @return A new \code{SimResult} object.
    initialize = function(config) {
      private$config <- config
      invisible(self)
    },
    
    #' @description Get the object's configuration information.
    #' @return Object of class \code{list} that contains the simulation's
    #'   configuration information.
    getConfig = function() {
      invisible(private$config)
    },
    
    #' @description Save summary information.
    #' @param period Period to which the data pertains.
    #' @param data_obj Data frame to save.
    saveSimSummary = function(period, data_obj) {
      private$sim_summary_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },
    
    #' @description Save detail (position-level) information.
    #' @param period Period to which the data pertains.
    #' @param data_obj Data frame to save.
    saveSimDetail = function(period, data_obj) {
      private$sim_detail_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },

    #' @description Save input statistics.
    #' @param period Period to which the data pertains.
    #' @param data_obj Data frame to save.
    saveInputStats = function(period, data_obj) {
      private$input_stats_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },
    
    #' @description Save loosening information.
    #' @param period Period to which the data pertains.
    #' @param data_obj Data frame to save.
    saveLooseningInfo = function(period, data_obj) {
      private$loosening_info_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)
    },
    
    #' @description Save optimization summary information.
    #' @param period Period to which the data pertains.
    #' @param data_obj Data frame to save.
    saveOptimizationSummary = function(period, data_obj) {
      private$optimization_summary_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },
    
    #' @description Save exposure information.
    #' @param period Period to which the data pertains.
    #' @param data_obj Data frame to save.
    saveExposures = function(period, data_obj) {
      private$exposures_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)
    },
    
    #' @description Save information on positions removed due to delisting.
    #' @param period Period to which the data pertains.
    #' @param data_obj Data frame to save.
    saveDelistings = function(period, data_obj) {
      private$delistings_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)
    },
    
    #' @description Get summary information.
    #' @return A data frame that contains all summary data.
    getSimSummary = function() {
      invisible(bind_rows(private$sim_summary_list))
    },

    #' @description Get detail information.
    #' @param sim_date Vector of length 1 of class Date or character that
    #'   specifies the period for which to get detail information. If
    #'   \code{NULL} then data from all periods is returned. Note that as
    #'   opposed to filtering on a date column in the detail data itself, the
    #'   period index (i.e., the list element name) is used when extracting data
    #'   for a single period. This is the same value that is supplied as the
    #'   \code{period} parameter when calling \code{saveSimDetail()}. Defaults
    #'   to \code{NULL}.
    #' @param strategy_name Character vector of length 1 that specifies the
    #'   strategy for which to get detail data. If \code{NULL} data for all
    #'   strategies is returned. Defaults to \code{NULL}.
    #' @param security_id Character vector of length 1 that specifies the
    #'   security for which to get detail data. If \code{NULL} data for all
    #'   securities is returned. Defaults to \code{NULL}.
    #' @return A data frame that contains detail data.
    getSimDetail = function(sim_date = NULL, strategy_name = NULL, security_id = NULL) {
      if (!is.null(sim_date)) {
        detail_data <- private$sim_detail_list[[sim_date]]
      } else {
        detail_data <- bind_rows(private$sim_detail_list)
      }
      
      if (!is.null(strategy_name)) {
        detail_data <- detail_data %>% filter(.data$strategy %in% !!strategy_name)
      }
      
      if (!is.null(security_id)) {
        detail_data <- detail_data %>% filter(.data$id %in% !!security_id)
      }
      
      invisible(detail_data)
    },
    
    #' @description Get summary information by security. This method can be
    #'   used, for example, to calculate the biggest winners and losers over the
    #'   course of the simulation.
    #' @param strategy_name Character vector of length 1 that specifies the
    #'   strategy for which to get detail data. If \code{NULL} data for all
    #'   strategies is returned. Defaults to \code{NULL}.
    #' @return A data frame of summary information aggregated by security.
    getPositionSummary = function(strategy_name = NULL) {
      detail_data <- bind_rows(private$sim_detail_list)
      
      if (!is.null(strategy_name)) {
        detail_data <- detail_data %>% filter(.data$strategy %in% !!strategy_name)
      }
      
      detail_data %>%
        group_by(.data$id, .data$strategy) %>%
        summarize(gross_pnl = round(sum(gross_pnl)),
                  net_pnl = round(sum(net_pnl)),
                  average_market_value = round(mean(end_nmv[end_shares != 0])),
                  total_trading = round(sum(market_fill_gmv)),
                  trade_costs = round(sum(trade_costs)),
                  financing_costs = round(sum(financing_costs)),
                  days_in_portfolio = sum(end_shares != 0)) %>%
        arrange(-.data$gross_pnl)
    },
    
    #' @description Get input statistics.
    #' @return A data frame that contains all input statistics.
    getInputStats = function() {
      invisible(bind_rows(private$input_stats_list))
    },
    
    #' @description Get loosening information.
    #' @return A data frame that contains all loosening information.
    getLooseningInfo = function() {
      invisible(bind_rows(private$loosening_info_list))
    },
    
    #' @description Get optimization summary information.
    #' @return A data frame that contains all optimization summary information.
    getOptimizationSummary = function() {
      invisible(bind_rows(private$optimization_summary_list))
    },

    #' @description Get exposure information.
    #' @return A data frame that contains all exposure information.
    getExposures = function() {
      invisible(bind_rows(private$exposures_list))
    },
    
    #' @description Get information on positions removed due to delisting.
    #' @return A data frame that contains all delisting information.
    getDelistings = function() {
      invisible(bind_rows(private$delistings_list))
    },
    
    #' @description Get summary information for a single strategy suitable for
    #'   plotting input.
    #' @param strategy_name Strategy for which to return summary data.
    #' @param include_zero_row Logical flag indicatiing whether to prepend a row
    #'   to the summary data with starting values at zero. Defaults to \code{TRUE}.
    #' @return A data frame that contains summary information for the desired
    #'   strategy, as well as columns for cumulative net and gross total return,
    #'   calculated as pnl divided by ending gross market value.
    getSingleStrategySummaryDf = function(strategy_name, include_zero_row = TRUE) {
      res <- filter(self$getSimSummary(), .data$strategy %in% !!strategy_name)

      if (isTRUE(include_zero_row)) {
        # Create a zero-value starting row with date lagged by one day.
        res <- res[c(1, 1:nrow(res)),]
        res$sim_date[1] <- res$sim_date[1] - 1
        res[1,] <- mutate_if(res[1,], is.numeric, function(x) { 0 })
      }
    
      # Compute cumulative (net) return
      mutate(res,
             net_cum_ret = cumsum(ifelse(end_gmv %in% 0, 0, .data$net_pnl / .data$end_gmv)),
             gross_cum_ret = cumsum(ifelse(end_gmv %in% 0, 0, .data$gross_pnl / .data$end_gmv)))
    },
    
    #' @description Plot cumulative gross and net return by date.
    plotPerformance = function() {
      
      self$getSingleStrategySummaryDf("joint") %>%
        
        select("sim_date", "gross_cum_ret", "net_cum_ret") %>%
        rename(Gross = "gross_cum_ret", Net = "net_cum_ret") %>%
        
        pivot_longer(
          -"sim_date",
          names_to = "type",
          values_to = "cum_ret"
        ) %>%
        
        ggplot(aes(x = sim_date, y = 100 * cum_ret, color = type, group = type)) + geom_line() +
        xlab("Date") + ylab("Return (%)") + 
        ggtitle("Cumulative Return (% GMV)") + 
        theme_light() + 
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())
      
    },
    
    #' @description Plot total gross, long, short, and net market value by date.
    plotMarketValue = function() {

      mv_plot_df <- select(self$getSingleStrategySummaryDf("joint"),
                           sim_date, GMV = end_gmv, LMV = end_lmv, SMV = end_smv, NMV = end_nmv) %>%
        gather(type, value, GMV:NMV) %>% 
        filter(!is.na(value))
      mv_plot_df$type <- factor(mv_plot_df$type, levels = c("GMV", "LMV", "NMV", "SMV"))
      
      ggplot(mv_plot_df, aes(sim_date, value/1e6, color = type, group = type)) + geom_line() +
        scale_color_manual(values = c("LMV" = "darkgreen", "SMV" = "darkred", 
                                      "GMV" = "dodgerblue1", "NMV" = "black")) + 
        xlab("Date") + 
        ylab("Market Value ($mm)") + 
        ggtitle("Market Values") + 
        theme_light() +
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())
      
    },
    
    #' @description Plot exposure to all levels in a category by date.
    #' @param in_var Category for which exposures are plotted.
    plotCategoryExposure = function(in_var) {
      exposures <- self$getExposures() %>% filter(strategy %in% "joint")
      
      exposures %>%
        select("sim_date", starts_with(in_var)) %>%
        pivot_longer(-"sim_date",
                     names_to = in_var,
                     names_prefix = paste0(in_var, "_"),
                     values_to = "exposure") %>%
        
        ggplot(aes(sim_date, exposure * 100, color = get(in_var), group = get(in_var))) + geom_line() +
        xlab("Date") + 
        ylab("Exposure (%)") + 
        ggtitle(paste0(in_var, " Exposure (% Capital)")) + 
        theme_light() +
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())
    },
    
    #' @description Plot exposure to factors by date.
    #' @param in_var Factors for which exposures are plotted.
    plotFactorExposure = function(in_var) {
      exposures <- self$getExposures() %>% filter(strategy %in% "joint")
      exposures %>%
        select("sim_date", one_of(!!in_var)) %>%
        pivot_longer(-"sim_date",
                     names_to = "factor_name",
                     values_to = "exposure") %>%
        
        ggplot(aes(sim_date, exposure * 100, color = factor_name, group = factor_name)) + geom_line() +
        xlab("Date") +
        ylab("Exposure (%)") +
        ggtitle("Factor Exposure (% Capital)") +
        theme_light() +
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())
    },
    
    #' @description Plot number of long and short positions by date.
    plotNumPositions = function() {
      self$getSingleStrategySummaryDf("joint") %>%
        select("sim_date", "end_num_long", "end_num_short") %>%
        rename(Long = "end_num_long", Short = "end_num_short") %>%
        pivot_longer(
          -"sim_date",
          names_to = "side",
          values_to = "count"
        ) %>%
        
        ggplot(aes(x = sim_date, y = count, color = side, group = side)) + geom_line() +
        xlab("Date") + ylab("Number of Positions") + 
        ggtitle("Number of Positions by Side") + 
        theme_light() + 
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())  
    },
    
    #' @description Calculate overall simulation summary statistics, such as
    #'   total P&L, Sharpe, average market values and counts, etc.
    #' @return A data frame that contains summary statistics, suitable for
    #'   reporting.
    overallStatsDf = function() {
      res <- self$getSingleStrategySummaryDf("joint", include_zero_row = FALSE)
      
      data.frame(
        Item = c(
          "Total P&L",
          "Total Return on GMV (%)",
          "Annualized Return on GMV (%)",
          "Annualized Vol (%)",
          "Annualized Sharpe",
          "Avg GMV",
          "Avg NMV",
          "Avg Count",
          "Avg Daily Turnover",
          "Holding Period (months)"
        ),
        Gross = c(
          formatC(sum(res$gross_pnl), big.mark = ",", digit = 0, format = "f"),
          sprintf("%0.1f", sum(res$gross_pnl / res$end_gmv) * 100),
          sprintf("%0.1f", mean(res$gross_pnl / res$end_gmv) * 100 * 252),
          sprintf("%0.1f", sd(res$gross_pnl / res$end_gmv) * 100 * sqrt(252)),
          sprintf("%0.2f", mean(res$gross_pnl / res$end_gmv) / sd(res$gross_pnl / res$end_gmv) * sqrt(252)),
          rep("", 5)
        ),
        Net = c(
          formatC(sum(res$net_pnl), big.mark = ",", digit = 0, format = "f"),
          sprintf("%0.1f", sum(res$net_pnl / res$end_gmv) * 100),
          sprintf("%0.1f", mean(res$net_pnl / res$end_gmv) * 100 * 252),
          sprintf("%0.1f", sd(res$net_pnl / res$end_gmv) * 100 * sqrt(252)),
          sprintf("%0.2f", mean(res$net_pnl / res$end_gmv) / sd(res$net_pnl / res$end_gmv) * sqrt(252)),
          formatC(mean(res$end_gmv), big.mark = ",", digit = 0, format = "f"),
          formatC(mean(res$end_nmv), big.mark = ",", digit = 0, format = "f"),
          formatC(mean(res$end_num), big.mark = ",", digit = 0, format = "f"),
          formatC(mean(res$market_fill_gmv), big.mark = ",", digit = 0, format = "f"),
          sprintf("%0.1f", 12 / (mean(res$market_fill_gmv) / mean(res$end_gmv) * 252 / 2))
        ))
    },
    
    #' @description Print overall simulation statistics.
    print = function() {
      print(self$overallStatsDf())
    },
    
    #' @description Write the data in the object to feather files.
    #' @param out_loc Directory in which output files should be created.
    writeFeather = function(out_loc) {
      
      # TODO Add getter and setter for raw config data in the Config class.
      yaml::write_yaml(private$config$config, paste0(out_loc, "/config.yaml"))
      
      write_feather(self$getSimSummary(), paste0(out_loc, "/sim_summary.feather"))
      write_feather(self$getSimDetail(), paste0(out_loc, "/sim_detail.feather"))
      write_feather(self$getInputStats(), paste0(out_loc, "/input_stats.feather"))
      write_feather(self$getLooseningInfo(), paste0(out_loc, "/loosening_info.feather"))
      write_feather(self$getOptimizationSummary(), paste0(out_loc, "/optimization_summary.feather"))
      write_feather(self$getExposures(), paste0(out_loc, "/exposures.feather"))
      write_feather(self$getDelistings(), paste0(out_loc, "/delistings.feather"))
      
      invisible(self)
    },
 
    #' @description Load files created with \code{writeFeather} into the object.
    #'   Note that because detail data is not re-split by period, it will not be
    #'   possible to use the \code{sim_date} parameter when calling
    #'   \code{getSimDetail} on the populated object.
    #' @param in_loc Directory that contains files to be loaded.
    readFeather = function(in_loc) {
      
      # TODO Check to see if this object is empty before loading up data.
      private$config$config <- yaml::yaml.load_file(paste0(in_loc, "/config.yaml"))
      
      private$sim_summary_list <- list(read_feather(paste0(in_loc, "/sim_summary.feather")))
      private$sim_detail_list <- list(read_feather(paste0(in_loc, "/sim_detail.feather")))
      private$input_stats_list <- list(read_feather(paste0(in_loc, "/input_stats.feather")))
      private$loosening_info_list <- list(read_feather(paste0(in_loc, "/loosening_info.feather")))
      private$optimization_summary_list <- list(read_feather(paste0(in_loc, "/optimization_summary.feather")))
      private$exposures_list <- list(read_feather(paste0(in_loc, "/exposures.feather")))
      private$delistings_list <- list(read_feather(paste0(in_loc, "/delistings.feather")))
      
      warning("It will not be possible to use the sim_date parameter of getSimData on this object to filter detail records by period")
      
      invisible(self)
    }
  )
)