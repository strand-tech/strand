#' Portfolio class
#'
#' @description
#' The portfolio class represents a collection of positions.
#'
#' @details This class wraps a tabular data structure that contains position
#' information. Methods are available for returning this information in long or
#' wide format.
#'
#' @export
Portfolio <- R6Class(
  "Portfolio",
  public = list(
    
    #' @description Create a new \code{Portfolio} object.
    #' @param strategy_names An object of class character that contains the
    #'   strategy names for this portfolio object.
    #' @return A new \code{Portfolio} object.
    initialize = function(strategy_names) {
      private$strategy_names <- strategy_names
      private$positions <-
        data.frame(id = character(0),
                   strategy = character(0),
                   int_shares = integer(0),
                   ext_shares = integer(0),
                   stringsAsFactors = FALSE)
      invisible(self)
    },
    
    #' @description Position access.
    #' @return A data frame that contains the raw position information for this
    #'   portfolio. The data frame contains the following columns: id
    #'   (character), strategy (character), int_shares (integer) for the number
    #'   of shares that net down with shares in other strategies, and
    #'   ext_shares (integer) for the number of shares that do not net down with
    #'   any other strategy's positions. The sum of the values in the ext_share
    #'   column represent the number of shares held at the joint level (i.e.,
    #'   that don't cancel out with other positions).
    getPositions = function() {
      invisible(private$positions)  
    },
    
    #' @description Replace this portfolio's position information.
    #' @param positions_df Data frame that contains the portfolio's low-level
    #'   position information. The data frame must contain the following
    #'   columns: id (character), strategy (character), int_shares (integer),
    #'   ext_shares (integer). No NAs are allowed. All values in the strategy
    #'   column must be one of the strategy names for this portfolio.
    setPositions = function(positions_df) {
      stopifnot(isTRUE(all.equal(names(positions_df),
                                 c("id", "strategy", "int_shares", "ext_shares"))),
                is.character(positions_df[["id"]]),
                is.character(positions_df[["strategy"]]),
                is.integer(positions_df[["int_shares"]]),
                is.integer(positions_df[["ext_shares"]]),
                !any(is.na(positions_df)),
                all(positions_df[["strategy"]] %in% private$strategy_names))
      private$positions <- positions_df
      invisible(self)
    },
    
    #' @description Remove positions from the portfolio
    #' @param ids Character vector that contains the security ids to remove.
    #'   Positions in all strategies for the supplied ids are removed from the
    #'   portfolio.
    removePositions = function(ids) {
      private$positions <- 
        filter(private$positions, !.data$id %in% ids)
      invisible(self)
    },

    #' @description Access to positions in consolidated wide format.
    #' @return A data frame of consolidated (external + internal) share values
    #'   in wide format. The columns in the data frame returned by this method
    #'   are id, strategy, and shares_{s} for each strategy s.
    getConsolidatedPositions = function() {
      
      pos <- private$positions %>%
        mutate(shares = .data$int_shares + .data$ext_shares) %>%
        select("id", "strategy", "shares")
      
      # Add dummy rows for strategies that have no positions so that we have all
      # strategies present in our pivotted table.
      empty_strategies <- private$strategy_names[!private$strategy_names %in% pos$strategy]
      if (length(empty_strategies > 0)) {
        pos <- rbind(pos,
                     data.frame(
                       id = "DUMMY",
                       strategy = empty_strategies,
                       shares = 0,
                       stringsAsFactors = FALSE))
      }
      
      pos %>% pivot_wider(
        names_from = "strategy",
        values_from = "shares",
        names_prefix = "shares_",
        values_fill = list(shares = 0)
      ) %>%
        filter(!.data$id %in% "DUMMY")
      
    },
    
    #' @description Helper for extracting strategy share columns.
    #' @return A vector of column names that matches the strategy columns in the
    #'   data frame returned by \code{getConsolidatedPositions}.
    getShareColumns = function() {
      paste0("shares_", private$strategy_names)
    },
    
    #' @description Adjust portfolio share values by an adjustment ratio. The
    #'   adjustment ratio is the number of old shares per new share. So for a
    #'   2:1 split, the adjustment ratio is 0.5.
    #' @param adjustment_df Data frame that contains adjustment ratios to apply.
    #'   The data frame must include two columns: id (character) and
    #'   adjustment_ratio (numeric). There must be an entry in adjustment_df for
    #'   each security in the portfolio (this requirement will likely be
    #'   loosened in future versions).
    applyAdjustmentRatio = function(adjustment_df) {
      stopifnot(is.data.frame(adjustment_df),
                isTRUE(all.equal(names(adjustment_df), c("id", "adjustment_ratio"))),
                is.character(adjustment_df[["id"]]),
                is.numeric(adjustment_df[["adjustment_ratio"]]),
                all(private$positions$id %in% adjustment_df$id),
                !any(is.na(adjustment_df)))
      
      # TODO Don't require all positions to have entries in the adjustment_df
      # data frame.
      
      # Note: Should we push handling of adjustment basis to the user by
      # requiring the input of prices that are already on the same basis? In
      # this system we allow the provision of unadjusted prices and an
      # adjustment ratio. (Note that if only a pre-adjusted price series is
      # available, adjustment ratio can simply be set to 1 everywhere.)
      
      
      private$positions <- private$positions %>% 
        inner_join(adjustment_df, by = "id") %>%
        mutate(int_shares = .data$int_shares / .data$adjustment_ratio,
               ext_shares = .data$ext_shares / .data$adjustment_ratio) %>%
        select(!!names(private$positions))
      
      invisible(self)
    }
  ),
  
  private = list(
    strategy_names = NULL,
    positions = NULL
  )
)
