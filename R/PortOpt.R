#' Portfolio optimization class
#'
#' @description The \code{PortOpt} object is used to set up and solve a
#'   portfolio optimization problem.
#'
#' @details A \code{PortOpt} object is configured in the same way as a
#'   \code{Simulation} object, by supplying configuration in a yaml file or list
#'   to the object constructor. Methods are available for adding constraints and
#'   retrieving information about the optimization setup and results. See the
#'   package vignette for information on configuration file setup.
#'
#' @export
PortOpt <- R6Class(
  "PortOpt",
  public = list(

    #' @description Create a new \code{PortOpt} object.
    #' @param config An object of class \code{list} or \code{character}. If the
    #'   value passed is a character vector, it should be of length 1 and
    #'   specify the path to a yaml configuration file that contains the
    #'   object's configuration info. If the value passed is of class list(),
    #'   the list should contain the object's configuration info in list form
    #'   (e.g, the return value of calling \code{yaml.load_file} on the
    #'   configuration file).
    #' @param input_data A \code{data.frame} that contains all necessary input
    #'   for the optimization.
    #'   
    #'   If the top-level configuration item \code{price_var} is not set, prices will be expected
    #'   in the \code{ref_price} column of \code{input_data}.
    #' @return A new \code{PortOpt} object.
    #' @examples
    #' library(dplyr)
    #' data(sample_secref)
    #' data(sample_inputs)
    #' data(sample_pricing)
    #'
    #' # Construct optimization input for one day from sample data. The columns
    #' # of the input data must match the input configuration.
    #' optim_input <-
    #'   dplyr::inner_join(sample_inputs, sample_pricing,
    #'                     by = c("id", "date")) %>%
    #'   dplyr::left_join(sample_secref, by = "id") %>%
    #'   dplyr::filter(date %in% as.Date("2019-01-02")) %>%
    #'   dplyr::mutate(ref_price = price_unadj,
    #'                 shares_strategy_1 = 0)
    #'
    #' opt <-
    #'   PortOpt$new(config = example_strategy_config(),
    #'               input_data = optim_input)
    #'
    #' # The problem is not solved until the \code{solve} method is called
    #' # explicitly.
    #' opt$solve()
    initialize = function(config, input_data) {
      
      if (is.character(config)) {
        config <- yaml.load_file(config)
        private$config <- StrategyConfig$new(config)
      } else if (is.list(config)) {
        private$config <- StrategyConfig$new(config)
      } else if (is(config, "StrategyConfig")) {
        private$config <- config
      } else {
        stop("config must be of class list, character, or StrategyConfig")
      }
      
      private$input_data <- input_data
      
      # If there is no investable flag passed in input_data, assume all stocks
      # are investable.
      if (!"investable" %in% names(private$input_data)) {
        private$input_data$investable <- rep(TRUE, nrow(private$input_data))
      }
      
      private$validateInputData()
      
      # Set target weights from the config file if necessary (this overrides any
      # calculation of target weights based on ideal/current weights and the
      # target weight policy).
      for (strategy in private$config$getStrategyNames()) {

        target_long_weight <- private$config$getStrategyConfig(strategy, "target_long_weight")
        target_short_weight <- private$config$getStrategyConfig(strategy, "target_short_weight")
        if (!is.null(target_long_weight) && !is.null(target_short_weight)) {
          private$setTargetWeight(strategy, target_long_weight, target_short_weight)  
        } else if (!(is.null(target_long_weight) && is.null(target_short_weight))) {
          stop(paste0("For strategy ", strategy, " must supply both target_long_weight ", 
                      "and target_short_weight"))
        }
      }
      
      private$computeTargetWeights()
      
      # Should create a valid() method for checks like the below.
      for (strategy in private$config$getStrategyNames()) {
        tw_strat <- private$target_weights[[strategy]]
        
        stopifnot(!is.null(tw_strat),
                  !is.null(tw_strat$long),
                  is.numeric(tw_strat$long),
                  length(tw_strat$long) %in% 1,
                  !is.null(tw_strat$short),
                  is.numeric(tw_strat$short),
                  length(tw_strat$short) %in% 1)
      }

      
      private$addObjectiveFunction()
      private$addTradingLimits()
      private$addStrategyMarketValueConstraints()
      private$addFactorConstraints()
      private$addCategoryConstraints()
      private$addAbsNetTradeConstraint()
      private$addTurnoverConstraint()
      
      invisible(self)
    },
    
    #' @description Set the verbose flag to control the amount of informational
    #'   output.
    #' @param verbose Logical flag indicating whether to be verbose or not.
    #' @return No return value, called for side effects.
    setVerbose = function(verbose) {
      stopifnot(is.logical(verbose),
                length(verbose) %in% 1)
      private$verbose <- verbose
      invisible(self)
    },
    
    #' @description Add optimization constraints.
    #' @param constraint_matrix Matrix with one row per constraint and \eqn{(S+1) \times N}
    #'   columns, where S is number of strategies and N is the number of stocks.
    #'
    #'   The variables in the optimization are
    #'
    #'   \deqn{x_{1,1}, x_{2,1}, \ldots, x_{N,1},}
    #'   \deqn{x_{1,2}, x_{2,2}, \ldots, x_{N,2},}
    #'   \deqn{\vdots}
    #'   \deqn{x_{1,S}, x_{2,S}, \ldots, x_{N,S},}
    #'   \deqn{y_1, \ldots, y_N}
    #'
    #'   The first \eqn{N \times S} variables are the individual strategy
    #'   trades. Variable \eqn{x_{i,s}} represents the signed trade for stock i
    #'   in strategy s. The following N auxillary variables \eqn{y_1, \ldots, y_N}
    #'   represent the absolute value of the net trade in each stock. So
    #'   for a stock i, we have:
    #'
    #'   \deqn{y_i = \sum_s |x_{i,s}|}
    #'
    #' @param dir Vector of class character of length
    #'   \code{nrow(constraint_matrix)} that specifies the direction of the
    #'   constraints. All elements must be one of ">=", "==", or "<=".
    #' @param rhs Vector of class numeric of length
    #'   \code{nrow(constraint_matrix)} that specifies the bounds of the
    #'   constraints.
    #' @param name Character vector of length 1 that specifies a name for the
    #'   set of constraints that are being created.
    #' @return No return value, called for side effects.
    addConstraints = function(constraint_matrix, dir, rhs, name) {
      stopifnot(is.matrix(constraint_matrix) ||
                inherits(constraint_matrix, "Matrix"))
              

      if (is.null(private$objective_function) || length(private$objective_function) %in% 0) {
        stop("Must set objective function before adding constraints")
      }
      
      if (ncol(constraint_matrix) != length(private$objective_function)) {
        stop(paste0("Constraint matrix does not have the correct number of columns (",
                    ncol(constraint_matrix), "). Should be ", length(private$objective_function), "."))
      }
      
      if (name %in% names(private$constraint_matrices)) {
        stop("Cannot add constraints: name already exists.")
      }
      
      private$constraint_matrices[[name]] <- constraint_matrix

      # If dir is of length 1, and length(rhs) > 1, extend dir to be the same
      # length as rhs by recycling.
      if (length(dir) %in% 1 && length(rhs) > 1) {
        dir <- rep(dir, times = length(rhs))
      }

      private$dir <- c(private$dir, dir)
      private$rhs <- c(private$rhs, rhs)
            
      invisible(self)
    },

    #' @description Constraint matrix access.
    #' @return The optimization's constraint matrix.
    getConstraintMatrix = function() {
      do.call(rbind, lapply(private$constraint_matrices, as, "sparseMatrix"))
    },
    
    #' @description Provide high-level constraint information.
    #' @return A data frame that contains constraint metadata, such as current constraint value and
    #'   whether a constraint is currently within bounds, for all single-row
    #'   constraints. Explicitly exclude net trade constraints and constraints
    #'   that involve net trade variables.
    getConstraintMeta = function() {
      constr_meta <- data.frame(name = names(private$constraint_matrices),
                                stringsAsFactors = FALSE)
      constr_meta$rows <- sapply(private$constraint_matrices, nrow)
      constr_meta$idx_start <- cumsum(constr_meta$rows)
      constr_meta$idx_end <- constr_meta$idx_start + constr_meta$rows - 1
      
      constr_meta <- filter(constr_meta,
                            !.data$name %in% c("Net trade <=", "Net trade >=", "Turnover limit"))
      stopifnot(isTRUE(all.equal(constr_meta$idx_start, constr_meta$idx_end)))

      constr_meta$current_value <-
        sapply(constr_meta$name,
               function(x) {
                 if (nrow(private$constraint_matrices[[x]]) %in% 1) {
                   as.vector(private$getConstraintValue(x))
                 } else {
                   NA
                 }
               })
      
      constr_meta$dir <- private$dir[constr_meta$idx_start]
      constr_meta$rhs <- private$rhs[constr_meta$idx_start]
      constr_meta$bound_value <- constr_meta$current_value + constr_meta$rhs
      constr_meta$within_bounds <- sapply(paste(constr_meta$current_value,
                                                constr_meta$dir,
                                                constr_meta$bound_value),
                                          function(x) { eval(parse(text = x)) })
      constr_meta
    },

    #' @description Solve the optimization. After running \code{solve()},
    #'   results can be retrieved using \code{getResultData()}.
    #' @return No return value, called for side effects.
    solve = function() {
      indices <- 1:length(private$objective_function)
      
      solver <- private$config$getConfig("solver")
      res <- NULL
      
      for (loosen_coef in c(0, 0.5, 0.5, 1)) {
        
        private$loosen(loosen_coef)

        # If loosen_coef != 0 then we must have attempted an optimization and
        # it must have failed.
        if (loosen_coef != 0) {
          
          stopifnot(!is.null(res))
          if (isTRUE(private$verbose)) {
            cat("Optimization failed with status code ", res$status,
                ". Loosening out-of-bounds constraints by ",
                round(loosen_coef * 100), "%.\n", sep = "")
          }
        }

        if (solver %in% "symphony") {
          
          res <- Rsymphony::Rsymphony_solve_LP(
            obj = private$objective_function,
            mat = self$getConstraintMatrix(),
            dir = private$dir,
            rhs = private$rhs,
            max = TRUE,
            bounds = list(
              lower = list(ind = indices,
                           val = private$variable_bounds$lower),
              upper = list(ind = indices,
                           val = private$variable_bounds$upper)
            ),
            time_limit = 500
            
            # Set the below parameters to write MPS and LP files.
            # , write_mps = TRUE, write_lp = TRUE
          )
          
          private$solution <-  res$solution
          private$model <- res
          
        } else if (solver %in% "glpk") {
          res <- Rglpk_solve_LP(
            obj = private$objective_function,
            mat = self$getConstraintMatrix(),
            dir = private$dir,
            rhs = private$rhs,
            max = TRUE,
            bounds = list(
              lower = list(ind = indices,
                           val = private$variable_bounds$lower),
              upper = list(ind = indices,
                           val = private$variable_bounds$upper)
            ),
            contols = list(tm_limit = 500 * 1000)
          )
          
          private$solution <-  res$solution
          private$model <- res
          
        } else {
          stop(paste0("Unsupported solver: ", solver))
        }
        
        if (res$status %in% 0) break;
        
      }
      
      if (!res$status %in% 0) {
        if (solver %in% "glpk" && res$status %in% 1 &&
            length(res$solution) == length(private$objective_function)) {
          
          # We have encountered a case where symphony solves the problem and
          # returns success, but GLPK solves the problem but returns 1. Here a
          # better approach might be to try an alternate solver as opposed to
          # checking the length of the solution vector.
          #
          # It's not clear why in this situation GLPK thinks the problem has an
          # invalid basis.
          warning("Solution found but encountered GLPK return value 1 (GLP_EBADB, invalid basis). Check results")
        
        } else {
          stop(paste0("Optimization failed with status code: ", res$status))
        }
      }
      
      invisible(self)
    },
    
    #' @description Get optimization result.
    #' @return A data frame that contains the number of shares and the net
    #'   market value of the trades at the strategy and joint (net) level
    #'   for each stock in the optimization's input.
    getResultData = function() {
      
      if (length(private$solution) %in% 0) {
        stop("No result data found")
      }
      
      # Construct result data frame    
      res <- private$input_data[c("id")]
      res$order_shares_joint <- 0
      
      price_var <- private$config$getConfig("price_var")
      
      all_strategies <- private$config$getStrategyNames()
      num_securities <- nrow(private$input_data)
      
      for (strategy in private$config$getStrategyNames()) {
        strategy_index <- which(all_strategies %in% strategy)
        
        # ord_indices specify the range of columns for the orders for this
        # strategy.
        ord_indices <- 1:num_securities + (strategy_index - 1) * num_securities
        
        # Subtract the negative change variable from the positive change
        # variable to get the net trade value in each security for the strategy.
        res[[paste0("order_nmv_", strategy)]] <- private$solution[ord_indices]
        
        # Convert net market values to shares
        #
        # A subtle point here is that rounding up the gmv may be undesireable
        # for very high priced stocks.
        res[[paste0("order_shares_", strategy)]] <-
          round(res[[paste0("order_nmv_", strategy)]] / private$input_data[[price_var]])
        
        # Recompute net market value post-rounding
        res[[paste0("order_nmv_", strategy)]] <- res[[paste0("order_shares_", strategy)]] *
          private$input_data[[price_var]]
        
        res$order_shares_joint <- res$order_shares_joint + res[[paste0("order_shares_", strategy)]]
      }
      
      # Compute trade.value.joint after netting down shares.
      res$order_nmv_joint <- res$order_shares_joint * private$input_data[[price_var]]
      
      invisible(res)
    },
    
    #' @description Provide aggregate level optimization information if the
    #'   problem has been solved.
    #' @return A data frame with one row per strategy, including the joint (net)
    #'   level, and columns for starting and ending market values and factor
    #'   expoure values.
    summaryDf = function() {
      
      # TODO Provide pre-solution output.
      if (is.null(private$solution)) {
        stop("Call solve() before running summaryDf()")
      }
      
      price_var <- private$config$getConfig("price_var")
      all_strategies <- private$config$getStrategyNames()

      # Grab a list of all numeric factors used in constraints.
      factor_vars <- c()
      for (strategy in all_strategies) {
        constraint_config <- private$config$getStrategyConfig(strategy, "constraints")
        factor_vars <- c(factor_vars,
                         sapply(constraint_config, function(x) {
                           if (x$type %in% "factor") {
                             x$in_var
                           } else {
                             NULL
                           }
                         }))
      }
      factor_vars <- unique(unlist(factor_vars))

      # Compute start nmv
      start_nmv_df <- private$input_data[c("id", factor_vars)]
      start_nmv_df[["joint"]] <- 0

      for (strategy in all_strategies) {
        this_start_shares_var <- paste0("shares_", strategy)
        start_nmv_df[[strategy]] <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
        start_nmv_df[["joint"]] <- start_nmv_df[["joint"]] + start_nmv_df[[strategy]]
      }
      summary_df_long <- start_nmv_df %>%
        gather(strategy, start_nmv, one_of(c(all_strategies, "joint")))
      
      
      order_nmv_df <- self$getResultData() %>% select(id, contains("order_nmv_"))
      names(order_nmv_df) <- gsub("order_nmv_", "", names(order_nmv_df))
      order_nmv_df_long <- order_nmv_df %>%
        gather(strategy, order_nmv, one_of(c(all_strategies, "joint")))
      summary_df_long <- inner_join(summary_df_long, order_nmv_df_long, by = c("id","strategy"))
      
      stopifnot(isTRUE(all.equal(nrow(summary_df_long),
                                 nrow(private$input_data) * (length(all_strategies) + 1))))
      
      
      summary_df_long <- summary_df_long %>% mutate(end_nmv = start_nmv + order_nmv)
      
      # Compute factor exposures
      for (factor_var in factor_vars) {
        summary_df_long[[paste0("start_", factor_var)]] <-
          summary_df_long[[factor_var]] * summary_df_long$start_nmv
        summary_df_long[[paste0("end_", factor_var)]] <-
          summary_df_long[[factor_var]] * summary_df_long$end_nmv
        
      }
      
      # Calculate aggregates
      res <- summary_df_long %>%
        group_by(strategy) %>%
        summarise(start_smv = sum(start_nmv[start_nmv < 0]),
                  start_lmv = sum(start_nmv[start_nmv > 0]),
                  order_gmv = sum(abs(order_nmv)),
                  end_smv = sum(end_nmv[end_nmv < 0]),
                  end_lmv = sum(end_nmv[end_nmv > 0]))

      
      if (!is.null(factor_vars)) {
        # Perhaps there's a cleaner way; selecting factor columns below depends on the construction
        # process of summary_df_long above.
        start_factor_col <- paste0("start_", factor_vars[[1]])
        end_factor_col <- paste0("end_", factor_vars[[length(factor_vars)]])
        res <- inner_join(
          res,
          select(summary_df_long, c("strategy", start_factor_col:end_factor_col)) %>%
            group_by(strategy) %>%
            summarise_all(sum, na.rm = TRUE),
          by = c("strategy")
        )
      }
      res[order(match(res$strategy, c(all_strategies, "joint"))),] %>% data.frame
    },
    
    #' @description Print summary information. 
    #' @return No return value, called for side effects.
    print = function() {
      # TODO Improve this print method.
      if (is.null(private$solution)) {
        print("Unsolved PortOpt")
      } else {
        cat("PortOpt:\n\n")
        print(self$summaryDf())
      }
    }),
    
    private = list(
      
      # The objective function and bounds on variables.
      objective_function = c(),
      
      variable_bounds = list(upper = c(), lower = c()),
      
      # Constraints, direction (e.g., >=, ==, <=), and right-hand-side value.
      #
      # Because constraints are not added all at once, store as a list of
      # sub-matrices that are combined as needed.
      constraint_matrices = list(),
      dir = c(),
      rhs = c(),
      
      # A record of loosened constraints.
      loosened_constraints = list(),
      
      # Model and solution (after calling solve()). Should solve() return a new object?
      solution = NULL,
      model = NULL,
      
      # Settings outside the config file
      target_weights = list(),
      
      # Object of class StrategyConfig
      config = NULL,
      
      input_data = NULL,
      
      verbose = FALSE,
      
      setTargetWeight = function(strategy, long, short) {
        stopifnot(strategy %in% private$config$getStrategyNames())
        private$target_weights[[strategy]] <- list(long = long, short = short)
        invisible(self)
      },

      addObjectiveFunction = function() {
        
        for (strategy in private$config$getStrategyNames()) {
          
          this_in_var <- private$config$getStrategyConfig(strategy, "in_var")
          if (!isTRUE(this_in_var %in% names(private$input_data))) {
            stop(paste0("Input variable ", this_in_var, " not found for strategy ", strategy))
          }
          private$objective_function <- c(private$objective_function, private$input_data[[this_in_var]])
        }
        
        # Net trade variables get a coefficient of zero.
        private$objective_function <- c(private$objective_function, rep(0, nrow(private$input_data)))
        invisible(self)
      },
      
      validateInputData = function() {
        
        # Checks need to be expanded
        required_columns <- c("id",
                              "investable",
                              private$config$getConfig("vol_var"),
                              private$config$getConfig("price_var"))
        
        missing_columns <- required_columns[!required_columns %in%
                                              names(private$input_data)]
        if (length(missing_columns) > 0) {
          stop(paste0("Must have the following columns in input_data: ",
                      paste0(missing_columns, collapse = ", ")))
        }
        
        stopifnot(all(!is.na(private$input_data$id)),
                  sum(duplicated(private$id)) %in% 0)
        
        stopifnot(is.logical(private$input_data$investable),
                  !any(is.na(private$input_data$investable)))
        
        for (strategy in private$config$getStrategyNames())
          strategy_shares_var <- paste0("shares_", strategy)
        
        if (!strategy_shares_var %in% names(private$input_data)) {
          stop(paste0("Missing column ", strategy_shares_var, " in input data."))
        }
        
        TRUE
      },
      
      getConstraintValue = function(constraint_name) {
        private$constraint_matrices[[constraint_name]] %*%
          private$getPortfolioMatrix()
      },
      
      # Helpers for computing total GMV
      getOverallIdealGMV = function() {
        overall_ideal_gmv <- 0
        
        for (strategy in private$config$getStrategyNames()) {
          
          strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
          
          ideal_long_weight <- private$config$getStrategyConfig(strategy, "ideal_long_weight")
          ideal_short_weight <- private$config$getStrategyConfig(strategy, "ideal_short_weight")
          
          overall_ideal_gmv <- overall_ideal_gmv +
            strategy_capital * (ideal_long_weight + ideal_short_weight)
        }
        overall_ideal_gmv
      },
      
      getOverallTargetGMV = function() {
        overall_target_gmv <- 0
        
        for (strategy in private$config$getStrategyNames()) {
          strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
          
          overall_target_gmv <- overall_target_gmv + 
            strategy_capital * (private$target_weights[[strategy]]$long + private$target_weights[[strategy]]$short)
          
        }
        overall_target_gmv
      },
      
      # Helpers that could be pushed to a different class
      getOverallCurrentGMV = function() {
        overall_current_gmv <- 0
        
        price_var <- private$config$getConfig("price_var")
        
        for (strategy in private$config$getStrategyNames()) {
          this_start_shares_var <- paste0("shares_", strategy)
          strategy_nmv <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
          
          # Calculate current long/short weight.
          overall_current_gmv <- overall_current_gmv +
            sum(strategy_nmv[strategy_nmv > 0]) +
            abs(sum(strategy_nmv[strategy_nmv < 0]))
        }
        overall_current_gmv
      },
      
      getOverallTargetLMV = function() {
        overall_target_lmv <- 0
        
        for (strategy in private$config$getStrategyNames()) {
          strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
          
          overall_target_lmv <- overall_target_lmv + 
            strategy_capital * private$target_weights[[strategy]]$long
          
        }
        overall_target_lmv
      },
      
      # Helpers that could be pushed to a different class
      getOverallCurrentLMV = function() {
        overall_current_lmv <- 0
        
        price_var <- private$config$getConfig("price_var")
        
        for (strategy in private$config$getStrategyNames()) {
          this_start_shares_var <- paste0("shares_", strategy)
          strategy_nmv <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
          
          overall_current_lmv <- overall_current_lmv +
            sum(strategy_nmv[strategy_nmv > 0])
        }
        overall_current_lmv
      },
      
      getOverallTargetSMV = function() {
        overall_target_smv <- 0
        
        for (strategy in private$config$getStrategyNames()) {
          strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
          
          overall_target_smv <- overall_target_smv + 
            -1 * strategy_capital * private$target_weights[[strategy]]$short
          
        }
        overall_target_smv
      },
      
      # Helpers that could be pushed to a different class
      getOverallCurrentSMV = function() {
        overall_current_smv <- 0
        
        price_var <- private$config$getConfig("price_var")
        
        for (strategy in private$config$getStrategyNames()) {
          this_start_shares_var <- paste0("shares_", strategy)
          strategy_nmv <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
          
          # Calculate current long/short weight.
          overall_current_smv <- overall_current_smv +
            sum(strategy_nmv[strategy_nmv < 0])
        }
        overall_current_smv
      },
      
      # From config file values compute and set the weight targets by populating
      # the target_weights member of the object. If the target_weights member is
      # already populated for a strategy, do nothing.
      computeTargetWeights = function() {
        
        for (strategy in private$config$getStrategyNames()) {
          
          if (!is.null(private$target_weights[[strategy]])) {
            next;
          }
          
          price_var <- private$config$getConfig("price_var")
          this_start_shares_var <- paste0("shares_", strategy)
          strategy_nmv <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
          strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
          
          ideal_long_weight <- private$config$getStrategyConfig(strategy, "ideal_long_weight")
          ideal_short_weight <- private$config$getStrategyConfig(strategy, "ideal_short_weight")
          
          # Calculate current long/short weight.
          current_lmv <- sum(strategy_nmv[strategy_nmv > 0])
          current_smv <- sum(strategy_nmv[strategy_nmv < 0])
          current_long_weight <- current_lmv / strategy_capital
          current_short_weight <- abs(current_smv / strategy_capital)
          
          target_weight_policy <- private$config$getConfig("target_weight_policy")
          if (isTRUE(all.equal(target_weight_policy, "half-way"))) {
            
            # If there is a target_weight_policy of 'half-way' (trade half-way to
            # the ideal weight) set in the config file, set the target long/short
            # weight for this strategy accordingly.
            private$setTargetWeight(strategy,
                                    (ideal_long_weight - current_long_weight) * 0.5 + current_long_weight,
                                    (ideal_short_weight - current_short_weight) * 0.5 + current_short_weight)
            
          } else if (is.null(target_weight_policy) || isTRUE(all.equal(target_weight_policy, "full"))) {
            private$setTargetWeight(strategy,
                                    ideal_long_weight,
                                    ideal_short_weight)
          } else {
            stop(paste0("Invalid target_weight_policy: ", target_weight_policy))
          }
        }
        
        invisible(self)
      },
      
      # Construct a constraint matrix row that only affects a single strategy. The
      # row has 0s for all strategies other than 'strategy'. 'strategy_constr' has
      # length 2 * the number of securities.
      getStrategyConstraintMatrix = function(strategy, strategy_constr) {
        
        num_securities <- nrow(private$input_data)
        num_strategies <- length(private$config$getStrategyNames())
        strategy_index <- which(private$config$getStrategyNames() %in% strategy)
        
        matrix(
          c(
            # 0s for all strategies that come before this strategy:
            rep(0, (strategy_index - 1) * num_securities),
            
            # The constraint values for the strategy
            strategy_constr,
            
            # 0s for all strategies that come after this strategy:
            rep(0, (num_strategies - strategy_index) * num_securities),
            
            # 0s for the net trade variables
            rep(0, num_securities)
          ),
          nrow = 1,
          byrow = TRUE
        )
      },
      
      # Compute a vector representing starting portfolio positions consistent with
      # the problem formulation. For example, if strategy 1 has a position of
      # -10,000 in stock 1, we have x_{1,1} = -10,000. All net trade variables are
      # set to zero (should we set them to the absolute value of the position?).
      getPortfolioMatrix = function() {
        
        price_var <- private$config$getConfig("price_var")
        
        portfolio <- c()
        
        for (strategy in private$config$getStrategyNames()) {
          
          this_start_shares_var <- paste0("shares_", strategy)
          strategy_nmv <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
          stopifnot(!any(is.na(strategy_nmv)))
          
          portfolio <- c(portfolio, strategy_nmv)
        }
        
        # Add entries for net trades.
        portfolio <- matrix(c(portfolio, rep(0, nrow(private$input_data))), ncol = 1)
        invisible(portfolio)
        
      },
      
      # Set stock-by-stock trading limits
      #
      # Very important note about trading/position limits. The following two
      # assumptions affect the implementation of other constraints:
      #
      # 1. Side switching is not allowed
      #
      # 2. Sign of input signal governs side. If a stock has an alpha of 0 it
      # will be treated as uninvestable.
      addTradingLimits = function() {
        
        stopifnot(length(private$variable_bounds$upper) %in% 0,
                  length(private$variable_bounds$lower) %in% 0)
        
        vol_var <- private$config$getConfig("vol_var")
        price_var <- private$config$getConfig("price_var")
        
        for (strategy in private$config$getStrategyNames()) {
          
          trading_limit_pct_adv <- private$config$getStrategyConfig(strategy, "trading_limit_pct_adv")
          
          # Calculate per-stock trading limit by multiplying the percentage volume
          # limit by anticipated average trading volume.
          trading_limit <- private$input_data[[price_var]] * 
            private$input_data[[vol_var]] * trading_limit_pct_adv / 100
          
          # Should have a config method that returns the starting shares column
          # for a given strategy (or even a method that returns a vector of
          # position nmvs).
          this_start_shares_var <- paste0("shares_", strategy)
          strategy_nmv <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
          
          stopifnot(!any(is.na(strategy_nmv)))
          
          # What are the upper and lower bounds on our positions' market value?
          #
          # First, compute the bounds dictated by the strategy's position limit
          # configuration parameters:
          #   * position_limit_pct_adv
          #   * position_limit_pct_lmv
          #   * position_limit_pct_smv
          position_limit_pct_adv <- private$config$getStrategyConfig(strategy, "position_limit_pct_adv")
          position_limit_pct_lmv <- private$config$getStrategyConfig(strategy, "position_limit_pct_lmv")
          position_limit_pct_smv <- private$config$getStrategyConfig(strategy, "position_limit_pct_smv")
          
          # Grab the strategy's capital level to compute the ideal long and short market value.
          strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
          ideal_lmv <- strategy_capital * private$config$getStrategyConfig(strategy, "ideal_long_weight")
          ideal_smv <- strategy_capital * private$config$getStrategyConfig(strategy, "ideal_short_weight")
          
          # pos_upper_limit is the higest (signed) market value allowed based on position
          # limit configuration.
          pos_upper_limit <- pmin(private$input_data[[price_var]] * private$input_data[[vol_var]] * position_limit_pct_adv / 100,
                                  ideal_lmv * position_limit_pct_lmv / 100)
          
          # pos_lower_limit is the lowest (signed) market value allowed based on
          # position limit configuration.
          pos_lower_limit <- -1 * 
            pmin(private$input_data[[price_var]] * private$input_data[[vol_var]] * position_limit_pct_adv / 100,
                 ideal_smv * position_limit_pct_smv / 100)
          
          # Set pos_upper_limit and pos_lower_limit to zero for stocks that are
          # not investable
          pos_upper_limit[!private$input_data$investable] <- 0
          pos_lower_limit[!private$input_data$investable] <- 0
          
          # Side switching is not allowed. So the pos_upper_limit value for a
          # current short position is 0. Vice-versa, the pos_lower_limit value for
          # a current long position is 0.
          pos_upper_limit <- ifelse(strategy_nmv >= 0, pos_upper_limit, 0)
          pos_lower_limit <- ifelse(strategy_nmv <= 0, pos_lower_limit, 0)
          
          # Restrict side of entry based on the sign of the input
          # variable (alpha). We can only add new longs for securities with
          # positive alpha, while we can only add new shorts for securities with
          # negative alpha.
          this_in_var <- private$config$getStrategyConfig(strategy, "in_var")
          strategy_alpha <- private$input_data[[this_in_var]]
          pos_upper_limit <- ifelse(strategy_nmv == 0 & strategy_alpha <= 0, 0, pos_upper_limit)
          pos_lower_limit <- ifelse(strategy_nmv == 0 & strategy_alpha >= 0, 0, pos_lower_limit)
          
          # A position can't be larger (smaller, meaning more negative) than it's current level plus the amount
          # we're allowed to add (subtract) in one day.
          pos_upper_limit <- pmin(pos_upper_limit, strategy_nmv + trading_limit)
          pos_lower_limit <- pmax(pos_lower_limit, strategy_nmv - trading_limit)
          
          # That said, if a position is already larger (smaller) than the amount allowed by
          # position limit configuration, the current level is the upper (lower) limit.
          pos_upper_limit <- ifelse(strategy_nmv > pos_upper_limit,
                                    strategy_nmv,
                                    pos_upper_limit)
          pos_lower_limit <- ifelse(strategy_nmv < pos_lower_limit,
                                    strategy_nmv,
                                    pos_lower_limit)
          
          stopifnot(all(pos_upper_limit >= pos_lower_limit))
          
          # Now that we've worked out the allowable position range, compute the
          # allowable trade amount (our variable limits) by comparing the range
          # to the current positions.
          trade_lower_limit <- round(pos_lower_limit - strategy_nmv, 20)
          trade_upper_limit <- round(pos_upper_limit - strategy_nmv, 20)
          
          private$variable_bounds$upper <- c(private$variable_bounds$upper, trade_upper_limit)
          private$variable_bounds$lower <- c(private$variable_bounds$lower, trade_lower_limit)
        }
        
        # Add lower and upper bounds for the N net trade variables, all [0, Inf).
        private$variable_bounds$upper <- c(private$variable_bounds$upper,
                                        rep(Inf, nrow(private$input_data)))
        private$variable_bounds$lower <- c(private$variable_bounds$lower,
                                        rep(0, nrow(private$input_data)))
        
        invisible(self)
      },
      
      
      # Add individual strategy total long market value and short market value
      # constraints.
      addStrategyMarketValueConstraints = function() {
        
        for (strategy in private$config$getStrategyNames()) {
          
          price_var <- private$config$getConfig("price_var")
          this_start_shares_var <- paste0("shares_", strategy)
          this_in_var <- private$config$getStrategyConfig(strategy, "in_var")
          strategy_nmv <- private$input_data[[this_start_shares_var]] * private$input_data[[price_var]]
          strategy_alpha <- private$input_data[[this_in_var]]
          strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
          
          # Calculate current lmv/smv and current long/short weight.
          current_lmv <- sum(strategy_nmv[strategy_nmv > 0])
          current_smv <- sum(strategy_nmv[strategy_nmv < 0])
          current_long_weight <- current_lmv / strategy_capital
          current_short_weight <- abs(current_smv / strategy_capital)
          
          # The bounds are set to the ideal long and short market values specified
          # in the config, unless target values have been provided to this object
          # using setTargetWeight.
          
          target_lmv <- strategy_capital * private$target_weights[[strategy]]$long
          target_smv <- -1 * strategy_capital * private$target_weights[[strategy]]$short
          
          if (isTRUE(private$verbose)) {
            cat("Strategy ", strategy,
                " target LMV = ", target_lmv,
                ", target SMV =",target_smv, "\n", sep = "")
          }
          
          # Note that this constraint relies on knowing the side of the position
          # in a stock *before* trading. To do this we assume the following:
          #
          # 1. Side switching is impossible, which is enforced when setting
          # position limits. So if we have a position in a stock, we count the
          # market value for that stock toward the total market value of the side
          # corresponding to the position.
          #
          # 2. For stocks in which we are flat (no position) we count the market
          # value of the stock toward the side that matches the sign of the alpha.
          # In other words, we can only enter long positions in stocks with
          # positive alpha, and we can only enter short positions in stocks with
          # negative alpha.
          
          constr_lmv <- as.numeric(strategy_nmv > 0 | (strategy_nmv == 0 & strategy_alpha > 0))
          constr_smv <- as.numeric(strategy_nmv < 0 | (strategy_nmv == 0 & strategy_alpha < 0))
          
          # Since our variables represent trades, the bounds on our gmv
          # constraints are the amount required to trade from the current long or
          # short market value to the target values.
          
          self$addConstraints(private$getStrategyConstraintMatrix(strategy, constr_lmv),
                              "==", target_lmv - current_lmv,
                              paste0(strategy, " lmv"))
          self$addConstraints(private$getStrategyConstraintMatrix(strategy, constr_smv),
                              "==", target_smv - current_smv,
                              paste0(strategy, " smv"))
          
          invisible(self)
        }
      },
      
      addFactorConstraints = function() {
        
        for (strategy in private$config$getStrategyNames()) {
          constraint_config <- private$config$getStrategyConfig(strategy, "constraints")
          
          for (constraint_name in names(constraint_config)) {
            
            in_var <- constraint_config[[constraint_name]]$in_var
            constraint_type <- constraint_config[[constraint_name]]$type
            
            if (!constraint_type %in% "factor") next
            if (!in_var %in% names(private$input_data)) {
              stop(paste0("Constraint ", constraint_name, " in_var ", in_var, " not present in input data"))
            }
            
            factor_values <- private$input_data[[in_var]]
            
            # Set NA factor values to 0. Another option would be to require all factor
            # values in input_data to be non-NA.
            if (any(is.na(factor_values)) && isTRUE(private$verbose)) {
              cat("Setting NA values of ", in_var, " to 0 when building constraint.\n", sep = "")
            }
            factor_values[is.na(factor_values)] <- 0
            
            strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")          
            upper_bound <- constraint_config[[constraint_name]]$upper_bound * strategy_capital
            lower_bound <- constraint_config[[constraint_name]]$lower_bound * strategy_capital
            
            # To calculate a factor exposure constraint we multiply the factor
            # value by the order amount.
            constraint <- private$getStrategyConstraintMatrix(strategy, factor_values)
            
            current_value <- as.vector(constraint %*% private$getPortfolioMatrix())
            
            self$addConstraints(constraint, "<=", upper_bound - current_value,
                                paste0(strategy, " ", constraint_name, " upper"))
            self$addConstraints(constraint, ">=", lower_bound - current_value,
                                paste0(strategy, " ", constraint_name, " lower"))
          }
        }
        invisible(self)
      },
      
      addCategoryConstraints = function() {
        for (strategy in private$config$getStrategyNames()) {
          constraint_config <- private$config$getStrategyConfig(strategy, "constraints")
          
          for (constraint_name in names(constraint_config)) {
            in_var <- constraint_config[[constraint_name]]$in_var
            constraint_type <- constraint_config[[constraint_name]]$type
            
            # Only work on category constraints
            if (!constraint_type %in% "category") next
            
            stopifnot(in_var %in% names(private$input_data))
            category_values <- private$input_data[[in_var]]
            if (any(is.na(category_values))) {
              stop(paste0("Missing category values for ", in_var))
            }
            
            strategy_capital <- private$config$getStrategyConfig(strategy, "strategy_capital")
            upper_bound <- constraint_config[[constraint_name]]$upper_bound * strategy_capital
            lower_bound <- constraint_config[[constraint_name]]$lower_bound * strategy_capital
            
            for (category_level in unique(category_values)) {
              # For each level of the category construct a row for the upper and
              # lower bound.
              #
              # TODO collapse the addCategoryConstraints and addFactorConstraints
              # methods. We can think of a category constraint as simply a
              # constraint on a factor that has 1s for securities in the category
              # and 0s for securities not in the category.
              
              constraint <- private$getStrategyConstraintMatrix(
                strategy, as.numeric(category_values %in% category_level))
              
              current_value <- as.vector(constraint %*% private$getPortfolioMatrix())
              
              self$addConstraints(constraint, "<=", upper_bound - current_value,
                                  paste0(strategy, " ", constraint_name, " ", category_level, " upper"))
              self$addConstraints(constraint, ">=", lower_bound - current_value,
                                  paste0(strategy, " ", constraint_name, " ", category_level, " lower"))
            }
          }
        }
      },
      
      addAbsNetTradeConstraint = function() {
        
        num_securities <- nrow(private$input_data)
        num_strategies <- length(private$config$getStrategyNames())
  
        # Add constraints so that y_i equals the absolute value of the net trade
        # across strategies, i.e., y_i = |\sum_s x_{i,s}|.
        diag_mat <- Diagonal(num_securities, x = 1)
        
        net_trade_mat <- do.call("cbind",
                                 lapply(1:num_strategies, function(x) {
                                   diag_mat
                                 }))
        
        # For each i, \sum_s x_{i,s} - y_i <= 0
        lt_constraint_mat <- cbind2(net_trade_mat, -1 * diag_mat)
        self$addConstraints(lt_constraint_mat,
                            rep("<=", num_securities),
                            rep(0, num_securities),
                            "Net trade <=")
        
        # For each i, \sum_s x_{i,s} + y_i >= 0
        gt_constraint_mat <- cbind2(net_trade_mat, diag_mat)
        self$addConstraints(gt_constraint_mat,
                            rep(">=", num_securities),
                            rep(0, num_securities),
                            "Net trade >=")
        
        invisible(self)
      },
      
      addTurnoverConstraint = function() {
        
        turnover_limit <- private$config$getConfig("turnover_limit")
        if (is.null(turnover_limit)) {
          return(self)
        }
        
        turnover_limit <- as.numeric(turnover_limit)
        
        stopifnot(length(turnover_limit) %in% 1)
        
        # Set the effective turnover limit to be the maximum of the config file
        # turnover_limit value and the sum of the absolute value of the difference between
        # the current and target LMV and SMV.
        dist_lmv <- abs(private$getOverallCurrentLMV() - private$getOverallTargetLMV())
        dist_smv <- abs(private$getOverallCurrentSMV() - private$getOverallTargetSMV())
        turnover_limit <- max(turnover_limit, dist_lmv + dist_smv)
        
        num_securities <- nrow(private$input_data)
        num_strategies <- length(private$config$getStrategyNames())
        
        # \sum_i y_i <= turnover_limit
        net_trade_mat <- cbind2(Matrix(data = 0, nrow = 1, ncol = num_strategies * num_securities),
                                Matrix(data = 1, nrow = 1, ncol = num_securities))
        
        self$addConstraints(net_trade_mat,
                            "<=",
                            turnover_limit,
                            "Turnover limit")
        
        invisible(self)
      },
      loosen = function(loosen_coef) {
        if (loosen_coef %in% 0) {
          return(self)
        }
        
        # Grab all constraints that are currently out of bounds. Note that we
        # explicitly exclude the market value constraints, where are equality
        # constraints.
        #
        # TODO determine if, when and how to loosen market value constraints. TODO
        # write a method that returns constraint names to avoid ambiguity (and
        # possible disconnects).
        mkt_value_constr_names <- as.vector(
          sapply(private$config$getStrategyNames(),
                 function(x) { paste0(x, c(" lmv", " smv")) })
        )
        
        out_of_bounds <- filter(self$getConstraintMeta(),
                                !.data$name %in% mkt_value_constr_names &
                                  !.data$within_bounds)
        
        # For each constraint, set the constraint bound (rhs value) to be X%
        # closer to 0 where X = loosen_coef.
        for (i in seq_len(nrow(out_of_bounds))) {
          this_row <- out_of_bounds[i,]
          
          stopifnot(isTRUE(all.equal(this_row$idx_start, this_row$idx_end)))
          
          private$rhs[this_row$idx_start] <- private$rhs[this_row$idx_start] * (1 - loosen_coef)
          
          # Record the loosening. Note that the loosen_coef is cumulative.
          if (is.null(private$loosened_constraints[[this_row$name]])) {
            private$loosened_constraints[[this_row$name]] <- loosen_coef
          } else {
            private$loosened_constraints[[this_row$name]] <- 
              private$loosened_constraints[[this_row$name]] * (1 - loosen_coef)
          }
        }
        
      }
      
))

