#' Simulation class
#'
#' @description Class for running a simulation and getting results.
#'
#' @details The \code{Simulation} class is used to set up and run a daily
#'   simulation over a particular period. Portfolio construction parameters and
#'   other simulator settings can be configured in a yaml file that is passed to
#'   the object's constructor. See \code{vignette("strand")} for information on
#'   configuration file setup.
#' 
#' @export
Simulation <- R6Class(
  "Simulation",
  public = list(

    #' @description Create a new \code{Simulation} object.
    #' @param config An object of class \code{list} or \code{character}, or
    #'   \code{NULL}. If the value passed is a character vector, it should be of
    #'   length 1 and specify the path to a yaml configuration file that
    #'   contains the object's configuration info. If the value passed is of
    #'   class list(), the list should contain the object's configuration info
    #'   in list form (e.g, the return value of calling \code{yaml.load_file} on
    #'   the configuration file). If the value passed is \code{NULL}, then there
    #'   will be no configuration information associated with the simulation and
    #'   it will not possible to call the \code{run} method. Setting
    #'   \code{config = NULL} is useful when creating simulation objects into
    #'   which results will be loaded with \code{readFeather}.
    #' @param raw_input_data A data frame that contains all of the input data
    #'   (for all periods) for the simulation. The data frame must have a
    #'   \code{date} column. Data supplied using this parameter will be
    #'   used if the configuration option \code{simulator/input_data/type} is
    #'   set to \code{object}. Defaults to \code{NULL}.
    #' @param input_dates Vector of class \code{Date} that specifies  when input
    #'   data should be updated. If data is being supplied using the
    #'   \code{raw_input_data} parameter, then \code{input_dates} defaults to
    #'   set of dates present in this data.
    #' @param raw_pricing_data A data frame that contains all of the input data
    #'   (for all periods) for the simulation. The data frame must have a
    #'   \code{date} column. Data supplied using this parameter will only be
    #'   used if the configuration option \code{simulator/pricing_data/type} is
    #'   set to \code{object}. Defaults to \code{NULL}.
    #' @param security_reference_data A data frame that contains reference data
    #'   on the securities in the simulation, including any categories that are
    #'   used in portfolio construction constraints. Note that the simulator
    #'   will throw an error if there are input data records for which there is
    #'   no entry in the security reference. Data supplied using this parameter
    #'   will only be used if the configuration option
    #'   \code{simulator/secref_data/type} is set to \code{object}. Defaults to
    #'   \code{NULL}.
    #' @param delisting_data A data frame that contains delisting dates and
    #'   associated returns. It must contain three columns: id (character),
    #'   delisting_date (Date), and delisting_return (numeric). The date in the
    #'   delisting_date column means the day on which a stock will be removed
    #'   from the simulation portfolio. It is typically the day after the last
    #'   day of trading. The delisting_return column reflects what, if any, P&L
    #'   should be recorded on the delisting date. A delisting_return of -1
    #'   means that the shares were deemed worthless. The delisting return is
    #'   multiplied by the starting net market value of the position to
    #'   determine P&L for the delisted position on the delisting date. Note
    #'   that the portfolio optimization does not include stocks that are being
    #'   removed due to delisting. Data supplied using this parameter will only
    #'   be used if the configuration option
    #'   \code{simulator/delisting_data/type} is set to \code{object}. Defaults
    #'   to \code{NULL}.
    #' @return A new \code{Simulation} object.
    initialize = function(config = NULL,
                          raw_input_data = NULL,
                          input_dates = NULL,
                          raw_pricing_data = NULL,
                          security_reference_data = NULL,
                          delisting_data = NULL) {
      
      if (is.character(config)) {
        if (!file.exists(config)) {
          stop(paste0("Config file not found: ", config))
        }
        config <- yaml.load_file(config)
        private$config <- StrategyConfig$new(config)
      } else if (is.list(config)) {
        private$config <- StrategyConfig$new(config)
      } else if (is.null(config)) {
        return(self)
      } else {
        stop("config must be of class list or character, or NULL")
      }
      
      # Set security_reference field
      #
      # TODO Improve the mechanism by which we set up the object using
      # constructor parameters or file / database resources.
      secref_config <- private$config$getConfig("simulator")$secref_data
      stopifnot(!is.null(secref_config$type),
                length(secref_config$type) %in% 1,
                is.character(secref_config$type))
      
      if (secref_config$type %in% "file") {
        private$security_reference <- read_feather(secref_config$filename)
      } else if (secref_config$type %in% "object") {
        private$security_reference <- security_reference_data
      } else {
        stop(paste0("Invalid config value for secref_config/type: ", secref_config$type))
      }

      # Set delisting_data field. Delisting data is not required. If no
      # delisting_data parameter is specified in the config, set the
      # delisting_data field to an empty data frame with the appropriate
      # columns.
      #
      # TODO Pull setup logic for secref and delisting data into helper
      # methods.
      delistings_config <- private$config$getConfig("simulator")$delisting_data
      if (is.null(delistings_config)) {
        private$delisting_data <- data.frame(id = character(0),
                                              delisting_date = structure(numeric(0), class = "Date"),
                                              delisting_return = numeric(0))
      } else {
      
        stopifnot(!is.null(delistings_config$type),
                  length(delistings_config$type) %in% 1,
                  is.character(delistings_config$type))
        
        if (delistings_config$type %in% "file") {
          private$delisting_data <- read_feather(delistings_config$filename)
        } else if (delistings_config$type %in% "object") {
          private$delisting_data <- delisting_data
        } else {
          stop(paste0("Invalid config value for delistings_config/type: ", delistings_config$type))
        }
      }
      
      # Set raw data from constuctor parameters.
      
      if (!is.null(raw_input_data)) {
        if (private$config$getConfig("simulator")$input_data$type %in% "file") {
          stop("Passing data via raw_input_data but configuration specifies file-based inputs")
        }
        stopifnot("date" %in% names(raw_input_data))
        private$raw_input_data <- raw_input_data
      } else {
        if (private$config$getConfig("simulator")$input_data$type %in% "object") {
          stop("raw_input_data is NULL but configuration specifies object-based inputs")
        }
      }
      
      if (!is.null(raw_pricing_data)) {
        if (private$config$getConfig("simulator")$pricing_data$type %in% "file") {
          stop("Passing data via raw_pricing_data but configuration specifies file-based inputs")
        }
        stopifnot("date" %in% names(raw_pricing_data))
        private$raw_pricing_data <- raw_pricing_data
      } else {
        if (private$config$getConfig("simulator")$pricing_data$type %in% "object") {
          stop("raw_pricing_data is NULL but configuration specifies object-based inputs")
        }
      }
      
      if (isTRUE(private$config$getConfig("simulator")$verbose)) {
        private$verbose <- TRUE
      }
      
      # Set dates
      if (is.null(input_dates) & !is.null(raw_input_data)) {
        private$input_dates <- unique(sort(raw_input_data$date))
      } else {
        private$input_dates <- input_dates
      }
      
      invisible(self)
    },
    
    #' @description Set the verbose flag to control info output.
    #' @param verbose Logical flag indicating whether to be verbose or not.
    #' @return No return value, called for side effects.
    setVerbose = function(verbose) {
      stopifnot(is.logical(verbose),
                length(verbose) %in% 1)
      private$verbose <- verbose
      invisible(self)
    },

    #' @description Set the callback function for updating progress when running
    #'   a simulation in shiny.
    #' @param callback A function suitable for updating a shiny Progress object.
    #'   It must have two parameters: \code{value}, indicating the progress
    #'   amount, and detail, and \code{detail}, a text string for display on the
    #'   progress bar.
    #' @return No return value, called for side effects.
    setShinyCallback = function(callback) {
      if (!is.function(callback)) {
        stop("callback must be a function")
      }
      private$shiny_callback <- callback
      invisible(self)
    },
    
    #' @description Get security reference information.
    #' @return An object of class \code{data.frame} that contains the security
    #'   reference data for the simulation.
    getSecurityReference = function() {
      invisible(private$security_reference)
    },
  
    #' @description Run the simulation.
    #' @return No return value, called for side effects.
    run = function() {

      if (is.null(private$config)) {
        stop("Can not run simulation: no configuration defined.")  
      }
      
      # Grab simulator section of config
      simulator_config <- private$config$getConfig("simulator")

      # Input data object
      stopifnot(!is.null(simulator_config$input_data$type),
                length(simulator_config$input_data$type) %in% 1,
                is.character(simulator_config$input_data$type),
                simulator_config$input_data$type %in% c("object", "file"))
      
      if (simulator_config$input_data$type %in% "object") {
        input_data_obj <- CrossSection$new(TRUE)
        input_data_obj$setRaw(private$raw_input_data)
      } else if (simulator_config$input_data$type %in% "file") {
        input_data_obj <- CrossSectionFromFile$new(TRUE,
                                                   simulator_config$input_data$directory,
                                                   simulator_config$input_data$prefix)
      } else {
        stop("Unsupported value for input_data/type")
      }
      
      if (!is.null(simulator_config$input_data$na_replace)) {
        input_data_obj$setNAReplaceValue(simulator_config$input_data$na_replace)
      }

      # Pricing data object
      stopifnot(!is.null(simulator_config$pricing_data$type),
                length(simulator_config$pricing_data$type) %in% 1,
                is.character(simulator_config$pricing_data$type),
                simulator_config$pricing_data$type %in% c("object", "file"))

      if (simulator_config$pricing_data$type %in% "object") {
        pricing_data_obj <- CrossSection$new(TRUE)
        pricing_data_obj$setRaw(private$raw_pricing_data)
      } else if (simulator_config$pricing_data$type %in% "file") {
        pricing_data_obj <- CrossSectionFromFile$new(TRUE,
                                                   simulator_config$pricing_data$directory,
                                                   simulator_config$pricing_data$prefix)
      } else {
        stop("Unsupported value for pricing_data/type")
      }
      pricing_data_obj$setColumnMap(simulator_config$pricing_data$columns)
      pricing_data_obj$setCarryForwardValue(list(
        volume = 0,
        adjustment_ratio = 1,
        dividend = as.numeric(NA),
        distribution = as.numeric(NA)
      ))
      
      all_strategies <- private$config$getStrategyNames()
      portfolio <- Portfolio$new(all_strategies)
      
      all_dates <- self$getSimDates()

      for (current_date in as.list(all_dates)) {

        if (isTRUE(private$verbose)) {
          cat("[", private$config$getConfig("name"), "] Working on ", format(current_date), "\n", sep = "")
        }
        
        if (is.function(private$shiny_callback)) {
          private$shiny_callback(which(all_dates %in% current_date) / length(all_dates),
                              paste0("Working on ", format(current_date)))
        }

        # Retrieve data for the current period.
        #
        # TODO Impose restrictions on allowable input data column names. For
        # example, columns of the form shares_{strategy name} will collide with
        # the simulator's work columns. There are other ways around this issue
        # but imposing column restrictions is the easiest.
        
        if (!is.null(private$input_dates) && !current_date %in% private$input_dates) {
          input_data <- input_data_obj$getCurrent()
        } else {
          input_data <- input_data_obj$update(current_date)
          
          # Collect metadata as specified in the config (such as correlation of
          # values from one period to the next).
          if (!is.null(simulator_config$input_data$track_metadata)) {
            input_stats <- input_data_obj$periodStats(simulator_config$input_data$track_metadata)
            private$saveInputStats(current_date, input_stats)
          }
        }
        
        if (nrow(input_data) %in% 0) {
          stop("Cannot formulate optimization: no input data found")  
        }

        pricing_data <- pricing_data_obj$update(current_date)

        # Properties we enforce on input data:
        #
        # 1. Each security with an entry in input_data must have an entry in
        # pricing_data *unless* the simulator is configured to omit inputs
        # records that have not yet been priced.
        #
        # 2. Each security in which there is a non-zero position has an entry in
        # pricing_data and input_data.
        #
        # 3. All securities must be present in the security reference.
        stopifnot(
          all(input_data$id %in% private$security_reference$id),
          all(portfolio$getPositions()$id %in% private$security_reference$id),
          all(portfolio$getPositions()$id %in% pricing_data$id),
          all(portfolio$getPositions()$id %in% input_data$id)
        )
        
        if (!all(input_data$id %in% pricing_data$id)) {
          if (!isTRUE(simulator_config$inputs_without_pricing %in% "omit")) {
            stop(paste0("Input records (",
                        sum(!input_data$id %in% pricing_data$id),
                        ") found without pricing data. Consider setting simulator/inputs_without_pricing=omit"))
          } else {
            if (isTRUE(private$verbose)) {
              cat("Omitting ",
                  sum(!input_data$id %in% pricing_data$id),
                  " input records without pricing data: ",
                  paste0(input_data$id[!input_data$id %in% pricing_data$id], collapse = ", "),
                  "\n")
            }
            input_data <- filter(input_data, .data$id %in% pricing_data$id)
          }
        } 
        
        input_data <- input_data %>%
          rename(inputs_carry_forward = carry_forward)
        
        pricing_data <- pricing_data %>%
          select("id",
                 "close_price", "prior_close_price",
                 "adjustment_ratio", "volume",
                 "dividend", "distribution","carry_forward") %>%
          rename(pricing_carry_forward = carry_forward)
        
        stopifnot(all(pricing_data$close_price > 0),
                  all(pricing_data$prior_close_price > 0))
        
        # A further adjustment is required for pricing data that is
        # carried-forward: the prior close price must be set to the same value
        # as the close price.
        #
        # TODO Stop requiring that the user passes in the prior close price.
        # Once we remove this requirement we can keep track of prior values as
        # we iterate over period-by-period cross sections (and can remove logic
        # like the below).
        pricing_data$prior_close_price <- ifelse(pricing_data$pricing_carry_forward,
                                                 pricing_data$close_price,
                                                 pricing_data$prior_close_price)
        
        # Create start and end price columns to keep things clear. The start
        # price must be adjusted by the adjustment ratio. In the case of a 2:1
        # split, the adjustment ratio is 0.5 (number of old shares over number
        # of new shares). So, we multiply the previous day's unadjusted price by
        # the adjustment ratio to bring yesterday's price in line with today's.
        #
        # Note that data sanitization (replacing NAs with default values) should
        # be moved to CrossSection.
        pricing_data <- pricing_data %>%
          mutate(start_price = .data$prior_close_price * .data$adjustment_ratio,
                 end_price = .data$close_price,
                 volume = replace_na(.data$volume, 0),
                 adjustment_ratio = replace_na(.data$adjustment_ratio, 1),
                 dividend = replace_na(.data$dividend, 0),
                 distribution = replace_na(.data$distribution, 0)
                 )
        
        portfolio <- portfolio$applyAdjustmentRatio(
          select(pricing_data, "id", "adjustment_ratio"))

        # Process delistings
        
        # Delistings are handled in the following way:
        #
        # 1. All securities that have a delisting date that is equal to or
        # earlier than current_date are recorded in the id_delisted vector.
        # These stocks are marked not investable.
        #
        # 2. Positions in delisted stocks are recorded in the pos_delisted data
        # frame. They are omitted from the day's portfolio optimization.
        #
        # 3. "Delisting trades" are added as part of the EOD bookkeeping
        # sequence so that positions in delisted stocks are flattened. In the
        # day's detail data, the logical column 'delisting' indicates that the
        # trade was due to a delisting.
        #
        # TODO For delisted stocks we can go back to our data interfaces and
        # ensure data for them is not carried forward after the delisting date.
        # This will decrease the size of our inputs by omitting securities we
        # know we will never trade again.
        
        # We could save time and use equality on current_date here, but we run
        # the risk of having dead securities in the portfolio.
        id_delisted <- private$delisting_data$id[private$delisting_data$delisting_date <= current_date]
        pos_delisted <- portfolio$getPositions() %>% filter(.data$id %in% id_delisted &
                                                            (.data$int_shares != 0 | .data$ext_shares != 0))
        pos_delisted <- pos_delisted %>%
          left_join(select(pricing_data, "id", "start_price"), by = "id") %>%
          left_join(private$delisting_data, by = "id")
        
        if (nrow(pos_delisted) > 0) {
          # Record delisting info
          private$saveDelistings(current_date, pos_delisted)
        }
        
        # Merge together consolidated, wide format positions, pricing data,
        # security reference and inputs data for passing to the optimization.
        # This merge will create rows with NA volues for stocks that have not
        # previously appeared in the data feed. May want to use a special column
        # prefix for shares columns (as opposed to "shares_") to avoid name
        # conflict with user columns.
        #
        # Note that the reference price for the optimization is the start_price.
        # We could save some cycles by setting price_var = "start_price" in the
        # config.
        #
        # Note the importance of the assertion above that each position appear
        # in the input data feed. If the assertion were to be false, then we
        # would lose position records in first left join.
        #
        # TODO slim down security reference so that it only contains columns
        # used in category constraints.
        input_data <-
          left_join(input_data,
                    portfolio$getConsolidatedPositions(),
                    by = "id") %>%
          left_join(pricing_data,
                    by = "id") %>%
          left_join(private$security_reference,
                    by = "id") %>%
          mutate(ref_price = .data$start_price,
                 investable = TRUE,
                 delisting = .data$id %in% pos_delisted$id) %>%
          mutate_at(.vars = vars(portfolio$getShareColumns()),
                    .funs = ~ replace_na(., 0))
          
          
        # Set investable flag based on
        #
        # 1. Delisting status
        # 2. Universe configuration
        #
        # TODO Move universe logic into PortOpt class. It's OK to sort out
        # delistings in the simulator, but each strategy should be able to have
        # its own universe.
        input_data$investable <- !input_data$id %in% id_delisted
        
        # By default, presence in the latest set of input data affects
        # investability: stocks that are not in the latest update are not
        # investable. This makes it easy to define the investable universe
        # simply as the stocks present in the input cross-section.
        #
        # This behavior may not always be desirable, and can be controlled by
        # setting the simulator/inputs_define_universe configuration option to
        # FALSE. For example, a user may want to pass in updated factor data for
        # stocks that are no longer in the universe but may be in the portfolio.
        #
        # Note that simulator/inputs_define_universe = TRUE is the same as
        # having the expression `!inputs_carry_forward` as part of the
        # simulator/universe configuration parameter.
        #
        # Note also that any expression in simulator/universe will be applied
        # regardless of the setting of simulator/inputs_define_universe.
        
        # TODO We need to validate config entries and set defaults in the
        # StrategyConfig class.
        inputs_define_universe <- simulator_config$inputs_define_universe
        stopifnot(is.null(inputs_define_universe) || is.logical(inputs_define_universe))

        if (is.null(inputs_define_universe) || isTRUE(inputs_define_universe)) {
          input_data$investable <- input_data$investable & !input_data$inputs_carry_forward
        }
        
        if (!is.null(simulator_config$universe) && length(simulator_config$universe) > 0) {
          univ <- eval(rlang::parse_expr(simulator_config$universe), envir = input_data)
          if (any(is.na(univ))) {
            stop("Found NA universe values")
          }
          input_data$investable <- input_data$investable & univ
        }
  
        # Normalization of input variables and risk factors.
        
        # Perform normalization for variables listed in
        # simulator/normalize_in_vars.
        #
        # The procedure is as follows:
        #
        # 1. Set variable values for non-investable securities to NA.
        # 2. Normalize variable to N(0, 1).
        # 3. Replace NAs created in step 1 with 0.
        #
        # Store raw (pre-normalized) values for column foo in the detail dataset
        # column foo_raw.
        if (!is.null(simulator_config$normalize_in_vars)) {
          for (normalize_var in simulator_config$normalize_in_vars) {
            raw_normalize_var <- paste0(normalize_var, "_raw")
            input_data <- input_data %>%
              mutate(
                !! raw_normalize_var := get(normalize_var),
                !! normalize_var := replace_na(normalize(ifelse(investable, get(normalize_var), NA)), 0))
          }
        }

        # Perform normalization for variables listed in
        # simulator/normalize_factor_vars.
        #
        # factor_vars (variables used in constraint calculations) are normalized
        # as follows:
        #
        # 1. Set variable values for non-investable securities *in which there
        # is no position* to NA.
        # 2. Normalize variable to N(0, 1).
        # 3. Replace NAs created in step 1 with 0.
        #
        # The idea is that in_var values should be 0 for stocks that are
        # non-investable to encourage exiting, while factor_vars for positions
        # in such stocks should be preserved so that accurate exposures can be
        # calculated.
        if (!is.null(simulator_config$normalize_factor_vars)) {
          for (normalize_var in simulator_config$normalize_factor_vars) {
            raw_normalize_var <- paste0(normalize_var, "_raw")
            # Preserve factor_var value if there is a position in any strategy.
            non_zero_pos <- rowSums(abs(input_data[portfolio$getShareColumns()])) != 0
            input_data <- input_data %>%
              mutate(
                !! raw_normalize_var := get(normalize_var),
                !! normalize_var := replace_na(normalize(ifelse(investable | non_zero_pos, get(normalize_var), NA)), 0))
          }
        }

        stopifnot(!any(is.na(input_data)))

        # Make a copy of input_data to pass to the PortOpt class. We do this to
        # accomodate delistings, which will be removed from the portfolio later
        # in the process, and which should not contribute to the current day's
        # constraint calculations.

        opt_input <- input_data

        if(nrow(pos_delisted) > 0) {
          
          # Omit positions in stocks that will be exited due to delisting from the
          # optimization. Do this by setting their current share levels to 0 in
          # the optimization's input data.

          opt_input <- opt_input %>%
            mutate_at(.vars = portfolio$getShareColumns(),
                      .funs = ~ replace(., delisting, 0))
        }
        
        # Create problem and solve
        portOpt <- PortOpt$new(private$config, opt_input)
        portOpt$solve()
        
        # Record information on loosened constraints
        if (length(portOpt$getLoosenedConstraints()) > 0) {
          
          loosened_df <- data.frame(date = current_date,
                                    constraint_name = names(portOpt$getLoosenedConstraints()),
                                    pct_loosened = 100 * (1 - as.vector(unlist(portOpt$getLoosenedConstraints()))))
          private$saveLooseningInfo(current_date, loosened_df)
        }

        orders <- portOpt$getResultData() %>%
          select("id", contains("shares"))
        
        # Handle positions that have grown too large.
        #
        # After the optimization has generated orders, if the force_trim_factor
        # is set, add orders to trim positions that are too large.
        #
        # A force_trim_factor value of X means "if a position grows larger than
        # X times its max position size, force a trade to trim the position back
        # to X times its max position size."
        #
        # So if a security has a max long position size of 10,000, its current
        # size is 15,000, and force_trim_factor = 1.2, we add a trade to sell
        # 3,000 so that the post-trade position size is 1.2 times max position
        # size = 12,000.
        #
        # TODO Trim positions in PortOpt as part of the optimization process
        # (when setting variable limits). We will still want the ability to force
        # trim positions, however, since these limits will also be subject to
        # loosening when no solution can be found.
        if (!is.null(simulator_config$force_trim_factor)) {

          force_trim_factor <- simulator_config$force_trim_factor
          stopifnot(is.numeric(force_trim_factor),
                    force_trim_factor >= 1)
          
          vol_var <- private$config$getConfig("vol_var")
          too_big <- input_data %>%
            filter(!delisting) %>%
            select("id", !!vol_var, "start_price",
                   !!portfolio$getShareColumns()) %>%
            pivot_longer(cols = portfolio$getShareColumns(),
                         names_to = "strategy",
                         names_prefix = "shares_",
                         values_to = "shares") %>%
            left_join(portOpt$getMaxPosition(), by = c("id", "strategy")) %>%
            mutate(pos_nmv = shares * start_price,
                   max_pos_trim = ifelse(shares > 0, max_pos_lmv, max_pos_smv) *
                     force_trim_factor) %>%
            
            # Grab those positions that are above the market value trim
            # threshold.
            filter(abs(shares * start_price) > abs(max_pos_trim)) %>%
            
            # Compute how much can be trimmed
            left_join(portOpt$getMaxOrder(), by = c("id", "strategy")) %>%
            mutate(
              
              # trim_gmv is the lesser of the amount required to trade the
              # position down to max_pos_trim and the maximum order size.
              trim_gmv = pmin(abs(pos_nmv) - abs(max_pos_trim), max_order_gmv),
              
              order_shares = -1 * sign(shares) * floor(trim_gmv / start_price)) %>%
            
            # Filter out cases where we are less than 1 share away from the
            # max
            filter(order_shares != 0) %>%
            select("id", "strategy", "order_shares")

          if (nrow(too_big) > 0) {
            
            # Calculate joint level shares.
            joint_level <- too_big %>%
              group_by(id) %>%
              summarise(
                strategy = "joint",
                order_shares = sum(order_shares))
            
            too_big <- rbind(too_big, joint_level)
            
            # Now pivot back to wide format
            too_big <- too_big %>%
              pivot_wider(names_from = "strategy",
                          values_from = "order_shares",
                          names_prefix = "order_shares_")
            
            # Adjust column order to match data frame 'orders'
            too_big <- too_big[names(orders)]
            
            # Finally: remove orders generated by the optimization (if any) and
            # add the force-exit orders.
            orders <- filter(orders, !.data$id %in% too_big$id)
            orders <- rbind(orders, too_big)
          }
        }
        
        # Handle non-investable securities.
        #
        # Often due to a changing universe the portfolio will have positions in
        # stocks that are not part of the investable universe. Setting the
        # simulator/force_exit_non_investable configuration parameter controls
        # how these positions are handled. If the
        # simulator/force_exit_non_investable parameter is not set, such
        # positions are allowed in the portfolio (but increasing their size is
        # prohibited in the optimization).
        #
        # TODO Exit non-investable positions in PortOpt as part of the
        # optimization process (when setting variable limits). Like position
        # trimming, we will still want the ability to force exit these
        # positions, since these limits will also be subject to loosening when
        # no solution can be found.
        #
        # TODO Refactor force-trimming above and force-exiting below to remove
        # duplicated code.
        if (isTRUE(simulator_config$force_exit_non_investable)) {
          
          vol_var <- private$config$getConfig("vol_var")
          non_investable <- input_data %>%
            filter(!investable & ! delisting) %>%
            select("id", !!vol_var, start_price, !!portfolio$getShareColumns()) %>%
            pivot_longer(cols = portfolio$getShareColumns(),
                         names_to = "strategy",
                         names_prefix = "shares_",
                         values_to = "shares") %>%
            filter(shares != 0)
          
          if (nrow(non_investable) > 0) {

            # Call floor on abs share value to round toward zero (to avoid
            # over-sizing in corner cases).
            non_investable <- non_investable %>%
              left_join(portOpt$getMaxOrder(), by = c("id", "strategy")) %>%
              mutate(
                pos_nmv = shares * start_price,
                exit_gmv = pmin(abs(pos_nmv), max_order_gmv),
                order_shares = -1 * sign(shares) * floor(exit_gmv / start_price)) %>%
              select("id", "strategy", "order_shares")
            
            # Calculate joint level shares.
            joint_level <- non_investable %>%
              group_by(id) %>%
              summarise(
                strategy = "joint",
                order_shares = sum(order_shares))
            
            non_investable <- rbind(non_investable, joint_level)
            
            # Now pivot back to wide format
            non_investable <- non_investable %>%
              pivot_wider(names_from = "strategy",
                          values_from = "order_shares",
                          names_prefix = "order_shares_")
            
            # Adjust column order to match data frame 'orders'
            non_investable <- non_investable[names(orders)]
            
            # Finally: remove orders generated by the optimization (if any) and
            # add the force-exit orders.
            orders <- filter(orders, !.data$id %in% non_investable$id)
            orders <- rbind(orders, non_investable)
          }
        }

        # Handle delistings
        #
        # Here we enter delisting trades so that the entire positions in stocks
        # that are delisted are removed by EOD. Note that below, fill rate for
        # delisting trades will be set to 1 regardless of volume.
        if (any(input_data$delisting)) {

          delistings <- input_data %>%
            filter(delisting) %>%
            select("id", !!portfolio$getShareColumns()) %>%
            pivot_longer(cols = portfolio$getShareColumns(),
                         names_to = "strategy",
                         names_prefix = "shares_",
                         values_to = "order_shares") %>%
            # Completely exit the position
            mutate(order_shares = -1 * order_shares) %>%
            # Pivot back
            pivot_wider(names_from = "strategy",
                        values_from = "order_shares",
                        names_prefix = "order_shares_")
          
          
          # Recalculate order_shares_joint
          delistings$order_shares_joint <- rowSums(select(delistings, -"id"))
          
          # Adjust column order to match data frame 'orders'
          delistings <- delistings[names(orders)]
          
          orders <- filter(orders, !.data$id %in% delistings$id)
          orders <- rbind(orders, delistings)
        }

        # Now that the order generation process is complete, join orders back to the
        # original cross-section.
        stopifnot(setequal(orders$id, input_data$id))
        input_data <-
          inner_join(input_data, orders, by = "id")

        # Now for each strategy, separate orders that need to be worked in the
        # market from orders that net down with orders from other strategies.
        
        # 0. Pre-compute the maximum number of shares that are available for
        # fills, computed as the product of the configured fill rate and the
        # day's dollar trading volume.
        stopifnot(!is.null(simulator_config$fill_rate_pct_vol),
                  simulator_config$fill_rate_pct_vol > 0)
        
        input_data$fill_shares_max <-
            round(input_data$volume * simulator_config$fill_rate_pct_vol / 100)

        # Compute the fill rate ahead of time for each stock. The fill rate is 1
        # if the maximum number of shares available (fill_shares_max) is greater
        # than the size of the total order across all strategies. Computing this
        # value makes fill calculations easier.
        input_data$fill_rate <-
          ifelse(input_data$order_shares_joint %in% 0 |
                   abs(input_data$order_shares_joint) <= input_data$fill_shares_max |
                   input_data$delisting,
                 1,
                 input_data$fill_shares_max / abs(input_data$order_shares_joint))
        
        stopifnot(!any(is.na(input_data$fill_shares_max)))
        
        # 1. Compute the total number of shares bought and sold across all
        # strategies.
        input_data$buy_shares_joint <- input_data$sell_shares_joint <- 0
        for (strategy in private$config$getStrategyNames()) {
          this_order_shares <- input_data[[paste0("order_shares_", strategy)]]
          
          input_data$buy_shares_joint <- input_data$buy_shares_joint + 
            ifelse(this_order_shares > 0, this_order_shares, 0)
          input_data$sell_shares_joint <- input_data$sell_shares_joint +
            ifelse(this_order_shares < 0, this_order_shares, 0)
        }

        strategy_name_regexp <- paste0(private$config$getStrategyNames(), collapse = "|")
        
        # 2. Using the totals computed in (1), pivot the data to long format to
        # calculate fills.
        res <- input_data %>%
          select("id", "fill_rate",
                 contains("shares")) %>%
          # Is there a cleaner way to select all strategy columns for the pivot?
          # Note that the below names_pattern requires that strategy names only
          # consist of alpha numeric characters.
          pivot_longer(cols = matches(!!strategy_name_regexp),
                              names_to = c(".value", "strategy"),
                              names_pattern = paste0("(.*)_(", strategy_name_regexp, ")$")) %>%
          left_join(portfolio$getPositions(), by = c("id", "strategy")) %>%
          mutate(
            
            # Orders
            #
            # Calculate shares that must be traded in the market
            market_order_shares = as.integer(round(
              ifelse(.data$order_shares > 0 & .data$order_shares_joint > 0,
                     .data$order_shares / .data$buy_shares_joint * .data$order_shares_joint,
                     ifelse(.data$order_shares < 0 & .data$order_shares_joint < 0,
                            abs(.data$order_shares / .data$sell_shares_joint) * .data$order_shares_joint,
                            0)))),
            # Shares transferred to other strategies
            transfer_order_shares = as.integer(.data$order_shares - .data$market_order_shares),
            
            # Fills
            #
            # Market fills are subject to market liquidity  
            market_fill_shares = as.integer(floor(round(.data$market_order_shares * .data$fill_rate))),
            # Transfers are filled completely
            transfer_fill_shares = as.integer(.data$transfer_order_shares),
            
            int_shares = replace_na(.data$int_shares, 0L),
            ext_shares = replace_na(.data$ext_shares, 0L),
            
            # External and internal end positions are computed from market and
            # transfer fills
            end_int_shares = as.integer(.data$int_shares + .data$transfer_fill_shares),
            end_ext_shares = as.integer(.data$ext_shares + .data$market_fill_shares),
            
            # Total fill = market + transfer
            fill_shares = .data$transfer_fill_shares + .data$market_fill_shares,
            
            # End number of shares = start + fill
            end_shares = .data$shares + .data$fill_shares)
        
        res <- res %>%
          select("id", "strategy", "shares",
                        ends_with("_shares"))
        
        # Roll up strategy data to a "joint" top level with a sum.
        joint_data <- res %>%
          select(-"strategy") %>%
          group_by(.data$id) %>%
          summarise_all(sum) %>%
          mutate(strategy = "joint")
        
        res <- rbind(res, joint_data)
        
        # Compute market values, costs, and P&L.
        
        stopifnot(
          !is.null(simulator_config$transaction_cost_pct),
          simulator_config$transaction_cost_pct >= 0,
          !is.null(simulator_config$financing_cost_pct),
          simulator_config$financing_cost_pct >= 0
        )

        res <- res %>%
          inner_join(select(input_data, "id", "start_price", "end_price", "dividend", "distribution",
                            "investable", "delisting",
                            !!simulator_config$add_detail_columns),
                            by = "id") %>%
          mutate(
            # Position P&L is computed by comparing the value of the starting
            # position to the value of the ending position (including adjustments
            # for dividends and distributions).
            position_pnl = .data$shares * 
              (.data$end_price + .data$dividend + .data$distribution - .data$start_price),
            
            # Trading P&L is computed by comparing the end price to the benchmark
            # price.
            #
            # Temporarily: assuming trading at the close, so trading P&L is
            # zero.
            trading_pnl = 0,
            
            # Apply trade costs
            #
            # Temporarily: trade costs are a fixed percentage of the notional of the market
            # trade (valued at the close). There is no cost to transfer to other
            # strategies.
            trade_costs = abs(.data$market_fill_shares * .data$end_price) *
              simulator_config$transaction_cost_pct / 100,
            
            # Apply financing costs
            #
            # Temporarily: financing costs are a fixed percentage of the notional of the
            # starting value of the portfolio's external positions. External
            # positions are positions held on the street and are recorded in the
            # ext_shares column.
            #
            # Use a standard 360-day-count methodology, charging for three days
            # on Monday.
      
            financing_costs = abs(.data$ext_shares * .data$start_price) *
              simulator_config$financing_cost_pct / 100 / 360 *
              ifelse(weekdays(current_date, FALSE) %in% "Monday", 3, 1),

            gross_pnl = .data$position_pnl + .data$trading_pnl,
            net_pnl = .data$gross_pnl - .data$trade_costs - .data$financing_costs,
            
            # TODO use benchmark price in trade valuations
            market_order_gmv = abs(.data$market_order_shares * .data$end_price),
            market_fill_nmv = .data$market_fill_shares * .data$end_price,
            market_fill_gmv = abs(.data$market_fill_nmv),
            transfer_fill_nmv = .data$transfer_fill_shares * .data$end_price,
            transfer_fill_gmv = abs(.data$transfer_fill_nmv),
            
            start_nmv = (.data$int_shares + .data$ext_shares) * .data$start_price,
            end_nmv = .data$end_shares * .data$end_price,
            end_gmv = abs(.data$end_nmv))

        # Finish processing delistings
        if (any(res$delisting)) {
          # Work on rows corresponding to delistings
          res_delisting <- filter(res, delisting)
          
          # First make sure the ending position is flat.
          if (any(res_delisting$end_int_shares != 0) ||
              any(res_delisting$end_ext_shares != 0) ||
              any(res_delisting$end_shares != 0)) {
            stop("Found non-zero ending position in delisted security")
          }
          
          # Bring in delisting_return
          res_delisting <- res_delisting %>% left_join(select(pos_delisted, id, delisting_return), by = "id")
          
          if (any(is.na(res_delisting$delisting_return))) {
            stop("Found NA delisting return")
          }
          
          if (!is.numeric(res_delisting$delisting_return)) {
            stop("Delisting return must be numeric")
          }
          
          if (any(res_delisting$delisting_return < -1)) {
            stop("Delisting return can not be less than -1")
          }
          
          # Ensure that all P&L and costs for the delisting are zero. Then set
          # gross and net P&L to the delisting return times starting net
          # market value.
          res_delisting <- res_delisting %>%
            mutate(
              position_pnl = 0,
              trading_pnl = 0,
              trade_costs = 0,
              financing_costs = 0,
              gross_pnl = start_nmv * delisting_return,
              net_pnl = gross_pnl
            ) %>%
            select(-delisting_return)
          
          # Replace the original rows with the rows we worked on
          res <- filter(res, !delisting)
          res <- rbind(res, res_delisting)
        }
        
        # Bring in max position information
        res <- res %>%
          left_join(portOpt$getMaxPosition(), by = c("strategy", "id"))
        
        # if (nrow(filter(res, strategy %in% "joint" & transfer_fill_gmv != 0))) browser()
        
        # Simple sums
        summary_sum_data <- res %>%
          select("strategy",
                        ends_with("_nmv"),
                        ends_with("_gmv"),
                        ends_with("_pnl"),
                        ends_with("_costs")) %>%
          group_by(.data$strategy) %>%
          summarise_all(sum) %>%
          ungroup() %>%
          mutate(fill_rate_pct = 100 * .data$market_fill_gmv / .data$market_order_gmv)

        # More complex aggregation on nmv
        summary_addl_data <- res %>%
          select("strategy", "end_nmv", "start_nmv") %>%
          group_by(.data$strategy) %>%
          summarise(
            end_lmv = sum(.data$end_nmv[.data$end_nmv > 0]),
            end_smv = sum(.data$end_nmv[.data$end_nmv < 0]),
            start_lmv = sum(.data$start_nmv[.data$start_nmv > 0]),
            start_smv = sum(.data$start_nmv[.data$start_nmv < 0]),
            end_num = sum(.data$end_nmv != 0),
            end_num_long = sum(.data$end_nmv > 0),
            end_num_short = sum(.data$end_nmv < 0)
            )

        summary_data <- 
          inner_join(summary_sum_data,
                            summary_addl_data, by = "strategy")
        
        # Update positions
        portfolio$setPositions(
          filter(res, !.data$strategy %in% "joint") %>%
           select("id",
                  "strategy",
                  "int_shares" = "end_int_shares",
                  "ext_shares" = "end_ext_shares"))
        
        # Calculate EOD exposures.
        #
        # First prepare data. Columns for category grouping are static and live
        # in this object's security_reference member. Factor data is different
        # each day and can be found in 'input_data'.
        #
        # TODO automatically calculate exposures for all categories and factors
        # used in constraints.
        category_vars <- simulator_config$calculate_exposures$category_vars
        factor_vars <- simulator_config$calculate_exposures$factor_vars
        if (!is.null(category_vars) || !is.null(factor_vars)) {

          exposures_input <-
            res %>%
            select("strategy", "id", "end_nmv", "end_gmv") %>%
            left_join(select(private$security_reference, "id", one_of(!!category_vars)),
                             by = "id") %>%
            left_join(select(input_data, "id", one_of(!!factor_vars)),
                             by = "id") %>%
            mutate(end_lmv = ifelse(end_nmv > 0, end_nmv, 0),
                   end_smv = ifelse(end_nmv < 0, end_nmv, 0))
          
          # Save net exposures
          exposures <- calculate_exposures(detail_df = exposures_input,
                                           in_var = "end_nmv",
                                           weight_divisor = private$getStrategyCapital(),
                                           category_vars = category_vars,
                                           factor_vars = factor_vars)
          
          private$saveExposures(current_date, exposures, type = "net")
          
          # Save long exposures
          exposures <- calculate_exposures(detail_df = exposures_input,
                                           in_var = "end_lmv",
                                           weight_divisor = private$getStrategyCapital(),
                                           category_vars = category_vars,
                                           factor_vars = factor_vars)
          
          private$saveExposures(current_date, exposures, type = "long")

          # Save short exposures
          exposures <- calculate_exposures(detail_df = exposures_input,
                                           in_var = "end_smv",
                                           weight_divisor = private$getStrategyCapital(),
                                           category_vars = category_vars,
                                           factor_vars = factor_vars)
          
          private$saveExposures(current_date, exposures, type = "short")
          
          # Save gross exposures
          exposures <- calculate_exposures(detail_df = exposures_input,
                                           in_var = "end_gmv",
                                           weight_divisor = private$getStrategyCapital(),
                                           category_vars = category_vars,
                                           factor_vars = factor_vars)
          
          private$saveExposures(current_date, exposures, type = "gross")
          
        }
        
        # Save sim summary, sim detail, and optimization data.
        private$saveSimSummary(current_date, summary_data)
        
        # TODO Add flags that control saving of other data. Right now we can
        # only turn saving detail off and on.
        if (is.null(simulator_config$skip_saving) ||
            !"detail" %in% simulator_config$skip_saving) {
          
          # The full detail dataset is large. To save specific columns, use the
          # simulator/keep_detail_columns parameter.
          
          if (!is.null(simulator_config$keep_detail_columns)) {
            keep_detail_columns <-
                unique(c("id", "strategy",
                       simulator_config$keep_detail_columns))
            res <- res %>% select(!!keep_detail_columns)
          }
          
          private$saveSimDetail(current_date, res)
        }
        private$saveOptimizationSummary(current_date, portOpt$summaryDf())
      }

      invisible(self)
    },
    
    #' @description Get a list of all date for the simulation.
    #' @return A vector of class \code{Date} over which the simulation currently iterates: all
    #' weekdays between the 'from' and 'to' dates in the simulation's config.
    getSimDates = function() {
      from <- as.Date(private$config$getConfig("from"))
      to <- as.Date(private$config$getConfig("to"))
      
      if (to < from) {
        stop("to date must not be earlier than from date")
      }
      
      # The simulator currently iterates over each weekday between the dates
      # 'from' and 'to'.
      all_dates <- seq(from = from, to = to, by = "1 day")
      all_dates <- all_dates[!weekdays(all_dates, abbreviate = FALSE) %in% c("Saturday", "Sunday")]
      
      # Dates can be omitted using the omit_dates simulator config option.
      omit_dates <- private$config$getConfig("simulator")$omit_dates
      if (length(omit_dates) > 0) {
        omit_dates <- as.Date(omit_dates)
        all_dates <- all_dates[!all_dates %in% omit_dates]
      }

      all_dates
    },
    
    #' @description Get summary information.
    #' @param strategy_name Character vector of length 1 that specifies the
    #'   strategy for which to get detail data. If \code{NULL} data for all
    #'   strategies is returned. Defaults to \code{NULL}.
    #' @return An object of class \code{data.frame} that contains summary data
    #'   for the simulation, by period, at the joint and strategy level. The data
    #'   frame contains the following columns:
    #'   \describe{
    #'     \item{strategy}{Strategy name, or 'joint' for the aggregate strategy.}
    #'     \item{sim_date}{Date of the summary data.}
    #'     \item{market_fill_nmv}{Total net market value of fills that do not
    #'     net down across strategies.}
    #'     \item{transfer_fill_nmv}{Total net market value of fills that
    #'     represent "internal transfers", i.e., fills in one strategy that net
    #'     down with fills in another. Note that at the joint level this column
    #'     by definition is 0.}
    #'     \item{market_order_gmv}{Total gross market value of orders that do not
    #'     net down across strategies.}
    #'     \item{market_fill_gmv}{Total gross market value of fills that do not
    #'     net down across strategies.}
    #'     \item{transfer_fill_gmv}{Total gross market value of fills that
    #'     represent "internal transfers", i.e., fills in one strategy that net
    #'     down with fills in another.}
    #'     \item{start_nmv}{Total net market value of all positions at the start
    #'     of the period.}
    #'     \item{start_lmv}{Total net market value of all long positions at the
    #'     start of the period.}
    #'     \item{start_smv}{Total net market value of all short positions at the
    #'     start of the period.}
    #'     \item{end_nmv}{Total net market value of all positions at the end of
    #'     the period.}
    #'     \item{end_gmv}{Total gross market value of all positions at the end
    #'     of the period.}
    #'     \item{end_lmv}{Total net market value of all long positions at the
    #'     end of the period.}
    #'     \item{end_smv}{Total net market value of all short positions at the
    #'     end of the period.}
    #'     \item{end_num}{Total number of positions at the end of the period.}
    #'     \item{end_num_long}{Total number of long positions at the end of the
    #'     period.}
    #'     \item{end_num_short}{Total number of short positions at the end of
    #'     the period.}
    #'     \item{position_pnl}{The total difference between the end and start
    #'     market value of positions.}
    #'     \item{trading_pnl}{The total difference between the market value of
    #'     trades at the benchmark price and at the end price. Note: currently
    #'     assuming benchmark price is the closing price, so trading P&L is
    #'     zero.}
    #'     \item{gross_pnl}{Total P&L gross of costs, calculated as position_pnl
    #'     + trading_pnl.}
    #'     \item{trade_costs}{Total trade costs (slippage).}
    #'     \item{financing_costs}{Total financing/borrow costs.}
    #'     \item{net_pnl}{Total P&L net of costs, calculated as gross_pnl -
    #'     trade_costs - financing_costs.}
    #'     \item{fill_rate_pct}{Total fill rate across all market orders,
    #'     calculated as 100 * market_fill_gmv / market_order_gmv.}
    #'     
    #'   }
    #'   
    getSimSummary = function(strategy_name = NULL) {
      res <- bind_rows(private$sim_summary_list)
      if (!is.null(strategy_name)) {
        res <- filter(res, .data$strategy %in% !!strategy_name)
      }
      invisible(res)
    },
    
    #' @description Get detail information.
    #' @param sim_date Vector of length 1 of class Date or character that
    #'   specifies the period for which to get detail information. If
    #'   \code{NULL} then data from all periods is returned. Defaults
    #'   to \code{NULL}.
    #' @param strategy_name Character vector of length 1 that specifies the
    #'   strategy for which to get detail data. If \code{NULL} data for all
    #'   strategies is returned. Defaults to \code{NULL}.
    #' @param security_id Character vector of length 1 that specifies the
    #'   security for which to get detail data. If \code{NULL} data for all
    #'   securities is returned. Defaults to \code{NULL}.
    #' @param columns Vector of class character specifying the columns to
    #'   return. This parameter can be useful when dealing with very large
    #'   detail datasets.
    #' @return An object of class \code{data.frame} that contains security-level
    #'   detail data for the simulation for the desired strategies, securities,
    #'   dates, and columns. Available columns include:
    #'   \describe{
    #'     \item{id}{Security identifier.}
    #'     \item{strategy}{Strategy name, or 'joint' for the aggregate strategy.}
    #'     \item{sim_date}{Date to which the data pertains.}
    #'     \item{shares}{Shares at the start of the period.}
    #'     \item{int_shares}{Shares at the start of the period that net down
    #'     with positions in other strategies.}
    #'     \item{ext_shares}{Shares at the start of the period that do not net
    #'     down with positions in other strategies.}
    #'     \item{order_shares}{Order, in shares.}
    #'     \item{market_order_shares}{Order that does not net down with orders
    #'     in other strategies, in shares.}
    #'     \item{transfer_order_shares}{Order that nets down with orders in
    #'     other strategies, in shares.}
    #'     \item{fill_shares}{Fill, in shares.}
    #'     \item{market_fill_shares}{Fill that does not net down with fills in
    #'     other strategies, in shares.}
    #'     \item{transfer_fill_shares}{Fill that nets down with fills in other
    #'     strategies, in shares.}
    #'     \item{end_shares}{Shares at the end of the period.}
    #'     \item{end_int_shares}{Shares at the end of the period that net down
    #'     with positions in other strategies.}
    #'     \item{end_ext_shares}{Shares at the end of the period that do not net
    #'     down with positions in other strategies.}
    #'     \item{start_price}{Price for the security at the beginning of the
    #'     period.}
    #'     \item{end_price}{Price for the security at the end of the period.}
    #'     \item{dividend}{Dividend for the security, if any, for the
    #'     period.}
    #'     \item{distribution}{Distribution (e.g., spin-off) for the security, if
    #'     any, for the period.}
    #'     \item{investable}{Logical indicating whether the security is part of
    #'     the investable universe. The value of the flag is set to TRUE if the
    #'     security has not been delisted and satisfies the universe criterion
    #'     provided (if any) in the \code{simulator/universe} configuration
    #'     option.}
    #'     \item{delisting}{Logical indicating whether a position in the
    #'     security was removed due to delisting. If delisting is set to TRUE,
    #'     the gross_pnl and net_pnl columns will contain the P&L
    #'     due to delisting, if any. P&L due to delisting is calculated as the
    #'     delisting return times the \code{start_nmv} of the position.}
    #'     \item{position_pnl}{Position P&L, calculated as shares * (end_price +
    #'     dividend + distribution - start_price)}
    #'     \item{trading_pnl}{The difference between the market value of
    #'     trades at the benchmark price and at the end price. Note: currently
    #'     assuming benchmark price is the closing price, so trading P&L is
    #'     zero.}
    #'     \item{trade_costs}{Trade costs, calculated as a fixed percentage (set
    #'     in the simulation configuration) of the notional of the market trade
    #'     (valued at the close).}
    #'     \item{financing_costs}{Financing cost for the position, calculated as
    #'     a fixed percentage (set in the simulation configuration) of the
    #'     notional of the starting value of the portfolio's external positions.
    #'     External positions are positions held on the street and are recorded
    #'     in the ext_shares column.}
    #'     \item{gross_pnl}{Gross P&L, calculated as position_pnl + trading_pnl.}
    #'     \item{net_pnl}{Net P&L, calculated as gross_pnl - trade_costs -
    #'     financing_costs.}
    #'     \item{market_order_nmv}{Net market value of the order that does not
    #'     net down with orders in other strategies.}
    #'     \item{market_fill_gmv}{Gross market value of the order that does not
    #'     net down with orders in other strategies.}
    #'     \item{market_fill_nmv}{Net market value of the fill that does not net
    #'     down with orders in other strategies.}
    #'     \item{market_fill_gmv}{Gross market value of the fill that does not
    #'     net down with orders in other strategies.}
    #'     \item{transfer_fill_nmv}{Net market value of the fill that nets down
    #'     with fills in other strategies.}
    #'     \item{transfer_fill_gmv}{Gross market value of the fill that nets down
    #'     with fills in other strategies.}
    #'     \item{start_nmv}{Net market value of the position at the start of the
    #'     period.}
    #'     \item{end_nmv}{Net market value of the position at the end of the
    #'     period.}
    #'     \item{end_gmv}{Gross market value of the position at the end of the
    #'     period.}
    #'     
    #'   }
    getSimDetail = function(sim_date = NULL,
                            strategy_name = NULL,
                            security_id = NULL,
                            columns = NULL) {
      
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
      
      if (!is.null(columns)) {
        detail_data <- detail_data %>% select(!!columns)
      }
      
      invisible(detail_data)
    },
    
    #' @description Get summary information by security. This method can be
    #'   used, for example, to calculate the biggest winners and losers over the
    #'   course of the simulation.
    #' @param strategy_name Character vector of length 1 that specifies the
    #'   strategy for which to get detail data. If \code{NULL} data for all
    #'   strategies is returned. Defaults to \code{NULL}.
    #' @return An object of class \code{data.frame} that contains summary
    #'   information aggregated by security. The data frame contains the
    #'   following columns:
    #'   \describe{
    #'     \item{id}{Security identifier.}
    #'     \item{strategy}{Strategy name, or 'joint' for the aggregate
    #'     strategy.}
    #'     \item{gross_pnl}{Gross P&L for the position over the entire
    #'     simulation.}
    #'     \item{gross_pnl}{Net P&L for the position over the entire
    #'     simulation.}
    #'     \item{average_market_value}{Average net market value of the
    #'     position over days in the simulation where the position was not
    #'     flat.}
    #'     \item{total_trading}{Total gross market value of trades for the
    #'     security.}
    #'     \item{trade_costs}{Total cost of trades for the security over the
    #'     entire simulation.}
    #'     \item{trade_costs}{Total cost of financing for the position over the
    #'     entire simulation.}
    #'     \item{days_in_portfolio}{Total number of days there was a position in
    #'     the security in the portfolio over the entire simulation.}
    #'   }
    getPositionSummary = function(strategy_name = NULL) {
      detail_data <- bind_rows(private$sim_detail_list)
      
      if (!is.null(strategy_name)) {
        detail_data <- detail_data %>% filter(.data$strategy %in% !!strategy_name)
      }
      
      detail_data %>%
        group_by(.data$id, .data$strategy) %>%
        summarise(gross_pnl = round(sum(gross_pnl)),
                  net_pnl = round(sum(net_pnl)),
                  average_market_value = round(mean(end_nmv[end_shares != 0])),
                  total_trading = round(sum(market_fill_gmv)),
                  trade_costs = round(sum(trade_costs)),
                  financing_costs = round(sum(financing_costs)),
                  days_in_portfolio = sum(end_shares != 0)) %>%
        arrange(-.data$gross_pnl)
    },
    
    #' @description Get input statistics.
    #' @return An object of class \code{data.frame} that contains statistics on
    #'   select columns of input data. Statistics are tracked for the columns
    #'   listed in the configuration variable
    #'   \code{simulator/input_data/track_metadata}. The data frame contains the
    #'   following columns:
    #'   \describe{
    #'     \item{period}{Period to which statistics pertain.}
    #'     \item{input_rows}{Total number of rows of input data, including
    #'     rows carried forward from the previous period.}
    #'     \item{cf_rows}{Total number of rows carried forward from the previous
    #'     period.}
    #'     \item{num_na_\emph{column}}{Number of NA values in \emph{column}.  This
    #'     measure appears for each element of \code{track_metadata}.}
    #'     \item{cor_\emph{column}}{Period-over-period correlation for \emph{column}.
    #'     This measure appears for each element of \code{track_metadata}.}
    #'  }
    getInputStats = function() {
      invisible(bind_rows(private$input_stats_list))
    },
    
    #' @description Get loosening information.
    #' @return An object of class \code{data.frame} that contains, for each
    #'   period, which constraints were loosened in order to solve the portfolio
    #'   optimization problem, if any. The data frame contains the
    #'   following columns:
    #'   \describe{
    #'     \item{date}{Date for which the constraint was loosened.}
    #'     \item{constraint_name}{Name of the constraint that was loosened.}
    #'     \item{pct_loosened}{Percentage by which the constraint was loosened,
    #'     where 100 means loosened fully (i.e., the constraint is effectively
    #'     removed).}
    #'   }
    getLooseningInfo = function() {
      invisible(bind_rows(private$loosening_info_list))
    },
    
    #' @description Get optimization summary information.
    #' @return An object of class \code{data.frame} that contains optimization
    #'   summary information, such as starting and ending factor constraint
    #'   values, at the strategy and joint level. The data frame contains the
    #'   following columns:
    #'   \describe{
    #'     \item{strategy}{Strategy name, or 'joint' for the aggregate strategy.}
    #'     \item{sim_date}{Date to which the data pertains.}
    #'     \item{order_gmv}{Total gross market value of orders generated by the
    #'     optimization.}
    #'     \item{start_smv}{Total net market value of short positions at the
    #'     start of the optimization.}
    #'     \item{start_lmv}{Total net market value of long positions at the
    #'     start of the optimization.}
    #'     \item{end_smv}{Total net market value of short positions at the end
    #'     of the optimization.}
    #'     \item{end_lmv}{Total net market value of long positions at the end of
    #'     the optimization.}
    #'     \item{start_\emph{factor}}{Total net exposure to \emph{factor} at the
    #'     start of the optimization, for each factor constraint.}
    #'     \item{end_\emph{factor}}{Total net exposure to \emph{factor} at the
    #'     start of the optimization, for each factor constraint.}
    #'   }
    getOptimizationSummary = function() {
      invisible(bind_rows(private$optimization_summary_list))
    },
    
    #' @description Get end-of-period exposure information.
    #' @param type Vector of length 1 that may be one of \code{"net"},
    #'   \code{"long"}, \code{"short"}, and \code{"gross"}.
    #' @return An object of class \code{data.frame} that contains end-of-period
    #'   exposure information for the simulation portfolio. The units of the
    #'   exposures are portfolio weight relative to strategy_captial (i.e., net
    #'   market value of exposure divided by strategy capital). The data frame
    #'   contains the following columns:
    #'   \describe{
    #'     \item{strategy}{Strategy name, or 'joint' for the aggregate strategy.}
    #'     \item{sim_date}{Date of the exposure data.}
    #'     \item{\emph{category}_\emph{level}}{Exposure to \emph{level}
    #'     within \emph{category}, for all levels of all category constraints, at the end
    #'     of the period.}
    #'     \item{\emph{factor}}{Exposure to \emph{factor}, for all factor
    #'     constraints, at the end of the period.}
    #'   }
    getExposures = function(type = "net") {
      stopifnot(type %in% c("net", "long", "short", "gross"))
      invisible(bind_rows(private$exposures_list[[type]]))
    },
    
    #' @description Get information on positions removed due to delisting.
    #' @return An object of class \code{data.frame} that contains a row for each
    #'   position that is removed from the simulation portfolio due to a
    #'   delisting. Each row contains the size of the position on the day on
    #'   which it was removed from the portfolio.
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
    getSingleStrategySummaryDf = function(strategy_name = "joint", include_zero_row = TRUE) {
      res <- filter(self$getSimSummary(), .data$strategy %in% !!strategy_name)
      
      if (isTRUE(include_zero_row)) {
        # Create a zero-value starting row with date lagged by one day.
        res <- res[c(1, 1:nrow(res)),]
        res$sim_date[1] <- res$sim_date[1] - 1
        res[1,] <- mutate_if(res[1,], is.numeric, ~ 0)
      }
      
      # Compute returns as a percentage GMV, by day and cumulative.
      #
      # These calculations should probably be done up front in the simulation
      # loop and saved in the summary dataset.
      mutate(res,
             net_ret = ifelse(end_gmv %in% 0, 0, .data$net_pnl / .data$end_gmv),
             net_cum_ret = cumsum(net_ret),
             gross_ret = ifelse(end_gmv %in% 0, 0, .data$gross_pnl / .data$end_gmv),
             gross_cum_ret = cumsum(gross_ret))
    },
    
    #' @description Draw a plot of cumulative gross and net return by date.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotPerformance = function(strategy_name = "joint") {
      
      self$getSingleStrategySummaryDf(strategy_name) %>%
        
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
    
    #' @description Draw a plot of contribution to net return on GMV for levels
    #'   of a specified category.
    #' @param category_var Plot performance contribution for the levels of
    #'   \code{category_var}. \code{category_var} must be present in the
    #'   simulation's security reference, and detail data must be present in the
    #'   object's result data.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotContribution = function(category_var, strategy_name = "joint") {
      
      summary_data <- self$getSimSummary(strategy_name) %>%
        select(sim_date, end_gmv)
      
      contrib_data <- self$getSimDetail(strategy_name = strategy_name,
                                        columns = c("id", "sim_date", "net_pnl")) %>%
        left_join(select(self$getSecurityReference(), "id", !!category_var), by = "id") %>%
        group_by(sim_date, assay_sector) %>%
        summarise(net_pnl = sum(net_pnl)) %>%
        left_join(summary_data, by = "sim_date") %>%
        mutate(net_ret = net_pnl / end_gmv) %>%
        group_by(assay_sector) %>%
        mutate(cum_net_ret = cumsum(net_ret)) %>%
        ungroup()
      
      # Adding zero-row for each group
      zero_rows <- contrib_data %>% filter(!duplicated(get(category_var))) %>%
        mutate(sim_date = sim_date - 1) %>%
        mutate_if(is.numeric, ~ 0)
      
      contrib_data <- rbind(zero_rows, contrib_data)
      
      contrib_data %>%
        ggplot(aes(x = sim_date, y = 100 * cum_net_ret, color = get(category_var), group = get(category_var))) +
        geom_line() +
        xlab("Date") + ylab("Contribution (%)") + 
        ggtitle(paste0(category_var, " Contribution to Net Return (% GMV)")) + 
        theme_light() + 
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())

    },
      
    #' @description Draw a plot of total gross, long, short, and net market
    #'   value by date.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotMarketValue = function(strategy_name = "joint") {

      if (!strategy_name %in% c("joint", private$config$getStrategyNames())) {
        stop(paste0("Invalid strategy name: ", strategy_name))
      }
      
      mv_plot_df <- select(self$getSingleStrategySummaryDf(strategy_name),
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
    
    #' @description Draw a plot of exposure to all levels in a category by date.
    #' @param in_var Category for which exposures are plotted. In order to plot
    #'   exposures for category \code{in_var}, we must have run the simulation
    #'   with \code{in_var} in the config setting
    #'   \code{simulator/calculate_exposures/category_vars}.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotCategoryExposure = function(in_var, strategy_name = "joint") {
      
      exposures <- self$getExposures() %>% filter(strategy %in% strategy_name) %>%
        select("sim_date", starts_with(in_var))

      # Make sure that there is at least one level for in_var present in the
      # object's exposure data. If only sim_date is present, then most likely
      # exposure to in_var was not calculated by the simulation.
      if (ncol(exposures) %in% 1) {
        stop(paste0("No exposure data found for in_var: ", in_var))
      }
      
      exposures %>%
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
    
    #' @description Draw a plot of exposure to factors by date.
    #' @param in_var Factors for which exposures are plotted.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotFactorExposure = function(in_var, strategy_name = "joint") {
      exposures <- self$getExposures() %>% filter(strategy %in% strategy_name)
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
    
    #' @description Draw a plot of number of long and short positions by date.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotNumPositions = function(strategy_name = "joint") {
      self$getSingleStrategySummaryDf(strategy_name) %>%
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
    
    #' @description Draw a plot of number of long and short positions by date.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotTurnover = function(strategy_name = "joint") {
      
      self$getSimSummary(strategy_name) %>%
        select("sim_date", "market_fill_gmv") %>%
        ggplot(aes(x = sim_date, y = market_fill_gmv)) + geom_bar(stat = "identity") +
        xlab("Date") + ylab("Traded GMV ($)") + 
        ggtitle("Turnover") + 
        theme_light() + 
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())  
    },
    
    #' @description Draw a plot of the universe size, or number of investable
    #'   stocks, over time.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{joint}.
    plotUniverseSize = function(strategy_name = "joint") {
      investable_data <- self$getSimDetail(strategy_name = strategy_name,
                                           columns = c("sim_date", "id", "investable"))
      investable_data %>%
        group_by(sim_date) %>%
        summarise(num_investable = sum(investable)) %>%
        ggplot(aes(x = sim_date, y = num_investable)) + geom_line() +
        xlab("Date") + ylab("Number of securities") + 
        ggtitle("Universe size") + 
        theme_light() + 
        theme(
          plot.background = element_rect(fill = NA, colour = NA),
          plot.title = element_text(size = 18),
          
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0),
          legend.title = element_blank())  
        
      
    },
    
    #' @description Draw a plot of the percentage of portfolio GMV held in
    #'   non-investable stocks (e.g., stocks that do not satisfy universe criteria)
    #'   for a given strategy. Note that this plot requires detail data.
    #' @param strategy_name Character vector of length 1 specifying the strategy
    #'   for the plot. Defaults to \code{"joint"}.
    plotNonInvestablePct = function(strategy_name = "joint") {
      investable_data <- self$getSimDetail(strategy_name = strategy_name,
                                           columns = c("sim_date", "id","end_gmv", "investable"))
      investable_data %>%
        group_by(sim_date) %>%
        summarise(pct_non_investable = 100 * (1 - sum(end_gmv[investable]) / sum(end_gmv))) %>%
        ggplot(aes(x = sim_date, y = pct_non_investable)) + geom_line() +
        xlab("Date") + ylab("Percentage GMV") +
        ggtitle("Percentage of GMV in\nnon-investable securites") + 
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
          "Max Drawdown (%)",
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
          sprintf("%0.1f", drawdown(res$gross_pnl / res$end_gmv) * 100),
          rep("", 5)
        ),
        Net = c(
          formatC(sum(res$net_pnl), big.mark = ",", digit = 0, format = "f"),
          sprintf("%0.1f", sum(res$net_pnl / res$end_gmv) * 100),
          sprintf("%0.1f", mean(res$net_pnl / res$end_gmv) * 100 * 252),
          sprintf("%0.1f", sd(res$net_pnl / res$end_gmv) * 100 * sqrt(252)),
          sprintf("%0.2f", mean(res$net_pnl / res$end_gmv) / sd(res$net_pnl / res$end_gmv) * sqrt(252)),
          sprintf("%0.1f", drawdown(res$net_pnl / res$end_gmv) * 100),
          formatC(mean(res$end_gmv), big.mark = ",", digit = 0, format = "f"),
          formatC(mean(res$end_nmv), big.mark = ",", digit = 0, format = "f"),
          formatC(mean(res$end_num), big.mark = ",", digit = 0, format = "f"),
          formatC(mean(res$market_fill_gmv), big.mark = ",", digit = 0, format = "f"),
          sprintf("%0.1f", 12 / (mean(res$market_fill_gmv) / mean(res$end_gmv) * 252 / 2))
        
        ), stringsAsFactors = FALSE)
    },
    
    #' @description Calculate return for each month and summary statistics for
    #'   each year, such as total return and annualized Sharpe. Return in data
    #'   frame format suitable for reporting.
    #' @return  The data frame contains one row for each calendar year in the
    #'   simulation, and up to seventeen columns: one column for year, one
    #'   column for each calendar month, and columns for the year's total
    #'   return, annualized return, annualized volatility, and annualized
    #'   Sharpe. Total return is the sum of daily net returns. Annualized return
    #'   is the mean net return times 252. Annualized volatility is the standard
    #'   deviation of net return times the square root of 252. Annualized Sharpe
    #'   is the ratio of annualized return to annualized volatility. All returns
    #'   are in percent.
    overallReturnsByMonthDf = function() {
      res <- self$getSingleStrategySummaryDf("joint", include_zero_row = FALSE)
      
      # Group by month and calculate return.
      month_ret_long <- group_by(res, date = as.Date(paste0(format(sim_date, "%Y-%m"), "-01"))) %>%
        summarise(ret = sum(net_ret) * 100) %>%
        ungroup() %>%
        transmute(year = lubridate::year(date),
                  month = lubridate::month(date),
                  ret)
      
      # Organize monthly returns into rows, one for each year.
      month_ret_yearly <- month_ret_long %>%
        tidyr::spread(month, ret)
      
      # Compute summary statistics for each year.
      stats_yearly <- group_by(res, year = lubridate::year(sim_date)) %>%
        summarise(
          total_ret = 100 *sum(net_ret),
          ann_ret = 100 * mean(net_ret) * 252, 
          ann_vol = 100 * sd(net_ret) * sqrt(252)) %>%
        mutate(ann_sr = ann_ret / ann_vol)
      
      # Add stats to monthly returns.
      month_ret_yearly <- month_ret_yearly %>%
        left_join(stats_yearly, by = "year")
      
      month_cols <- names(month_ret_yearly)[names(month_ret_yearly) %in% as.character(1:12)]
      
      # Replace numerical month strings with month abbreviation. Do it this way
      # to handle cases where there are fewer than 12 months.
      month_ret_yearly %>%
        rename_at(month_cols, ~ month.abb[as.numeric(.x)])
    },  
      
    #' @description Print overall simulation statistics.
    print = function() {
      if (is.null(private$sim_summary_list)) {
        print("Simulation object with no result data.")
      } else {
        print(self$overallStatsDf())  
      }
      invisible()
    },
    
    #' @description Write the data in the object to feather files.
    #' @param out_loc Directory in which output files should be created.
    #' @return No return value, called for side effects.
    writeFeather = function(out_loc) {
      
      # TODO Add getter and setter for raw config data in the Config class.
      yaml::write_yaml(private$config$config, paste0(out_loc, "/config.yaml"))
      
      
      write_feather(self$getSecurityReference(), paste0(out_loc, "/security_reference.feather"))
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
    #' @return No return value, called for side effects.
    readFeather = function(in_loc) {
      
      # TODO Check to see if this object is empty before loading up data.
      private$config <- StrategyConfig$new(yaml::yaml.load_file(paste0(in_loc, "/config.yaml")))
      
      private$security_reference <- read_feather(paste0(in_loc, "/security_reference.feather"))
      private$sim_summary_list <- list(read_feather(paste0(in_loc, "/sim_summary.feather")))
      private$sim_detail_list <- list(read_feather(paste0(in_loc, "/sim_detail.feather")))
      private$input_stats_list <- list(read_feather(paste0(in_loc, "/input_stats.feather")))
      private$loosening_info_list <- list(read_feather(paste0(in_loc, "/loosening_info.feather")))
      private$optimization_summary_list <- list(read_feather(paste0(in_loc, "/optimization_summary.feather")))
      private$exposures_list$net <- list(read_feather(paste0(in_loc, "/exposures.feather")))
      private$delistings_list <- list(read_feather(paste0(in_loc, "/delistings.feather")))
      
      warning("It will not be possible to use the sim_date parameter of getSimDetail on this object to filter detail records by period")
      
      invisible(self)
    },
    
    #' @description Get the object's configuration information.
    #' @return Object of class \code{list} that contains the simulation's
    #'   configuration information.
    getConfig = function() {
      invisible(private$config)
    },
    
    
    #' @description Write an html document of simulation results.
    #' @param res The object of class 'Simulation' which we want to write the
    #'   report about.
    #' @param out_dir Directory in which output files should be created
    #' @param out_file File name for output
    #' @param out_fmt Format in which output files should be created. The
    #'   default is html and that is currently the only option.
    #' @param contrib_vars Security reference variables for which to plot return
    #'   contribution.
    writeReport = function(out_dir, out_file, out_fmt = "html", contrib_vars = NULL) {
      rmarkdown::render(input = system.file("reports/simReport.Rmd",
                                            package = "strand"),
                        output_format = paste0(out_fmt, "_document"),
                        output_file = out_file,
                        output_dir = out_dir,
                        params = list(res = self, contrib_vars = contrib_vars),
                        quiet = TRUE)
    }
  ),
  
  private = list(
    
    # Configuration and input data
    
    config = NULL,
    raw_input_data = NULL,
    input_dates = NULL,
    raw_pricing_data = NULL,
    security_reference = NULL,
    delisting_data = NULL,
    shiny_callback = NULL,
    verbose = FALSE,

    # Results
    
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
    
    # @description Get the strategy capital levels for the strategy, based on
    #   the simulation's config.
    # @return A list where the names are strategy names (or 'joint') and the
    #   values are the capital levels for each strategy,
    getStrategyCapital = function() {
      capital_list <- list(joint = 0)
      
      for (strategy_name in private$config$getStrategyNames()) {
          
        this_capital <- private$config$getStrategyConfig(strategy_name, "strategy_capital")
        capital_list[[strategy_name]] <- this_capital
        capital_list[["joint"]] <- capital_list[["joint"]] + this_capital
      }
      capital_list
    },
    
    # @description Save summary information.
    # @param period Period to which the data pertains.
    # @param data_obj Data frame to save.
    saveSimSummary = function(period, data_obj) {
      private$sim_summary_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },
    
    # @description Save detail (position-level) information.
    # @param period Period to which the data pertains.
    # @param data_obj Data frame to save.
    saveSimDetail = function(period, data_obj) {
      private$sim_detail_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },
    
    # @description Save input statistics.
    # @param period Period to which the data pertains.
    # @param data_obj Data frame to save.
    saveInputStats = function(period, data_obj) {
      private$input_stats_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },
    
    # @description Save loosening information.
    # @param period Period to which the data pertains.
    # @param data_obj Data frame to save.
    saveLooseningInfo = function(period, data_obj) {
      private$loosening_info_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)
    },
    
    # @description Save optimization summary information.
    # @param period Period to which the data pertains.
    # @param data_obj Data frame to save.
    saveOptimizationSummary = function(period, data_obj) {
      private$optimization_summary_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)      
    },
    
    # @description Save exposure information.
    # @param period Period to which the data pertains.
    # @param data_obj Data frame to save.
    saveExposures = function(period, data_obj, type = "net") {
      stopifnot(type %in% c("net", "long", "short", "gross"))
      private$exposures_list[[type]][[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)
    },
    
    # @description Save information on positions removed due to delisting.
    # @param period Period to which the data pertains.
    # @param data_obj Data frame to save.
    saveDelistings = function(period, data_obj) {
      private$delistings_list[[as.character(period)]] <-
        mutate(data_obj, sim_date = period)
      invisible(self)
    }
  )
)
