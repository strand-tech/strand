# CrossSection class
#
# @description Class for providing access to cross-sectional data.
#
# @details The CrossSection class provides an interface to cross-sectional data. It
# handles data retrieval, validation, value carry-forward and metadata
# tracking.
CrossSection <-  R6Class(
  "CrossSection",
  
  private = list(
    raw_data = NULL,
    
    carry_forward = NULL,
    carry_forward_value = NULL,
    
    na_replace_value = NULL,
    
    current_period = NULL,
    current_data = NULL,
    
    previous_period = NULL,
    previous_data = NULL,
    
    column_map = NULL
    
  ),
  
  public = list(

    # @description Create a new \code{CrossSection} object.
    # @param carry_forward Boolean that indicates whether data in the object
    #   should be carried forward from one period to the next. To carry forward
    #   data is to use data from the past for a security if no data is found
    #   for the present. Suppose data for security X has been retrieved in a
    #   prior call to \code{update()}. Setting \code{carry_forward = TRUE} will
    #   cause the most recent data for X to be used if no records are found for
    #   X in subsequent calls to \code{update()}. If carry-forward values are
    #   supplied using the \code{setCarryForwardValue} method these values are
    #   used instead of the most recent data points.
    # @return A new \code{CrossSection} object.
    initialize = function(carry_forward) {
      private$carry_forward <- carry_forward
      private$carry_forward_value <- list()
      invisible(self)
    },
    
    # @description Set the carry-forward value(s) for the cross section.
    # @param value_list Object of class \code{list}. Each name-value pair of
    #   the list specifies the name of a column and the value to be used for
    #   that column in records that are carried forward. For example, if
    #   \code{value_list = list(foo = 0)} the value 0 will be used for column
    #   foo in records that are carried forward.
    setCarryForwardValue = function(value_list) {
      stopifnot(is.list(value_list))
      private$carry_forward_value <- c(private$carry_forward_value, value_list)
      invisible(self)
    },
    
    # @description Set the value(s) to be used in place of NA observations.
    # @param value_list Object of class \code{list}. Each name-value pair of
    #   the list specifies the name of a column and the value to be used in
    #   place of NAs that appear in that column. For example, if
    #   \code{value_list = list(foo = 0)} the value 0 will be used in place of
    #   NA in column foo.
    setNAReplaceValue = function(value_list) {
      stopifnot(is.list(value_list))
      private$na_replace_value <- c(private$na_replace_value, value_list)
      invisible(self)
    },
    
    # @description Rename columns in input data.
    # @param column_map Object of class \code{list}. Each name-value pair of
    #   the list represents the new and old column names for the object's data.
    #   For example, if \code{column_map = list(foo = "bar")} values in the raw
    #   data in column bar will appear in the column foo in the data returned
    #   by \code{update()}.
    setColumnMap = function(column_map) {
      stopifnot(is.list(column_map))
      private$column_map <- column_map
      invisible(self)
    },
    
    # @description Set raw data.
    # @param raw_data Data frame that contains raw data. Must have a column
    #   \code{date} of class \code{Date}.
    setRaw = function(raw_data) {
      # TODO Allow data.table input and key off date for faster subsetting.
      if (!is.data.frame(raw_data)) {
        stop("raw_data must be a data.frame")
      }
      
      if (!"date" %in% names(raw_data) &&
          inherits(raw_data[["date"]], "Date")) {
        stop("raw_data must have date column with class Date")        
      }

      private$raw_data <- raw_data
      invisible(self)
    },
    
    # @description Get raw data for a given date.
    # @param date Date for which we want data.
    # @return A data frame of data for \code{date}.
    getRaw = function(date) {
      if (is.null(private$raw_data)) {
        stop("No raw data present. Provide with setRaw().")
      }
      
      filter(private$raw_data, .data$date %in% !!date)
    },

    # @description Get current cross-section.
    # @return A data frame that contains current cross-section data.
    getCurrent = function() {
      invisible(private$current_data)
    },

    # @description Update and return cross-section data for a given date.
    # @param date Date for which we want to update and return data.
    # @return A data frame of data for \code{date} that includes, if
    #   \code{carry_forward} was set to \code{TRUE} when creating the object,
    #   any data that has been carried-forward from previous periods. The
    #   records carried forward have default values as configured by calls to
    #   \code{setCarryForwardValue()}. In addition, NA values are replaced
    #   according the values passed to \code{setNAReplaceValue()}.
    update = function(date) {
      
      new_data <- self$getRaw(date)
      
      # Duplicate entries are not allowed.
      if (any(duplicated(new_data$id))) {
        stop("Duplicate cross-section entries are not allowed")
      }
      
      # Data is only carried forward for securities not found in the current
      # data. So the carry_forward flag is set to FALSE for new records.
      new_data <- new_data %>% mutate(carry_forward = FALSE)
      
      # Rename colums according to the column_map member
      if (!is.null(private$column_map) && length(private$column_map) > 0) {
        new_data <- new_data %>% rename(!!!private$column_map)
      }
      
      # Tag data with date. Note that in the case values are carried over from
      # one date to the next the cs_data_date column provides the as-of date for
      # the row.
      stopifnot(!"cs_data_date" %in% names(new_data))
      new_data[["cs_data_date"]] <- rep(date, nrow(new_data))

      # Perform value carry-forward, if applicable.
      if (isTRUE(private$carry_forward && !is.null(private$current_data))) {

        # The 'to_carry_forward' data frame contains records for securities that
        # have been seen in previous cross-sections but not appear in the 'new_data'
        # cross-section. The carry_forward flag is set to TRUE for these records.
        to_carry_forward <-
          filter(private$current_data, !.data$id %in% new_data$id) %>%
          mutate(carry_forward = TRUE)
        
        if (nrow(to_carry_forward) > 0) {
          # If there is a default value for a data column in
          # private$carry_forward_defaults, use this value in the records that are
          # carried forward. Otherwise, leave the record's values unchanged. For
          # example, prices are usually carried over from one period to the next
          # in the absense of new data. However, it makes sense to use a default
          # value of 0 for trading volume if no data for the current date is
          # provided.
          for (col in names(private$carry_forward_value)) {
            stopifnot(col %in% names(to_carry_forward))
            to_carry_forward[[col]] <- private$carry_forward_value[[col]]
          }
        }
        new_data <- rbind(new_data,
                          select(to_carry_forward, -"cs_date"))
      }
      
      # Replace NA values for any items in the na_value field.
      if (length(private$na_replace_value) > 0) {
        for (col in names(private$na_replace_value)) {
          stopifnot(col %in% names(new_data))
          new_data <- new_data %>% mutate_at(vars(!!col), ~replace(., is.na(.), private$na_replace_value[[col]]))
        }
      }
      
      new_data[["cs_date"]] <- date

      
      # Update previous and current object members.
      private$previous_period <- private$current_period
      private$previous_data <- private$current_data
      
      private$current_period <- date
      private$current_data <- new_data
      
      invisible(private$current_data)
    },
    
    # @description Get overall statistics for the period's data and statistics
    #   on specific columns (e.g., number of missing values, day-over-day
    #   correlations, etc.).
    # @param columns Columns for which to return statistics.
    periodStats = function(columns = NULL) {
      
      # Calculate basic summary stats and record as metadata
      #
      # TODO record metadata for first day
      this_meta <- data.frame(period = private$current_period,
                              input_rows = nrow(private$current_data),
                              cf_rows = sum(private$current_data$carry_forward))
      if (!is.null(columns) && length(columns) > 0 && !is.null(private$current_data)) {
        
        this_meta <- cbind(
          this_meta,
          select(private$current_data, !!columns) %>%
            summarise_all(~ sum(is.na(.))) %>%
            setNames(paste0("num_na_", names(.))))
        
        if (!is.null(private$previous_data)) {
          
          two_day <- inner_join(
            select(private$current_data, "id", !!columns),
            select(private$previous_data, "id", !!columns),
            by = "id")
          
          for (meta_col in columns) {
            this_meta[[paste0("cor_", meta_col)]] <-
              cor(two_day[[paste0(meta_col, ".x")]], two_day[[paste0(meta_col, ".y")]], use = "p")
          }
        } else {
          for (meta_col in columns) {
            this_meta[[paste0("cor_", meta_col)]] <- as.numeric(NA)
          }
        }
      }
      
      invisible(this_meta)
    },
    
    # @description Print the cross-section object.
    print = function() {
      cat("CrossSection:\n",
          "Number of raw records: ", nrow(private$raw_data), "\n", sep = "")
    }

  ))
