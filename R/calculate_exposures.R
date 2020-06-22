# @description Calculate ending portfolio exposures relative to strategy
#   capital, for factors and categories, for all strategies in a simulation
#   and for the joint strategy. This method is used to compute exposure
#   result data.
# @return A data frame of exposure information.
calculate_exposures <- function(detail_df,
                                in_var = "end_nmv",
                                weight_divisor = 1,
                                category_vars = NULL,
                                factor_vars = NULL) {
  
  stopifnot(inherits(detail_df, "data.frame"),
            all(c("strategy", category_vars, factor_vars) %in% names(detail_df)))
  
  # If weight_divisor is a list, the names of the list must match the set of
  # strategies present in the strategies column of 'detail_df'.
  if (is.list(weight_divisor)) {
    stopifnot(setequal(names(weight_divisor), unique(detail_df$strategy)))
  } else {
    stopifnot(length(weight_divisor) %in% 1)
  }
  
  # Construct a data frame of exposures.
  #
  # A data frame with two columns, strategy and weight divisor, serves as
  # the starting point. Exposures will be left-joined to the the exp_res
  # data frame and then returned. First set the strategy column:
  exp_res <-
    data.frame(strategy = unique(detail_df$strategy),
               stringsAsFactors = FALSE)
  
  # Next set the weight_divisor column. Handle cases where the
  # weight_divisor parameter is a list or vector or length 1.
  if (is.list(weight_divisor)) {
    exp_res$weight_divisor <- unlist(weight_divisor[exp_res$strategy])
  } else {
    exp_res$weight_divisor <- weight_divisor
  }
  
  # A copy of the starting point, weight_divisor_df, is used in the exposure
  # calculations.
  weight_divisor_df <- exp_res
  
  for (cat_var in category_vars) {
    this_exposures <- 
      detail_df %>%
      group_by(.dots = c("strategy", cat_var)) %>%
      summarise(exposure = sum(.data[[in_var]])) %>%
      left_join(weight_divisor_df,
                by = "strategy") %>%
      mutate(exposure = .data$exposure / .data$weight_divisor) %>%
      pivot_wider(
        names_from = cat_var,
        names_prefix = paste0(cat_var, "_"),
        values_from = "exposure") %>%
      select(-"weight_divisor")
    
    exp_res <- left_join(exp_res, this_exposures, by = "strategy")    
  }
  
  for (fact_var in factor_vars) {
    this_exposures <- 
      detail_df %>%
      group_by(.data$strategy) %>%
      summarise(!!fact_var := sum(.data[[in_var]] * .data[[fact_var]])) %>%
      left_join(weight_divisor_df,
                by = "strategy") %>%
      mutate(!!fact_var := .data[[fact_var]] / .data$weight_divisor) %>%
      select(-"weight_divisor")
    
    exp_res <- left_join(exp_res, this_exposures, by = "strategy")    
  }
  
  exp_res
}