context("Simulation")

library(arrow)
library(dplyr)

# Run the test simulation. Results will be used in several test blocks.
sim <- Simulation$new("data/test_Simulation.yaml")
sim$run()

test_that("simulation produces expected results", {
  
  test_summary <- filter(sim$getSimSummary())
  test_exposures <- filter(sim$getExposures())
  
  # truth_summary <- test_summary
  # truth_exposures <- test_exposures
  # save(truth_summary, truth_exposures, file = "data/test_Simulation.RData")
  load("data/test_Simulation.RData")
  expect_equal(as.data.frame(test_summary),
               as.data.frame(truth_summary))
  
  expect_equal(as.data.frame(test_exposures),
               as.data.frame(truth_exposures))
})


# For easier testing, load up the feather file data into objects.

test_input_data <- strand:::load_data_files("data/test_input/inputs")
test_pricing_data <- strand:::load_data_files("data/test_input/pricing")
test_secref_data <- read_feather("data/test_input/secref.feather")

test_that("simulation produces same result when data supplied as objects", {
  
  # Same test but with data coming from objects instead of files.
  

  sim_config <- yaml::yaml.load_file("data/test_Simulation.yaml")
  sim_config$simulator$input_data$type <- "object"
  sim_config$simulator$pricing_data$type <- "object"
  sim_config$simulator$secref_data$type <- "object"
  
  sim <- Simulation$new(sim_config,
                        raw_input_data = test_input_data,
                        raw_pricing_data = test_pricing_data,
                        security_reference_data = test_secref_data)
  sim$run()
  test_summary <- sim$getSimSummary()

  load("data/test_Simulation.RData")
  expect_equal(as.data.frame(test_summary),
               as.data.frame(truth_summary))
  
})



# Simple simulation tests

# Setup: two securities, 101 and 102. One strategy, alpha_1. 3 days:
# 2019-01-02 to 2019-01-04. Target equity is 500.
#
# Max position size is 100% of portfolio. 101 has more positive alpha_1 on day
# 1. On days 2 and 3, 102 has more positive alpha_1.
test_ids <- c("101", "102")
simple_input_data <- filter(test_input_data, .data$id %in% !!test_ids)
simple_pricing_data <- filter(test_pricing_data, .data$id %in% !!test_ids)
simple_secref_data <- filter(test_secref_data, .data$id %in% !!test_ids)

# Set higher alpha values for 102.  
simple_input_data$alpha_1[simple_input_data$id %in% "102" &
                          simple_input_data$date >= as.Date("2019-01-03")] <- 3

# To keep things simple set static prices at 5 for 101 and 10 for 102. So a
# full position for 101 is 100 shares and a full position for 102 is 50.
simple_pricing_data[c("price_unadj", "prior_close_unadj")][simple_pricing_data$id %in% "101",] <- 5
simple_pricing_data[c("price_unadj", "prior_close_unadj")][simple_pricing_data$id %in% "102",] <- 10

test_that("simple long-only simulation with two assets trades properly", {

  sim_config <- yaml::yaml.load_file("data/test_Simulation_simple.yaml")
  
  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data, 
                        raw_pricing_data = simple_pricing_data,
                        security_reference_data = simple_secref_data)
  # sim$setVerbose(TRUE)
  sim$run()
  id_101 <- sim$getSimDetail(strategy_name = "joint", security_id = "101")
  id_102 <- sim$getSimDetail(strategy_name = "joint", security_id = "102")

  # 101 trades into a full position for day 1, but then on day 2 we swap a full
  # position in 102 for the full position in 101 (due to the alpha change).
  expect_equal(id_101$end_shares, c(100, 0, 0))
  expect_equal(id_102$end_shares, c(0, 50, 50))
})

test_that("alphas are carried forward when missing in simple long-only sim", {
  
  # Remove data for day 2 (1/3) causing the trading out of 101 and into 102 to
  # be performed on day 3 instead of day 2.
  simple_input_data <- filter(simple_input_data, !(date %in% as.Date("2019-01-03") & id %in% "102"))
  
  sim_config <- yaml::yaml.load_file("data/test_Simulation_simple.yaml")
  
  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data, 
                        raw_pricing_data = simple_pricing_data,
                        security_reference_data = simple_secref_data)
  # sim$setVerbose(TRUE)
  sim$run()
  id_101 <- sim$getSimDetail(strategy_name = "joint", security_id = "101")
  id_102 <- sim$getSimDetail(strategy_name = "joint", security_id = "102")
  
  # 101 trades into a full position for day 1, but then on day 2 we swap a full
  # position in 102 for the full position in 101 (due to the alpha change).
  expect_equal(id_101$end_shares, c(100, 100, 0))
  expect_equal(id_102$end_shares, c(0, 0, 50))
})

test_that("NAs are not allowed in input data", {
  
  # Remove data for day 2 (1/3) causing the trading out of 101 and into 102 to
  # be performed on day 3 instead of day 2.
  simple_input_data$alpha_1[simple_input_data$date %in% as.Date("2019-01-03") & simple_input_data$id %in% "101"] <- NA
  
  sim_config <- yaml::yaml.load_file("data/test_Simulation_simple.yaml")
  
  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data, 
                        raw_pricing_data = simple_pricing_data,
                        security_reference_data = simple_secref_data)
  # sim$setVerbose(TRUE)
  expect_error(sim$run(), regexp = "\\!any\\(is.na\\(input_data\\)\\) is not TRUE")
})
  
test_that("fill_rate_pct limits order filling", {
  
  # Test one day of trading for a portfolio that should have one position long,
  # one position short.
  #
  # There should be a full long position in 101 and full short position in 102,
  # according to the sign of alpha_1 for 2019-01-02.
  #
  # Set volume for 101 such that only half of the order is filled (250 shares of
  # volume * 20% fill rate = 50 shares filled out of 100).
  #
  # Set volume for 102 such that 80% of the order is filled (200 shares of
  # volume * 20% fill rate = 40 shares filled out of 50).
  
  simple_pricing_data$volume[simple_pricing_data$date %in% as.Date("2019-01-02") &
                             simple_pricing_data$id %in% "101"] <- 250
  simple_pricing_data$volume[simple_pricing_data$date %in% as.Date("2019-01-02") &
                               simple_pricing_data$id %in% "102"] <- 200
  
  sim_config <- yaml::yaml.load_file("data/test_Simulation_simple.yaml")
  sim_config$to <- "2019-01-02"
  sim_config$strategies$strategy_1$ideal_short_weight <- 1

  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data, 
                        raw_pricing_data = simple_pricing_data,
                        security_reference_data = simple_secref_data)
  # sim$setVerbose(TRUE)
  sim$run()
  id_101 <- sim$getSimDetail(strategy_name = "joint", security_id = "101")
  id_102 <- sim$getSimDetail(strategy_name = "joint", security_id = "102")
  
  expect_equal(id_101$end_shares, c(50))
  expect_equal(id_102$end_shares, c(-40))
  
  # Fill rate, percentange of filled GMV, is 250 + 400 / 1000 = 65%
  summary_df <- sim$getSimSummary() %>% filter(strategy %in% "joint")
  expect_equal(summary_df$fill_rate_pct, 65)
  
})


# Slightly more complicated setup with 4 securities.
#
# Default setup is that 101 and 102 have positive alpha, 103 and 104 have
# negative alpha.
test_ids <- as.character(101:104)
simple_input_data_2 <- filter(test_input_data, .data$id %in% !!test_ids) %>%
  mutate(alpha_1 = case_when(id %in% "101" ~ 3,
                             id %in% "102" ~ 2.5,
                             id %in% "103" ~ -2.5,
                             id %in% "104" ~ -3))

# Prices are 1 for 101, 2, for 102, ..., etc.
simple_pricing_data_2 <- filter(test_pricing_data, .data$id %in% !!test_ids) %>%
  mutate(price_unadj = as.numeric(id) %% 100,
         prior_close_unadj = price_unadj)

simple_secref_data_2 <- filter(test_secref_data, .data$id %in% !!test_ids)

test_that("force_trim_factor triggers trimming of large positions", {
  
  # Price of stock 101 goes from 1 to 1.5 on 1/2:
  simple_pricing_data_2 <- 
    simple_pricing_data_2 %>%
    mutate(price_unadj = replace(price_unadj, id %in% 101 & date >= as.Date("2019-01-02"), 1.5),
           prior_close_unadj = replace(prior_close_unadj, id %in% 101 & date >= as.Date("2019-01-03"), 1.5))
  
  
  # Setup: long-short balanced. Max position 50%.
  sim_config <- yaml::yaml.load_file("data/test_Simulation_simple.yaml")
  sim_config$to <- "2019-01-04"
  sim_config$strategies$strategy_1$ideal_short_weight <- 1
  sim_config$strategies$strategy_1$position_limit_pct_lmv <- 50
  sim_config$strategies$strategy_1$position_limit_pct_smv <- 50

  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data_2, 
                        raw_pricing_data = simple_pricing_data_2,
                        security_reference_data = simple_secref_data_2)
  sim$run()
  
  # Investigate:
  #
  # sim$getSimDetail(strategy_name = "joint") %>%
  #   select(sim_date, id, strategy, shares, start_price, end_price, end_nmv, end_shares, max_pos_lmv, max_pos_smv)

  # Without force_trim_factor set, position in 101 remains at
  # gmv of 375 despite max position of 250.
  
  expect_equal(
    sim$getSimDetail(strategy_name = "joint", security_id = "101") %>%
      filter(sim_date %in% as.Date("2019-01-03")) %>%
      pull(end_gmv), 
    375)
  
  # Now run with force_trim_factor set to 1.2.
  sim_config$simulator$force_trim_factor <- 1.2
  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data_2, 
                        raw_pricing_data = simple_pricing_data_2,
                        security_reference_data = simple_secref_data_2)
  sim$run()

  # With the force_trim_factor setting of 1.2, a trade is generated to trim
  # the position in 101 to 120% * 250 = 300.
  expect_equal(
    sim$getSimDetail(strategy_name = "joint", security_id = "101") %>%
      filter(sim_date %in% as.Date("2019-01-03")) %>%
      pull(end_gmv), 
    300)
  
  # Finally, re-run but set average volume of 101 to 50 for 2019-01-03. With
  # trading_limit_pct_adv set to 100, that means trimming will be limited to 50
  # on 1/3. The remaining 25 is trimmed on 1/4.
  simple_input_data_2 <- simple_input_data_2 %>%
    mutate(average_volume = replace(average_volume, id %in% "101" & date %in% as.Date("2019-01-03"), 45))
    
  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data_2, 
                        raw_pricing_data = simple_pricing_data_2,
                        security_reference_data = simple_secref_data_2)
  sim$run()
  
  expect_equal(
    sim$getSimDetail(strategy_name = "joint", security_id = "101") %>%
      filter(sim_date %in% as.Date(c("2019-01-03", "2019-01-04"))) %>%
      pull(end_gmv), 
    c(330, 300))
})


test_that("force_exit_non_investable triggers exiting positions in non-investable stocks", {

  # Here we go back to a 100% max position size so that at the end of 1/2 we
  # have a portfolio that has one position long and one position short. 104 has
  # the most negative alpha score, so we will have a short position in it at the
  # end of 1/2.
  #
  # We define the universe to be stocks with an average_volume measure greater
  # than 5000.
  #
  # We then set the average_volume to 4,000 for stock 104 for 2019-01-03
  # onwards; for all other stocks we set average_volume to 10,000.
  
  simple_pricing_data_2 <- simple_pricing_data_2 %>%
    mutate(price_unadj = replace(price_unadj, id %in% 101 & date >= as.Date("2019-01-02"), 1.5),
           prior_close_unadj = replace(prior_close_unadj, id %in% 101 & date >= as.Date("2019-01-03"), 1.5))
  
  simple_input_data_2 <- simple_input_data_2 %>%
    mutate(average_volume = if_else(id %in% 104 & date >= as.Date("2019-01-03"), 4000, 10000))
  
    
  # Setup: long-short balanced. Max position 50%.
  sim_config <- yaml::yaml.load_file("data/test_Simulation_simple.yaml")
  sim_config$to <- "2019-01-07"
  sim_config$strategies$strategy_1$ideal_short_weight <- 1
  sim_config$strategies$strategy_1$position_limit_pct_lmv <- 100
  sim_config$strategies$strategy_1$position_limit_pct_smv <- 100
  sim_config$strategies$strategy_1$trading_limit_pct_adv <- 10
  sim_config$simulator$force_exit_non_investable <- TRUE
  sim_config$simulator$universe <- "average_volume >= 5000"
  
  
  sim <- Simulation$new(sim_config,
                        raw_input_data = simple_input_data_2, 
                        raw_pricing_data = simple_pricing_data_2,
                        security_reference_data = simple_secref_data_2)
  sim$run()
  
  # Investigate:
  #
  # sim$getSimDetail(strategy_name = "joint") %>%
  #   select(sim_date, id, strategy, shares, start_price, end_price, end_nmv, end_shares, max_pos_lmv, max_pos_smv)
  
  # On 2019-01-03, stock 104 falls outside of the universe due to its low
  # average_volume measure. Since force_exit_non_investable is set, trades to
  # exit 104 are added on 2019-01-03 and 2019-01-04. 10% ADV = 400 can be traded
  # each day for 104. So at the end of the day on 2019-01-03 the position in 104
  # has nmv of -100. At the end of 2019-01-04 the position in 104 is flat, while
  # the position in 103 has taken its place and is up to size of 400. By the end
  # of 2019-01-07, the position in 104 has been fully replaced by the position
  # in 103.

  # Check that 104 is exited at rate of max 400 / day.
  expect_equal(
    sim$getSimDetail(strategy_name = "joint", security_id = "104") %>%
      pull(end_gmv), 
    c(500, 100, 0, 0))

  # Check the setting of the investable column for 104.
  expect_equal(
    sim$getSimDetail(strategy_name = "joint", security_id = "104") %>%
      pull(investable), 
    c(TRUE, FALSE, FALSE, FALSE))
  
  # Check that 104 is replaced by 103 (which has a price of 3). One day lag is
  # because force exit is applied outside of the optimization.
  expect_equal(
    sim$getSimDetail(strategy_name = "joint", security_id = "103") %>%
      pull(end_gmv), 
    c(0, 0, 399, 501))

})

# Tests of summary functions

test_that("overallStatsDf returns the correct values", {
  test_overall_stats_df <- sim$overallStatsDf()
  
  # truth_overall_stats_df <- test_overall_stats_df
  # save(truth_overall_stats_df, file = "data/test_Simulation_overallStatsDf.RData")
  load("data/test_Simulation_overallStatsDf.RData")
  
  expect_equal(test_overall_stats_df, truth_overall_stats_df)
})

test_that("overallReturnsByMonthDf returns the correct values", {
  # Would be nice to extend this test to include multiple years of results.
  test_overall_returns_by_month_df <- sim$overallReturnsByMonthDf()
  
  # truth_overall_returns_by_month_df <- test_overall_returns_by_month_df
  # save(truth_overall_returns_by_month_df, file = "data/test_Simulation_overallReturnsByMonthDf.RData")
  load("data/test_Simulation_overallReturnsByMonthDf.RData")
  
  expect_equal(test_overall_returns_by_month_df, truth_overall_returns_by_month_df)
  })


