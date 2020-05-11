context("Simulation")

library(feather)
library(dplyr)

test_that("simulation produces expected results", {
  
  sim <- Simulation$new("data/test_Simulation.yaml")
  sim$run()
  
  test_summary <- filter(sim$getSimSummary(), .data$strategy %in% "joint")
  
  # truth_summary <- test_summary
  # save(truth_summary, file = "data/test_Simulation.RData")
  load("data/test_Simulation.RData")
  expect_equal(as.data.frame(test_summary),
               as.data.frame(truth_summary))
})


# For easier testing, load up the feather file data into objects.
all_dates <- seq.Date(from = as.Date("2019-01-02"), to = as.Date("2019-01-08"), by = "days")
all_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
test_input_data <-
  lapply(all_dates,
         function(x) {
           read_feather(paste0("data/test_input/inputs/inputs_", format(x, "%Y%m%d"), ".feather")) %>%
             mutate(date = x)
         }) %>% bind_rows

test_pricing_data <-
  lapply(all_dates,
         function(x) {
           read_feather(paste0("data/test_input/pricing/pricing_", format(x, "%Y%m%d"), ".feather")) %>%
             mutate(date = x)
         }) %>% bind_rows

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
  test_summary <- filter(sim$getSimSummary(), .data$strategy %in% "joint")

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
