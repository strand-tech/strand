context("PortOpt turnover constraint")

# Test that the turnover limit configuration parameter effectively limits the
# amount traded in an optimization.

optim_input <- left_join(read_feather("data/test_input/inputs/inputs_20190103.feather"),
                         read_feather("data/test_input/pricing/pricing_20190103.feather"), by = "id")
optim_input$shares_strategy_1 <- 0
optim_input$shares_strategy_2 <- 0

test_that("setting turnover limit controls trading amount", {

  config <- yaml::yaml.load_file("data/test_PortOpt_factor_constraint.yaml")
  config$strategies$strategy_1$target_long_weight <- 1
  config$strategies$strategy_1$target_short_weight <- 1
  config$strategies$strategy_2$target_long_weight <- 1
  config$strategies$strategy_2$target_short_weight <- 1

  portOpt <- PortOpt$new(config, optim_input)
  portOpt$solve()
  test_result <- portOpt$getResultData()
  
  # Create a new optim_input data frame incorporating the share values from this
  # optimization.
  
  optim_input_2 <- optim_input %>%
    select(-starts_with("shares_")) %>%
    left_join(
      dplyr::transmute(test_result, id,
                shares_strategy_1 = order_shares_strategy_1,
                shares_strategy_2 = order_shares_strategy_2),
      by = "id")

  portOpt_2 <- PortOpt$new(config, optim_input_2)
  portOpt_2$solve()
  test_result_2 <- portOpt_2$getResultData()
  
  # Check that turnover is greater than 25,000.
  expect_true(sum(abs(test_result_2$order_nmv_joint)) > 25000)
  
  # Now limit turnover to 25,000 and re-solve the problem.
  config$turnover_limit <- 25000
  portOpt_2_lim <- PortOpt$new(config, optim_input_2)
  portOpt_2_lim$solve()
  test_result_2_lim <- portOpt_2_lim$getResultData()
  
  # Check that turnover is less than 25,000.
  expect_true(sum(abs(test_result_2_lim$order_nmv_joint)) < 25000)
})

