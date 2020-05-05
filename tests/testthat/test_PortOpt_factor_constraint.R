context("PortOpt factor constraint")

optim_input <- left_join(read_feather("data/test_input/inputs/inputs_20190103.feather"),
                         read_feather("data/test_input/pricing/pricing_20190103.feather"), by = "id")
optim_input$shares_strategy_1 <- 0
optim_input$shares_strategy_2 <- 0


test_that("simple optimization with factor constraints produces expected results", {

  config <- yaml::yaml.load_file("data/test_PortOpt_factor_constraint.yaml")
  config$strategies$strategy_1$target_long_weight <- 0.88125
  config$strategies$strategy_1$target_short_weight <- 0.61875
  config$strategies$strategy_2$target_long_weight <- 0.5
  config$strategies$strategy_2$target_short_weight <- 0.5
  
  portOpt <- PortOpt$new(config, optim_input)

  portOpt$solve()
  test_result <- portOpt$getResultData()
  
  # truth_result <- test_result
  # save(truth_result, file = "data/test_PortOpt_factor_constraint.RData")
  load("data/test_PortOpt_factor_constraint.RData")
  expect_equal(test_result, truth_result)
})

