context("StrategyConfig")

config <- strand:::StrategyConfig$new(yaml::yaml.load_file("data/test_PortOpt_simple.yaml"))

test_that("getStrategyNames returns the names of all strategies", {
  expect_equal(config$getStrategyNames(), c("strategy_1", "strategy_2"))
})
