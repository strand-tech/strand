#' @name strand-package
#' @aliases strand
#' @docType package
#' @title strand: a framework for investment strategy simulation
#' @author Jeff Enos \email{jeff@strand.tech} and David Kane \email{david@strand.tech}
#' @description
#' 
#' The strand package provides a framework for performing discrete (share-level)
#' simulations of investment strategies. Simulated portfolios optimize exposure
#' to an input signal subject to constraints such as position size and factor
#' exposure.
#'
#' For an introduction to running simulations using the package, see
#' \code{vignette("strand")}. For details on available methods see the
#' documentation for the \code{\link{Simulation}} class.
#' 
#' @import R6
#' @import ggplot2
#' @importFrom dplyr %>% select mutate mutate_at mutate_if filter inner_join left_join vars group_by summarise summarise_all rename contains matches bind_rows ungroup arrange
#' @import flextable
#' @importFrom tibble enframe
#' @importFrom officer fp_border
#' @importFrom tidyr replace_na starts_with ends_with one_of pivot_longer pivot_wider gather unnest_wider
#' @importFrom Matrix Matrix Diagonal
#' @importFrom Rglpk Rglpk_solve_LP
#' @importFrom yaml yaml.load_file
#' @importFrom rlang .data
#' @importFrom feather read_feather write_feather
#' @importFrom lubridate day month day<- month<-
#' @importFrom stats cor qnorm sd as.formula residuals lm
#' 
#' @examples
#' # Load up sample data
#' data(sample_secref)
#' data(sample_pricing)
#' data(sample_inputs)
#' 
#' # Load sample configuration
#' config <- example_strategy_config()
#'
#' # Create the Simulation object and run
#' sim <- Simulation$new(config,
#'                       raw_input_data = sample_inputs,
#'                       raw_pricing_data = sample_pricing,
#'                       security_reference_data = sample_secref)
#' sim$run()
#' 
#' # Print overall statistics
#' sim$overallStatsDf()
#' 
#' # Access tabular result data
#' head(sim$getSimSummary())
#' head(sim$getSimDetail())
#' head(sim$getPositionSummary())
#' head(sim$getInputStats())
#' head(sim$getOptimizationSummary())
#' head(sim$getExposures())
#' 
#' # Plot results
#' \dontrun{
#' sim$plotPerformance()
#' sim$plotMarketValue()
#' sim$plotCategoryExposure("category_1") 
#' sim$plotFactorExposure(c("factor_1", "factor_2", "factor_3"))
#' sim$plotNumPositions()
#' }
NULL
