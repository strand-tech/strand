#' Load example strategy configuration
#'
#' Loads an example strategy configuration file for use in examples.
#'
#' @return An object of class \code{list} that contains the example
#'   configuration. The list object is the result of loading the package's
#'   example yaml configuration file \code{application/strategy_config.yaml}.
#'
#' @examples
#' 
#' config <- example_strategy_config()
#' names(config$strategies)
#' show(config$strategies$strategy_1)
#' 
#' @export
example_strategy_config <- function() {
  yaml::yaml.load_file(system.file("application/strategy_config.yaml", package = "strand"))
}

#' Run an example shiny app
#'
#' Runs a shiny app that allows interactively configuring and running a
#' simulation. Once the simulation is finished results, such as performance
#' statistics and plots of exposures, are available in a results panel.
#' 
#' @examples
#' 
#' if (interactive()) {
#'   example_shiny_app()
#' }
#' 
#' @export
example_shiny_app <- function() {
  shiny::runApp(appDir = system.file("application", package = "strand"))
}