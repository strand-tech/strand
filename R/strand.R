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
#' \code{vignette("strand")} and the documentation for the \code{\link{Simulation}} class.
#' 
#' @import R6
#' @import ggplot2
#' @importFrom dplyr %>% select mutate mutate_at mutate_if filter inner_join left_join vars group_by summarise summarise_all rename contains matches bind_rows ungroup arrange
#' @importFrom tidyr replace_na starts_with ends_with one_of pivot_longer pivot_wider gather
#' @importFrom Matrix Matrix Diagonal
#' @importFrom Rglpk Rglpk_solve_LP
#' @importFrom Rsymphony Rsymphony_solve_LP
#' @importFrom yaml yaml.load_file
#' @importFrom rlang .data
#' @importFrom feather read_feather write_feather
#' @importFrom lubridate day month day<- month<-
#' @importFrom stats cor qnorm sd as.formula residuals lm
NULL
