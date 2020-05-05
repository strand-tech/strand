#' @name strand-package
#' @aliases strand
#' @docType package
#' @title strand: a framework for investment strategy simulation
#' @author Jeff Enos \email{jeff@strand.tech} and David Kane \email{david@strand.tech}
#' @description
#' For an introduction to strand see the package vignette:
#' \code{vignette("strand", package = "strand")}
#' @import R6
#' @import ggplot2
#' @importFrom dplyr %>% select mutate mutate_at mutate_if filter inner_join left_join vars group_by summarise summarise_all rename contains matches bind_rows ungroup
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
