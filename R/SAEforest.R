#' A package for estimating and mapping disaggregated indicators using Mixed Effects Random Forests
#'
#' The package \pkg{SAEforest} supports estimating and mapping regional
#' disaggregated indicators. Some Text compareable to emndi here...What can this package do?
#'
#' @details
#' This package has a main function called \code{\link{MERFranger}}. It is used in three wrapper functions
#' \code{\link{SAEforest_mean}}, \code{\link{SAEforest_nonLin}} and \code{\link{SAEforest_meanAGG}}.
#' Each function produces an object inheriting requested results as well as other information accessible
#' through functions such as a summary \code{\link{summary_SAEforest}} or a class specific plot function
#' \code{\link{plot_SAEforest}}. For a full list, please see \code{\link{SAEforestObject}}.
#' An overview of all currently provided functions can be requested by
#' \code{library(help=SAEforest)}.
#'
#' @references
#' Battese, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components
#'
#' @docType package
#' @name SAEforest
NULL
