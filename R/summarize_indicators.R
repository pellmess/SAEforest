#' Presents point, MSE and CV estimates
#'
#' Function \code{summarize_indicators} is a generic function used to present point and
#' mean squared error (MSE) estimates and calculated coefficients of variation
#' (CV).
#' @param object an object for which point and/or MSE estimates and/or
#' calculated CV's are desired.
#' @param indicator optional character vector that selects which indicators
#' shall be returned.
#' @param MSE optional logical. If \code{TRUE}, MSE estimates for selected indicators
#' per domain are added to the data frame of point estimates. Defaults to
#' \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, coefficients of variation for selected
#' indicators per domain are added to the data frame of point estimates.
#' Defaults to \code{FALSE}.
#' @return
#' The return of \code{estimators} depends on the class of its argument. The
#' documentation of particular methods gives detailed information about the
#' return of that method.
#'
#' @details Some scientific details here.
#'
#' @export


summarize_indicators <- function(object, indicator = "all", MSE = FALSE, CV = FALSE){

  summarize_indicators_check(object = object, indicator = indicator, MSE = MSE, CV = CV)

  if (indicator == "all" || indicator == "All"){
    indicator = colnames(object$Indicators)[-1]
  }

  if (inherits(object,"SAEforest_mean") || inherits(object,"SAEforest_meanAGG")){
    indicator <- "Mean"
  }

  out_var <- data.frame(object$Indicators[colnames(object$Indicators)[1]],
                        object$Indicators[indicator])

  selected <- colnames(out_var)[-1]

  if ( MSE == TRUE || CV == TRUE ) {
    mse_estims <- object$MSE_Estimates[indicator]
    cv_estims <- sqrt(object$MSE_Estimates[indicator])/object$Indicators[indicator]
    colnames(cv_estims) <- indicator
    colnames(mse_estims) <- paste0(colnames(mse_estims), "_MSE")
    colnames(cv_estims) <- paste0(colnames(cv_estims), "_CV")
    combined <- data.frame(out_var, mse_estims, cv_estims)
    endings <- c("","_MSE", "_CV")[c(TRUE,MSE,CV)]

    combined <- combined[,c(paste0(colnames(combined)[1]),paste0(rep(selected,each = length(endings)),
                                            endings))]
  } else {
    combined <- out_var
  }
  estimators_SAEforest <- list(ind = combined, ind_name = indicator)

  class(estimators_SAEforest) <- "summarize_indicators.SAEforest"
  return(estimators_SAEforest)
}


# Prints estimators.emdi objects
#' @export

print.summarize_indicators.SAEforest <- function(x) {
  cat(paste0("Indicator/s: ", x$ind_name, "\n"))
  print(x$ind)
}

