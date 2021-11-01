#' Main function for nonlinear indicators of MERF with unit-level data
#'
#' Some detailed description and scientific references here
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param smp_data sample data set
#' @param pop_data aggregated census level covariates
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default set to 0
#' @param mse Choose between "none, "wild" and "nonparametric"
#' @param B number of bootstrap replications for the MSE
#' @param threshold Set a custom threshold for indicators. Threshold can be a value or function of \code{Y}
#' Default set to NULL
#' @param importance variable importance mode processed by the
#' random forest from the \pkg{ranger}. Must be 'none', 'impurity', 'impurity_corrected',
#' 'permutation'
#' @param custom_indicator a list of functions containing the indicators to be
#' calculated additionally. Such functions must and must only depend on the
#' target variable \code{Y} and the \code{threshold}. Defaults to \code{NULL}
#' @param ... variables such as mtry, num.tree essentially anything that can be passed to \pkg{ranger}
#'
#' @return returns object including Mean predictions and model details
#' @export
#' @details Some scientific or function specific details
#' @seealso \code{\link{SAEforest}}
#'
#'
SAEforest_nonLin <- function(Y, X, dName, smp_data, pop_data,
                           initialRandomEffects = 0, ErrorTolerance = 0.0001,
                           MaxIterations = 25, mse = "none", B=100, threshold = NULL, importance ="none",
                           custom_indicator =NULL, ...){

  # ERROR CHECKS OF INPUTS
  #________________________________________

  input_checks_nonLin(Y = Y, X = X, dName = dName, smp_data = smp_data, pop_data =pop_data,
                      initialRandomEffects =initialRandomEffects, ErrorTolerance =ErrorTolerance,
                      MaxIterations =MaxIterations, mse=mse, B=B, threshold = threshold, importance = importance)

  out_call <- match.call()

  # Make domain variable to character and sort data-sets
  smp_data[[dName]] <- factor(smp_data[[dName]], levels=unique(smp_data[[dName]]))
  pop_data[[dName]] <- factor(pop_data[[dName]], levels=unique(pop_data[[dName]]))

  # Point Estimation
  #________________________________________

  nonLin_preds <- point_nonLin(Y = Y, X = X, dName = dName, threshold = threshold, smp_data = smp_data,
                               pop_data = pop_data, initialRandomEffects = initialRandomEffects,
                               ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, importance = importance,
                               custom_indicator=custom_indicator,...)

  data_specs <- sae_specs(dName = dName,cns = pop_data,smp = smp_data)

  if(mse == "none"){
    result <- list(
      Indicators = nonLin_preds[[1]],
      MERFmodel = c(nonLin_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)))

    class(result) <- c("SAEforest_nonLin", "SAEforest")
    return(result)
  }

  # MSE Estimation
  #________________________________________

  if(mse != "none"){
    adj_SD <- adjust_ErrorSD(Y=Y, X=X, smp_data = smp_data, mod = nonLin_preds[[2]], B=100, ...)
    print(paste("Bootstrap with", B,"rounds started"))
  }

  if(mse == "wild"){
    mse_estims <- MSE_SAEforest_nonLin_wild(Y=Y, X = X, mod=nonLin_preds[[2]], smp_data = smp_data,
                                            pop_data = pop_data, dName = dName, ADJsd = adj_SD , B=B,
                                            threshold = threshold, initialRandomEffects = initialRandomEffects,
                                            ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                                            custom_indicator =custom_indicator, ...)

    result <- list(
      MERFmodel = c(nonLin_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
      Indicators = nonLin_preds[[1]],
      MSE_Estimates = mse_estims,
      AdjustedSD = adj_SD)

    class(result) <- c("SAEforest_nonLin", "SAEforest")
    return(result)
  }

  if(mse == "nonparametric"){
    mse_estims <- MSE_SAEforest_nonLin_REB(Y=Y, X = X, mod=nonLin_preds[[2]], smp_data = smp_data,
                                           pop_data = pop_data, dName = dName, ADJsd = adj_SD , B=B,
                                           threshold = threshold, initialRandomEffects = initialRandomEffects,
                                           ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                                           custom_indicator =custom_indicator, ...)

    result <- list(
      MERFmodel = c(nonLin_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
      Indicators = nonLin_preds[[1]],
      MSE_estimates = mse_estims,
      AdjustedSD = adj_SD)

    class(result) <- c("SAEforest_nonLin", "SAEforest")
    return(result)
  }

}
