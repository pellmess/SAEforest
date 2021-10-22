#' Main function for means of MERF with unit-level data
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
#' @param threshold Set a custom threshold for indicators. Default set to NULL
#'
#' @return returns object including Mean predictions and model details
#' @export
#'
#' @examples
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
      MSE_estimates = mse_estims,
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
