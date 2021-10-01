#' Main function for means of MERF with unit-level data
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param survey_data survey data set
#' @param census_data aggregated census level covariates
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default set to 0
#' @param m_try default set to 1
#' @param survey_weigths default set to NULL
#'
#' @return returns object including Mean predictions and model details
#' @export
#'
#' @examples
SAEforest_nonLinV1 <- function(Y, X, dName, survey_data, census_data,
                           initialRandomEffects = 0, ErrorTolerance = 0.0001,
                           MaxIterations = 25, mse = "none", B=100, threshold = NULL, ...){

  # ERROR CHECKS OF INPUTS
  #________________________________________




  # Point Estimation
  #________________________________________
  nonLin_preds <- point_nonLin(Y = Y, X = X, dName = dName, threshold = threshold, survey_data = survey_data, census_data = census_data,
                           initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations,...)


  if(mse == "none"){
    result <- list(
      Indicators = nonLin_preds[[1]],
      MERFmodel = nonLin_preds[[2]])

    return(result)
  }

  # MSE Estimation
  #________________________________________

  if(mse != "none"){
    adj_SD <- adjust_ErrorSD(Y=Y, X=X, surv_data = survey_data, mod = nonLin_preds[[2]], B=100, ...)
  }

  if(mse == "wild"){
    mse_estims <- MSE_SAEforest_nonLin_wild(Y=Y, X = X, mod=nonLin_preds[[2]], survey_data = survey_data,
                                            cens_data = census_data, dName = dName, ADJsd = adj_SD , B=B,
                                            threshold = threshold, initialRandomEffects = initialRandomEffects,
                                            ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
      MERFmodel = nonLin_preds[[2]],
      Indicators = nonLin_preds[[1]],
      MSE_estimates = mse_estims)

    return(result)
  }

  if(mse == "nonparametric"){
    mse_estims <- MSE_SAEforest_nonLin_REB(Y=Y, X = X, mod=nonLin_preds[[2]], survey_data = survey_data,
                                           cens_data = census_data, dName = dName, ADJsd = adj_SD , B=B,
                                           threshold = threshold, initialRandomEffects = initialRandomEffects,
                                           ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
      MERFmodel = nonLin_preds[[2]],
      Mean_predictions = nonLin_preds[[1]],
      MSE_estimates = mse_estims)

    return(result)
  }

}
