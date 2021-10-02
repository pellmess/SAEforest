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
SAEforest_meanAGG <- function(Y, X, dName, survey_data, Xcensus_agg,
                             initialRandomEffects = 0, ErrorTolerance = 0.0001,
                             MaxIterations = 25, mse = "none", B=100, popnsize,
                             OOsample_obs = 25, ADDsamp_obs=0, w_min=3, ...){


  # ERROR CHECKS OF INPUTS
  #________________________________________




  # Point Estimation
  #________________________________________
  meanAGG_preds <- point_meanAGG(Y = Y, X = X, dName = dName, survey_data = survey_data, Xcensus_agg = Xcensus_agg,
                               initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
                               MaxIterations = MaxIterations, OOsample_obs = OOsample_obs, ADDsamp_obs = ADDsamp_obs, w_min = w_min, ...)

  if(mse == "none"){
    result <- meanAGG_preds

    return(result)
  }

  # MSE Estimation
  #________________________________________

  if(mse != "none"){
    adj_SD <- adjust_ErrorSD(Y=Y, X=X, surv_data = survey_data, mod = meanAGG_preds[[2]], B=100, ...)
  }

  if(mse == "nonparametric"){
    mse_estims <- MSE_SAEforest_aggOOB_wSet(Y=Y, X = X, mod = meanAGG_preds, survey_data = survey_data,
                                            Xcensus_agg = Xcensus_agg, dName = dName, ADJsd = adj_SD , B=B,
                                            initialRandomEffects = initialRandomEffects,
                                           ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                                           popnsize = popnsize, ...)

    result <- list(
      MERFmodel = meanAGG_preds[[2]],
      Mean_Predictions = meanAGG_preds[[1]],
      MSE_estimates = mse_estims,
      wAreaInfo = meanAGG_preds$wAreaInfo)

    return(result)
  }

}
