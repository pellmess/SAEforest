#' Main function for means of MERF with unit-level data
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param survey_data survey data set
#' @param Xcensus_agg aggregated census level covariates
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default set to 0
#' @param mse Choose between "none and "nonparametric"
#' @param B number of bootstrap replications for the MSE
#' @param popnsize population size of domains
#' @param OOsample_obs Number of Out-of-sample observations taken from the closest area. Default set to 25
#' @param ADDsamp_obs Number of Out-of-sample observations taken from the closest area if first iteration failes. Default is set to 0
#' @param w_min Minimal number of covariates from which informative weights are calculated. Default set to 3
#'
#' @return returns object including Mean predictions and model details
#' @export
#'
#' @examples
SAEforest_meanAGG <- function(Y, X, dName, survey_data, Xcensus_agg,
                             initialRandomEffects = 0, ErrorTolerance = 0.0001,
                             MaxIterations = 25, mse = "none", B=100, popnsize,
                             OOsample_obs = 25, ADDsamp_obs=0, w_min=3, importance = "impurity",...){


  # ERROR CHECKS OF INPUTS
  #________________________________________
  input_checks_meanAGG(Y = Y, X = X, dName = dName, survey_data =survey_data, Xcensus_agg =Xcensus_agg,
                       initialRandomEffects = initialRandomEffects, ErrorTolerance =ErrorTolerance,
                       MaxIterations =MaxIterations, mse =mse, B = B, popnsize =popnsize,
                       OOsample_obs =OOsample_obs, ADDsamp_obs = ADDsamp_obs, w_min =w_min, importance = importance)

  out_call <- match.call()

  # Point Estimation
  #________________________________________
  meanAGG_preds <- point_meanAGG(Y = Y, X = X, dName = dName, survey_data = survey_data, Xcensus_agg = Xcensus_agg,
                               initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
                               MaxIterations = MaxIterations, OOsample_obs = OOsample_obs, ADDsamp_obs = ADDsamp_obs, w_min = w_min,
                               importance = importance, ...)

  data_specs <- sae_specs(dName = dName,cns = Xcensus_agg, smp = survey_data)


  if(mse == "none"){
    result <- list(
      MERFmodel =  c(meanAGG_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(survey_data)),
      Mean_Predictions = meanAGG_preds[[1]])

    class(result) <- c("SAEforest_meanAGG", "SAEforest")
    return(result)
  }

  # MSE Estimation
  #________________________________________

  if(mse != "none"){
    adj_SD <- adjust_ErrorSD(Y=Y, X=X, surv_data = survey_data, mod = meanAGG_preds[[2]], B=100, ...)
    print(paste("Bootstrap with", B,"rounds started"))
  }

  if(mse == "nonparametric"){
    mse_estims <- MSE_SAEforest_aggOOB_wSet(Y=Y, X = X, mod = meanAGG_preds, survey_data = survey_data,
                                            Xcensus_agg = Xcensus_agg, dName = dName, ADJsd = adj_SD , B=B,
                                            initialRandomEffects = initialRandomEffects,
                                           ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                                           popnsize = popnsize, ...)

    result <- list(
      MERFmodel =  c(meanAGG_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(survey_data)),
      Mean_Predictions = meanAGG_preds[[1]],
      MSE_estimates = mse_estims,
      wAreaInfo = meanAGG_preds$wAreaInfo,
      AdjustedSD = adj_SD)

    class(result) <- c("SAEforest_meanAGG", "SAEforest")
    return(result)
  }

}
