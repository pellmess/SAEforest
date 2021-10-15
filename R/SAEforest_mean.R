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
#' @param mse Choose between "none, "analytic" and "nonparametric"
#' @param B number of bootstrap replications for the MSE
#'
#' @return returns object including Mean predictions and model details
#' @export
#'
#' @examples
SAEforest_mean <- function(Y, X, dName, survey_data, census_data,
                           initialRandomEffects = 0, ErrorTolerance = 0.0001,
                           MaxIterations = 25, mse = "none", B=100, ...){

# ERROR CHECKS OF INPUTS
#________________________________________




# Point Estimation
#________________________________________
  mean_preds <- point_mean(Y = Y, X = X, dName = dName, survey_data = survey_data, census_data = census_data,
             initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
             MaxIterations = MaxIterations,...)


  if(mse == "none"){
  result <- list(
    Mean_predictions = mean_preds[[1]],
    MERFmodel = mean_preds[[2]])

  return(result)
}

# MSE Estimation
#________________________________________

  if(mse != "none"){
  adj_SD <- adjust_ErrorSD(Y=Y, X=X, surv_data = survey_data, mod = mean_preds[[2]], B=100, ...)
  print(paste("Bootstrap with", B,"rounds started"))
  }

  if(mse == "analytic"){
    mse_estims <- MSE_MERFanalytical(mod=mean_preds[[2]], survey_data = survey_data, X = X,
                                   dName = dName, err_sd = adj_SD , B=B,
                                   initialRandomEffects = initialRandomEffects,
                                   ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
     MERFmodel = mean_preds[[2]],
     Mean_predictions = mean_preds[[1]],
     MSE_estimates = mse_estims,
     AdjustedSD = adj_SD)

    return(result)
  }

  if(mse == "nonparametric"){
    mse_estims <- MSE_SAEforest_mean_REB(Y=Y, X = X, dName = dName, survey_data = survey_data,
                                         mod=mean_preds[[2]], ADJsd = adj_SD, cens_data = census_data, B = B,
                                         initialRandomEffects = initialRandomEffects,
                                         ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
      MERFmodel = mean_preds[[2]],
      Mean_predictions = mean_preds[[1]],
      MSE_estimates = mse_estims,
      AdjustedSD = adj_SD)

    return(result)
  }
}
