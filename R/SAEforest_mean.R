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
SAEforest_mean <- function(Y, X, dName, survey_data, census_data,
                           initialRandomEffects = 0, ErrorTolerance = 0.0001,
                           MaxIterations = 25, mse = "none", B=100, ...){

  random = paste0(paste0("(1|",dName),")")

  unit_model <- MERFranger(Y = Y,
                          X = X,
                          random = random,
                          data = survey_data,
                          initialRandomEffects = initialRandomEffects,
                          ErrorTolerance = ErrorTolerance,
                          MaxIterations = MaxIterations,...)

  unit_preds <- predict(unit_model$Forest, census_data)$predictions+
                predict(unit_model$EffectModel,census_data, allow.new.levels=TRUE)

  unit_preds_ID <- cbind(census_data[dName],unit_preds)

  f0 <- as.formula(paste0("unit_preds ", " ~ ", dName))

  mean_preds <- aggregate(f0,data = unit_preds_ID,
                          FUN=mean)
  colnames(mean_preds) <- c(dName,"Mean")

if(mse == "none"){
  result <- list(
    Mean_predictions = mean_preds,
    MERFmodel = unit_model)

  return(result)
}

if(mse != "none"){
  adj_SD <- adjust_ErrorSD(Y=Y, X=X, surv_data = survey_data, mod = unit_model, B=100, ...)
}

if(mse == "analytic"){
  mse_estims <- MSE_MERFanalytical(mod=unit_model, survey_data = survey_data, X = X,
                                   dName = dName, err_sd = adj_SD , B=B,
                                   initialRandomEffects = initialRandomEffects,
                                   ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

  result <- list(
    MERFmodel = unit_model,
    Mean_predictions = mean_preds,
    MSE_estimates = mse_estims)
  return(result)
}

if(mse == "nonparametric"){
    mse_estims <- MSE_SAEforest_mean_REB(Y=Y, X = X, dName = dName, survey_data = survey_data,
                                         mod=unit_model, ADJsd = adj_SD, cens_data = census_data, B = B,
                                         initialRandomEffects = initialRandomEffects,
                                         ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
      MERFmodel = unit_model,
      Mean_predictions = mean_preds,
      MSE_estimates = mse_estims)
    return(result)
  }


}
