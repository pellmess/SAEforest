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
#' @param mse Choose between "none, "analytic" and "nonparametric"
#' @param B number of bootstrap replications for the MSE
#'
#' @return returns object including Mean predictions and model details
#' @export
#'
#' @examples
SAEforest_mean <- function(Y, X, dName, smp_data, pop_data,
                           initialRandomEffects = 0, ErrorTolerance = 0.0001,
                           MaxIterations = 25, mse = "none", B=100, importance = "none", ...){

# ERROR CHECKS OF INPUTS
#________________________________________

  input_checks_mean(Y = Y, X = X, dName = dName, smp_data = smp_data, pop_data =pop_data,
                    initialRandomEffects =initialRandomEffects, ErrorTolerance =ErrorTolerance, MaxIterations =MaxIterations,
                    mse=mse, B=B, importance = importance)

  out_call <- match.call()

# Make domain variable to character and sort data-sets
  smp_data[[dName]] <- factor(smp_data[[dName]], levels=unique(smp_data[[dName]]))
  pop_data[[dName]] <- factor(pop_data[[dName]], levels=unique(pop_data[[dName]]))



# Point Estimation
#________________________________________
  mean_preds <- point_mean(Y = Y, X = X, dName = dName, smp_data = smp_data, pop_data = pop_data,
             initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
             MaxIterations = MaxIterations,importance = importance,...)

  data_specs <- sae_specs(dName = dName,cns = pop_data,smp = smp_data)


  if(mse == "none"){
  result <- list(
    Mean_Predictions = mean_preds[[1]],
    MERFmodel = c(mean_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)))

  class(result) <- c("SAEforest_mean", "SAEforest")
  return(result)
}

# MSE Estimation
#________________________________________

  if(mse != "none"){
  adj_SD <- adjust_ErrorSD(Y=Y, X=X, smp_data = smp_data, mod = mean_preds[[2]], B=100, ...)
  print(paste("Bootstrap with", B,"rounds started"))
  }

  if(mse == "analytic"){
    mse_estims <- MSE_MERFanalytical(mod=mean_preds[[2]], smp_data = smp_data, X = X,
                                   dName = dName, err_sd = adj_SD , B=B,
                                   initialRandomEffects = initialRandomEffects,
                                   ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
     MERFmodel = c(mean_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
     Mean_predictions = mean_preds[[1]],
     MSE_estimates = mse_estims,
     AdjustedSD = adj_SD)

    class(result) <- c("SAEforest_mean", "SAEforest")
    return(result)
  }

  if(mse == "nonparametric"){
    mse_estims <- MSE_SAEforest_mean_REB(Y=Y, X = X, dName = dName, smp_data = smp_data,
                                         mod=mean_preds[[2]], ADJsd = adj_SD, pop_data = pop_data, B = B,
                                         initialRandomEffects = initialRandomEffects,
                                         ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
      MERFmodel = c(mean_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
      Mean_predictions = mean_preds[[1]],
      MSE_estimates = mse_estims,
      AdjustedSD = adj_SD)

    class(result) <- c("SAEforest_mean", "SAEforest")
    return(result)
  }
}
