#' Main function for means of MERF with aggregated-level data
#'
#' Some detailed description and scientific references here
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param smp_data sample data set
#' @param Xpop_agg aggregated population level covariates
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default set to 0
#' @param mse Choose between "none and "nonparametric"
#' @param B number of bootstrap replications for the MSE
#' @param popnsize population size of domains
#' @param OOsample_obs Number of Out-of-sample observations taken from the closest area. Default set to 25
#' @param ADDsamp_obs Number of Out-of-sample observations taken from the closest area if first iteration fails. Default is set to 0
#' @param w_min Minimal number of covariates from which informative weights are calculated. Default set to 3
#' @param importance variable importance mode processed by the
#' random forest from the \pkg{ranger}. Must be 'none', 'impurity', 'impurity_corrected',
#' 'permutation'
#' @param ... variables such as mtry, num.tree essentially anything that can be passed to \pkg{ranger}
#'
#' @return returns object including Mean predictions and model details
#' @export
#' @details Some scientific or function specific details
#' @seealso \code{\link{SAEforest}}
#'
#'
SAEforest_meanAGG <- function(Y, X, dName, smp_data, Xpop_agg,
                             initialRandomEffects = 0, ErrorTolerance = 0.0001,
                             MaxIterations = 25, mse = "none", B=100, popnsize,
                             OOsample_obs = 25, ADDsamp_obs=0, w_min=3, importance = "impurity",
                             na.rm =TRUE, B_adj = 100,...){


  # ERROR CHECKS OF INPUTS
  #________________________________________

  if(na.rm == TRUE){
    comp_smp <- complete.cases(smp_data)
    smp_data <- smp_data[comp_smp,]
    Y <- Y[comp_smp]
    X <- X[comp_smp,]
    Xpop_agg <- Xpop_agg[complete.cases(Xpop_agg),]
  }

  input_checks_meanAGG(Y = Y, X = X, dName = dName, smp_data =smp_data, Xpop_agg =Xpop_agg,
                       initialRandomEffects = initialRandomEffects, ErrorTolerance =ErrorTolerance,
                       MaxIterations =MaxIterations, mse =mse, B = B, popnsize =popnsize,
                       OOsample_obs =OOsample_obs, ADDsamp_obs = ADDsamp_obs, w_min =w_min,
                       importance = importance, na.rm = na.rm)

  out_call <- match.call()


  # Make domain variable to character and sort data-sets
  smp_data[[dName]] <- factor(smp_data[[dName]], levels=unique(smp_data[[dName]]))
  Xpop_agg[[dName]] <- factor(Xpop_agg[[dName]], levels = unique(Xpop_agg[[dName]]))
  if(mse != "none"){
    popnsize[[dName]] <- factor(popnsize[[dName]], levels = unique(Xpop_agg[[dName]]))
  }

  # Order Data according to factors to ease MSE-estimation
  order_in <- order(smp_data[[dName]])
  smp_data <- smp_data[order_in,]
  X <- X[order_in,]
  Y <- Y[order_in]

    # Point Estimation
  #________________________________________
  meanAGG_preds <- point_meanAGG(Y = Y, X = X, dName = dName, smp_data = smp_data, Xpop_agg = Xpop_agg,
                               initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
                               MaxIterations = MaxIterations, OOsample_obs = OOsample_obs, ADDsamp_obs = ADDsamp_obs, w_min = w_min,
                               importance = importance, ...)

  data_specs <- sae_specs(dName = dName,cns = Xpop_agg, smp = smp_data)


  if(mse == "none"){
    result <- list(
      MERFmodel =  c(meanAGG_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
      Indicators = meanAGG_preds[[1]],
      MSE_Estimates = NULL,
      AdjustedSD = NULL,
      NrCovar = meanAGG_preds$wAreaInfo)

    class(result) <- c("SAEforest_meanAGG", "SAEforest")
    return(result)
  }

  # MSE Estimation
  #________________________________________

  if(mse != "none"){
    print(paste("Error SD Bootstrap started:"))
    adj_SD <- adjust_ErrorSD(Y=Y, X=X, smp_data = smp_data, mod = meanAGG_preds[[2]], B = B_adj, ...)
    print(paste("Bootstrap with", B,"rounds started"))
  }

  if(mse == "nonparametric"){

    mse_estims <- MSE_SAEforest_aggOOB_wSet(Y=Y, X = X, mod = meanAGG_preds, smp_data = smp_data,
                                            Xpop_agg = Xpop_agg, dName = dName, ADJsd = adj_SD , B=B,
                                            initialRandomEffects = initialRandomEffects,
                                           ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                                           popnsize = popnsize, ...)

    result <- list(
      MERFmodel =  c(meanAGG_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
      Indicators = meanAGG_preds[[1]],
      MSE_Estimates = mse_estims,
      AdjustedSD = adj_SD,
      NrCovar = meanAGG_preds$wAreaInfo)

    class(result) <- c("SAEforest_meanAGG", "SAEforest")
    return(result)
  }

}
