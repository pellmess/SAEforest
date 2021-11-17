#' Main function for means using MERFs with unit-level data
#'
#' This function facilitates the use of Mixed Effects Random Forests (MERFs) for applications
#' of Small Area Estimation (SAE). Unit-level survey-sample and unit-level covariate data on
#' predictive covariates is required to produce reliable estimates of domain-specific means.
#' The MERF algorithm is an algorithmic procedure reminiscent of an EM-algorithm (see Details).
#' Overall, the function serves as a coherent framework for the estimation of point-estimates
#' and if requested for assessing the uncertainty of the estimates. Methodological details are
#' provided by Krennmair & Schmid (202X) and following examples showcase potential applications.
#'
#' @param Y metric input value of target variable.
#' @param X matrix or data.frame of predictive covariates.
#' @param dName character specifying the name of domain identifier, for which random intercepts
#' are modeled.
#' @param smp_data data.frame of survey sample data including the specified elements of \code{Y} and
#' \code{X}.
#' @param pop_data data.frame of unit-level population or census level covariate data for
#' covariates \code{X}. Please note that the column names of predictive covariates must match
#' column names of \code{smp_data}. This holds especially for the name of the domain identifier.
#' @param mse character input specifying the type of uncertainty estimates. Available options are:
#' (i) "analytic" following Mendez (2008) (ii) "nonparametric" following the mse boostrap procedure
#' proposed by Krennmair & Schmid (202X) or (iii) "none" if only point estimates are requested. Defaults to "none".
#' @param importance variable importance mode processed by the
#' random forest from the \pkg{ranger}. Must be 'none', 'impurity', 'impurity_corrected',
#' 'permutation'. Defaults to "none". If you wish to produce informative plot with the generic function
#' \code{\link{plot}}, set \code{importance} not to 'none'. For further details see \link[ranger]{ranger}.
#' @param initialRandomEffects numeric value or vector of initial estimate of random effects.
#' Defaults to 0.
#' @param ErrorTolerance numeric value to monitor the MERF algorithm's convergence. Defaults to 1e-04.
#' @param MaxIterations numeric value specifying the maximal amount of iterations for the
#' MERF algorithm. Defaults to 25.
#' @param B numeric number of bootstrap replications for mse estimation procedure proposed by
#' Krennmair et al. (202X). Defaults to 100.
#' @param B_adj numeric number of bootstrap replications for the adjustment of residual variance. Defaults to 100.
#' @param na.rm logical. Whether missing values should be removed. Defaults to TRUE.
#' @param ... additional parameters are directly passed to the random forest \link[ranger]{ranger}.
#' Most important parameters are for instance mtry (number of variables to possibly split at
#' in each node), or num.tree (number of trees). For further details on possible parameters
#' see \link[ranger]{ranger} and the example below.
#'
#' @return An object of class "SAEforest" always includes point estimates for disaggregated indicators
#' as well as information on the MERF-model. Optionally corresponding MSE estimates are returned.
#' Several generic functions have methods for the returned object of class "SAEforest". Additionally,
#' the included \code{MERFmodel} object allows the use of generic functions for classes "ranger" and
#' "merMod". For a full list and explanation of components and possibilities for objects of class "SAEforest",
#' see \code{\link{SAEforestObject}}.
#'
#' @details
#' Mixed effects random forests combine advantages of regression forests (such as implicit model-selection
#' and robustness properties) with the ability to model hierarchical dependencies.
#'
#' The MERF algorithm iteratively optimizes two separate steps: a) the random forest function, assuming the random
#' effects term to be correct and b) estimates the random effects part, assuming the OOB-predictions from the
#' forest to be correct. Overall convergence of the algorithm is monitored by log-likelihood of a joint model
#' of both components. For further details see Krennmair and Schmid or Hajem et. al. (2014).
#'
#' For the estimation of the MSE, the bootstrap population is built based on a bias-corrected residual variance as
#' proposed Krennmair and Schmid (202X). The bootstrap bias correction follows Mendez and Lohr (2011).
#'
#' Note that the \code{MERFmodel} object is a composition of elements from a random forest of class 'ranger'
#' and a random effects model of class 'lmerMod'.  Thus, all generic functions applicable to objects of classes
#' 'ranger' and 'lmerMod' can be used on these elements. For furhter details on generic functions see
#' \code{\link[ranger]{ranger}} and \code{\link[lme4]{lmer}} as well as the examples below.
#'
#' @references
#' Krennmair, P. and Schmid, T. (202X). WP 1
#'
#' Mendez, G. and Lohr, S. (2011) Paper
#'
#' @seealso \code{\link{SAEforestObject}}, \code{\link[ranger]{ranger}}, \code{\link[lme4]{lmer}}
#' @export
#'
#'
SAEforest_mean <- function(Y, X, dName, smp_data, pop_data, mse = "none",importance = "none",
                           initialRandomEffects = 0, ErrorTolerance = 0.0001, MaxIterations = 25,  B=100,
                           B_adj = 100, na.rm =TRUE,...){

# ERROR CHECKS OF INPUTS
#________________________________________

  if(na.rm == TRUE){
    comp_smp <- complete.cases(smp_data)
    smp_data <- smp_data[comp_smp,]
    Y <- Y[comp_smp]
    X <- X[comp_smp,]
    pop_data <- pop_data[complete.cases(pop_data),]
  }

  input_checks_mean(Y = Y, X = X, dName = dName, smp_data = smp_data, pop_data =pop_data,
                    initialRandomEffects =initialRandomEffects, ErrorTolerance =ErrorTolerance, MaxIterations =MaxIterations,
                    mse=mse, B=B, importance = importance, na.rm = na.rm)

  out_call <- match.call()

# Make domain variable to character and sort data-sets
  smp_data[[dName]] <- factor(smp_data[[dName]], levels=unique(smp_data[[dName]]))
  pop_data[[dName]] <- factor(pop_data[[dName]], levels=unique(pop_data[[dName]]))

# Order Data according to factors to ease MSE-estimation
  order_in <- order(smp_data[[dName]])
  smp_data <- smp_data[order_in,]
  X <- X[order_in,]
  Y <- Y[order_in]
  pop_data <- pop_data[order(pop_data[[dName]]),]


# Point Estimation
#________________________________________
  mean_preds <- point_mean(Y = Y, X = X, dName = dName, smp_data = smp_data, pop_data = pop_data,
             initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
             MaxIterations = MaxIterations,importance = importance,...)

  data_specs <- sae_specs(dName = dName,cns = pop_data,smp = smp_data)


  if(mse == "none"){
  result <- list(
    Indicators = mean_preds[[1]],
    MERFmodel = c(mean_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
    MSE_Estimates = NULL,
    AdjustedSD = NULL)

  class(result) <- c("SAEforest_mean", "SAEforest")
  return(result)
}

# MSE Estimation
#________________________________________

  if(mse != "none"){
    print(paste("Error SD Bootstrap started:"))
    adj_SD <- adjust_ErrorSD(Y=Y, X=X, smp_data = smp_data, mod = mean_preds[[2]], B = B_adj, ...)
    print(paste("Bootstrap with", B,"rounds started"))
  }

  if(mse == "analytic"){
    mse_estims <- MSE_MERFanalytical(mod=mean_preds[[2]], smp_data = smp_data, X = X,
                                   dName = dName, err_sd = adj_SD , B=B,
                                   initialRandomEffects = initialRandomEffects,
                                   ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)

    result <- list(
     MERFmodel = c(mean_preds[[2]], call = out_call, data_specs = list(data_specs), data=list(smp_data)),
     Indicators = mean_preds[[1]],
     MSE_Estimates = mse_estims,
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
      Indicators = mean_preds[[1]],
      MSE_Estimates = mse_estims,
      AdjustedSD = adj_SD)

    class(result) <- c("SAEforest_mean", "SAEforest")
    return(result)
  }
}
