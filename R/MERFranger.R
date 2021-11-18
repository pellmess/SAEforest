#' Main function for unit-level MERF
#'
#' This function enables the use of Mixed Effects Random Forests (MERFs) by effectively
#' combining a random forest from \pkg{ranger} with a model capturing random effects from
#' \pkg{lme4}. The MERF algorithm is an algorithmic procedure reminiscent of an EM-algorithm
#' (see Details). The function is the base-function for the three wrapping functions
#' (\code{\link{SAEforest_mean}}, \code{\link{SAEforest_meanAGG}}, \code{\link{SAEforest_nonLin}})
#' and should not be directly used by the user. Recommended exceptions are applications exceeding
#' the scope of existing wrapper functions or further research. The function \code{MERFranger}
#' allows to model complex patterns of structural relations (see Examples). The function returns
#' an object of class MERFmodel, which can be used to produce unit-level predictions. In contrast to
#' the wrapping functions, this function does not directly provide SAE estimates.
#'
#' @param Y metric input value of target variable
#' @param X matrix of predictive covariates
#' @param random Specification of random effects terms following the syntax of \link[lme4]{lmer}.
#' Random-effect terms are specified by vertical bars (|) separating expressions for design matrices
#' from grouping factors. For further details see \link[lme4]{lmer} and the example below.
#' @param data data.frame of sample data including the specified elements of \code{Y} and
#' \code{X}.
#' @param initialRandomEffects numeric value or vector of initial estimate of random effects.
#' Defaults to 0.
#' @param ErrorTolerance numeric value to monitor the MERF algorithm's convergence. Defaults to 1e-04
#' @param MaxIterations numeric value specifying the maximal amount of iterations for the
#' MERF algorithm. Defaults to 25.
#' @param importance variable importance mode processed by the
#' random forest from the \pkg{ranger}. Must be 'none', 'impurity', 'impurity_corrected',
#' 'permutation'. For further details see \link[ranger]{ranger}.
#' @param na.rm logical. Whether missing values should be removed. Defaults to TRUE.
#' @param ... additional parameters are directly passed to the random forest \link[ranger]{ranger}.
#' Most important parameters are for instance mtry (number of variables to possibly split at
#' in each node), or num.tree (number of trees). For further details on possible parameters
#' see \link[ranger]{ranger} and the example below.
#'
#' @return object of class MERFmodel includes the following elements:
#'
#' \item{\code{Forest}}{a random forest of type 'ranger' modelling fixed effects
#' of the model.}
#' \item{\code{EffectModel}}{a model of random effects of type 'lmer' capturing
#' tructural component of MERFs and modeling random components.}
#' \item{\code{RandomEffects}}{list element containing the values of random intercepts from \code{EffectModel}.}
#' \item{\code{RanEffSD}}{numeric value of the standard deviation of random intercepts.}
#' \item{\code{ErrorSD}}{numeric value of standard devition of unit-level errors.}
#' \item{\code{VarianceCovariance}}{VarCor matrix from \code{EffectModel}.}
#' \item{\code{LogLik}}{vector with numerical entries showing the loglikelihood of the MERF algorithm.}
#' \item{\code{IterationsUsed}}{numeric number of interatirons used until covergence of the MERF algorithm.}
#' \item{\code{OOBresiduals}}{vector of OOB-residuals.}
#' \item{\code{Random}}{character specifying the random intercept in the random effects model.}
#' \item{\code{ErrorTolerance}}{numerical value to monitor the MERF algorithm's convergence.}
#' \item{\code{initialRandomEffects}}{numeric value or vector of intial specification of random effects.}
#' \item{\code{MaxIterations}}{numeric value specifying the maximal amount of iterations for the
#' MERF algorithm.}
#' \item{\code{call}}{the summarized function call producing the object.}
#' \item{\code{data_specs}}{data characteristics such as domain specific sample sizes or number of
#' out-of-sample areas.}
#' \item{\code{data}}{the used survey sample data.}
#'
#' @details The MERF algorithm iteratively optimizes two separate steps: a) the random forest
#' function, assuming the random effects term to be correct and b) estimates the random
#' effects part, assuming the OOB-predictions from the forest to be correct. Overall convergence
#' of the algorithm is monitored by log-likelihood of a joint model of both components. For
#' further details see Krennmair and Schmid or Hajem et. al. (2014).
#'
#' Note that the \code{MERFmodel} object is a composition of elements from a random forest of class
#' 'ranger' and a random effects model of class 'lmerMod'.  Thus, all generic functions applicable
#' to objects of classes 'ranger' and 'lmerMod' can be used on these elements. For furhter details
#' on generic functions see \code{\link[ranger]{ranger}} and \code{\link[lme4]{lmer}} as well as
#' the examples below.
#'
#' @references
#' Krennmair, P. and Schmid, T. (202X). WP 1
#'
#' Hajjem et al. 2014. MERF paper
#'
#' @seealso \code{\link{SAEforest}},\code{\link[ranger]{ranger}},\code{\link[lme4]{lmer}},
#' \code{\link{SAEforest_mean}}, \code{\link{SAEforest_meanAGG}}, \code{\link{SAEforest_nonLin}}
#'
#' @export

#'
MERFranger <- function(Y, X, random, data, importance = "none", initialRandomEffects = 0, ErrorTolerance = 0.0001,
                        MaxIterations = 25, na.rm = T, ...) {

  if(na.rm == TRUE){
    comp_smp <- complete.cases(data)
    data <- data[comp_smp,]
    Y <- Y[comp_smp]
    X <- X[comp_smp,]
  }

  input_checks_MERF(Y = Y, X = X, data = data, initialRandomEffects = initialRandomEffects,
                    ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                    importance = importance, na.rm = na.rm)

  Target <- Y
  ContinueCondition <- TRUE
  iterations <- 0

  AdjustedTarget <- Target - initialRandomEffects
  oldLogLik <- 0
  while (ContinueCondition) {
    iterations <- iterations + 1
    rf <- ranger::ranger(x = X, y = AdjustedTarget, importance = importance, ...)
    forest_preds  <- rf$predictions
    f0 <- as.formula(paste0("Target ~ -1+", random))
    lmefit <- lme4::lmer(f0, data = data, REML = FALSE, offset = forest_preds)

    newLogLik <- as.numeric(stats::logLik(lmefit))

    ContinueCondition <- (abs(abs(newLogLik - oldLogLik[iterations]) / oldLogLik[iterations]) > ErrorTolerance &
      iterations < MaxIterations)
    oldLogLik <- c(oldLogLik, newLogLik)
    AllEffects <- predict(lmefit)
    AdjustedTarget <- Target - (AllEffects-forest_preds)
    # print(iterations)
  }

  data$forest_preds <- NULL
  residuals <- Target - predict(lmefit)

  result <- list(Forest = rf,
                 EffectModel = lmefit,
                 RandomEffects = lme4::ranef(lmefit),
                 RanEffSD = as.data.frame(lme4::VarCorr(lmefit))$sdcor[1],
                 ErrorSD = stats::sigma(lmefit),
                 VarianceCovariance =lme4::VarCorr(lmefit),
                 LogLik = oldLogLik,
                 IterationsUsed = iterations,
                 OOBresiduals = residuals,
                 Random = random,
                 ErrorTolerance = ErrorTolerance,
                 initialRandomEffects = initialRandomEffects,
                 MaxIterations = MaxIterations)

  class(result) <- "MERFmodel"

  return(result)
}
