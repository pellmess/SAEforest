#' Main function for unit-level MERF
#'
#' This function uses forests from the ranger package combined with lmer from \pkg{lme4}.
#' The function is the base-function for following extensions. Offset is equivalent to
#' previous versions of subtraction. Advantage of offset is the use of the Likelihood as a whole!
#'
#'
#' @param Y metric input value of target variable
#' @param X metric matrix of predictive covariates
#' @param random specification of random effects lmer-style
#' @param data specify sample data
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default to 25
#' @param importance variable importance mode processed by the
#' random forest from the \pkg{ranger}. Must be 'none', 'impurity', 'impurity_corrected',
#' 'permutation'.
#' @param ... variables such as mtry, num.tree essentially anything that can be passed to \pkg{ranger}
#'
#' @return object of class SAEforest
#' @export
#' @details Some scientific or function specific details
#' @seealso \code{\link{SAEforest}}
#'
#'
MERFranger <- function(Y, X, random, data, initialRandomEffects = 0, ErrorTolerance = 0.0001,
                        MaxIterations = 25, importance = "none", ...) {

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

  class(result) <- "MERF"

  return(result)
}
