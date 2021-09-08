#' Main function for unit-level MERF
#'
#' This function uses forests from the ranger package combined with lmer from lme4.
#' The function is the base-function for following extensions. Offest is equivalent to
#' previous verisons of subtraction. Advantage of offset is the use of the Likelihood as a whole!
#'
#'
#' @param Y metric input value of target variable
#' @param X metric matrix of predictive covariates
#' @param random specification of random effects lmer-style
#' @param data specify sample data
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default to 25
#' @param m_try change the number of trees for split decision. default to 1
#' @param survey_weigths specify survey weights. default to NULL
#' @param imp variable importance passed to ranger
#'
#' @return object of class SAEforest
#' @export
#'
#' @examples
MERFranger <- function(Y, X, random, data, initialRandomEffects = 0, ErrorTolerance = 0.0001,
                        MaxIterations = 25, m_try = 1, survey_weigths = NULL, seed=NULL, keep.inbag = FALSE,
                       imp ="none") {

  Target <- Y
  ContinueCondition <- TRUE
  iterations <- 0

  AdjustedTarget <- Target - initialRandomEffects
  oldLogLik <- 0
  while (ContinueCondition) {
    iterations <- iterations + 1
    rf <- ranger::ranger(x = X, y = AdjustedTarget, mtry = m_try, case.weights = survey_weigths,
                         seed= seed, keep.inbag=keep.inbag, importance = imp)
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
                 data = data,
                 LogLik = oldLogLik,
                 IterationsUsed = iterations,
                 Random = random,
                 ErrorTolerance = ErrorTolerance,
                 OOBresiduals = residuals )

  class(result) <- "SAEforest"

  return(result)
}
