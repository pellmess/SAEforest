MERF_ranger <- function(Y, X, random, data, initialRandomEffects = 0, ErrorTolerance = 0.0001,
                        MaxIterations = 25, m_try = 1, survey_weigths = NULL) {

  Target <- Y
  ContinueCondition <- TRUE
  iterations <- 0

  AdjustedTarget <- Target - initialRandomEffects
  oldLogLik <- 0
  while (ContinueCondition) {
    iterations <- iterations + 1
    rf <- ranger::ranger(x = X, y = AdjustedTarget, mtry = m_try, case.weights = survey_weigths)
    forest_preds <- rf$predictions
    f0 <- as.formula(paste0("Target ~ -1+offset(forest_preds)+", random))
    lmefit <- lme4::lmer(f0, data = data, REML = FALSE)

    newLogLik <- as.numeric(logLik(lmefit))

    ContinueCondition <- (abs(newLogLik - oldLogLik[iterations]) / oldLogLik[iterations] > ErrorTolerance &
      iterations < MaxIterations)
    oldLogLik <- c(oldLogLik, newLogLik)
    AllEffects <- predict(lmefit)
    AdjustedTarget <- Target - (AllEffects-forest_preds)
    # print(iterations)
  }


  residuals <- rep(NA, length = length(Target))
  residuals <- Target - predict(lmefit) - rf$predicted
  attr(residuals, "label") <- NULL

  result <- list(Forest = rf,
                 EffectModel = lmefit,
                 RandomEffects = ranef(lmefit),
                 RanEffSD = as.data.frame(VarCorr(lmefit))$sdcor[1],
                 ErrorSD = stats::sigma(lmefit),
                 VarianceCovariance =VarCorr(lmefit),
                 data = data,
                 LogLik = oldLogLik,
                 IterationsUsed = iterations,
                 Random = random,
                 ErrorTolerance = ErrorTolerance,
                 residuals = residuals )

  class(result) <- "SAEforest"

  return(result)
}
