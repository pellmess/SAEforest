SAEforest_mean <- function(Y, X, dName, survey_data, census_data,
                           initialRandomEffects = 0, ErrorTolerance = 0.0001,
                           MaxIterations = 25, m_try = 1, survey_weigths = NULL){

  random = paste0(paste0("(1|",dName),")")

  unit_model <- MERFranger(Y = Y,
                          X = X,
                          random = random,
                          data = survey_data,
                          initialRandomEffects = initialRandomEffects,
                          ErrorTolerance = ErrorTolerance,
                          MaxIterations = MaxIterations,
                          m_try = m_try,
                          survey_weigths = survey_weigths)

  unit_preds <- predict(unit_model$Forest, census_data)$predictions
                + predict(unit_model$EffectModel, allow.new.levels=TRUE)

  unit_preds_ID <- cbind(census_data[dName],unit_preds)

  f0 <- as.formula(paste0("unit_preds ", " ~ ", dName))

  mean_preds <- aggregate(f0,data = unit_preds_ID,
                          FUN=mean)
  colnames(mean_preds) <- c(dName,"Mean")


  result <- list(
    Mean_predictions = mean_preds,
    MERFmodel = unit_model)

  return(result)

}




