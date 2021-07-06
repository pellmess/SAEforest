SAEforest_nonLin <- function(Y, X, dName, survey_data, census_data,
                           initialRandomEffects = 0, ErrorTolerance = 0.0001,
                           MaxIterations = 25, m_try = 1, survey_weigths = NULL){

  random = paste0(paste0("(1|",dName),")")
  threshold = 0.6*median(Y, na.rm=TRUE)


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

  unit_preds_ID <- data.frame(census_data[dName],"predictions" = unit_preds)

  # SMEARING STEP HERE------------
  smearing_grid <- tidyr::expand_grid("e_ij" = unit_model$OOBresiduals, "predictions" = unit_preds)
  smearing_grid <- dplyr::inner_join(smearing_grid, unit_preds_ID, by = "predictions")

  smearing_grid <- smearing_grid %>% mutate(., y_star = predictions + e_ij, .keep="unused")

  smear_list <- smearing_grid %>% dplyr::group_split(idD)


  indicators <- sapply(smear_list, FUN = function(x){calc_indicat(x$y_star, threshold = threshold)})
  indicators <- t(indicators)

  colnames(indicators) <- c("mean","quant10","quant25","median","quant75",
                            "quant90","gini","hcr","pgap","qsr")

  indicators_out <- data.frame(cbind("Domain" = unique(census_data[dName])[,1],indicators))

  return(list(Indicator_predictions = indicators_out,
                MERFmodel = unit_model))


}






