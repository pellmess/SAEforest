
point_mean <- function(Y, X, dName, survey_data, census_data, initialRandomEffects,
                       ErrorTolerance, MaxIterations, ...){

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

out_ob <- vector(mode="list", length = 2)

out_ob[[1]] <- mean_preds
out_ob[[2]] <- unit_model

return(out_ob)
}





point_nonLin <- function(Y, X, dName, threshold, survey_data, census_data, initialRandomEffects,
                       ErrorTolerance, MaxIterations, ...){

  random = paste0(paste0("(1|",dName),")")

  if(is.null(threshold)){
    threshold = 0.6*median(Y, na.rm=TRUE)
  }

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = survey_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations,...)

  unit_preds <- predict(unit_model$Forest, census_data)$predictions+
    predict(unit_model$EffectModel,census_data, allow.new.levels=TRUE)

  unit_preds_ID <- data.frame(census_data[dName],"predictions" = unit_preds)

  # SMEARING STEP HERE------------
  smearing_grid <- tidyr::expand_grid("e_ij" = unit_model$OOBresiduals, unit_preds_ID)
  smearing_grid <- dplyr::mutate(smearing_grid, y_star = predictions + e_ij, .keep="unused")

  smear_list <-  smearing_grid %>% dplyr::group_split(idD, .keep = FALSE) %>% map(~calc_indicat(.x$y_star,threshold = threshold))


  indicators <- do.call(rbind.data.frame, smear_list)
  indicators_out <- cbind("Domain" = unique(census_data[dName])[,1],indicators)

 #_______________________________________
  out_ob <- vector(mode="list", length = 2)

  out_ob[[1]] <- indicators_out
  out_ob[[2]] <- unit_model

  return(out_ob)
}


