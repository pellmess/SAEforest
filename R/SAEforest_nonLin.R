calc_indicat <- function(Y, threshold){

  hcr_function <- function(y,threshold){
    mean(y < threshold, na.rm = TRUE)
  }
  qsr_function <- function(y){
    sum(y[(y > quantile(y,0.8, na.rm = TRUE))]) / sum(y[(y < quantile(y,0.2, na.rm = TRUE))])
  }
  qsr2_function <- function(y){
    sum(y[(y > quantile(y,0.6, na.rm = TRUE))]) / sum(y[(y < quantile(y,0.4, na.rm = TRUE))])
  }
  pgap_function <- function(y,threshold) {
    mean((y < threshold)*(threshold - y) / threshold, na.rm = TRUE)
  }

  quant_preds <- quantile(Y, prob=c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  mean_est <- mean(Y, na.rm = TRUE)
  Gini_est <- ineq::Gini(Y, na.rm = TRUE)
  Hcr_est <- hcr_function(y = Y, threshold = threshold)
  Qsr_est <- qsr_function(y = Y)
  Pgap_est <- pgap_function(y = Y, threshold = threshold)

  indicators <- cbind(mean_est, t(quant_preds), Gini_est, Hcr_est, Pgap_est, Qsr_est)

  colnames(indicators) <- c("mean","quant10","quant25","median","quant75",
                            "quant90","gini","hcr","pgap","qsr")

  return(indicators)
}



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
  smearing_grid <- tidyr::expand_grid("e_ij" = unit_model$OOBresiduals, unit_preds_ID)
  smearing_grid <- smearing_grid %>% mutate(., y_star = predictions + e_ij, .keep="unused")

  smear_list <- smearing_grid %>% dplyr::group_split(idD)

  cl <- parallel::makeCluster(detectCores()-1 )

  parallel::clusterExport(cl, c("calc_indicat","threshold"))

  indicators <- parallel::parLapply(cl,smear_list, fun =
                             function(x){calc_indicat(x$y_star,
                                   threshold = threshold)})

  stopCluster(cl)

  indicators <- do.call(rbind.data.frame, indicators)

  indicators_out <- cbind("Domain" = unique(census_data[dName])[,1],indicators)

  return( list(Indicator_predictions = indicators_out,
                MERFmodel = unit_model))


}






