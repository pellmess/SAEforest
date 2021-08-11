#' Main function for ALL Indicators of MERF with unit-level data
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param survey_data survey data set
#' @param census_data aggregated census level covariates
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default set to 0
#' @param m_try default set to 1
#' @param survey_weigths default set to NULL
#'
#' @return returns object including area level indicators such as quantiles, hcr, gini etc.
#' as well as model details
#' @export
#'
#' @examples
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

  unit_preds <- predict(unit_model$Forest, census_data)$predictions+
    predict(unit_model$EffectModel,census_data, allow.new.levels=TRUE)

  unit_preds_ID <- data.frame(census_data[dName],"predictions" = unit_preds)

  # SMEARING STEP HERE------------
  smearing_grid <- tidyr::expand_grid("e_ij" = unit_model$OOBresiduals, unit_preds_ID)
  smearing_grid <- dplyr::mutate(smearing_grid, y_star = predictions + e_ij, .keep="unused")

  smear_list <-  smearing_grid %>% dplyr::group_split(idD, .keep = FALSE) %>% map(~calc_indicat(.x$y_star,threshold = threshold))


  indicators <- do.call(rbind.data.frame, smear_list)
  indicators_out <- cbind("Domain" = unique(census_data[dName])[,1],indicators)

  # THIS PARA WORKS AS SCRIPT BUT NOT IN THE PACKAGE..THINK ABOUT!
  #cl <- parallel::makeCluster(parallel::detectCores()-1)

  #calc_indicat2 <- SAEforest::calc_indicat
  #helpfun <- function(x){calc_indicat2(x, threshold = threshold)}

  #parallel::clusterExport(cl, c("calc_indicat2","threshold"))
  #parallel::clusterEvalQ(cl, c("threshold"))

  #indicators <- parallel::parLapply(cl,smear_list, fun = helpfun)

  #parallel::stopCluster(cl)

  return( list(Indicator_predictions = indicators_out,
                MERFmodel = unit_model))
}






