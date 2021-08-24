#' Main function for means of MERF with aggregated data
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param survey_data survey data set
#' @param Xcensus_agg aggregated census level covariates. Note that the
#' first column should include the group identifier
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default to 25
#' @param m_try default set to 1
#' @param survey_weigths default set to NULL
#'
#' @return returns object with Mean Predictions, model details and the modified dataset including weights
#' @export
#'
#' @examples

SAEforest_agg_SIMPLE <- function(Y, X, dName, survey_data, Xcensus_agg, initialRandomEffects = 0,
                          ErrorTolerance = 0.0001, MaxIterations = 25, m_try = 1,
                          survey_weigths = NULL, too_tiny = 5){

  random <- paste0(paste0("(1|",dName),")")
  groupNames <- as.vector(t(unique(survey_data[dName])))
  groupNamesCens <- as.vector(t(unique(Xcensus_agg[dName])))
  OOsamp <- groupNamesCens[!groupNamesCens %in% groupNames]
  n_smp <- as.numeric(table(survey_data[dName]))

  # Similarity Out-of-Sample
  similarXcens <- Xcensus_agg[,-1]
  rownames(similarXcens) <- Xcensus_agg[,1]
  simXcensMatrix <- as.matrix(dist(similarXcens))
  diag(simXcensMatrix) <- NA

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = survey_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations,
                           m_try = m_try,
                           survey_weigths = survey_weigths)

  unit_preds <- predict(unit_model$Forest, survey_data)$predictions +
    predict(unit_model$EffectModel,survey_data, allow.new.levels=TRUE)

  survey_data$forest_preds <- unit_preds
  joint_survey_data <- survey_data

  if (length(OOsamp) != 0){
    sim_groups <- apply(simXcensMatrix[-OOsamp, OOsamp], 2, FUN = which.min)

    # sample and use OOsample data
    smp_oos <- vector(mode="list", length = length(OOsamp))

    for (i in seq(length(OOsamp))) {
      samp_from <- survey_data[survey_data[dName]==sim_groups[i],]
      return_oos <- dplyr::sample_n(samp_from, OOsample_obs,replace = TRUE)
      return_oos[dName] <- OOsamp[i]
      smp_oos[[i]] <- return_oos
    }

    OOs_survey_data <- do.call(rbind.data.frame, smp_oos)

    # WORK FROM HERE
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    # Pseudo-Beobachtung einfügen, für Gewichtung (insbesondere kleine Areas notwendig)
    #  smp_add <- survey_data[1:length(n_smp),]
    #  X_add <- matrix(ncol = dim(X)[2], nrow = dim(Xcensus_agg)[1])

    #  for (i in groupNames) {
    #    X_input_elm  <- as.matrix(X[survey_data[dName]==i,])
    #    mu_input_elm <- as.matrix(Xcensus_agg[Xcensus_agg[dName] == i, -1])

    #    n = nrow(X_input_elm)

    #    add   = -(apply(X_input_elm, 2, mean) - mu_input_elm)/(2*n) + mu_input_elm
    #    smp_add[i,] <- c(i, add, NA)
    #    X_add[i,] <- add
    #  }

    # Out-of-sample observations
    unit_preds_add <- predict(unit_model$Forest,OOs_survey_data)$predictions+
      predict(unit_model$EffectModel,OOs_survey_data, allow.new.levels=TRUE)

    OOs_survey_data$forest_preds <- unit_preds_add

    joint_survey_data <- rbind(survey_data,OOs_survey_data)
  }

  # find weights and adjust for failure

  smp_weightsIncluded <- vector(mode="list", length = length(groupNamesCens))

  for(i in groupNamesCens){
    X_input_elm  <- as.matrix(joint_survey_data[colnames(X)])[joint_survey_data[dName]==i,]
    mu_input_elm <- as.matrix(Xcensus_agg[Xcensus_agg[dName] == i, -1])

    ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)
    sum_w <- round(sum(ELMweight$prob), digits = 7)

    if (sum_w == 1){
      w_survey_data <- joint_survey_data[joint_survey_data[dName] == i,]

      w_survey_data$weights <- ELMweight$prob
      w_survey_data$exper_weights <- ELMweight$experWeight
      smp_weightsIncluded[[i]] <- w_survey_data
    }

    else{
      w_survey_data <- joint_survey_data[joint_survey_data[dName] == i,]
        print(paste("Calculation of weights failed for area:", i))
      w_survey_data$weights <- 1/dim(w_survey_data)[1]
      w_survey_data$exper_weights <- NA
      smp_weightsIncluded[[i]] <- w_survey_data

    }
  }

  final_survey_data <- do.call(rbind, smp_weightsIncluded)

  # ESTIMATE MEANS
  mean_est <- c()

  final_survey_data$W_mean <- with(final_survey_data, forest_preds  * weights)
  f0 <- as.formula(paste("W_mean", paste(dName), sep=" ~ "))
  Mean_preds <- aggregate(f0, data=final_survey_data, FUN=sum)
  colnames(Mean_preds)[2] <- c("Mean")

  # Prepare return object

  return(list(Mean_Predictions = Mean_preds,
              MERFmodel = unit_model, ModifiedSet = final_survey_data))

}

