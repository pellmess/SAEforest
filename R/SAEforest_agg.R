SAEforest_agg <- function(Y, X, dName, survey_data, Xcensus_agg,
                             initialRandomEffects = 0, ErrorTolerance = 0.0001,
                             MaxIterations = 25, m_try = 1, survey_weigths = NULL){

  random <- paste0(paste0("(1|",dName),")")
  groupNames <- as.vector(t(unique(survey_data[dName])))
  n_smp <- as.numeric(table(survey_data[dName]))

  # Pseudo-Beobachtung einfügen, für Gewichtung (insbesondere kleine Areas notwendig)
  smp_add <- survey_data[1:length(n_smp),]
  X_add <- matrix(ncol = dim(X)[2], nrow = dim(Xcensus_agg)[1])

  for (i in groupNames) {
    X_input_elm  <- as.matrix(X[survey_data[dName]==i,])
    mu_input_elm <- as.matrix(Xcensus_agg[Xcensus_agg[dName] == i, -1])

    n = nrow(X_input_elm)

    add   = -(apply(X_input_elm, 2, mean) - mu_input_elm)/(2*n) + mu_input_elm
    smp_add[i,] <- c(i, add, NA)
    X_add[i,] <- add
  }


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

  # pseudo_observation
  unit_preds_add <- predict(unit_model$Forest,smp_add)$predictions+
    predict(unit_model$EffectModel,smp_add, allow.new.levels=TRUE)

  smp_add$forest_preds <- unit_preds_add
  survey_data$forest_preds <- unit_preds

  # find weights and adjust for failure

    smp_weightsIncluded <- vector(mode="list", length = length(n_smp))

    for(i in groupNames){
      X_input_elm  <- as.matrix(X[survey_data[dName]==i,])
      mu_input_elm <- as.matrix(Xcensus_agg[Xcensus_agg[dName] == i, -1])

      ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)
      sum_w <- round(sum(ELMweight$prob), digits = 7)

      if (sum_w == 1){
        w_survey_data <- survey_data[survey_data[dName] == i,]

        w_survey_data$weights <- ELMweight$prob
        w_survey_data$exper_weights <- ELMweight$experWeight
        smp_weightsIncluded[[i]] <- w_survey_data
      }

      else{
        X_input_elm  <- as.matrix(rbind(X[survey_data[dName]==i,],
                                        X_add[smp_add[dName]==i,]))
        mu_input_elm <- as.matrix(Xcensus_agg[Xcensus_agg[dName] == i, -1])

        ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)

        mod_survey_data <- rbind(survey_data[survey_data[dName] == i,],
                                 smp_add[smp_add[dName]==i,])

        mod_survey_data$weights <- ELMweight$prob
        mod_survey_data$exper_weights <- ELMweight$experWeight
        smp_weightsIncluded[[i]] <- mod_survey_data
      }
    }

    final_survey_data <- do.call(rbind.data.frame, smp_weightsIncluded)

    # ESTIMATE MEANS
    mean_est <- c()

    final_survey_data$W_mean <- with(final_survey_data, forest_preds  * weights)
    f0 <- as.formula(paste("W_mean", paste(dName), sep=" ~ "))
    Mean_preds <- aggregate(f0, data=final_survey_data, FUN=sum)
    colnames(Mean_preds)[2] <- c("Mean")

    # Prepare return object

    return(list(Mean_Predictions = Mean_preds,
                 MERFmodel = unit_model), ModifiedSet = final_survey_data)

  }












  return( list(Indicator_predictions = indicators_out,
               MERFmodel = unit_model))
}



