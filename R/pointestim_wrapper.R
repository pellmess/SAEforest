
point_mean <- function(Y, X, dName, survey_data, census_data, initialRandomEffects,
                       ErrorTolerance, MaxIterations, importance = "none", ...){

  random = paste0(paste0("(1|",dName),")")

  unit_model <- MERFranger(Y = Y,
                         X = X,
                         random = random,
                         data = survey_data,
                         initialRandomEffects = initialRandomEffects,
                         ErrorTolerance = ErrorTolerance,
                         MaxIterations = MaxIterations,importance = importance, ...)

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
                       ErrorTolerance, MaxIterations, importance = "none", ...){

  random = paste0(paste0("(1|",dName),")")

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = survey_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations,importance = importance, ...)

  unit_preds <- predict(unit_model$Forest, census_data)$predictions+
    predict(unit_model$EffectModel,census_data, allow.new.levels=TRUE)

  unit_preds_ID <- data.frame(census_data[dName],"predictions" = unit_preds)

  # SMEARING STEP HERE------------
  smear_list <- vector(mode="list", length = length(unique(census_data[[dName]])))

  for (i in seq_along(unique(census_data[[dName]]))){

  smear_i <- rowSums(expand_gridALT(unit_preds_ID[as.character(unit_preds_ID[[dName]]) == as.character(unique(census_data[[dName]])[i]), 2], unit_model$OOBresiduals))
  smear_list[[i]] <-  calc_indicat(smear_i, threshold = threshold)
  }

  indicators <- do.call(rbind.data.frame, smear_list)
  indicators_out <- cbind("Domain" = unique(census_data[[dName]]), indicators)
  # __________________________________

  out_ob <- vector(mode="list", length = 2)

  out_ob[[1]] <- indicators_out
  out_ob[[2]] <- unit_model

  return(out_ob)
}



point_meanAGG <- function(Y, X, dName, survey_data, Xcensus_agg, initialRandomEffects, ErrorTolerance,
                         MaxIterations, OOsample_obs, ADDsamp_obs, w_min, wSet = NULL, importance = "none", ...){

  random <- paste0(paste0("(1|",dName),")")
  groupNames <- as.vector(t(unique(survey_data[dName])))
  groupNamesCens <- as.vector(t(unique(Xcensus_agg[dName])))
  OOsamp <- groupNamesCens[!groupNamesCens %in% groupNames]
  n_smp <- as.numeric(table(survey_data[dName]))


  # Similarity Out-of-Sample
  similarXcens <- Xcensus_agg[,colnames(Xcensus_agg)!= dName]
  rownames(similarXcens) <- groupNamesCens
  simXcensMatrix <- as.matrix(dist(similarXcens))
  diag(simXcensMatrix) <- NA

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = survey_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations, importance = importance, ...)

  unit_preds <- predict(unit_model$Forest, survey_data)$predictions +
    predict(unit_model$EffectModel,survey_data, allow.new.levels=TRUE)

  survey_data$forest_preds <- unit_preds
  joint_survey_data <- survey_data

  # Order vars by simularity
  #______________________________________________--
 if(is.null(wSet)){
  wSet <- names(sort(ranger::importance(unit_model$Forest), decreasing = TRUE))
}

  if (length(OOsamp) != 0){
    sim_groups <- apply(simXcensMatrix[-OOsamp, OOsamp], 2, FUN = which.min)
    sim_groups <- groupNamesCens[-OOsamp][sim_groups]

    # sample and use OOsample data
    smp_oos <- vector(mode="list", length = length(OOsamp))

    for (i in seq(length(OOsamp))) {
      samp_from <- survey_data[as.vector(survey_data[dName] == sim_groups[i]),]
      return_oos <- dplyr::sample_n(samp_from, OOsample_obs,replace = TRUE)
      return_oos[dName] <- OOsamp[i]
      smp_oos[[i]] <- return_oos
    }

    OOs_survey_data <- do.call(rbind.data.frame, smp_oos)

    # Out-of-sample observations
    unit_preds_add <- predict(unit_model$Forest,OOs_survey_data)$predictions+
      predict(unit_model$EffectModel,OOs_survey_data, allow.new.levels=TRUE)

    OOs_survey_data$forest_preds <- unit_preds_add

    joint_survey_data <- rbind(survey_data,OOs_survey_data)
  }

  # find weights and adjust for failure
  # USED CVXR as solver due to the observation that results are equivalent to the Lagrange Multiplier!

  smp_weightsIncluded <- vector(mode="list", length = length(groupNamesCens))
  smp_weightsNames <- vector(mode="list", length = length(groupNamesCens))

    for(i in groupNamesCens){
    pos <- which(i == groupNamesCens)

    X_input_elm  <- as.matrix(joint_survey_data[wSet])[joint_survey_data[dName]==i,]
    mu_input_elm <- as.matrix(Xcensus_agg[wSet])[Xcensus_agg[dName] == i, ]

    X_input_elm <- X_input_elm[, colSums(X_input_elm != 0) > 0]
    mu_input_elm <- mu_input_elm[colnames(X_input_elm)]

    ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)
    sum_w <- round(sum(ELMweight$prob), digits = 7)

    w_survey_data <- joint_survey_data[joint_survey_data[dName] == i,]

    if (sum_w==1){
      w_survey_data$weights <- ELMweight$prob
      smp_weightsIncluded[[pos]] <- w_survey_data
      smp_weightsNames[[pos]] <- colnames(X_input_elm)
    }

    else{
      if(ADDsamp_obs != 0){
        similarXcens <- Xcensus_agg[,colnames(Xcensus_agg)!= dName]
        rownames(similarXcens) <- groupNamesCens
        simXcensMatrix <- as.matrix(dist(similarXcens))
        diag(simXcensMatrix) <- NA

        sim_group <- which.min(simXcensMatrix[, pos])
        samp_add <- joint_survey_data[joint_survey_data[dName]==groupNamesCens[sim_group],]
        return_add <- dplyr::sample_n(samp_add, ADDsamp_obs,replace = TRUE)
        return_add[dName] <- i

        mod_survey_data <- rbind(joint_survey_data[joint_survey_data[dName] == i,],
                                 return_add)

        rownames(mod_survey_data)<- NULL
      }

      mod_survey_data <- joint_survey_data[joint_survey_data[dName] == i,]
      rownames(mod_survey_data)<- NULL

      # RECALCULATE
      X_input_elm  <- as.matrix(mod_survey_data[wSet])
      mu_input_elm <- as.matrix(Xcensus_agg[wSet])[Xcensus_agg[dName] == i, ]

      X_input_elm <- X_input_elm[, colSums(X_input_elm != 0) > 0]
      mu_input_elm <- mu_input_elm[colnames(X_input_elm)]

      ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)
      sum_w <- round(sum(ELMweight$prob), digits = 7)

      if (sum_w == 1){
        mod_survey_data$weights <- ELMweight$prob
        smp_weightsIncluded[[pos]] <- mod_survey_data
        smp_weightsNames[[pos]] <- colnames(X_input_elm)
      }

      else{
        sum_w <- 0

        while((sum_w!=1) & (dim(X_input_elm)[2] > w_min)){
          X_input_elm <- X_input_elm[,-dim(X_input_elm)[2]]
          mu_input_elm <- mu_input_elm[-dim(X_input_elm)[2]]

          ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)
          sum_w <- round(sum(ELMweight$prob), digits = 7)
        }
      }

      if (sum_w == 1){
        mod_survey_data$weights <- ELMweight$prob
        smp_weightsIncluded[[pos]] <- mod_survey_data
        smp_weightsNames[[pos]] <- colnames(X_input_elm)
      }

      else{
        print(paste("Calculation of weights failed for area:", i))
        w_survey_data$weights <- 1/dim(w_survey_data)[1]
        smp_weightsIncluded[[pos]] <- w_survey_data
        smp_weightsNames[[pos]] <- NA
      }
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
              MERFmodel = unit_model, ModifiedSet = final_survey_data, ADDsamp_obs = ADDsamp_obs,
              wSet=wSet, w_min=w_min, wAreaInfo=smp_weightsNames))

}





