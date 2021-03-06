
# Point mean ##############

point_mean <- function(Y, X, dName, smp_data, pop_data, initialRandomEffects,
                       ErrorTolerance, MaxIterations, importance = "none", ...){

  random = paste0(paste0("(1|",dName),")")

  unit_model <- MERFranger(Y = Y,
                         X = X,
                         random = random,
                         data = smp_data,
                         initialRandomEffects = initialRandomEffects,
                         ErrorTolerance = ErrorTolerance,
                         MaxIterations = MaxIterations,importance = importance, ...)

  unit_preds <- predict(unit_model$Forest, pop_data)$predictions+
    predict(unit_model$EffectModel,pop_data, allow.new.levels=TRUE)

  unit_preds_ID <- cbind(pop_data[dName],unit_preds)

  f0 <- as.formula(paste0("unit_preds ", " ~ ", dName))

  mean_preds <- aggregate(f0,data = unit_preds_ID,
                          FUN=mean)
  colnames(mean_preds) <- c(dName,"Mean")

  out_ob <- vector(mode="list", length = 2)

  out_ob[[1]] <- mean_preds
  out_ob[[2]] <- unit_model

return(out_ob)
}

# Point nonLin ##############

point_nonLin <- function(Y, X, dName, threshold, smp_data, pop_data, initialRandomEffects,
                       ErrorTolerance, MaxIterations, importance = "none", custom_indicator, ...){

  random = paste0(paste0("(1|",dName),")")
  domains = names(table(pop_data[[dName]]))
  popSize <- as.numeric(table(pop_data[[dName]]))

  if(is.null(threshold)){
    thresh = 0.6*median(Y, na.rm=TRUE)
  }
  if(is.function(threshold)){
    thresh = threshold(Y)
  }
  if(is.numeric(threshold)){
    thresh = threshold
  }

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = smp_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations,importance = importance, ...)

  unit_preds <- predict(unit_model$Forest, pop_data)$predictions+
    predict(unit_model$EffectModel,pop_data, allow.new.levels=TRUE)


#  if(is.null(custom_indicator)){
#  unit_list <- split(unit_preds, pop_data[dName])

#  indicators <- smear_fun(popSize = popSize, unit_preds=unit_list, oob_res =
#              unit_model$OOBresiduals, threshold = thresh)

#  indicators_out <- data.frame(domains, indicators)
#}

# SMEARING STEP HERE------------
  smear_list <- vector(mode="list", length = length(domains))

  for (i in seq_along(domains)){
    smear_i <- matrix(rep(unit_model$OOBresiduals,popSize[i]), nrow=popSize[i],ncol=length(unit_model$OOBresiduals),byrow=TRUE)
    smear_i <- smear_i + unit_preds[pop_data[[dName]] == domains[i]]
    val_i <- c(smear_i)
    smear_list[[i]] <-  calc_indicatR(val_i, threshold = thresh, custom = custom_indicator)
  }

  indicators <- do.call(rbind.data.frame, smear_list)
  indicators_out <- cbind(domains, indicators)
  names(indicators_out)[1] <- dName
# __________________________________


  out_ob <- vector(mode="list", length = 2)

  out_ob[[1]] <- indicators_out
  out_ob[[2]] <- unit_model

  return(out_ob)
}

# Point meanAGG ##############

point_meanAGG <- function(Y, X, dName, smp_data, Xpop_agg, initialRandomEffects, ErrorTolerance,
                         MaxIterations, OOsample_obs, ADDsamp_obs, w_min, wSet = NULL, importance = "none",
                         verbose=TRUE, samp_seed=0712,...){

  random <- paste0(paste0("(1|",dName),")")
  groupNames <- as.character(unique(smp_data[[dName]]))
  groupNamesCens <- as.character(unique(Xpop_agg[[dName]]))
  OOsamp <- groupNamesCens[!groupNamesCens %in% groupNames]


  # Similarity Out-of-Sample
  similarXcens <- Xpop_agg[,colnames(Xpop_agg)!= dName]
  rownames(similarXcens) <- groupNamesCens
  simXcensMatrix <- as.matrix(dist(similarXcens))
  diag(simXcensMatrix) <- NA

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = smp_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations, importance = importance, ...)

  unit_preds <- predict(unit_model$Forest, smp_data)$predictions
  u_ij <- predict(unit_model$EffectModel,smp_data, allow.new.levels=TRUE)

  smp_data$forest_preds <- unit_preds + u_ij
  smp_data$u_ij <- u_ij

  joint_smp_data <- smp_data

  # Order vars by simularity
  #______________________________________________--
 if(is.null(wSet)){
  wSet <- names(sort(ranger::importance(unit_model$Forest), decreasing = TRUE))
 }

  # Can only respect variables that are part of Xpop_agg
  wSet <- wSet[wSet %in% names(Xpop_agg)]

  if (length(OOsamp) != 0){
    sim_groups <- apply(simXcensMatrix[!(groupNamesCens %in% OOsamp), OOsamp], 2, FUN = which.min)
    sim_groups <- groupNamesCens[!(groupNamesCens %in% OOsamp)][sim_groups]

    # sample and use OOsample data
    smp_oos <- vector(mode="list", length = length(OOsamp))

    for (i in seq(length(OOsamp))) {
      samp_from <- smp_data[as.character(smp_data[[dName]]) == sim_groups[i],]
      set.seed(samp_seed)
      return_oos <- dplyr::sample_n(samp_from, OOsample_obs,replace = TRUE)
      return_oos[dName] <- OOsamp[i]
      smp_oos[[i]] <- return_oos
    }

    OOs_smp_data <- do.call(rbind.data.frame, smp_oos)

    # Out-of-sample observations
    unit_preds_add <- predict(unit_model$Forest,OOs_smp_data)$predictions
    # just taking f(x_ij) from other areas - no random effects from other areas! (Best so..tested!)
    u_ij <- 0

    OOs_smp_data$forest_preds <- unit_preds_add
    OOs_smp_data$u_ij <- u_ij

    joint_smp_data <- rbind(smp_data,OOs_smp_data)
  }

  # find weights and adjust for failure

  smp_weightsIncluded <- vector(mode="list", length = length(groupNamesCens))
  smp_weightsNames <- vector(mode="list", length = length(groupNamesCens))

    for(i in groupNamesCens){
    pos <- which(i == groupNamesCens)

    X_input_elm  <- as.matrix(joint_smp_data[wSet])[joint_smp_data[dName]==i,]
    mu_input_elm <- as.matrix(Xpop_agg[wSet])[Xpop_agg[dName] == i, ]

    X_input_elm <- X_input_elm[, colSums(X_input_elm != 0) > 0]
    mu_input_elm <- mu_input_elm[colnames(X_input_elm)]

    ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)
    sum_w <- round(sum(ELMweight$prob), digits = 7)

    w_smp_data <- joint_smp_data[joint_smp_data[dName] == i,]

    if (sum_w==1){
      w_smp_data$weights <- ELMweight$prob
      rownames(w_smp_data)<- NULL
      smp_weightsIncluded[[pos]] <- w_smp_data
      smp_weightsNames[[pos]] <- colnames(X_input_elm)
    }

    else{
      mod_smp_data <- joint_smp_data[joint_smp_data[dName] == i,]
      rownames(mod_smp_data)<- NULL

      if(ADDsamp_obs != 0){
        similarXcens <- Xpop_agg[,colnames(Xpop_agg)!= dName]
        rownames(similarXcens) <- groupNamesCens
        simXcensMatrix <- as.matrix(dist(similarXcens))
        diag(simXcensMatrix) <- NA

        sim_group <- which.min(simXcensMatrix[, pos])
        samp_add <- joint_smp_data[joint_smp_data[dName]==groupNamesCens[sim_group],]
        set.seed(samp_seed)
        return_add <- dplyr::sample_n(samp_add, ADDsamp_obs,replace = TRUE)
        return_add[dName] <- i

        mod_smp_data <- rbind(joint_smp_data[joint_smp_data[dName] == i,],
                                 return_add)

        rownames(mod_smp_data)<- NULL
      }

      # RECALCULATE
      X_input_elm  <- as.matrix(mod_smp_data[wSet])
      mu_input_elm <- as.matrix(Xpop_agg[wSet])[Xpop_agg[dName] == i, ]

      X_input_elm <- X_input_elm[, colSums(X_input_elm != 0) > 0]
      mu_input_elm <- mu_input_elm[colnames(X_input_elm)]

      ELMweight <- elm_wrapper(X_input_elm, mu_input_elm)
      sum_w <- round(sum(ELMweight$prob), digits = 7)

      if (sum_w == 1){
        mod_smp_data$weights <- ELMweight$prob
        rownames(mod_smp_data)<- NULL
        smp_weightsIncluded[[pos]] <- mod_smp_data
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
        mod_smp_data$weights <- ELMweight$prob
        rownames(mod_smp_data)<- NULL
        smp_weightsIncluded[[pos]] <- mod_smp_data
        smp_weightsNames[[pos]] <- colnames(X_input_elm)

      }

      else{
        if(verbose ==TRUE){
        print(paste("Calculation of weights failed for area:", i))
        }
        rownames(w_smp_data)<- NULL
        w_smp_data$weights <- 1/dim(w_smp_data)[1]
        smp_weightsIncluded[[pos]] <- w_smp_data
        smp_weightsNames[[pos]] <- NA
      }
    }
  }

  final_smp_data <- do.call(dplyr::bind_rows, smp_weightsIncluded)

  # ESTIMATE MEANS

  final_smp_data$W_mean <- with(final_smp_data, forest_preds  * weights)
  f0 <- as.formula(paste("W_mean", paste(dName), sep=" ~ "))
  Mean_preds <- aggregate(f0, data=final_smp_data, FUN=sum)
  colnames(Mean_preds)[2] <- c("Mean")

  # Prepare return object

  return(list(Indicators = Mean_preds,
              MERFmodel = unit_model, ModifiedSet = final_smp_data, ADDsamp_obs = ADDsamp_obs,
              OOsample_obs = OOsample_obs, wSet=wSet, w_min=w_min, wAreaInfo=smp_weightsNames))

}


# Point MC nonLin ##############

point_MC_nonLin <- function(Y, X, dName, threshold, smp_data, pop_data, initialRandomEffects,B_point,
                         ErrorTolerance, MaxIterations, importance = "none", custom_indicator, ...){

  domains = names(table(pop_data[[dName]]))
  random = paste0(paste0("(1|",dName),")")

  ifelse(is.null(custom_indicator),
         calc_indicat <- calc_indicatC,
         calc_indicat <- calc_indicatR)

  popSize_N <- data.frame(table(pop_data[[dName]]))
  popSize_n <- data.frame(table(smp_data[[dName]]))
  colnames(popSize_N) <- c(dName, "N_i")
  colnames(popSize_n) <- c(dName, "n_i")
  popSize <- dplyr::left_join(popSize_N, popSize_n, by = dName)
  popSize[,3][is.na(popSize[,3])] <-0

  if(is.null(threshold)){
    thresh = 0.6*median(Y, na.rm=TRUE)
  }
  if(is.function(threshold)){
    thresh = threshold(Y)
  }
  if(is.numeric(threshold)){
    thresh = threshold
  }

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = smp_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations,importance = importance, ...)

  unit_preds <- predict(unit_model$Forest, pop_data)$predictions +
    predict(unit_model$EffectModel, pop_data, allow.new.levels=TRUE)

  # DATA PREP ________________________________________________
  ran_obj <- ran_comp(Y=Y, smp_data = smp_data, mod=unit_model, ADJsd = unit_model$ErrorSD, dName = dName)
  ran_effs <- ran_obj$ran_effs
  forest_res <- ran_obj$forest_res
  smp_data <- ran_obj$smp_data

  pred_val <- matrix(unit_preds, ncol =B_point,
                     nrow = length(unit_preds), byrow = FALSE)

  y_hat <- pred_val + sample(forest_res, size = length(pred_val), replace = TRUE)

  # EBP-special term:
  gamm_i <- (unit_model$RanEffSD^2)/(unit_model$RanEffSD^2+(unit_model$ErrorSD^2/popSize$n_i))

  v_i <- apply(y_hat, 2,  function(x){rep(sample(ran_effs, size= length(popSize$N_i), replace = TRUE), popSize$N_i)})
  gamm_1 <- rep((1-gamm_i), popSize$N_i)
  y_star <- y_hat + gamm_1 * v_i

  indi_agg <- rep(1:length(popSize$N_i), popSize$N_i)
  my_agg <-  function(x){tapply(x, indi_agg, calc_indicat, threshold = thresh, custom = custom_indicator)}
  tau_star <- apply(y_star, my_agg, MARGIN = 2, simplify = FALSE)

  ifelse(is.null(custom_indicator),
         col_names <- names(tau_star[[1]]$`1`),
         col_names <- colnames(tau_star[[1]]$`1`))

  comb <- function(x){matrix(unlist(x), nrow = length(popSize$N_i), byrow = TRUE)}
  tau_star <- sapply(tau_star, comb, simplify = FALSE)

  indicators <- Reduce('+',tau_star)/length(tau_star)
  colnames(indicators) <- col_names

  # __________________________________

  result <- list(
    Indicators = cbind(popSize[dName], indicators),
    model = unit_model)
  return(result)
}

