# RANDOM FOREST WITH SMEARING BUT WITHOUT RANDOM EFFECTS

RangerForest_nonLin <- function(Y, X, dName, smp_data, pop_data,
                             threshold = NULL, importance ="none",
                             custom_indicator =NULL, na.rm = TRUE, full_smear = TRUE,...){

  # CHECK NON_FULL SMEAR ARGUMENT for adjustments!

  # ERROR CHECKS OF INPUTS
  #________________________________________

  if(na.rm == TRUE){
    comp_smp <- complete.cases(smp_data)
    smp_data <- smp_data[comp_smp,]
    Y <- Y[comp_smp]
    X <- X[comp_smp,]
    pop_data <- pop_data[complete.cases(pop_data),]
  }

  out_call <- match.call()

  # Make domain variable to character and sort data-sets
  smp_data[[dName]] <- factor(smp_data[[dName]], levels=unique(smp_data[[dName]]))
  pop_data[[dName]] <- factor(pop_data[[dName]], levels=unique(pop_data[[dName]]))

  # Point Estimation
  #________________________________________
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

  unit_model <- ranger(y = Y, x = X, data = smp_data, importance = importance, ...)

  unit_preds <- predict(unit_model, pop_data)$predictions
  OOBresiduals <- Y - unit_model$predictions

  # SMEARING STEP HERE------------
  smear_list <- vector(mode="list", length = length(domains))

  if(full_smear == TRUE){
    for (i in seq_along(domains)){
     smear_i <- matrix(rep(OOBresiduals,popSize[i]), nrow=popSize[i],ncol=length(OOBresiduals), byrow=TRUE)
     smear_i <- smear_i + unit_preds[pop_data[[dName]] == domains[i]]

     smear_list[[i]] <-  calc_indicatR(c(smear_i), threshold = thresh, custom = custom_indicator)
    }
  }

  if(full_smear == FALSE){
    for (i in seq_along(domains)){
      smear_i <- matrix(rep(sample(OOBresiduals,1900),popSize[i]), nrow=popSize[i], ncol=1900,byrow=TRUE)
      smear_i <- smear_i + unit_preds[pop_data[[dName]] == domains[i]]

      smear_list[[i]] <-  calc_indicatR(c(smear_i), threshold = thresh, custom = custom_indicator)
    }
  }

  indicators <- do.call(rbind.data.frame, smear_list)
  indicators_out <- cbind(domains, indicators)
  names(indicators_out)[1] <- dName

  # __________________________________

    result <- list(
      Indicators = indicators_out,
      model = c(unit_model, call = out_call, data=list(smp_data)))

    class(result) <- c("RangerForest_nonLin")
    return(result)

}
