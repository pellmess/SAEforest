MSE_SAEforest_nonLin_REB <- function(Y, X, dName, threshold, survey_data, mod, ADJsd, cens_data, B=100,
                                      initialRandomEffects = 0, ErrorTolerance = 0.0001,
                                      MaxIterations = 25, custom_indicator,...){

  forest_m1 <- mod
  rand_struc = paste0(paste0("(1|",dName),")")

  n_i <- as.numeric(table(cens_data[[dName]]))

  boots_pop <- vector(mode="list",length = B)
  boots_pop <- sapply(boots_pop,function(x){cens_data},simplify =FALSE)

  # DATA PREP ________________________________________________
  forest_res1 <- Y - predict(forest_m1$Forest, survey_data)$predictions

  survey_data$forest_res <- forest_res1

  # Random Effects
  formRF <- formula(paste("forest_res ~", paste0(dName)))
  ran_effs1 <- aggregate(data=survey_data, formRF, FUN=mean)
  colnames(ran_effs1) <- c(dName,"r_bar")

  survey_data <- merge(survey_data,ran_effs1,by = dName)
  survey_data$forest_eij <- survey_data$forest_res-survey_data$r_bar

  # prepare for sampling
  forest_res <- survey_data$forest_eij
  forest_res<-(forest_res/sd(forest_res))*ADJsd

  # CENTER
  forest_res <- forest_res-mean(forest_res)

  # prepare for sampling
  ran_effs <- ran_effs1$r_bar
  ran_effs <- (ran_effs/sd(ran_effs))*forest_m1$RanEffSD

  # CENTER
  ran_effs <- ran_effs-mean(ran_effs)

  pred_vals<- predict(forest_m1$Forest, cens_data)$predictions
  my_pred_f <- function(x){pred_vals}
  pred_t <- vector(mode="list", length = B)
  pred_t <- sapply(pred_t, my_pred_f,simplify = FALSE)


  # Errors
  block_sample <- function(x){

    in_samp <- t(unique(cens_data[dName]))  %in% t(unique(survey_data[dName]))
    domains <- t(unique(cens_data[dName]))

    block_err <- vector(mode="list",length = length(domains))

    for (idd in which(in_samp)){
      block_err[[idd]] <- sample(forest_res,size = sum(cens_data[dName] == domains[idd]),replace=TRUE)
    }

    if (sum(in_samp)!= length(domains)){
      for (idd in which(!in_samp)){
        block_err[[idd]] <- sample(forest_res, size = sum(cens_data[dName] == domains[idd]), replace=TRUE)
      }
    }

    return(unlist(block_err))
  }

  e_ij <- sapply(boots_pop, block_sample, simplify = FALSE)

  survey_data$forest_res <- NULL

  u_i <- replicate(length(boots_pop),rep(sample(ran_effs, size = length(t(unique(cens_data[dName]))),
                                                replace=TRUE), n_i), simplify = FALSE)

  # combine
  y_star_u_star <-  Map(function(x,y,z){x+y+z}, pred_t, e_ij, u_i)

  if(is.numeric(threshold)){
    thresh <- sapply(y_star_u_star, function(x){threshold}, simplify = FALSE)
  }
  if(is.null(threshold)){
    thresh <- sapply(y_star_u_star, function(x){0.6*median(x, na.rm=TRUE)}, simplify = FALSE)
  }
  if(is.function(threshold)){
    thresh <- sapply(y_star_u_star, threshold, simplify = FALSE)
  }

  boots_pop <- Map(cbind, boots_pop, "y_star_u_star" = y_star_u_star, "thresh"=thresh)


  my_agg <-  function(x){tapply(x[["y_star_u_star"]], x[[dName]], calc_indicat, threshold = unique(x[["thresh"]]), custom = custom_indicator)}
  tau_star <- sapply(boots_pop, my_agg, simplify = FALSE)
  comb <- function(x){matrix(unlist(x), nrow = length(n_i), byrow = TRUE)}
  tau_star <- sapply(tau_star, comb, simplify = FALSE)


  # THINK ABOUT SEED
  sample_b <- function(x){sample_select(x,smp=survey_data,times=1, dName = dName)}
  boots_sample <- sapply(boots_pop, sample_b)


  # USE BOOTSTRAP SAMPLE TO ESITMATE

  my_estim_f <- function(x){point_nonLin(Y = x$y_star_u_star, X = x[,colnames(X)], dName = dName, threshold = threshold, survey_data = x, census_data = cens_data,
                                         initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
                                         MaxIterations = MaxIterations, custom_indicator=custom_indicator,...)[[1]][,-1]}

  tau_b <- pbapply::pbsapply(boots_sample, my_estim_f,simplify = FALSE)

  mean_square <- function(x,y){(x-y)^2}

  Mean_square_B <- mapply(mean_square, tau_b,tau_star, SIMPLIFY = FALSE)

  MSE_estimates <- Reduce('+',Mean_square_B)/length(Mean_square_B)
  MSE_estimates <- data.frame(unique(cens_data[dName]), MSE_estimates)
  rownames(MSE_estimates) <- NULL

  #__________________________
  return(MSE_estimates)

}
