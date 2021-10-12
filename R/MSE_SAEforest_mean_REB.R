MSE_SAEforest_mean_REB <- function(Y, X, dName, survey_data, mod, ADJsd, cens_data, B=100,
                                   initialRandomEffects = 0, ErrorTolerance = 0.0001,
                                   MaxIterations = 25, ...){


  forest_m1 <- mod
  rand_struc = paste0(paste0("(1|",dName),")")
  boots_pop <- vector(mode="list",length = B)
  boots_pop <- sapply(boots_pop,function(x){cens_data},simplify =FALSE)

# DEFINE AND USE SAMPLE SELECT WRAPPER FUNCTION
  sample_select <- function(pop, smp, times=100, set_seed = 1234){
    # pop.............. the population or census data
    # smp.............. the survey data
    # n................ the number of samples drawn
    # set_seed......... set seed for reproduceability

    smpSizes <- table(smp[dName])
    smpSizes <- data.frame(smpidD = as.numeric(names(smpSizes)), n_smp = as.numeric(smpSizes),
                           stringsAsFactors = FALSE)

    smpSizes <- left_join(data.frame(idD = unique(pop[dName])),
                          smpSizes, by = c("idD" = "smpidD"))

    smpSizes$n_smp[is.na(smpSizes$n_smp)] <- 0

    splitPop <- split(pop, pop$idD)

    stratSamp <- function(dfList, ns) {
      do.call(rbind, mapply(dfList, ns, FUN = function(df, n) {
        popInd <- seq_len(nrow(df))
        sel <- base::sample(popInd, n, replace = FALSE)
        df[sel, ]
      }, SIMPLIFY = F))
    }

    set.seed(set_seed)
    samples <- replicate(times, stratSamp(splitPop, smpSizes$n_smp), simplify = FALSE)

    rm(splitPop)
    return(samples)
  }

# DATA PREP ________________________________________________
forest_res1 <- Y - predict(mod$Forest, survey_data)$predictions

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
ran_effs <- (ran_effs/sd(ran_effs))*mod$RanEffSD

# CENTER
ran_effs <- ran_effs-mean(ran_effs)

  my_pred_f <- function(x){predict(forest_m1$Forest, x)$predictions}

  pred_t <-sapply(boots_pop,my_pred_f,simplify = FALSE)

# DATA PREP ________________________________________________

  # Errors
  block_sample <- function(x){

    in_samp <- t(unique(cens_data[dName]))  %in% t(unique(survey_data[dName]))
    domains <- t(unique(cens_data[dName]))

    block_err <- vector(mode="list",length = length(domains))

    for (idd in which(in_samp)){
      block_err[[idd]] <- sample(forest_res[survey_data[dName]==domains[idd]],size = sum(cens_data[dName] == domains[idd]),replace=TRUE)
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

  # combine
  y_star <- mapply("+", pred_t,e_ij, SIMPLIFY = FALSE)

  boots_pop <- Map(cbind,boots_pop,"y_star"=y_star, "predz"=pred_t)


  u_i <- replicate(length(boots_pop),data.frame(u_i_star=sample(ran_effs, size = length(t(unique(cens_data[dName]))),
                                                replace=TRUE), unique(cens_data[dName])), simplify = FALSE)

  boots_pop <- map2(boots_pop, u_i, left_join, by = dName ,simplify=FALSE)

  boots_pop <- boots_pop %>%  map(~mutate(., y_star_u_star = y_star+u_i_star))
  boots_pop <- boots_pop %>%  map(~dplyr::select(., -one_of(c("u_i_star","y_star","predz"))))

  agg_form <- formula(paste(". ~ ", paste0(dName)))

  my_agg <- function(x){aggregate(agg_form, x[,c("y_star_u_star", dName)],  mean)}
  tau_star <- sapply(boots_pop,my_agg,simplify = FALSE)

  # THINK ABOUT SEED
  sample_b <- function(x){sample_select(x, smp = survey_data, times = 1)}
  boots_sample <- sapply(boots_pop, sample_b)


  # USE BOOTSTRAP SAMPLE TO ESITMATES
  my_estim_f <- function(x){point_mean(Y = x$y_star_u_star, X = x[,colnames(X)], dName = dName, survey_data = x,
                                       census_data = cens_data, initialRandomEffects = initialRandomEffects,
                                       ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)[[1]]}
  tau_b <- sapply(boots_sample, my_estim_f,simplify = FALSE)

  mean_square <- function(x,y){(x[,2] - y[,2])^2}

  Mean_square_B <- mapply(mean_square, tau_b,tau_star, SIMPLIFY = FALSE)

  MSE_estimates <- Reduce('+',Mean_square_B)/length(Mean_square_B)
  MSE_estimates <- data.frame(unique(cens_data[dName]), MSE=MSE_estimates)
  rownames(MSE_estimates) <- NULL

  #___________________________

  return(MSE_estimates)

}


