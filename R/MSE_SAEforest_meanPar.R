# DOES NOT WORK AS WANTED...takes about 4 times longer when applied through package.
# One reason might be that that ranger already uses parallization such that jobs cannot be effieciently
# allocated

MSE_SAEforest_mean_REB_par <- function(Y, X, dName, smp_data, mod, ADJsd, pop_data, B=100,
                                   initialRandomEffects = 0, ErrorTolerance = 0.0001,
                                   MaxIterations = 25, ...){

  forest_m1 <- mod
  rand_struc = paste0(paste0("(1|",dName),")")
  boots_pop <- vector(mode="list",length = B)
  boots_pop <- sapply(boots_pop,function(x){pop_data},simplify =FALSE)
  n_i <- as.numeric(table(pop_data[[dName]]))

  # DATA PREP ________________________________________________
  forest_res1 <- Y - predict(mod$Forest, smp_data)$predictions

  smp_data$forest_res <- forest_res1

  # Random Effects
  formRF <- formula(paste("forest_res ~", paste0(dName)))
  ran_effs1 <- aggregate(data=smp_data, formRF, FUN=mean)
  colnames(ran_effs1) <- c(dName,"r_bar")

  smp_data <- merge(smp_data,ran_effs1,by = dName)
  smp_data$forest_eij <- smp_data$forest_res-smp_data$r_bar

  # prepare for sampling
  forest_res <- smp_data$forest_eij
  forest_res<-(forest_res/sd(forest_res))*ADJsd

  # CENTER
  forest_res <- forest_res-mean(forest_res)

  # prepare for sampling
  ran_effs <- ran_effs1$r_bar
  ran_effs <- (ran_effs/sd(ran_effs))*mod$RanEffSD

  # CENTER
  ran_effs <- ran_effs-mean(ran_effs)

  pred_vals<- predict(forest_m1$Forest, pop_data)$predictions
  my_pred_f <- function(x){pred_vals}
  pred_t <- vector(mode="list", length = B)
  pred_t <- sapply(pred_t, my_pred_f,simplify = FALSE)

  # DATA PREP ________________________________________________

  # Errors
  block_sample <- function(x){

    in_samp <- t(unique(pop_data[dName]))  %in% t(unique(smp_data[dName]))
    domains <- t(unique(pop_data[dName]))

    block_err <- vector(mode="list",length = length(domains))

    for (idd in which(in_samp)){
      block_err[[idd]] <- sample(forest_res[smp_data[dName]==domains[idd]],size = sum(pop_data[dName] == domains[idd]),replace=TRUE)
    }

    if (sum(in_samp)!= length(domains)){
      for (idd in which(!in_samp)){
        block_err[[idd]] <- sample(forest_res, size = sum(pop_data[dName] == domains[idd]), replace=TRUE)
      }
    }

    return(unlist(block_err))
  }

  e_ij <- sapply(boots_pop, block_sample, simplify = FALSE)

  smp_data$forest_res <- NULL

  u_i <- replicate(length(boots_pop),rep(sample(ran_effs, size = length(t(unique(pop_data[dName]))),
                                                replace=TRUE), n_i), simplify = FALSE)

  # combine
  y_star_u_star <-  Map(function(x,y,z){x+y+z}, pred_t, e_ij, u_i)

  boots_pop <- Map(cbind, boots_pop, "y_star_u_star" = y_star_u_star)


  agg_form <- formula(paste("y_star_u_star ~ ", paste0(dName)))
  my_agg <- function(x){aggregate(agg_form, x,  mean)[,2]}

  my_estim_f <- function(x){point_mean(Y = x$y_star_u_star, X = x[,colnames(X)], dName = dName, smp_data = x,
                                       pop_data = pop_data, initialRandomEffects = initialRandomEffects,
                                       ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,...)[[1]][,2]}

  # THINK ABOUT SEED
  sample_b <- function(x){sample_select(x, smp = smp_data, times = 1, dName = dName)}
  boots_sample <- sapply(boots_pop, sample_b)

  parallel_mode=ifelse(grepl("windows", .Platform$OS.type), "socket", "multicore")
  cpus <- min(parallel::detectCores()-2)
  parallelMap::parallelStart(mode = parallel_mode,
                             cpus = cpus, show.info = FALSE)

  parallelMap::parallelExport(objnames =c("dName", "my_agg", "agg_form", "pop_data","initialRandomEffects",
                                          "ErrorTolerance", "MaxIterations", "my_estim_f", "point_mean", "MERFranger",
                                          "X"))

  if (parallel_mode == "socket") {
    parallel::clusterSetRNGStream()
  }

  tau_star <- parallelMap::parallelSapply(boots_pop, my_agg)

  tau_b <- parallelMap::parallelSapply(boots_sample, my_estim_f)

  parallelMap::parallelStop()


  MSE_estimates <- rowMeans((tau_star - tau_b)^2)

  MSE_estimates <- data.frame(unique(pop_data[dName]), MSE=MSE_estimates)
  rownames(MSE_estimates) <- NULL

  #___________________________

  return(MSE_estimates)

}


