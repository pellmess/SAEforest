# THIS IS THE MSE FUNCTION FRO THE SAEforest_agg
# IT BUILD STRONGLY on the CODE from the sae package (!)
# Recoding might be needed (!)


MSE_SAEforest_aggOOB_wSet <- function (Y, X, dName, smp_data, mod, ADJsd, Xpop_agg, B=100,
                               popnsize, initialRandomEffects, ErrorTolerance,
                               MaxIterations, ...){

  dom <- smp_data[[dName]]
  in_dom <- unique(smp_data[[dName]])
  total_dom <- mod$Mean_Predictions[[dName]]
  p <- dim(X)[2]

  sigmae2est <- mod$MERFmodel$ErrorSD^2
  sigmau2est <- mod$MERFmodel$RanEffSD^2


  # NOPARA Errors ____________________________________________
  forest_res1 <- Y - mod$MERFmodel$Forest$predictions

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
  ran_effs <- (ran_effs/sd(ran_effs))*sqrt(sigmau2est)

  # CENTER
  ran_effs <- ran_effs-mean(ran_effs)

  #________________________________________________________________

  pred_t <- matrix(mod$MERFmodel$Forest$predictions, nrow = length(dom), ncol=B)

  random_eff <- tapply(mod$ModifiedSet$u_ij, mod$ModifiedSet[[dName]],mean)
  mu_t <- mod$Mean_Predictions$Mean - random_eff

  mu_pred <- matrix(mu_t, nrow = length(total_dom), ncol=B)

  N_i <- popnsize[,!colnames(popnsize) %in% dName]
  n_i <- rep(0, length(total_dom))
  n_i[total_dom %in% in_dom] <- as.numeric(table(dom))

  rd <- N_i - n_i

  e_ij <- matrix(sample(forest_res, size = length(pred_t), replace=TRUE),
                 nrow = length(dom), ncol=B, byrow = FALSE)

  e_i_mean <- as.matrix(aggregate(.~dom, data=data.frame(dom,e_ij), mean)[,-1])

  u_i <- apply(mu_pred, 2, function(x){sample(ran_effs, size=length(total_dom), replace = TRUE )})
  u_ij <- apply(u_i[total_dom %in% in_dom,], 2, function(x){rep(x, n_i[total_dom %in% in_dom])})

  y_star <- pred_t + u_ij + e_ij


  samp_erd <- function(x){sample((forest_res/ADJsd)*sqrt(ADJsd^2/x), size = B, replace = TRUE )}
  erd_mean <- t(sapply(rd, samp_erd))

  insamp_ei <- e_i_mean * (n_i/N_i)[total_dom %in% in_dom] +
    erd_mean[total_dom %in% in_dom,] * (rd/N_i)[total_dom %in% in_dom]

  tau_star <- mu_pred + u_i + erd_mean

  tau_star[total_dom %in% in_dom,] <- mu_pred[total_dom %in% in_dom,] + u_i[total_dom %in% in_dom,]+
    insamp_ei


  my_estim_f <- function(x){point_meanAGG(Y = x, X=X, dName = dName, smp_data = smp_data,
                                          Xpop_agg = Xpop_agg, initialRandomEffects = initialRandomEffects,
                                          ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                                          OOsample_obs = mod$OOsample_obs, ADDsamp_obs = mod$ADDsamp_obs,
                                          w_min = mod$w_min, wSet = mod$wSet, verbose=FALSE,...)[[1]]$Mean}

  tau_b <- pbapply::pbapply(y_star, 2, my_estim_f)

  MSE_estimates <- rowMeans((tau_star - tau_b)^2)

  MSE_estimates <- data.frame(mod$Mean_Predictions[dName], MSE=MSE_estimates)
  rownames(MSE_estimates) <- NULL

  return(MSE_estimates)
  }
