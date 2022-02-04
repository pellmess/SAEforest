# ADD error generation such as in the other Bootstraps (!)


# Is also in support functions..but use for now, as long as analytical is not fully integrated
# in the package.

ran_comp <- function(mod, smp_data, Y, dName, ADJsd){

  forest_res1 <- Y - predict(mod$Forest, smp_data)$predictions
  smp_data$forest_res <- forest_res1

  # Random Effects
  formRF <- formula(paste("forest_res ~", paste0(dName)))
  ran_effs1 <- aggregate(data=smp_data, formRF, FUN=mean)
  colnames(ran_effs1) <- c(dName,"r_bar")

  smp_data <- dplyr::left_join(smp_data,ran_effs1,by = dName)
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

  return(list(forest_res = forest_res,
              ran_effs = ran_effs,
              smp_data = smp_data))
}



MSE_MERFanalytical <- function(Y, mod, smp_data, X, dName, err_sd, B=25,
                               initialRandomEffects = 0, ErrorTolerance = 0.0001,
                               MaxIterations = 25, ...){

  # JUST FOR IN-sample observations

  in_dom <- smp_data[[dName]]
  rand_struc <- paste0(paste0("(1|",dName),")")
  ran_sd <- mod$RanEffSD

  n_i <- as.numeric(table(in_dom))
  n_all <- length(t(in_dom))
  m_all <- length(n_i)

  gam_i <- ran_sd^2 /(ran_sd^2+(err_sd^2/n_i))

  g_1 <- (1-gam_i)*ran_sd^2

  h_n <- err_sd^4*(2/(n_all^2)*( (n_all*ran_sd^2+err_sd^2)^2/(m_all-1) + err_sd^4/(
    m_all*(n_all-1)))) + ran_sd^4*(2*err_sd^2 / m_all*(n_all-1) ) - 2* ran_sd^2 *
    err_sd^2 * (-(2*err_sd^4)/(m_all*n_all*(n_all-1)))

  g_3 <- n_i^(-2)*(ran_sd^2+(err_sd^2/n_i))^(-3) * h_n


  # BOOTSTRAP FOR g_2
  pred_val <- matrix(predict(mod$Forest, smp_data)$predictions, ncol = B,
                     nrow = length(mod$Forest$predictions), byrow = FALSE)

  ran_obj <- ran_comp(Y=Y, smp_data = smp_data, mod=mod, ADJsd = err_sd, dName = dName)
  ran_effs <- ran_obj$ran_effs
  forest_res <- ran_obj$forest_res

  y_hat <- pred_val + sample(forest_res, size = length(pred_val), replace = TRUE)
  u_i <- apply(y_hat, 2, function(x){rep(sample(ran_effs, size = m_all, replace = TRUE), n_i)})
  y_star <- y_hat + u_i

  my_estim_f <- function(x){MERFranger(Y=x, X = X, random = rand_struc,
                                       data=smp_data, initialRandomEffects = initialRandomEffects,
                                       ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)}

  est_mods <- pbapply::pbapply(y_star, 2, my_estim_f)

  new_preds <- function(x){predict(x$Forest, smp_data)$predictions}
  f_b <- sapply(est_mods, new_preds)

  f_diff <- data.frame(in_dom, pred_val - f_b)
  colnames(f_diff)[1] <- dName

  formRF <- formula(paste(". ~", paste0(dName)))
  agg_diffs <- aggregate(formRF, data=f_diff, mean)[,-1]

  B_gams <- function(x){1-((x$RanEffSD^2)/(x$RanEffSD^2 + x$ErrorSD^2/n_i))}
  One_minus_gam_b <- sapply(est_mods,B_gams)

  g_2 <- rowMeans((One_minus_gam_b * agg_diffs)^2)

  #______________________________________
  MSE_analytical <- g_1 + g_2 + 2*g_3

  MSE_analytical <- data.frame(unique(smp_data[dName]), Mean=MSE_analytical)
  rownames(MSE_analytical) <- NULL

  return(MSE_analytical)
}



MSE_MERFanalytical_Ni <- function(Y, mod, smp_data, cens_data, X, dName, err_sd, B=25,
                                  initialRandomEffects = 0, ErrorTolerance = 0.0001,
                                  MaxIterations = 25, ...){

  # JUST FOR IN-sample observations

  in_dom <- smp_data[[dName]]
  rand_struc <- paste0(paste0("(1|",dName),")")
  ran_sd <- mod$RanEffSD

  n_i <- as.numeric(table(in_dom))
  n_all <- length(t(in_dom))
  m_all <- length(n_i)

  gam_i <- ran_sd^2 /(ran_sd^2+(err_sd^2/n_i))

  g_1 <- (1-gam_i)*ran_sd^2

  h_n <- err_sd^4*(2/(n_all^2)*( (n_all*ran_sd^2+err_sd^2)^2/(m_all-1) + err_sd^4/(
    m_all*(n_all-1)))) + ran_sd^4*(2*err_sd^2 / m_all*(n_all-1) ) - 2* ran_sd^2 *
    err_sd^2 * (-(2*err_sd^4)/(m_all*n_all*(n_all-1)))

  g_3 <- n_i^(-2)*(ran_sd^2+(err_sd^2/n_i))^(-3) * h_n


  # BOOTSTRAP FOR g_2
  pred_val <- matrix(predict(mod$Forest, smp_data)$predictions, ncol = B,
                     nrow = length(mod$Forest$predictions), byrow = FALSE)

  pred_val_NI <- matrix(predict(mod$Forest, cens_data)$predictions, ncol = B,
                     nrow = dim(cens_data)[1], byrow = FALSE)

  ran_obj <- ran_comp(Y=Y, smp_data = smp_data, mod=mod, ADJsd = err_sd, dName = dName)
  ran_effs <- ran_obj$ran_effs
  forest_res <- ran_obj$forest_res

  y_hat <- pred_val + sample(forest_res, size = length(pred_val), replace = TRUE)
  u_i <- apply(y_hat, 2, function(x){rep(sample(ran_effs, size = m_all, replace = TRUE), n_i)})
  y_star <- y_hat + u_i

  my_estim_f <- function(x){MERFranger(Y=x, X = X, random = rand_struc,
                                       data=smp_data, initialRandomEffects = initialRandomEffects,
                                       ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations)}

  est_mods <- pbapply::pbapply(y_star, 2, my_estim_f)

  new_preds <- function(x){predict(x$Forest, cens_data)$predictions}
  f_b <- sapply(est_mods, new_preds)

  f_diff <- data.frame(cens_data[[dName]], pred_val_NI - f_b)
  colnames(f_diff)[1] <- dName

  formRF <- formula(paste(". ~", paste0(dName)))
  agg_diffs <- aggregate(formRF, data=f_diff, mean)[,-1]

  B_gams <- function(x){1-((x$RanEffSD^2)/(x$RanEffSD^2 + x$ErrorSD^2/n_i))}
  One_minus_gam_b <- sapply(est_mods,B_gams)

  g_2 <- rowMeans((One_minus_gam_b * agg_diffs)^2)

  #______________________________________
  MSE_analytical <- g_1 + g_2 + 2*g_3

  MSE_analytical <- data.frame(unique(smp_data[dName]), Mean=MSE_analytical)
  rownames(MSE_analytical) <- NULL

  return(MSE_analytical)
}



# Example Runs


#load("C:/Users/pkrennmair/Documents/MERF_aggSim_2408/s1_normal_500_aggForest.RData")
#load("C:/Users/Patrick/Documents/Studium/FU Berlin/Diss/Data/Simulation_Data_AGGMERFPaper/MBsimu1008/s1_normal_500_aggForest.RData")

#pop_data <- Pop[[15]]
#smp_data <- samp[[15]]
#X_covar <- smp_data[,c("x1","x2")]
#income <- smp_data$y
#Example 1:
#Calculating point-estimates and discussing basic generic functions

#model1 <- SAEforest_mean(Y = income, X = X_covar, dName = "idD",
#                         smp_data = smp_data, pop_data = pop_data, mse = "nonparametric")


#MSE_ni <- MSE_MERFanalytical(mod = model1$MERFmodel, smp_data = smp_data, X = X_covar,
#                             dName = "idD", err_sd = model1$AdjustedSD, B=100)

#MSE_Ni <- MSE_MERFanalytical_Ni(Y=income, mod = model1$MERFmodel, cens_data= pop_data, smp_data = smp_data, X = X_covar,
#                                dName = "idD", err_sd = model1$AdjustedSD, B=100)


#MSE_comp <- cbind(model1$MSE_Estimates$Mean, MSE_Ni$Mean, MSE_ni$Mean)
#boxplot(sqrt(MSE_comp), col= 2:4)
#summary(sqrt(MSE_comp))
