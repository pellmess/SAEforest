RE_Forest_ind_smearing_MSE_WILD_ORIG_unscaled <- function(surv_data,cens_data,mod, B=200, adjustSD,y_dat,x_dat,m_try=1,n_tree=500,
                                                          ErrorTolerance = 0.00001, MaxIterations = 50){
  # surv_data..............input of survey data
  # cens_data..............input of census data
  # B......................number of bootstrap replications


  getTrueVal <- function(pop, target, domain){
    library(ineq)
    threshold <- 0.6*median(pop[[target]], na.rm=TRUE)

    hcr_function <- function(y,threshold){
      mean(y < threshold, na.rm=TRUE)
    }
    qsr_function <- function(y){
      sum(y[(y > quantile(y,0.8, na.rm=TRUE))]) / sum(y[(y < quantile(y,0.2, na.rm=TRUE))])
    }
    qsr2_function <- function(y){
      sum(y[(y > quantile(y,0.6, na.rm=TRUE))]) / sum(y[(y < quantile(y,0.4, na.rm=TRUE))])
    }
    pgap_function <- function(y,threshold) {
      mean((y < threshold)*(threshold - y) / threshold, na.rm=TRUE)
    }

    est_hcr <- tapply(pop[[target]],pop[[domain]],hcr_function,threshold = threshold)
    est_pgap <- tapply(pop[[target]],pop[[domain]],pgap_function,threshold = threshold)
    est_median <- tapply(pop[[target]],pop[[domain]],median)
    est_mean <- tapply(pop[[target]],pop[[domain]],mean)
    est_gini <- tapply(pop[[target]],pop[[domain]],ineq)
    est_qsr <- tapply(pop[[target]],pop[[domain]],qsr_function)
    est_qsr2 <- tapply(pop[[target]],pop[[domain]],qsr2_function)
    est_quant01 <- tapply(pop[[target]],pop[[domain]],quantile,probs = 0.1, na.rm=TRUE)
    est_quant025 <- tapply(pop[[target]],pop[[domain]],quantile,probs = 0.25, na.rm=TRUE)
    est_quant075 <- tapply(pop[[target]],pop[[domain]],quantile,probs = 0.75, na.rm=TRUE)
    est_quant09 <- tapply(pop[[target]],pop[[domain]],quantile,probs = 0.9, na.rm=TRUE)

    return( data.frame(
      quant10 = est_quant01,
      quant25 = est_quant025,
      quant75 = est_quant075,
      quant90 =  est_quant09,
      gini = est_gini,
      mean = est_mean,
      hcr = est_hcr,
      qsr = est_qsr,
      qsr64 = est_qsr2,
      pgap = est_pgap,
      median = est_median
    )
    )
  }

  # DEFINE AND USE SAMPLE SELECT WRAPPER FUNCTION
  sample_select <- function(pop,smp,times=100,set_seed = 1234){
    # pop.............. the population or census data
    # smp.............. the survey data
    # n................ the number of samples drawn
    # set_seed......... set seed for reproduceability

    smpSizes <- table(smp$idD)
    smpSizes <- data.frame(smpidD = as.numeric(names(smpSizes)), n_smp = as.numeric(smpSizes),
                           stringsAsFactors = FALSE)

    smpSizes <- left_join(data.frame(idD = unique(pop$idD)),
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

  # REAL FUNCTION CODE
  forest_m1 <- mod

  fitted_s <- predict(mod$Forest,surv_data)+predict(mod$EffectModel, surv_data)

  boots_pop <- vector(mode="list",length = B)
  boots_pop <- sapply(boots_pop,function(x){cens_data},simplify =FALSE)

  forest_res <- y_dat -predict(mod$Forest)-predict(mod$EffectModel, surv_data)

  # forest_res<-(forest_res/sd(forest_res)) * adjustSD
  # CENTER
  forest_res <- forest_res-mean(forest_res)


  pred_val <- predict(forest_m1$Forest,boots_pop[[1]])

  pred_t <-sapply(boots_pop,function(x){pred_val},simplify = FALSE)

  # combine
  #  y_star <- mapply("+", pred_t,e_ij, SIMPLIFY = FALSE)

  boots_pop<-Map(cbind,boots_pop, "predz"=pred_t)


  u_i <- replicate(length(boots_pop),data.frame(u_i_star=rnorm(n =
                                                                 length(unique(cens_data$idD)),sd=forest_m1$RanEffSD),
                                                idD=unique(cens_data$idD)),simplify = FALSE)


  boots_pop<- map2(boots_pop, u_i, left_join, by = "idD",simplify=FALSE)

  boots_pop <- boots_pop %>%  map(~mutate(., y_hat_MSE = predz+u_i_star))
  boots_pop <- boots_pop %>%  map(~dplyr::select(., -one_of(c("u_i_star","predz"))))


  wild_errors <- function(z){
    indexer <- vapply(z$y_hat_MSE, function(x) {which.min(abs(x - fitted_s))},
                      FUN.VALUE = integer(1))

    # superpopulation individual errors
    eps <- forest_res[indexer]
    wu <- sample(c(-1,1),size = length(eps), replace = TRUE)
    eps <- abs(eps) * wu

    return(eps)}

  e_ij <- sapply(boots_pop,wild_errors,simplify = FALSE)

  boots_pop<-Map(cbind,boots_pop, "e_ij"=e_ij)

  boots_pop <- boots_pop %>%  map(~mutate(., y_star_MSE = y_hat_MSE+e_ij))
  boots_pop <- boots_pop %>%  map(~dplyr::select(., -one_of(c("y_hat_MSE","e_ij"))))

  my_agg <- function(x){getTrueVal(x, target = "y_star_MSE", domain = "idD")[,c("mean","quant10","quant25","median","quant75",
                                                                                "quant90","gini","hcr","pgap","qsr")]}
  tau_star <- sapply(boots_pop,my_agg,simplify = FALSE)

  # THINK ABOUT SEED
  sample_b <- function(x){sample_select(x,smp=surv_data,times=1)}
  boots_sample <- sapply(boots_pop, sample_b)


  # USE BOOTSTRAP SAMPLE TO ESITMATE

  ## WORK HERE
  #___________________-
  my_estim_f <- function(x,y){RE_Forest_ind_smearing_ErrorSD(Y=x$y_star_MSE, X=x[,colnames(x_dat)], random = "(1|idD)",
                                                             surv_data=x, cens_data = y, m_try = m_try, calcADJ =FALSE)[[6]]}

  tau_b <- mapply(my_estim_f,boots_sample,boots_pop, SIMPLIFY = FALSE)


  mean_square <- function(x,y){(x-y)^2}
  bias <- function(x,y){(x-y)}
  mean_square_scale <- function(x,y){scale(x-y, scale=FALSE)^2}

  Mean_square_B <- mapply(mean_square, tau_b,tau_star, SIMPLIFY = FALSE)
  Mean_Bias <- mapply(bias, tau_b,tau_star, SIMPLIFY = FALSE)
  Mean_square_B_scale <- mapply(mean_square_scale, tau_b,tau_star, SIMPLIFY = FALSE)

  MSE_estimates <- Reduce('+',Mean_square_B)/length(Mean_square_B)
  MSE_estimates <- as.data.frame(MSE_estimates)

  MSE_estimates_scaled <- Reduce('+',Mean_square_B_scale)/length(Mean_square_B_scale)
  MSE_estimates_scaled <- as.data.frame(MSE_estimates_scaled)

  MSE_Bias <- Reduce('+',Mean_Bias)/length(Mean_Bias)
  MSE_Bias2 <- as.data.frame(MSE_Bias^2)


  MSE_estimates_debias <- MSE_estimates -(MSE_Bias2-MSE_Bias2[,1])


  #___________________________
  out_list <- vector(length = 5, mode = "list")

  out_list[[1]] <- MSE_estimates
  out_list[[2]] <- MSE_estimates_debias[,c(1:6)]
  out_list[[3]] <- Mean_square_B_scale
  out_list[[4]] <- MSE_Bias2
  out_list[[5]] <- MSE_Bias


  return(out_list)

}
