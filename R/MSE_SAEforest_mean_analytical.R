MSE_MERFanalytical <- function(mod, survey_data, X, dName, err_sd, B=25,
                               initialRandomEffects, ErrorTolerance, MaxIterations, ...){

  # JUST FOR IN-sample observations

  in_dom <- survey_data[[dName]]
  rand_struc <- paste0(paste0("(1|",dName),")")
  ran_sd <- mod$RanEffSD

  n_i <- as.numeric(table(in_dom))
  n_all <- length(t(in_dom))
  m_all <- length(t(unique(in_dom)))

  gam_i <- ran_sd^2 /(ran_sd^2+(err_sd^2/n_i))

  g_1 <- (1-gam_i)*ran_sd^2

  h_n <- err_sd^4*(2/(n_all^2)*( (n_all*ran_sd^2+err_sd^2)^2/(m_all-1) + err_sd^4/(
    m_all*(n_all-1)))) + ran_sd^4*(2*err_sd^2 / m_all*(n_all-1) ) - 2* ran_sd^2 *
    err_sd^2 * (-(2*err_sd^4)/(m_all*n_all*(n_all-1)))

  g_3 <- n_i^(-2)*(ran_sd^2+(err_sd^2/n_i))^(-3) * h_n


  # BOOTSTRAP FOR g_2
  pred_val <- matrix(predict(mod$Forest, survey_data)$predictions, ncol = B,
                     nrow = length(mod$Forest$predictions), byrow = FALSE)

  y_hat <- pred_val + rnorm(sd=err_sd, n = length(pred_val))
  u_i <- apply(y_hat, 2, function(x){rep(rnorm(sd=ran_sd, n = m_all), n_i)})
  y_star <- y_hat + u_i

  my_estim_f <- function(x){MERFranger(Y=x, X = X, random = rand_struc,
                                       data=survey_data, initialRandomEffects = initialRandomEffects,
                                       ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, ...)}

  est_mods <- apply(y_star, 2, my_estim_f)

  new_preds <- function(x){predict(x$Forest, survey_data)$predictions}
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

  MSE_analytical <- data.frame(unique(survey_data[dName]), MSE=MSE_analytical)
  rownames(MSE_analytical) <- NULL

  return(MSE_analytical)
}






