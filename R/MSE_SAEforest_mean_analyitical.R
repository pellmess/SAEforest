MSE_MERFanalytical <- function(mod, survey_data, X, dName, err_sd, B=25){

  mod <- mod$MERFmodel
  in_dom <- survey_data[dName]

  rand_struc = paste0(paste0("(1|",dName),")")
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
  boots_pop <- vector(mode="list",length = B)
  boots_pop <- sapply(boots_pop,function(x){survey_data},simplify =FALSE)


  pred_val <- predict(mod$Forest,boots_pop[[1]])$predictions

  pred_t <-sapply(boots_pop,function(x){pred_val},simplify = FALSE)

  # Errors
  err_sample <- function(x){
    return(rnorm(sd=err_sd, n = n_all))
  }

  e_ij <- sapply(boots_pop,err_sample,simplify = FALSE)

  # combine
  y_star <- mapply("+", pred_t,e_ij, SIMPLIFY = FALSE)

  boots_pop<-Map(cbind,boots_pop,"y_star"=y_star, "predz"=pred_t)

  u_i <- replicate(length(boots_pop),data.frame(u_i_star=rnorm(sd=ran_sd, n = m_all),
                                                unique(survey_data[dName])),simplify = FALSE)

  boots_pop <- map2(boots_pop, u_i, left_join, by = dName ,simplify=FALSE)

  boots_pop <- boots_pop %>%  map(~mutate(., y_star_MSE = y_star+u_i_star))
  boots_pop <- boots_pop %>%  map(~dplyr::select(., -one_of(c("u_i_star","y_star","predz"))))


  my_estim_f <- function(x){MERFranger(Y=x$y_star_MSE, X = X, random = rand_struc,
                                       data=x, ErrorTolerance = mod$ErrorTolerance, m_try = mod$Forest$mtry)}

  est_mods <- sapply(boots_pop,my_estim_f,simplify = FALSE)

  new_preds <- function(x){predict(x$Forest,boots_pop[[1]])$predictions}
  f_b <- sapply(est_mods,new_preds,simplify = FALSE)

  f_diffs <- mapply("-", pred_t,f_b, SIMPLIFY = FALSE)
  boots_pop<-Map(cbind,boots_pop,"f_diffs"=f_diffs)

  formRF <- formula(paste("f_diffs ~", paste0(dName)))
  diff_agg <- function(x){aggregate(formRF, data=x, FUN = mean)[,-1]}

  agg_diffs <- sapply(boots_pop,diff_agg,simplify = FALSE)

  B_gams <- function(x){1-((x$RanEffSD^2)/(x$RanEffSD^2 + x$ErrorSD^2/n_i))}
  One_minus_gam_b <- sapply(est_mods,B_gams,simplify = FALSE)


  D_i_hat <- mapply("*", agg_diffs,One_minus_gam_b, SIMPLIFY = FALSE)
  D_i_hat_2 <- sapply(D_i_hat,function(x){x^2},simplify = FALSE)

  g_2 <- Reduce('+',D_i_hat_2)/B

  #______________________________________
  MSE_analytical <- g_1 + g_2 + 2*g_3

  return(MSE_analytical)
}






