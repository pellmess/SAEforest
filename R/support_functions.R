
calc_indicatR <- function(Y, threshold, custom){

  hcr_function <- function(y,threshold){
    mean(y < threshold, na.rm = TRUE)
  }
  qsr_function <- function(y){
    sum(y[(y > quantile(y,0.8, na.rm = TRUE))]) / sum(y[(y < quantile(y,0.2, na.rm = TRUE))])
  }
  pgap_function <- function(y,threshold) {
    mean((y < threshold)*(threshold - y) / threshold, na.rm = TRUE)
  }

  quant_preds <- quantile(Y, prob=c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  mean_est <- mean(Y, na.rm = TRUE)
  Gini_est <- ineq::Gini(Y, na.rm = TRUE)
  Hcr_est <- hcr_function(y = Y, threshold = threshold)
  Qsr_est <- qsr_function(y = Y)
  Pgap_est <- pgap_function(y = Y, threshold = threshold)

  indicators <- cbind(mean_est, t(quant_preds), Gini_est, Hcr_est, Pgap_est, Qsr_est)

  colnames(indicators) <- c("Mean","Quant10","Quant25","Median","Quant75",
                          "Quant90","Gini","Hcr","Pgap","Qsr")

  if(!is.null(custom)){
    custom_ind <- unlist(lapply(custom, function(f) f(Y, threshold)))
    indicators <- cbind(indicators,t(custom_ind))
  }

  return(indicators)

}

#expand_gridALT <- function(s1, s2) {
#  cbind(rep.int(s1, length(s2)),
#        c(t(matrix(rep.int(s2, length(s1)), nrow=length(s2)))))
#}


sae_specs <- function(dName,cns,smp){
  in_dom<- unique(smp[[dName]])
  total_dom <- unique(cns[[dName]])
  OOsamp <- !total_dom %in% in_dom

  return(list(
    n_surv = length(smp[[dName]]),
    n_pop = length(cns[[dName]]),
    n_out = sum(OOsamp),
    n_in = length(in_dom),
    n_total = length(total_dom),
    n_smp = table(smp[[dName]]),
    n_cns = table(cns[[dName]])))
}

#sort_input <- function(dName, smp_data, pop_data, Y, X){

#  smp_data[[dName]] <- as.character(smp_data[[dName]])

#  smp_sort <- order(smp_data[[dName]])
#  smp_data <- smp_data[smp_sort,]

#  Y <- Y[smp_sort]
#  X <- X[smp_sort,]

#  pop_data[[dName]] <- as.character(pop_data[[dName]])
#  pop_data <- pop_data[order(pop_data[[dName]]),]

#  return(list(smp_data, pop_data, X, Y))
#}

