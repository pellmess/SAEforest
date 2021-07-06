calc_indicat <- function(Y, threshold){

  hcr_function <- function(y,threshold){
    mean(y < threshold, na.rm = TRUE)
  }
  qsr_function <- function(y){
    sum(y[(y > quantile(y,0.8, na.rm = TRUE))]) / sum(y[(y < quantile(y,0.2, na.rm = TRUE))])
  }
  qsr2_function <- function(y){
    sum(y[(y > quantile(y,0.6, na.rm = TRUE))]) / sum(y[(y < quantile(y,0.4, na.rm = TRUE))])
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

  colnames(indicators) <- c("mean","quant10","quant25","median","quant75",
                          "quant90","gini","hcr","pgap","qsr")

  return(indicators)
}


