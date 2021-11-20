adjust_ErrorSD_C <- function(Y, X, smp_data, mod, B=100, ...){

  pred_OOB <- matrix(mod$Forest$predictions, ncol = B, nrow = length(mod$Forest$predictions), byrow = FALSE)

  termNodes <- predict(mod$Forest, type="terminalNodes", smp_data)$predictions

  f_data <- lapply(seq_len(ncol(termNodes)), function(i) termNodes[,i])
  f_data <-lapply(f_data, as.integer)
  fnewdata <- list()
  inbag <- lapply(mod$Forest$inbag.counts, as.integer)
  scale <- TRUE

  w_oob <- .rfweights(f_data, fnewdata, inbag, scale)

  e_ij <- Y - predict(mod$Forest, smp_data)$predictions
  e_ij <- e_ij- mean(e_ij)

  y_star_OOB <- pred_OOB + sample(e_ij, size = length(pred_OOB), replace = TRUE)


  pred_OOB_star <-(t(w_oob)  %*% y_star_OOB)

  pred_OOB_star <- pred_OOB_star / matrix(colSums(w_oob), nrow=dim(pred_OOB_star)[1],
                                          ncol=dim(pred_OOB_star)[2], byrow=FALSE)

  Adjustment <- (pred_OOB_star-pred_OOB)^2

  outvar <- sqrt(mod$ErrorSD^2 - mean(rowMeans(Adjustment)))

  return(outvar)
}

