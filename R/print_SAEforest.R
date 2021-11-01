#' Prints an SAEforestObject
#'
#' Basic information of an SAEforest object is printed.
#' @param x an x of type "SAEforest", representing point and MSE
#' estimates obtained by Mean (see also \code{\link{SAEforest_mean}}),
#' nonLin (see also \code{\link{SAEforest_nonLin}}),
#' or meanAGG (see also \code{\link{SAEforest_meanAGG}}).
#' @seealso \code{\link{SAEforest}}

#' @export
print.SAEforest <- function(obj) {
  class_error(obj)
  cat("________________________________________________________________\n")
  cat("Mixed Effects Random Forest for Small Area Estimation\n")
  cat("________________________________________________________________\n")
  cat("\n")
  cat("Information on Domains\n")
  total_dom <- obj$MERFmodel$data_specs$n_total
  in_dom <- obj$MERFmodel$data_specs$n_in
  oos_dom <- obj$MERFmodel$data_specs$n_out

  dom_info <- data.frame(in_dom, oos_dom, total_dom)
  rownames(dom_info) <- c("")
  colnames(dom_info) <- c("In-sample", "Out-of-sample", "Total")

  smp_size <- obj$MERFmodel$data_specs$n_surv
  pop_size <- obj$MERFmodel$data_specs$n_pop

  if(inherits(object, "SAEforest_meanAGG")){
    sizedom_smp_pop <- rbind(Sample_domains = smp_size_dom)
  }

  print(dom_info)
  cat("\n")
  cat("Units in sample:", smp_size, "\n")
  if(!is.null(pop_size)){
    cat("Units in population:", pop_size, "\n")
  }
  cat("\n")
  cat("Model Information:\n")
  cat("An SAEforest Object contains results for point- and uncertainty estiamtes as well as seperate\n")
  cat("model components of the random forest part as well as the mixed effects part\n")
  cat("\n")
  cat("Mehtods of lme4 are applicable to the random effects components model accessible through\n")
  cat("'object$MERFmodel$EffectModel'.\n")
  cat("Mehtods for random forests from ranger are applicable to the fixed effects components model accessible\n")
  cat("through 'object$MERFmodel$Forest'.\n")
}
