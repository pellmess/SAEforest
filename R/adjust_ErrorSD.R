#' Nonparametric Adjustment of Residual Variance
#'
#' This is a bootstrap adjustment of the residual variance
#' Used only internally as preparation for bootstrap-MSEs
#'
#' @param Y metric input target variable
#' @param X variable of predictive covariates
#' @param mod model trained by MERFranger
#' @param B number of bootstrap replications. default to B= 100
#' @param survey_weights possibility to include weights. default is NULL
#'
#' @return returns an numeric integer of the corrected SD
#' @export
#'
#' @examples
adjust_ErrorSD <- function(Y, X, mod, B=100, survey_weights =NULL){

  surv_data <- mod$data
  m_try <- mod$Forest$mtry
  n_tree <- mod$Forest$num.trees

  OOB_samp <- vector(mode="list",length = B)

  OOB_samp <- sapply(OOB_samp,function(x){surv_data},simplify =FALSE)

  my_pred_f <- function(x){mod$Forest$predictions}

  pred_OOB <- sapply(OOB_samp,my_pred_f,simplify = FALSE)


  e_ij <- Y - predict(mod$Forest, surv_data)$predictions
  e_ij <- e_ij- mean(e_ij)
  e_ij_oob <- replicate(length(OOB_samp),sample(e_ij,length(e_ij),replace = TRUE),simplify = FALSE)

  y_star_OOB <- mapply("+", pred_OOB, e_ij_oob, SIMPLIFY = FALSE)
  OOB_samp <-Map(cbind,OOB_samp,"y_star_OOB"=y_star_OOB)


  my_estim_f2 <- function(x){ranger::ranger(y=x$y_star_OOB, x=x[,colnames(X)], data=x, mtry = m_try, num.trees = n_tree,
                                    case.weights=survey_weights)}
  my_f_n2 <- sapply(OOB_samp, my_estim_f2,simplify = FALSE)

  my_pred_f <- function(x){x$predictions}
  pred_OOB_star <- sapply(my_f_n2,my_pred_f,simplify = FALSE)


  mean_square <- function(x,y){(x-y)^2}

  Adjustment <- mapply(mean_square, pred_OOB_star,pred_OOB, SIMPLIFY = FALSE)

  Adjustment <- Reduce('+',Adjustment)/length(Adjustment)

  outvar <- sqrt(mod$ErrorSD^2 - mean(Adjustment))

  return(outvar)
}
