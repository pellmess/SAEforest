#' Experimental function for nonlinear indicators of Standard RF with unit-level data
#'
#' Some detailed description and scientific references here
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param smp_data sample data set
#' @param pop_data aggregated census level covariates
#' @param threshold Set a custom threshold for indicators. Threshold can be a value or function of \code{Y}
#' Default set to NULL
#' @param importance variable importance mode processed by the
#' random forest from the \pkg{ranger}. Must be 'none', 'impurity', 'impurity_corrected',
#' 'permutation'
#' @param custom_indicator a list of functions containing the indicators to be
#' calculated additionally. Such functions must and must only depend on the
#' target variable \code{Y} and the \code{threshold}. Defaults to \code{NULL}
#' @param ... variables such as mtry, num.tree essentially anything that can be passed to \pkg{ranger}
#'
#' @return returns object including Mean predictions and model details
#' @export
#' @details Some scientific or function specific details
#' @seealso \code{\link{SAEforest}}
#'
#'
RangerForest_nonLin <- function(Y, X, dName, smp_data, pop_data,
                             threshold = NULL, importance ="none",
                             custom_indicator =NULL, na.rm = TRUE, ...){

  # ERROR CHECKS OF INPUTS
  #________________________________________

  if(na.rm == TRUE){
    comp_smp <- complete.cases(smp_data)
    smp_data <- smp_data[comp_smp,]
    Y <- Y[comp_smp]
    X <- X[comp_smp,]
    pop_data <- pop_data[complete.cases(pop_data),]
  }

  out_call <- match.call()

  # Make domain variable to character and sort data-sets
  smp_data[[dName]] <- factor(smp_data[[dName]], levels=unique(smp_data[[dName]]))
  pop_data[[dName]] <- factor(pop_data[[dName]], levels=unique(pop_data[[dName]]))

  # Point Estimation
  #________________________________________
  domains = names(table(pop_data[[dName]]))
  popSize <- as.numeric(table(pop_data[[dName]]))


  if(is.null(threshold)){
    thresh = 0.6*median(Y, na.rm=TRUE)
  }
  if(is.function(threshold)){
    thresh = threshold(Y)
  }
  if(is.numeric(threshold)){
    thresh = threshold
  }

  unit_model <- ranger(y = Y, x = X, data = smp_data, importance = importance, ...)

  unit_preds <- predict(unit_model, pop_data)$predictions
  OOBresiduals <- Y - unit_model$predictions

  tic()
  # SMEARING STEP HERE------------
  smear_list <- vector(mode="list", length = length(domains))

  for (i in seq_along(domains)){
    smear_i <- matrix(rep(OOBresiduals,popSize[i]), nrow=popSize[i],ncol=length(OOBresiduals),byrow=TRUE)
    smear_i <- smear_i + unit_preds[pop_data[[dName]] == domains[i]]

    smear_list[[i]] <-  calc_indicat(c(smear_i), threshold = thresh, custom = custom_indicator)
  }
toc()

  indicators <- do.call(rbind.data.frame, smear_list)
  indicators_out <- cbind(domains, indicators)
  names(indicators_out)[1] <- dName

  # __________________________________

    result <- list(
      Indicators = indicators_out,
      model = c(unit_model, call = out_call, data=list(smp_data)))

    class(result) <- c("RangerForest_nonLin")
    return(result)

}
