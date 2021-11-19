#' @useDynLib SAEforest R_rfweights
.rfweights <- function(fdata, fnewdata, rw, scale)
  w <- .Call(R_rfweights, fdata, fnewdata, rw, scale)
