#' @useDynLib mypackage R_rfweights
.rfweights <- function(fdata, fnewdata, rw, scale)
  w <- .External(R_rfweights, fdata, fnewdata, rw, scale)
