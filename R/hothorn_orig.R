# HOTHORN PACKAGE STUFF
# BUT WORKS NOW - TEST HERE...ready for implementation!

# TRANSLATE FROM C to C++ such that it can be used via Rcpp
# rn <- ranger(dist ~ speed, data= cars, keep.inbag = TRUE)
# a<-predict(rn,type="terminalNodes",cars)$predictions

# f_data_test <- lapply(seq_len(ncol(a)), function(i) a[,i])
# f_data_test <-lapply(f_data_test, as.integer)
# rw_test <- lapply(rn$inbag.counts, as.integer)
# scale <- TRUE

# w <- .rfweights(f_data_test, f_data_test, rw_test, scale)

# t(t(w) %*% cars$dist)/500
# predict(rn, cars)$predictions

# OOB predictions
# fnewdata <- list()
# w_oob <- .rfweights(f_data_test, fnewdata, rw_test, scale)

# t(t(w_oob) %*% cars$dist)/colSums(w_oob)
# rn$predictions
