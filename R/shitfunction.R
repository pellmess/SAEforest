shitfun <- function(Y, X){

  threshold = 0.6*median(Y, na.rm=TRUE)

  cl <- parallel::makeCluster(parallel::detectCores()-1 )

  calc_indicat2 <- calc_indicat
  helpfun <- function(x){calc_indicat2(x, threshold =threshold)}

  parallel::clusterEvalQ(cl, c("threshold"))

  smear_list <- list(a=1,b=2,c=3)

  indicators <- parallel::parLapply(cl,smear_list, fun =helpfun )

  parallel::stopCluster(cl)

#  indicators <- do.call(rbind.data.frame, indicators)

#  indicators_out <- cbind("Domain" = unique(census_data[dName])[,1],indicators)

  return( indicators)


}
