GLL <- function(Target, data, rf, lmefit){
  res <- Target - predict(lmefit)-rf$predictions
  id <- data$idD
  v <- ranef(lmefit)
  Z <- model.matrix(formula(lmefit$modelStruct$reStr)[[1]],
                    data=lmefit$data)
  B <- getVarCov(lmefit)
  sigma <-sigma(lmefit)^2
  Vraisem <- 0


  for (i in 1:length(unique(id))){
    w <- which(id==unique(id)[i])
    V <- Z[w,,drop=FALSE]%*%B%*%t(Z[w,,drop=FALSE])+diag(as.numeric(sigma),length(w),length(w))
    Vraisem <- Vraisem + log(det(V))+ t(res[w])%*%solve(V)%*%(res[w])
    if (Vraisem == Inf | Vraisem == -Inf) {
      Vraisem <- 0
    }
  }
  return(as.integer(Vraisem))
}

