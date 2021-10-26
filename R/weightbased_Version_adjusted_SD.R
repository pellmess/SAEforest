# Prediction function works, but the matrices and matrix operations are to big
# to bring an efficiency gain. In this case the calculation and prediction with
# standard ranger is preferred.


#Forest_weight_preds <- function(inbagRanger, termNodes, OOB = FALSE){

#  termList <- lapply(seq_len(ncol(termNodes)), function(i) termNodes[,i])
#  countList <- inbagRanger


#  makeMatrix <- function(x){matrix(unlist(x), nrow= length(termList[[1]]),
#                                   ncol=length(termList[[1]]), byrow = TRUE)}

#  find_tree_weight <- function(x){
#    TerNode_unique <- unique(x)

#    out_list <- vector(mode="list", length = length(x))

#    for (i in 1:length(TerNode_unique)){
#      out_list[x == TerNode_unique[i]] <- list(1 *(x == TerNode_unique[i]))}

    # rowise weights
#    return(matrix(unlist(out_list), ncol = length(x), byrow=TRUE))
#  }

#  makeRowSums <- function(x){matrix(rep(rowSums(x),dim(x)[1]), ncol=dim(x)[1])}

#  makeDiag0<-function(x){diag(x) <- NA
#  return(x)}

#  check_fun <- function(x){x[rowSums(x, na.rm = TRUE)!=1,] <- NA
#  return(x)}

#  termMatrix <- lapply(countList, makeMatrix)

#  countMatrix <- lapply(termList, find_tree_weight)

#  multMat <- mapply('*',termMatrix, countMatrix, SIMPLIFY = FALSE)

#  N_row <- lapply(multMat, makeRowSums)

#  weightList <- mapply('/', multMat, N_row, SIMPLIFY = FALSE)

#  if(OOB == FALSE){
#    weightForest <- Reduce('+',weightList)/length(weightList)
#    return(weightForest)
#  }

#  if(OOB == TRUE){
#    weightList <- lapply(weightList, makeDiag0)
#    weightList <- lapply(weightList, check_fun)

#    OOBweightList <- t(apply(array(unlist(weightList), c(dim(weightList[[1]]), length(weightList)),
#                         dimnames=c(dimnames(weightList[[1]]), NULL)), 1, rowMeans, na.rm=TRUE))

#    diag(OOBweightList) <- 0
#    return(OOBweightList)
#  }
#}


#adjust_ErrorSD_alternative <- function(Y, X, smp_data, mod, B=100, ...){

#  pred_OOB <- matrix(mod$Forest$predictions, ncol = B, nrow = length(mod$Forest$predictions), byrow = FALSE)

#  e_ij <- Y - predict(mod$Forest, smp_data)$predictions
#  e_ij <- e_ij- mean(e_ij)

#  y_star_OOB <- pred_OOB + sample(e_ij, size = length(pred_OOB), replace = TRUE)

#  termNodes <- predict(mod$Forest, type = 'terminalNodes', data=smp_data)$predictions
#  inbagRanger <- mod$Forest$inbag.counts

#  pred_weights <- Forest_weight_preds(inbagRanger=inbagRanger, termNodes=termNodes, OOB = TRUE)

#  pred_OOB_star <- pred_weights %*% y_star_OOB

#  Adjustment <- (pred_OOB_star-pred_OOB)^2

#  outvar <- sqrt(mod$ErrorSD^2 - mean(rowMeans(Adjustment)))

#  return(outvar)
#}


