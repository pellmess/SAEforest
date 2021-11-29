#data("eusilcA_pop")
#data("eusilcA_smp")

#income <- eusilcA_smp$eqIncome
#X_covar <- eusilcA_smp[,-c(1,16,17,18)]

#Example 1:
#Calculating point-estimates and discussing basic generic functions

#model1 <- SAEforest_nonLin(Y = income, X = X_covar, dName = "district", smp_data = eusilcA_smp,
#                           pop_data = eusilcA_pop, smearing = FALSE, custom_indicator = list(my_max = function(Y, threshold){max(Y)}))

#example of SAEforest generic
#summary(model1)

#tic()
#model1 <- SAEforest_nonLin(Y = income, X = X_covar, dName = "district", smp_data = eusilcA_smp,
#                           pop_data = eusilcA_pop, smearing = FALSE, B_MC = 200, mse = "nonparametric", B=20,custom_indicator = list(my_max = function(Y, threshold){max(Y)}))
#toc()

#tic()
#model2 <- SAEforest_nonLin(Y = income, X = X_covar, dName = "district", smp_data = eusilcA_smp,
#                           pop_data = eusilcA_pop, smearing = TRUE, custom_indicator = list(my_max = function(Y, threshold){max(Y)}))
#toc()

# Speed_compares
#tic()
#  if(is.null(custom_indicator)){
#unit_list <- split(unit_preds, pop_data[dName])

#indicators <- smear_fun(popSize = popSize, unit_preds=unit_list, oob_res =
#                          unit_model$OOBresiduals, threshold = thresh)

#indicators_out <- data.frame(domains, indicators)
#}
#toc()

#ifelse(is.null(custom_indicator),
#       calc_indicat <- calc_indicatC,
#       calc_indicat <- calc_indicatR)

#tic()
# SMEARING STEP HERE------------
#smear_list <- vector(mode="list", length = length(domains))

#for (i in seq_along(domains)){
#  smear_i <- matrix(rep(unit_model$OOBresiduals,popSize[i]), nrow=popSize[i],ncol=length(unit_model$OOBresiduals),byrow=TRUE)
#  smear_i <- smear_i + unit_preds[pop_data[[dName]] == domains[i]]

#  smear_list[[i]] <-  calc_indicat(c(smear_i), threshold = thresh, custom = custom_indicator)
#}

#indicators <- do.call(rbind.data.frame, smear_list)
#indicators_out <- cbind(domains, indicators)
#names(indicators_out)[1] <- dName
# __________________________________
#toc()


#tic()
# SMEARING STEP HERE------------
#smear_list <- vector(mode="list", length = length(domains))

#for (i in seq_along(domains)){
#  smear_i <- matrix(rep(unit_model$OOBresiduals,popSize[i]), nrow=popSize[i],ncol=length(unit_model$OOBresiduals),byrow=TRUE)
#  smear_i <- smear_i + unit_preds[pop_data[[dName]] == domains[i]]

#  smear_list[[i]] <- c(smear_i)
#}

#fun1 <- function(x){calc_indicatC(Y=x, threshold = thresh, custom=NULL)}
#sapply(smear_list, fun1)

# __________________________________
#toc()


#microbenchmark(calc_indicatC(c(smear_i), threshold = thresh), calc_indicatR(c(smear_i), threshold = thresh, custom = NULL))





