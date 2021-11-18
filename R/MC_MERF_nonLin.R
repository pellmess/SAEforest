#MC VERSION FOR POINT ESTIMATES - MSE MUST BE ADDED. (WILD no problem - but check
# wheter coniditional or block-sampling is better in bevor deciding.)

# ALSO WE USE ErrorSD -scaled Residuals vs OOB comparable to the smearing approach (!)
# Before going deeper into methods generally keep this in Mind!

MC_MERF_nonLin <- function(Y, X, dName, smp_data, pop_data,
                           threshold = NULL, importance ="none",
                           initialRandomEffects = 0, ErrorTolerance = 0.0001, MaxIterations = 25,
                           custom_indicator =NULL, na.rm = TRUE, B=100,...){

  # CHECK NON_FULL SMEAR ARGUMENT for adjustments!

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
  random = paste0(paste0("(1|",dName),")")

  popSize_N <- data.frame(table(pop_data[[dName]]))
  popSize_n <- data.frame(table(smp_data[[dName]]))
  colnames(popSize_N) <- c(dName, "N_i")
  colnames(popSize_n) <- c(dName, "n_i")
  popSize <- dplyr::left_join(popSize_N, popSize_n, by = dName)
  popSize[,3][is.na(popSize[,3])] <-0

  if(is.null(threshold)){
    thresh = 0.6*median(Y, na.rm=TRUE)
  }
  if(is.function(threshold)){
    thresh = threshold(Y)
  }
  if(is.numeric(threshold)){
    thresh = threshold
  }

  unit_model <- MERFranger(Y = Y,
                           X = X,
                           random = random,
                           data = smp_data,
                           initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance,
                           MaxIterations = MaxIterations,importance = importance, ...)

unit_preds <- predict(unit_model$Forest, pop_data)$predictions +
  predict(unit_model$EffectModel, pop_data, allow.new.levels=TRUE)

# DATA PREP ________________________________________________
forest_res1 <- Y - predict(unit_model$Forest, smp_data)$predictions
smp_data$forest_res <- forest_res1

# Random Effects
formRF <- formula(paste("forest_res ~", paste0(dName)))
ran_effs1 <- aggregate(data=smp_data, formRF, FUN=mean)
colnames(ran_effs1) <- c(dName,"r_bar")

smp_data <- dplyr::left_join(smp_data,ran_effs1,by = dName)
smp_data$forest_eij <- smp_data$forest_res-smp_data$r_bar

# prepare for sampling
forest_res <- smp_data$forest_eij
forest_res<-(forest_res/sd(forest_res))*unit_model$ErrorSD

# CENTER
forest_res <- forest_res-mean(forest_res)

# prepare for sampling
ran_effs <- ran_effs1$r_bar
ran_effs <- (ran_effs/sd(ran_effs))*unit_model$RanEffSD

# CENTER
ran_effs <- ran_effs-mean(ran_effs)
# DATA PREP ________________________________________________

pred_val <- matrix(unit_preds, ncol = B,
                   nrow = length(unit_preds), byrow = FALSE)

y_hat <- pred_val + sample(forest_res, size = length(pred_val), replace = TRUE)

# EBP-special term:
gamm_i <- (unit_model$RanEffSD^2)/(unit_model$RanEffSD^2+(unit_model$ErrorSD^2/popSize$n_i))

v_i <- apply(y_hat, 2,  function(x){rep(sample(ran_effs, size= length(popSize$N_i), replace = TRUE), popSize$N_i)})
gamm_1 <- rep((1-gamm_i), popSize$N_i)
y_star <- y_hat + gamm_1 * v_i

indi_agg <- rep(1:length(popSize$N_i), popSize$N_i)
my_agg <-  function(x){tapply(x, indi_agg, calc_indicat, threshold = thresh, custom = custom_indicator)}
tau_star <- apply(y_star, my_agg, MARGIN = 2, simplify = FALSE)
col_names <- colnames(tau_star[[1]]$`1`)

comb <- function(x){matrix(unlist(x), nrow = length(popSize$N_i), byrow = TRUE)}
tau_star <- sapply(tau_star, comb, simplify = FALSE)

indicators <- Reduce('+',tau_star)/length(tau_star)
colnames(indicators) <- col_names

indicators <-

  # __________________________________

  result <- list(
    Indicators = cbind(popSize[dName], indicators),
    model = c(unit_model, call = out_call, data=list(smp_data)))

class(result) <- c("MC_MERF_nonLin")
return(result)
}

