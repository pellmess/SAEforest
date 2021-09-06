# THIS IS THE MSE FUNCTION FRO THE SAEforest_agg
# IT BUILD STRONGLY on the CODE from the sae package (!)
# Recoding might be needed (!)

#' MSE function for MERF with aggregated data
#'
#' @param Y metric target variable
#' @param X data.frame of covariates
#' @param dName name of group-identifier
#' @param survey_data survey data set
#' @param mod explicit model from SAE_forest_agg
#' @param ADJsd input for adjusted variance
#' @param Xcensus_agg aggregated census level covariates. Note that the
#' first column should include the group identifier
#' @param B number of bootstrap replications
#' @param popnsize known number of population size in specific areas
#' @param initialRandomEffects default set to 0
#' @param ErrorTolerance default set to 1e-04
#' @param MaxIterations default to 25
#' @param m_try default set to 1
#' @param survey_weigths default set to NULL
#' @param seed default set 1234
#'
#' @return returns a data.set with group-identifier and MSE results
#' @export
#'
#' @examples

MSE_SAEforest_aggOOB <- function (Y, X, dName, survey_data, mod, ADJsd, Xcensus_agg, B=100,
                               popnsize, initialRandomEffects = 0, ErrorTolerance = 0.0001,
                               MaxIterations = 25, m_try = 1, survey_weigths = NULL, seed=1234){

  dom <- survey_data[dName]
  in_dom <- t(unique(survey_data[dName]))
  total_dom <- t(unique(Xcensus_agg[dName]))
  p <- dim(X)[2]

  sigmae2est <- mod$MERFmodel$ErrorSD^2
  sigmau2est <- mod$MERFmodel$RanEffSD^2


  # NOPARA Errors ____________________________________________
  forest_res1 <- Y - mod$MERFmodel$Forest$predictions

  survey_data$forest_res <- forest_res1

  # Random Effects
  formRF <- formula(paste("forest_res ~", paste0(dName)))
  ran_effs1 <- aggregate(data=survey_data, formRF, FUN=mean)
  colnames(ran_effs1) <- c("idD","r_bar")

  survey_data <- merge(survey_data,ran_effs1,by="idD")
  survey_data$forest_eij <- survey_data$forest_res-survey_data$r_bar

  # prepare for sampling
  forest_res <- survey_data$forest_eij
  forest_res<-(forest_res/sd(forest_res))*ADJsd

  # CENTER
  forest_res <- forest_res-mean(forest_res)

  # prepare for sampling
  ran_effs <- ran_effs1$r_bar
  ran_effs <- (ran_effs/sd(ran_effs))*sqrt(sigmau2est)

  # CENTER
  ran_effs <- ran_effs-mean(ran_effs)

  #________________________________________________________________

  I <- length(total_dom)
  n <- length(Y)
  popnsize <- as.matrix(popnsize)

  D <- length(in_dom)
  nd <- rep(0, D)
  SampSizeselectdom <- rep(0, I)
  musd.B <- mud.B <- list()

  for (d in 1:D) {
    rowsd <- (dom == in_dom[d])
    musd.B[[d]] <- mod$MERFmodel$Forest$predictions[rowsd]
    nd[d] <- sum(rowsd)
  }

  for (i in 1:I) {
    mud.B[[i]] <- mod$Mean_Predictions$Mean[i] - unlist(ranef(mod$MERFmodel$EffectModel))[i]
    if (is.na(mud.B[[i]])){
      mud.B[[i]] <- mod$Mean_Predictions$Mean[i]
    }
    d <- total_dom[i]
    posd <- which(in_dom == d)
    if (length(posd) > 0)
      SampSizeselectdom[i] <- nd[posd]
  }

  Ni <- popnsize[,2]
  rd <- Ni - SampSizeselectdom

  MSE.B <- BLOCK1MSE.B<-BLOCK2MSE.B <- truemean.B_w0 <- truemean.B <-BLOCK1truemean.B<- rep(0, I)
  MSE.B_oob <- BLOCK1MSE.B_oob <-BLOCK2MSE.B_oob <-BLOCK2truemean.B<- rep(0, I)


  b <- 1
  while (b <= B) {
    print(b)
    set.seed(seed + b)
    ys.B<-BLOCKys.B <- rep(0, n)
    ys.B_oob<-BLOCKys.B_oob <- rep(0, n)

    ud.B <- rep(0, D)
    esdmean.B <-BLOCKesdmean.B<- rep(0, D)
    for (d in 1:D) {

      esd.B <- sample(forest_res,
                      size = nd[d], replace=TRUE)

      ud.B[d] <- sample(ran_effs, size=1, replace = TRUE )

      rowsd <- (dom == in_dom[d])
      ys.B[rowsd] <- musd.B[[d]] + ud.B[d] + esd.B
      esdmean.B[d] <- mean(esd.B)
    }
    for (i in 1:I) {
      erdmean.B <- sample((forest_res/ADJsd)*sqrt(ADJsd^2/rd[i]), size=1, replace = TRUE )
      posd <- which(in_dom == total_dom[i])

      if (length(posd) != 0) {
        edmean.B <- esdmean.B[posd] * nd[posd]/Ni[i] +
          erdmean.B * rd[i]/Ni[i]

        truemean.B[i] <- mud.B[[i]] + edmean.B + ud.B[posd]
      }
      else truemean.B[i] <- mud.B[[i]] + sample(ran_effs, size=1, replace = TRUE ) +
        erdmean.B
    }

    mse_dat <- data.frame(survey_data[dName], X , y=ys.B)

    mod_2 <- SAEforest_agg(Y = mse_dat$y, X=X, dName = dName, survey_data = mse_dat,
                           Xcensus_agg = Xcensus_agg, initialRandomEffects = initialRandomEffects,
                           ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
                           m_try = m_try, survey_weigths = survey_weigths, OOsample_obs = mod$OOsample_obs)

    if(round(sum(mod_2$ModifiedSet$weights)) != length(total_dom)){
      print("Consider choosing a higher Level of OOsample_obs")
    }


    mse_estims <- mod_2$Mean_Predictions$Mean

    MSE.B <- MSE.B + (mse_estims - truemean.B)^2
    b <- b + 1

#    if (adBLOCK== TRUE){
#      mse_dat$y <- BLOCKys.B

#      mod_3 <- SAEforest_agg(Y = mse_dat$y, X=X, dName = dName, survey_data = mse_dat,
#                             Xcensus_agg = Xcensus_agg, initialRandomEffects = initialRandomEffects,
#                             ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations,
#                             m_try = m_try, survey_weigths = survey_weigths)

#      mse_estimsBLOCK <- mod_3$Mean_Predictions
#      BLOCK1MSE.B<- BLOCK1MSE.B +(mse_estimsBLOCK - BLOCK1truemean.B)^2
#    }

  }


  MSEEB.B <- MSE.B/B
 # BLOCK1MSE.B <- BLOCK1MSE.B/B

  return(data.frame(Xcensus_agg[dName],MSE=MSEEB.B))
  }
