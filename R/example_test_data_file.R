
# SMALL FILE TO TEST AND LOAD DATA FOR IMPROVEMENTS
#

# my PC:
#load("C:/Users/Patrick/Documents/Studium/FU Berlin/Diss/Data/Simulation_Data_MERFPaper/AggForest_Data/s1_normal_250_aggForest.RData")
#load("C:/Users/pkrennmair/Documents/MERF_aggSim_2408/s7_interaction_500_aggForestV2.RData")

#cens <- Pop[[17]][,c(1,4,5,8)]
#surv <- samp[[17]][,c(1,4,5,8)]

#Y <- surv$y
#X <- surv[,c(2:3)]

#survey_data <- surv
#census_data <- cens

#dName <- "idD"
#initialRandomEffects = 0
#ErrorTolerance = 0.0001
#MaxIterations = 25
#m_try = 1
#survey_weigths = NULL

#AGG
#x_bar1 <- as.numeric(tapply(cens$x1, INDEX = cens$idD, FUN = mean))
#x_bar2 <- as.numeric(tapply(cens$x2, INDEX = cens$idD, FUN = mean))

#Xcensus_agg <- cbind(unique(surv[dName]), x_bar1, x_bar2)
#popnsize <- data.frame(idD= 1:50, PopnSegments=1000)


# APPLICATION OF FUNCTION
#tic()
#mod <- SAEforest_nonLin(Y=Y, X=X, dName = "idD", survey_data =surv, census_data=cens)
#toc()


#tic()
#set.seed(1234)
#mod <- SAEforest_agg(Y=Y, X=X, dName = "idD", survey_data =surv, Xcensus_agg=Xcensus_agg, OOsample_obs = 20)
#mod2 <- SAEforest_agg_SIMPLE(Y=Y, X=X, dName = "idD", survey_data =surv, Xcensus_agg=Xcensus_agg)
#toc()

#tic()
#aggMSE <- MSE_SAEforest_agg_SIMPLE(Y=Y, X=X, dName = "idD", survey_data=surv, mod=mod2, ADJsd=1500,
#                Xcensus_agg = Xcensus_agg, B=25, popnsize = popnsize)
#toc()

#true <- aggregate(y~idD, data= surv, FUN=mean)

#boxplot(sqrt(cbind((true$y-mod2$Mean_Predictions$Mean)^2, aggMSE$MSE,(true$y-mod$Mean_Predictions$Mean)^2)))
#summary(sqrt(cbind((true$y-mod2$Mean_Predictions$Mean)^2, aggMSE$MSE, (true$y-mod$Mean_Predictions$Mean)^2)))

#summary(sqrt(cbind(aggMSE$MSE,aggMSE2$MSE, (true$y-mod$Mean_Predictions$Mean)^2)))
