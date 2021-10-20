
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
#x1 <- as.numeric(tapply(cens$x1, INDEX = cens$idD, FUN = mean))
#x2 <- as.numeric(tapply(cens$x2, INDEX = cens$idD, FUN = mean))

#Xcensus_agg <- cbind(unique(surv[dName]), x1, x2)
#popnsize <- data.frame(idD= 1:50, PopnSegments=1000)

#unit_model <- MERFranger(Y = Y,
#                         X = X,
#                         random = random,
#                         data = survey_data,
#                         initialRandomEffects = initialRandomEffects,
#                         ErrorTolerance = ErrorTolerance,
#                         MaxIterations = MaxIterations, min.node.size=15)


# APPLICATION OF FUNCTION
#library(emdi)
#data("eusilcA_pop")
#data("eusilcA_smp")

#tic()
#mod_alt <- SAEforest_mean(Y=eusilcA_smp$eqIncome, X=eusilcA_smp[,-c(1,18)], dName = "district", survey_data =eusilcA_smp, census_data=eusilcA_pop,
#                          mse ="none", B=0, importance = "impurity")
#toc()

#test <- MSE_MERFanalytical(mod=mod, survey_data = surv, X = X, dName = "idD", err_sd=1000, B=2)
#test2 <-MSE_SAEforest_nonLin_wild(Y=Y, X=X, dName = "idD", survey_data =surv, cens_data=cens, B=5,
#                               ADJsd = 500, mod=mod, threshold = NULL)

#tic()
#set.seed(1234)
#mod <- point_meanAGG(Y=Y, X=X, dName = "idD", survey_data =surv, Xcensus_agg=Xcensus_agg, w_min = 2,
#                     initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
#                     MaxIterations = MaxIterations, importance = "impurity", ADDsamp_obs = 0 )#
#mod2 <- SAEforest_meanAGG(Y=Y, X=X, dName = "idD", survey_data =surv, Xcensus_agg=Xcensus_agg, mse="none",
#                          popnsize = popnsize, B=2, mtry=2, importance = "impurity")
#toc()

#tic()
#aggMSE <- MSE_SAEforest_agg_SIMPLE(Y=Y, X=X, dName = "idD", survey_data=surv, mod=mod2, ADJsd=1500,
#                Xcensus_agg = Xcensus_agg, B=25, popnsize = popnsize)
#toc()

#true <- aggregate(y~idD, data= surv, FUN=mean)

#boxplot(sqrt(cbind((true$y-mod2$Mean_Predictions$Mean)^2, aggMSE$MSE,(true$y-mod$Mean_Predictions$Mean)^2)))
#summary(sqrt(cbind((true$y-mod2$Mean_Predictions$Mean)^2, aggMSE$MSE, (true$y-mod$Mean_Predictions$Mean)^2)))

#summary(sqrt(cbind(aggMSE$MSE,aggMSE2$MSE, (true$y-mod$Mean_Predictions$Mean)^2)))
