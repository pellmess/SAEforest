
# SMALL FILE TO TEST AND LOAD DATA FOR IMPROVEMENTS
#


# my PC:
#load("C:/Users/Patrick/Documents/Studium/FU Berlin/Diss/Data/Simulation_Data_MERFPaper/AggForest_Data/s1_normal_250_aggForest.RData")
#load("C:/Users/pkrennmair/Documents/MERF_aggSim_2408/s7_interaction_500_aggForestV2.RData")

#cens <- Pop[[17]][,c(1,4,5,8)]
#surv <- samp[[17]][,c(1,4,5,8)]

#Y <- surv$y
#X <- surv[,c(2:3)]

#samp_data <- surv
#pop_data <- cens

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

#cust_ind = list(my_max = function(y, threshold){max(y)},
#     my_min = function(y, threshold){min(y)},
#     my_quant = function(y, threshold){quantile(y, probs=c(0.01,0.99))})


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
#names(eusilcA_popAgg)[1] <- "district"

#RangerForest_nonLin(Y=eusilcA_smp$eqIncome, X=eusilcA_smp[,-c(1,16,17,18)], dName = "district", smp_data =eusilcA_smp, pop_data=eusilcA_pop)

#tic()
#mod_alt_NP <- SAEforest_nonLin(Y=eusilcA_smp$eqIncome, X=eusilcA_smp[,-c(1,16,17,18)], dName = "district", smp_data =eusilcA_smp, pop_data=eusilcA_pop,
#                                                     mse ="none", B=50, importance = "impurity", mtry=7,na.rm=FALSE)
#toc()

#mod_alt2 <- SAEforest_meanAGG(Y=eusilcA_smp$eqIncome, OOsample_obs = 25 ,X=eusilcA_smp[,-c(1,17,18)], dName = "district", smp_data =eusilcA_smp, Xpop_agg=eusilcA_popAgg,
#                            mse ="none", B=50, importance = "impurity")

#emdi_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#                    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#                    house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#                  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#                  na.rm = TRUE, MSE = TRUE, B=50)

#mean_method <- mod_alt$MSE_estimates
#agg_method <- mod_alt_NP$MSE_estimates
#plot_values <- left_join(agg_method, mean_method, by="district")
#plot_values <- merge(emdi_model$MSE[,1:2], plot_values, by.x="Domain", by.y="district")

# Last 24 are Out-of-sample obs
#matplot(sqrt(plot_values[,-1]), type="l")


#test <- MSE_MERFanalytical(mod=mod, survey_data = surv, X = X, dName = "idD", err_sd=1000, B=2)
#test2 <-MSE_SAEforest_nonLin_wild(Y=Y, X=X, dName = "idD", survey_data =surv, cens_data=cens, B=5,
#                               ADJsd = 500, mod=mod, threshold = NULL)


#set.seed(1234)
#mod <- point_meanAGG(Y=Y, X=X, dName = "idD", survey_data =surv, Xcensus_agg=Xcensus_agg, w_min = 2,
#                     initialRandomEffects = initialRandomEffects, ErrorTolerance = ErrorTolerance,
#                     MaxIterations = MaxIterations, importance = "impurity", ADDsamp_obs = 0 )#
#tic()
#mod3 <- SAEforest_nonLin(Y=Y, X=X, dName = "idD", survey_data =surv, census_data=census_data, mse="none", B=5)
#toc()

#tic()
#aggMSE <- MSE_SAEforest_agg_SIMPLE(Y=Y, X=X, dName = "idD", survey_data=surv, mod=mod2, ADJsd=1500,
#                Xcensus_agg = Xcensus_agg, B=25, popnsize = popnsize)
#toc()

#true <- aggregate(y~idD, data= surv, FUN=mean)

#boxplot(sqrt(cbind((true$y-mod2$Mean_Predictions$Mean)^2, aggMSE$MSE,(true$y-mod$Mean_Predictions$Mean)^2)))
#summary(sqrt(cbind((true$y-mod2$Mean_Predictions$Mean)^2, aggMSE$MSE, (true$y-mod$Mean_Predictions$Mean)^2)))

#summary(sqrt(cbind(aggMSE$MSE,aggMSE2$MSE, (true$y-mod$Mean_Predictions$Mean)^2)))






# TEST WITH NUMERICAL DATA ON EMDI
#emdi_model <- ebp(fixed = y~ x1+ x2, pop_data = cens,
#                  pop_domains = "idD", smp_data = surv, smp_domains = "idD",
#                  na.rm = TRUE)


#mod_alt <- SAEforest_nonLin(Y=surv$y, X=surv[,2:3], dName = "idD", smp_data =surv, pop_data=cens,
#                            mse ="none", B=50, importance = "impurity", mtry=1)


#mod_alt2 <- SAEforest_meanAGG(Y=surv$y, OOsample_obs = 25 , X=surv[,2:3], dName = "idD", smp_data =surv,
#                              Xpop_agg=Xcensus_agg,
#                              mse ="none", B=50, importance = "impurity")

#mean_method <- mod_alt2$Mean_Predictions
#agg_method <- emdi_model$ind[,1:2]
#plot_values <- merge(agg_method,mean_method, by.x="Domain", by.y="idD")
#matplot(plot_values[,-1], type="l")




# ADD FOR PLOTTING EXAMPLE OF map_indicators
# a<- map_indicators(object = mod_alt_nonLin, MSE = FALSE, CV = TRUE,
#                   map_obj = shape_austria_dis, indicator = c("Mean", "Gini"),
#                   map_dom_id = "PB", return_plot = TRUE, return_data = TRUE, gg_theme = theme_minimal())

# a$plotOb$Gini_CV +scale_fill_gradientn(colours = rainbow(10))


