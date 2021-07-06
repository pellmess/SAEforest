
test1 <- MERFranger(Y=smp$y, X=smp[,c(2:3)], random = "(1|idD)", data = smp, ErrorTolerance = 0.000001)

adjust_ErrorSD(Y = smp$y, X=smp[,c(2:3)], mod = test1, B=100)


forestmeans <- SAEforest_mean(Y=smp$y, X=smp[,c(2:3)], dName="idD", survey_data =smp,
                census_data = cns)

forestmeans$Mean_predictions


forestind <- SAEforest_nonLin(Y=smp$y, X=smp[,c(2:3)], dName="idD", survey_data =smp,
                              census_data = cns)
