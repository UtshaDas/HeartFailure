rm(list=ls())
data<-read.csv("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_training_wo_time.csv")

library(caret)


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="LOOCV")
# run the RFE algorithm
system.time(results <- rfe(data[,1:11], as.factor(data[,12]),sizes =c(1:2), rfeControl=control))
# summarize the results
print(results)
# list the chosen features
rfeRanked = predictors(results)
# plot the results
plot(results, type=c("g", "o"))