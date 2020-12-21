rm(list=ls())
library(caret)
library(e1071)
library(pROC)
training<-read.csv("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_training_wo_time_Selected_2.csv")
testing<-read.csv("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_testing_wo_time_Selected_2.csv")

train <- training[,1:2]
test <- testing[,1:2]
cl <- as.factor(training[,3])

x <- train
y <- cl
best_model <- tune.knn(x, y, k = 1:100, tunecontrol = tune.control(sampling = "cross"))
print(best_model[["best.parameters"]])
plot(best_model)

predicted=knn3Train(train, test, cl, k =best_model[["best.parameters"]], prob = TRUE)


cm=table(predicted,testing[,3],dnn=c("Prediction","Actual"))
acc=((sum(diag(cm))/sum(cm)))
tp<-cm[2,2]
tn<-cm[1,1]
fn<-cm[1,2]
fp<-cm[2,1]

sen=tp/(tp+fn)
spe=tn/(tn+fp)
mcc=((tp*tn) - (fp*fn))/(sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
f1=2*tp/((2*tp)+fp+fn)

roc_obj<-roc(testing[,3],as.numeric(predicted))
rocauc<-auc(roc_obj)

print('Accuracy')
print(acc)
print('sensitivity')
print(sen)
print('Specificity')
print(spe)
print('MCC')
print(mcc)
print('F1')
print(f1)
print('AUC')
print(rocauc)
