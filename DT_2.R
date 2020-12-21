rm(list=ls())
library(rpart)
library(pROC)
training<-read.csv("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_training_wo_time_Selected_2.csv")
testing<-read.csv("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_testing_wo_time_Selected_2.csv")
sapdata<-read.csv("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/Sap_DT_Selected.csv")

base_model <- rpart(as.factor(DEATH_EVENT) ~., data = training,method="class",control = rpart.control(cp = 0))
postpruned_model <- rpart(as.factor(DEATH_EVENT) ~., data = training,method="class",control = rpart.control(cp = 0))

max_dep=0
min_split=0
accu=0
cp_op=0
k=1
cp_array=c(0,0.01,0.05,0.1,0.5,1)
for(cp_value in 1:length(cp_array)){
  for (i in 1:20) {
    for (j in 1:32) {
      
      print(i)
      print(j)
      prepruned_model <- rpart(as.factor(DEATH_EVENT) ~., data = training,method="class",control = rpart.control(cp = cp_array[cp_value], maxdepth = i,minsplit =j))
      
      testing$pred <- predict(object = prepruned_model,  newdata = testing[,-3],    type = "class")
      #printcp()
      #plotcp()
      
      cm=table(testing$pred,testing$DEATH_EVENT,dnn=c("Prediction","Actual"))
      acc=((sum(diag(cm))/sum(cm)))
      tp<-cm[2,2]
      tn<-cm[1,1]
      fn<-cm[1,2]
      fp<-cm[2,1]
      
      sen=tp/(tp+fn)
      spe=tn/(tn+fp)
      mcc=((tp*tn) - (fp*fn))/(sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
      f1=2*tp/((2*tp)+fp+fn)
      
      roc_obj<-roc(testing[,3],as.numeric(testing$pred))
      rocauc<-auc(roc_obj)
      
      sapdata[k,1]=cp_array[cp_value]
      sapdata[k,2]=i
      sapdata[k,3]=j
      sapdata[k,4]=acc
      sapdata[k,5]=sen
      sapdata[k,6]=spe
      sapdata[k,7]=mcc
      sapdata[k,8]=f1
      sapdata[k,9]=rocauc
      k=k+1
      
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
      if(acc>accu){
        max_dep=i
        min_split=j
        accu=acc
        cp_op=cp_array[cp_value]
      }
    }
  }
}
write.csv(sapdata,"F:/Thesis/DataMing+MachieLeaning/Heart_Failure/Sap_DT_Selected.csv")
print(cp_op)
print(max_dep)
print(min_split)
print(accu)
rpart.plot(prepruned_model)