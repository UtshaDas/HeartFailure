library(data.table)
#library(mltools)
data<-read.csv("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_clinical_records_dataset_organized.csv")

for (i in 1:7){
  minimum=min(data[,i])
  maximum=max(data[,i])
  for(j in 1:299){
    x=data[j,i]
    data[j,i]=(x-minimum)/(maximum-minimum)
  }
}

write.csv(data,"F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_normalized.csv")
