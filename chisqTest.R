library(mRMRe)
library(Biocomb)
file_n<-paste0("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_training_wo_time.csv")
data <- read.csv(file_n, header = TRUE)

# class label must be factor
data[,12]<-as.factor(data$DEATH_EVENT)
disc<-"equal interval width"
attrs.nominal=numeric()
out=select.inf.chi2(data,disc.method=disc,attrs.nominal=attrs.nominal)
