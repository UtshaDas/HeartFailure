library(mRMRe)
file_n<-paste0("F:/Thesis/DataMing+MachieLeaning/Heart_Failure/heart_failure_training_wo_time.csv")
df <- read.csv(file_n, header = TRUE)
for (i in 1:12){
  df[[i]] <- as.numeric(df[[i]])
}
f_data <- mRMR.data(data = data.frame(df))
results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 12,
                        feature_count = 11)
print(solutions(results))

print(results@scores)