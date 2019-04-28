#Load library and Data
rm(list=ls())
setwd("F:/PRANAV/Project/Employee Absentism/R_Code")
getwd()
install.packages('xlsx')
library("readxl")
library(randomForest)
Data = read_excel("Absenteeism_at_work_Project.xls")

#Missing Value Analysis and Outlier Analysis
View(data)
df<-Data[-which(is.na(Data$Reason_for_absence)),]
df<-df[-which(is.na(df$Month_of_absence)),]
df<-df[-which(is.na(df$Transportation_expense)),]
df<-df[-which(is.na(df$Distance_from_Residence_to_Work)),]
df<-df[-which(is.na(df$Service_time)),]
df<-df[-which(is.na(df$Age)),]
df<-df[-which(is.na(df$Work_load_Average_per_day)),]
df<-df[-which(is.na(df$Hit_target)),]
df<-df[-which(is.na(df$Disciplinary_failure)),]
df<-df[-which(is.na(df$Education)),]
df<-df[-which(is.na(df$Son)),]
df<-df[-which(is.na(df$Social_drinker)),]
df<-df[-which(is.na(df$Social_smoker)),]
df<-df[-which(is.na(df$Pet)),]
df<-df[-which(is.na(df$Weight)),]
df<-df[-which(is.na(df$Height)),]
df<-df[-which(is.na(df$Body_mass_index)),]
df<-df[-which(is.na(df$Absenteeism_time_in_hours)),]

#Split data into Test and Train
names(df)
library(randomForest)
library(dplyr)
data <- df[,-c(1)]
train <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)

#Random Forest
Train <- data[train,]
Valid <- data[-train,]
rfFit = randomForest(Absenteeism_time_in_hours ~ .,                     # formula
                     data = Train,                   # data set
                     ntree = 500,                   # number of trees
                     mtry = 3,                     # variables for split
                     importance = TRUE)
#RMSE Prediction
test.pred.forest <- predict(rfFit,Valid)
test.pred.forest
RMSE.forest <- sqrt(mean((test.pred.forest-Valid$Absenteeism_time_in_hours)^2))
RMSE.forest


#Sample Input
write.csv(Valid, "Sample_Input.csv", row.names = T)

#adding predicted column to the data set
Valid$Prediction <- predict(rfFit,Valid)

#Sample Output
write.csv(Valid, "Sample_Output.csv", row.names = T)











