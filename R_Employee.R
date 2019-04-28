rm(list=ls())
setwd("F:/PRANAV/Project/Employee Absentism")
getwd()
install.packages(c("dmm","dplyr","plyr","reshape","ggplot2","data.table","corrgram","DMWR","caret","randomForest","unbalanced","c50","dummies","e1071","information","Mass","rpart","gbm","ROSE"))
install.packages("C50")
install.packages('xlsx')
library("readxl")
df = read_excel("Absenteeism_at_work_Project.xls")
View(df)
names(df)
str(df)
missing_Val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_Val$columns = row.names(missing_Val)
row.names(missing_Val) = NULL
names(missing_Val)[1] = "Missing_Percentage"
missing_Val$Missing_Percentage = (missing_Val$Missing_Percentage/nrow(df))*100
missing_Val = missing_Val[order(-missing_Val$Missing_Percentage),]
missing_Val = missing_Val[,c(2,1)]
View(missing_Val)

ggplot(data= missing_Val[1:3,],aes(x=reorder(columns, Missing_Percentage),y = Missing_Percentage))+
  geom_bar(stat = "identity", fill="grey0")+xlab("parameter")+
  ggtitle("Missing_Data_Percentage")+theme_bw()

df$Body_mass_index[is.na(df$Body_mass_index)] = median(df$Body_mass_index, na.rm = T)
df$Absenteeism_time_in_hours[is.na(df$Absenteeism_time_in_hours)] = median(df$Absenteeism_time_in_hours, na.rm = T)
df$Height[is.na(df$Height)] = median(df$Height, na.rm = T)
df$`Work_load_Average/day`[is.na(df$`Work_load_Average/day`)] = median(df$`Work_load_Average/day`, na.rm = T)
df$Education[is.na(df$Education)] = median(df$Education, na.rm = T)
df$Transportation_expense[is.na(df$Transportation_expense)] = median(df$Transportation_expense, na.rm = T)
df$Hit_target[is.na(df$Hit_target)] = median(df$Hit_target, na.rm = T)
df$Disciplinary_failure[is.na(df$Disciplinary_failure)] = median(df$Disciplinary_failure, na.rm = T)
df$Son[is.na(df$Son)] = median(df$Son, na.rm = T)
df$Social_smoker[is.na(df$Social_smoker)] = median(df$Social_smoker, na.rm = T)
df$Reason_for_absence[is.na(df$Reason_for_absence)] = median(df$Reason_for_absence, na.rm = T)
df$Distance_from_Residence_to_Work[is.na(df$Distance_from_Residence_to_Work)] = median(df$Distance_from_Residence_to_Work, na.rm = T)
df$Service_time[is.na(df$Service_time)] = median(df$Service_time, na.rm = T)
df$Age[is.na(df$Age)] = median(df$Age, na.rm = T)
df$Social_drinker[is.na(df$Social_drinker)] = median(df$Social_drinker, na.rm = T)
df$Pet[is.na(df$Pet)] = median(df$Pet, na.rm = T)
df$Month_of_absence[is.na(df$Month_of_absence)] = median(df$Month_of_absence, na.rm = T)
df$Weight[is.na(df$Weight)] = median(df$Weight, na.rm = T)
sum(is.na(df))

cnames = colnames(df)

library("ggplot2")
library("scales")
library("psych")
library("gplots")

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y=(cnames[i]), x= "Absenteeism_time_in_hours"), data= subset(df))+
           stat_boxplot(geom = "errorbar", width = 0.5)+
           geom_boxplot(outlier.colour= "red", fill= "grey", outlier.shape= 18, oulier.size= 1, notch= "Flase")+
           theme(legend.position= "bottom")+
           labs(y=cnames[i],x= "Absenteeism_time_in_hours")+
           ggtitle(paste("Box Plot of Absenteeism for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)
gridExtra::grid.arrange(gn11,gn12,ncol=2)
gridExtra::grid.arrange(gn13,gn14,ncol=2)
gridExtra::grid.arrange(gn15,gn16,ncol=2)
gridExtra::grid.arrange(gn17,gn18,ncol=2)
gridExtra::grid.arrange(gn19,gn20,ncol=2)
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7,gn8,gn9,gn11,gn12,gn13,gn14,gn15,gn16,gn17,gn18,gn19,gn20,ncol=16)
gridExtra::grid.arrange(gn9,ncol=1)
