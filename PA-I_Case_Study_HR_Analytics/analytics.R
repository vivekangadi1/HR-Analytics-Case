setwd("D:\\ml\\R\\HRAnalytics\\PA-I_Case_Study_HR_Analytics")
# install.packages("e1071")
#loading all the required libraries

pkgs <- c("keras", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr")
# install.packages(pkgs)

library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(car)
library(caret)
library(caTools)
library(MASS)
library(lubridate)
library(e1071)
library(cowplot)

#understanding the data based on business perspective
#there are 5 .csv data files used for analysis in this case study
#1.employee_survey_data.csv - contains details from employees about how well they are satisfied with their job and work-life balance
#2.manager_survey_data.csv - contains details from managers about how well employees perform under them
#3.general_data.csv - contains details about employee location and other aspects like employee behaviour and their involvement in job
#4.in_time.csv - contains details regarding in time of employees in calendar year 2015
#5.out_time.csv - contains details regarding out time of employees in calendar year 2015

#business perspective
#A large company XYZ having around 4000 employees has a very high level of attrition.
#This has a significant negative impact on the company.
#The main objective of this case study is to model the probability of attrition using
#logistic regression to understand what are the main factors that cause the high rate of attrition
#The company management can use these results to determine how the high rate of attrition can be reduced
#by making appropriate changes in the workplace.

#1. Data preparation, cleaning, analysis and EDA

#loading all the required dataset files
employee <- read.csv("employee_survey_data.csv",stringsAsFactors=F)
manager <- read.csv("manager_survey_data.csv",stringsAsFactors=F)
general <- read.csv("general_data.csv",stringsAsFactors=F)
intime <- read.csv("in_time.csv",stringsAsFactors=F)
outtime <- read.csv("out_time.csv",stringsAsFactors=F)

#display the file details
str(employee)
str(manager)
str(general)
str(intime)
str(outtime)

#renaming first columns in intime and outtime as EmployeeID to ensure consistency and ease of analysis
colnames(intime)[1]<-"EmployeeID"
colnames(outtime)[1]<-"EmployeeID"

#check for duplicate employee ID values in each data frame
sum(duplicated(employee$EmployeeID))
sum(duplicated(manager$EmployeeID))
sum(duplicated(general$EmployeeID))
sum(duplicated(intime$EmployeeID))
sum(duplicated(outtime$EmployeeID))

#check for missing values
sapply(employee, function(x) length(which(x == '')))
sapply(manager, function(x) length(which(x == '')))
sapply(general, function(x) length(which(x == '')))
sapply(intime, function(x) length(which(x == '')))
sapply(outtime, function(x) length(which(x == '')))

#check for NA values
sapply(employee, function(x) length(which(is.na(x))))
sapply(manager, function(x) length(which(is.na(x))))
sapply(general, function(x) length(which(is.na(x))))
sapply(intime, function(x) length(which(is.na(x))))
sapply(outtime, function(x) length(which(is.na(x))))

#replace NA values in individual columns of the employee dataset with median for ease of analysis
employee$EnvironmentSatisfaction[which(is.na(employee$EnvironmentSatisfaction))]<-median(employee$EnvironmentSatisfaction,na.rm = TRUE)
employee$JobSatisfaction[which(is.na(employee$JobSatisfaction))]<-median(employee$JobSatisfaction,na.rm = TRUE)
employee$WorkLifeBalance[which(is.na(employee$WorkLifeBalance))]<-median(employee$WorkLifeBalance,na.rm = TRUE)

#replace NA values in individual columns of the general dataset with mean for ease of analysis
general$NumCompaniesWorked[which(is.na(general$NumCompaniesWorked))]<-mean(general$NumCompaniesWorked,na.rm = TRUE)
general$TotalWorkingYears[which(is.na(general$TotalWorkingYears))]<-mean(general$TotalWorkingYears,na.rm = TRUE)

#Remove the columns with all the row values as NA
employee <- employee[,colSums(is.na(employee))<nrow(employee)]
manager <- manager[,colSums(is.na(manager))<nrow(manager)]
general <- general[,colSums(is.na(general))<nrow(general)]


#convert all character type columns in general dataframe into factor variables
general[,sapply(general, is.character)] <- lapply(general[,sapply(general, is.character)], as.factor)
#remove EmployeeCount and Over18 columns from general dataframe as all the row values are equal to 1
general <- general[ , -c(8,16)]
#remove StandardHours column from general dataframe as all the row values are equal to 8
general <- general[ , -c(16)]

#check whether EmployeeID values are consistent across different dataframes
setdiff(employee$EmployeeID,general$EmployeeID)
setdiff(manager$EmployeeID,general$EmployeeID)
setdiff(intime$EmployeeID,general$EmployeeID)
setdiff(outtime$EmployeeID,general$EmployeeID)

#convert some numeric variables into categorical (factor) variables as per format given in the data dictionary
general$Education[which(general$Education==1)] <- "Below College" 
general$Education[which(general$Education==2)] <- "College"
general$Education[which(general$Education==3)] <- "Bachelor"
general$Education[which(general$Education==4)] <- "Master"
general$Education[which(general$Education==5)] <- "Doctor"
general$Education <- as.factor(general$Education)

employee$EnvironmentSatisfaction[which(employee$EnvironmentSatisfaction==1)] <- "Low" 
employee$EnvironmentSatisfaction[which(employee$EnvironmentSatisfaction==2)] <- "Medium"
employee$EnvironmentSatisfaction[which(employee$EnvironmentSatisfaction==3)] <- "High"
employee$EnvironmentSatisfaction[which(employee$EnvironmentSatisfaction==4)] <- "Very High"
employee$EnvironmentSatisfaction <- as.factor(employee$EnvironmentSatisfaction)

employee$JobSatisfaction[which(employee$JobSatisfaction==1)] <- "Low" 
employee$JobSatisfaction[which(employee$JobSatisfaction==2)] <- "Medium"
employee$JobSatisfaction[which(employee$JobSatisfaction==3)] <- "High"
employee$JobSatisfaction[which(employee$JobSatisfaction==4)] <- "Very High"
employee$JobSatisfaction <- as.factor(employee$JobSatisfaction)

employee$WorkLifeBalance[which(employee$WorkLifeBalance==1)] <- "Bad" 
employee$WorkLifeBalance[which(employee$WorkLifeBalance==2)] <- "Good"
employee$WorkLifeBalance[which(employee$WorkLifeBalance==3)] <- "Better"
employee$WorkLifeBalance[which(employee$WorkLifeBalance==4)] <- "Best"
employee$WorkLifeBalance <- as.factor(employee$WorkLifeBalance)

manager$JobInvolvement[which(manager$JobInvolvement==1)] <- "Low" 
manager$JobInvolvement[which(manager$JobInvolvement==2)] <- "Medium"
manager$JobInvolvement[which(manager$JobInvolvement==3)] <- "High"
manager$JobInvolvement[which(manager$JobInvolvement==4)] <- "Very High"
manager$JobInvolvement <- as.factor(manager$JobInvolvement)

manager$PerformanceRating[which(manager$PerformanceRating==1)] <- "Low" 
manager$PerformanceRating[which(manager$PerformanceRating==2)] <- "Good"
manager$PerformanceRating[which(manager$PerformanceRating==3)] <- "Excellent"
manager$PerformanceRating[which(manager$PerformanceRating==4)] <- "Outstanding"
manager$PerformanceRating <- as.factor(manager$PerformanceRating)

#merging dataframes for analysis using common attribute (EmployeeID)
hrdata <- merge(general, employee, by="EmployeeID")
hrdata <- merge(hrdata, manager, by="EmployeeID")

sum(is.na(employee$JobSatisfaction))

sum(is.na(employee$WorkLifeBalance))
sum(is.na(manager$JobInvolvement))


sum(is.na(hrdata))



#removing the columns which have na more than 15% in intime and outtime datasets

missing_values_intime <- intime %>% summarise_all(funs(sum(is.na(.))/n()))

missing_values_intime <- gather(missing_values_intime,key='feature',value = 'missing_percentage')

missing_values_outtime <- outtime %>% summarise_all(funs(sum(is.na(.))/n()))

missing_values_outtime <- gather(missing_values_outtime,key='feature',value = 'missing_percentage')

intime_clean_cols <- filter(missing_values_intime,missing_percentage<0.15)
outtime_clean_cols <- filter(missing_values_outtime,missing_percentage<0.15)

intime_clean <- intime[,(colnames(intime) %in% intime_clean_cols$feature)]
outtime_clean <- outtime[,(colnames(outtime) %in% outtime_clean_cols$feature)]

sum(is.na(intime_clean))
sum(is.na(outtime_clean))

ncol(intime_clean)
ncol(intime)
ncol(outtime)
ncol(outtime_clean)


#Convert all the date columns from character to DateTime format
intime_clean[,2:ncol(intime_clean)] <- lapply(intime_clean[,2:ncol(intime_clean)], function(x) as_datetime(x))
outtime_clean[,2:ncol(outtime_clean)] <- lapply(outtime_clean[,2:ncol(outtime_clean)], function(x) as_datetime(x))


x<-outtime_clean[,2:ncol(outtime_clean)]-intime_clean[,2:ncol(intime_clean)]
x[,2:ncol(x)] <- lapply(x[,2:ncol(x)],function(x) as.numeric(x))
avg_hours <- rowMeans(x[,2:ncol(x)],na.rm = T)



##################################################################
# Barcharts for categorical features with stacked Attrtion information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(hrdata, aes(x=EducationField,fill=Attrition))+ geom_bar(), 
          ggplot(hrdata, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
ggplot(hrdata, aes(x=EducationField,fill=Attrition))+ geom_bar()
plot_grid(ggplot(hrdata, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(hrdata, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hrdata, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 


# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(hrdata, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(hrdata, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hrdata, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 20),
          ggplot(hrdata, aes(x="",y=JobInvolvement))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hrdata, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(hrdata, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#No outliers in numeric variables

# Boxplots of numeric variables relative to attriion status status
plot_grid(ggplot(hrdata, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hrdata, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hrdata, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)






######################################################################@@@@@@

nrow(x)
length(avg_hours)
hrdata_mergd <- cbind(hrdata,avg_hours)
hrdata_mergd$overtime <- ifelse(hrdata_mergd$avg_hours>8,1,0)
hrdata_mergd$undertime <- ifelse(hrdata_mergd$avg_hours<7,1,0)


for(i in 1:ncol(x)){
  hrdata_mergd$leaves[i] <- sum(is.na(x[i,]))
}


#masterData
str(hrdata_mergd)
#2. Model preparation and building
#converting all categorical variables (two-level and multi-level) into dummy variables for model building
#converting attrition to dummy data
dummy_1 <- data.frame(model.matrix( ~Attrition, data = hrdata_mergd))

hrdata_mergd$AttritionYes <- dummy_1$AttritionYes
hrdata_mergd$Attrition <- NULL

#converting BusinessTravel to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]

hrdata_mergd <- cbind(hrdata_mergd[,-3],dummy_1)


#converting Department to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~Department, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]

hrdata_mergd <- cbind(hrdata_mergd[,-3],dummy_1)


#converting Education to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~Education, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-4],dummy_1)


#converting EducationField to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~EducationField, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-4],dummy_1)


#converting Gender to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~Gender, data = hrdata_mergd))
str(hrdata_mergd)
hrdata_mergd$GenderMale <- dummy_1$GenderMale
hrdata_mergd$Gender <- NULL


#converting JobRole to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~JobRole, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-5],dummy_1)


#converting MaritalStatus to dummy data

dummy_1 <- data.frame(model.matrix( ~MaritalStatus, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-5],dummy_1)


#converting EnvironmentSatisfaction to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-14],dummy_1)

#converting JobSatisfaction to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~JobSatisfaction, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-14],dummy_1)


#converting WorkLifeBalance to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~WorkLifeBalance, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-14],dummy_1)


#converting JobInvolvement to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~JobInvolvement, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-14],dummy_1)


#converting PerformanceRating to dummy data
str(hrdata_mergd)
dummy_1 <- data.frame(model.matrix( ~PerformanceRating, data = hrdata_mergd))

str(hrdata_mergd)
hrdata_mergd$PerformanceRatingOutstanding <- dummy_1$PerformanceRatingOutstanding
hrdata_mergd$PerformanceRating <- NULL


#converting JobLevel to dummy data
str(hrdata_mergd)
hrdata_mergd$JobLevel <- as.factor(hrdata_mergd$JobLevel)
dummy_1 <- data.frame(model.matrix( ~JobLevel, data = hrdata_mergd))
dummy_1 <- dummy_1[,-1]
str(hrdata_mergd)
hrdata_mergd <- cbind(hrdata_mergd[,-4],dummy_1)

#data with dummy variabels

str(hrdata_mergd)
ncol(hrdata_mergd)
################################Check which columns needs to be scaled############
#EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,Education
#JobInvolvement,JobLevel,PerformanceRating
#Above variables have been converted into dummy variables.

ind<-c(2:16)
hr_analytics_scaled<-hrdata_mergd
for(i in ind)
{
  hr_analytics_scaled[,i]<-scale(x=hrdata_mergd[,i],center = TRUE,scale = TRUE)
}
summary(hr_analytics_scaled)

# separate training and testing data 
# 70% data for training and remaining for testing

set.seed(100)

indices = sample.split(hrdata_mergd$EmployeeID, SplitRatio = 0.7)

train = hr_analytics_scaled[indices,]

test = hr_analytics_scaled[-indices,]



########################################################################
#model building
#initial model

model_1<-glm(AttritionYes~.,data=train,family = 'binomial')
summary(model_1)

#variable selection using stepAIC() function
model_2<-stepAIC(model_1, direction="both")
summary(model_2)

vif(model_2)

#build model based on variables selected based on last AIC call
model_3<-glm(formula = AttritionYes ~ YearsAtCompany+JobRoleSales.Executive+PerformanceRatingOutstanding+
             MaritalStatusMarried+JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
             PercentSalaryHike+EducationCollege+DepartmentResearch...Development+JobInvolvementLow+
             DepartmentSales+JobRoleResearch.Director+BusinessTravelTravel_Rarely+TrainingTimesLastYear+
             Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
             JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
             YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_3)
vif(model_3)

#Remove YearsAtCompany due to high VIF (4.77) and high p-value (0.12910)
model_4<-glm(formula = AttritionYes ~ JobRoleSales.Executive+PerformanceRatingOutstanding+
               MaritalStatusMarried+JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
               PercentSalaryHike+EducationCollege+DepartmentResearch...Development+JobInvolvementLow+
               DepartmentSales+JobRoleResearch.Director+BusinessTravelTravel_Rarely+TrainingTimesLastYear+
               Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_4)
vif(model_4)


#Remove BusinessTravelTravel_Rarely due to high VIF (3.762446) and high p-value (0.008117)
#BusinessTravelTravel_Rarely
model_5<-glm(formula = AttritionYes ~ JobRoleSales.Executive+PerformanceRatingOutstanding+
               MaritalStatusMarried+JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
               PercentSalaryHike+EducationCollege+DepartmentResearch...Development+JobInvolvementLow+
               DepartmentSales+JobRoleResearch.Director+TrainingTimesLastYear+
               Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_5)
vif(model_5)

#Remove DepartmentResearch...Development due to high VIF (4.201548) and high p-value (0.008679)
#DepartmentResearch...Development
model_6<-glm(formula = AttritionYes ~ JobRoleSales.Executive+PerformanceRatingOutstanding+
               MaritalStatusMarried+JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
               PercentSalaryHike+EducationCollege+JobInvolvementLow+
               DepartmentSales+JobRoleResearch.Director+TrainingTimesLastYear+
               Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_6)
vif(model_6)


#Remove PercentSalaryHike due to high VIF (2.408234) and high p-value (0.066460)
#PercentSalaryHike
model_7<-glm(formula = AttritionYes ~ JobRoleSales.Executive+PerformanceRatingOutstanding+
               MaritalStatusMarried+JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
               EducationCollege+JobInvolvementLow+
               DepartmentSales+JobRoleResearch.Director+TrainingTimesLastYear+
               Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_7)
vif(model_7)



#Remove MaritalStatusMarried due to high VIF (2.132230) and high p-value (0.082892)
#MaritalStatusMarried
model_8<-glm(formula = AttritionYes ~ JobRoleSales.Executive+PerformanceRatingOutstanding+
               JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
               EducationCollege+JobInvolvementLow+
               DepartmentSales+JobRoleResearch.Director+TrainingTimesLastYear+
               Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_8)
vif(model_8)



#Remove DepartmentSales due to  high p-value (0.282341)
#DepartmentSales
model_9<-glm(formula = AttritionYes ~ JobRoleSales.Executive+PerformanceRatingOutstanding+
               JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
               EducationCollege+JobInvolvementLow+
               JobRoleResearch.Director+TrainingTimesLastYear+
               Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_9)
vif(model_9)

#Remove PerformanceRatingOutstanding due to  high p-value (0.777756)
#PerformanceRatingOutstanding
model_10<-glm(formula = AttritionYes ~ JobRoleSales.Executive+
               JobInvolvementVery.High+EnvironmentSatisfactionVery.High+JobLevel5+
               EducationCollege+JobInvolvementLow+
               JobRoleResearch.Director+TrainingTimesLastYear+
               Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial", data = train)

summary(model_10)
vif(model_10)

#Remove JobInvolvementVery.High due to  high p-value (0.154716)
#JobInvolvementVery.High  
model_11<-glm(formula = AttritionYes ~ JobRoleSales.Executive+
                EnvironmentSatisfactionVery.High+JobLevel5+
                EducationCollege+JobInvolvementLow+
                JobRoleResearch.Director+TrainingTimesLastYear+
                Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial", data = train)

summary(model_11)
vif(model_11)


#Remove JobLevel5 due to  high p-value (0.115706)
#JobLevel5  
model_12<-glm(formula = AttritionYes ~ JobRoleSales.Executive+
                EnvironmentSatisfactionVery.High+
                EducationCollege+JobInvolvementLow+
                JobRoleResearch.Director+TrainingTimesLastYear+
                Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial", data = train)

summary(model_12)
vif(model_12)


#Remove JobRoleSales.Executive due to  high p-value (0.116878)
#JobRoleSales.Executive   
model_13<-glm(formula = AttritionYes ~ EnvironmentSatisfactionVery.High+
                EducationCollege+JobInvolvementLow+
                JobRoleResearch.Director+TrainingTimesLastYear+
                Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial", data = train)

summary(model_13)
vif(model_13)


#Remove EnvironmentSatisfactionVery.High due to  high p-value (0.085664)
#EnvironmentSatisfactionVery.High   
model_14<-glm(formula = AttritionYes ~ EducationCollege+JobInvolvementLow+
                JobRoleResearch.Director+TrainingTimesLastYear+
                Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial", data = train)

summary(model_14)
vif(model_14)



#Remove EducationCollege due to  high p-value (0.085664)
#EducationCollege   
model_15<-glm(formula = AttritionYes ~ JobInvolvementLow+
                JobRoleResearch.Director+TrainingTimesLastYear+
                Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial", data = train)

summary(model_15)
vif(model_15)

#Remove JobRoleResearch.Director due to  high p-value (0.015714)
#JobRoleResearch.Director   
model_16<-glm(formula = AttritionYes ~ JobInvolvementLow+TrainingTimesLastYear+
                Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial", data = train)

summary(model_16)
vif(model_16)

#Remove JobInvolvementLow due to  high p-value (0.007243)
#JobInvolvementLow   
model_17<-glm(formula = AttritionYes ~ TrainingTimesLastYear+
                Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial", data = train)

summary(model_17)
vif(model_17)


#Remove TrainingTimesLastYear due to  high p-value (0.003009)
#TrainingTimesLastYear   
model_18<-glm(formula = AttritionYes ~ Age+JobRoleManufacturing.Director+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial",data = train)

summary(model_18)
vif(model_18)
#Remove JobRoleManufacturing.Director due to  high p-value (0.00108)
#JobRoleManufacturing.Director
model_19<-glm(formula = AttritionYes ~ Age+WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
               JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
               YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
             family = "binomial",data = train)

summary(model_19)
vif(model_19)
#Remove Age due to  high p-value (0.00108)
#Age
model_20<-glm(formula = AttritionYes ~ WorkLifeBalanceGood+JobSatisfactionLow+WorkLifeBalanceBest+
                JobSatisfactionVery.High+NumCompaniesWorked+YearsWithCurrManager+WorkLifeBalanceBetter+BusinessTravelTravel_Frequently+
                YearsSinceLastPromotion+EnvironmentSatisfactionLow+TotalWorkingYears+MaritalStatusSingle+overtime,
              family = "binomial",data = train)

summary(model_20)
vif(model_20)


#now all variables have p-values less than 0.001 and VIF values less than 2
#therefore, model_21 is deemed to be the final model

#The major assumption is that the VIF of the model variables has to be less than 2 and p-value of each variable in the final model is less than 0.001

#the method for eliminating variables after each model, is as follows:
#1. If the variables have higher VIF(>2) and low signifance(p-value>0.05), the variable having the highest p-value is considered if the p-value is less than 0.05
#2. If the variables all have p-values less than 0.05 (or 0.01 if applicable), the variable having the highest VIF is considered (if VIF>3)
#3. If the variables all have VIF less than 2, the variable having the highest p-value is considered if the p-value is more than 0.05
#4. Still, if variables are remaining, they are eliminated in order of p-values until all have p-value<0.001
#5. The above steps effectively mean that, irrespective of the VIF of the values, the variable elimination in models is done based on the p-value
#(highest p-value value would be eliminated at each step, regardless of the VIF)


Predict_1 <- predict(model_20,type = "response",test)

summary(Predict_1)
test$predcit_attrition <- Predict_1
# View(test)

r <- cor(test$AttritionYes,test$predcit_attrition)
rsquared <- cor(test$AttritionYes,test$predcit_attrition)^2
rsquared

ggplot(test, aes(avg_hours, AttritionYes)) + geom_line(aes(colour = "blue" )) + geom_line(aes(x=avg_hours, y=predcit_attrition, colour="red"))


ggplot(test, aes(EmployeeID, AttritionYes)) + geom_line(aes(colour = "blue" )) + 
  geom_line(aes(x=EmployeeID, y=predcit_attrition, colour="red"))



# View(test)

test_actual_attrition<-factor(ifelse(test$AttritionYes==1,"Yes","No"))


#P_Cutoff>=0.5
test_predict_attrition<-factor(ifelse(Predict_1 >= 0.50, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

#P_Cutoff>=0.4
test_predict_attrition<-factor(ifelse(Predict_1 >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Finding the Optimal Probability Cutoff



s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(Predict_1 >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
dev.off()



op_p_cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]


test_predict_attrition<-factor(ifelse(Predict_1 >= op_p_cutoff, "Yes", "No"))
conf_final <- confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
conf_final
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  2848  171
# Yes  850  540
# 
# Accuracy : 0.7684          
# 95% CI : (0.7557, 0.7808)
# No Information Rate : 0.8387          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.3822          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.7595          
# Specificity : 0.7701          
# Pos Pred Value : 0.3885          
# Neg Pred Value : 0.9434          
# Prevalence : 0.1613          
# Detection Rate : 0.1225          
# Detection Prevalence : 0.3153          
# Balanced Accuracy : 0.7648          
# 
# 'Positive' Class : Yes   
# 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_churn <- factor(ifelse(Predict_1 >=op_p_cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_churn, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_churn <- ifelse(test_actual_attrition=="Yes",1,0)
test_actual_churn <- ifelse(test_predict_attrition=="Yes",1,0)
decile<-c(1:10)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, Predict_1, groups = 10)
Churn_decile$Gain
Churn_decile$Cumlift
Churn_decile$total

plot(x=Churn_decile$Cumlift,y=decile,type = 'o')
plot(x=Churn_decile$Gain,y=decile,type ='o')
plot(x=Churn_decile$Gain,y=Churn_decile$Cumlift,type ='o')
plot(x=max(decile),y=max(ks_table_test),type = 'o')
 
model_keras <- keras_model_sequential()


model_keras %>% 
  # First hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(train)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Second hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )
model_keras


fit_keras <- fit(
  object           = model_keras, 
  x                = as.matrix(train), 
  y                = train$AttritionYes,
  batch_size       = 50, 
  epochs           = 35,
  validation_split = 0.30
)

fit_keras

plot(fit_keras) +
  theme_tq() +
  scale_color_tq() +
  scale_fill_tq() +
  labs(title = "Deep Learning Training Results")


yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(test)) %>%
  as.vector()


yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(test)) %>%
  as.vector()
