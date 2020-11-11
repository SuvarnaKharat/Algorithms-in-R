#Prepare a classification model using SVM for salary data 

#Data Description:
  
#age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
#race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual

install.packages("readr")
library(readr)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//SVM")
getwd()

Train <- read.csv("SalaryData_Train(1).csv")
View(Train)
str(Train)
Train$workclass <- as.numeric(as.factor(Train$workclass))
Train$education <- as.numeric(as.factor(Train$education))
Train$maritalstatus <- as.numeric(as.factor(Train$maritalstatus))
Train$occupation<- as.numeric(as.factor(Train$occupation))
Train$relationship <- as.numeric(as.factor(Train$relationship))
Train$race <- as.numeric(as.factor(Train$race))
Train$native <- as.numeric(as.factor(Train$native))
Train$sex <- as.numeric(as.factor(Train$sex))

View(Train)
str(Train)
summary(Train)
var(Train)
sd(Train$age)
sd(Train$workclass)
sd(Train$education)
sd(Train$educationno)
sd(Train$maritalstatus)
sd(Train$occupation)
sd(Train$relationship)
sd(Train$race)
sd(Train$sex)
sd(Train$capitalgain)
sd(Train$capitalloss)
sd(Train$hoursperweek)
sd(Train$native)
install.packages("moments")
library(moments)
skewness(Train[,-14])
kurtosis(Train[,-14])
hist(Train$age)
hist(Train$workclass)
hist(Train$education)
hist(Train$educationno)
hist(Train$maritalstatus)
hist(Train$occupation)
hist(Train$relationship)
hist(Train$race)
hist(Train$sex)
hist(Train$capitalgain)
hist(Train$capitalloss)
hist(Train$hoursperweek)
hist(Train$native)
pairs(Train[,-14])
barplot(Train$age)
barplot(Train$workclass)
barplot(Train$education)
barplot(Train$educationno)
barplot(Train$maritalstatus)
barplot(Train$occupation)
barplot(Train$relationship)
barplot(Train$race)
barplot(Train$sex)
barplot(Train$capitalgain)
barplot(Train$capitalloss)
barplot(Train$hoursperweek)
barplot(Train$native)
boxplot(Train[,-14])
sum(is.na(Train))
attach(Train)
Norm <- function(x)
{return((x-min(x))/(max(x)-min(x)))}
library(kernlab)
Train_n <- as.data.frame(lapply(Train[c(1:13)], Norm))
View(Train_n)
summary(Train_n)
Train <- data.frame(Train_n[1:13],Train[14])
View(Train)
summary(Train)
#model building
install.packages("kernlab")
library(kernlab)

Model_1 <- ksvm(Salary~.,data=Train, kernel='vanilladot')

# Uploading test data
getwd()
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//SVM")
library(readr)
Test <- read.csv("SalaryData_Test(1).csv")
View(Test)
str(Test)
Test$workclass <- as.numeric(as.factor(Test$workclass))
Test$education <- as.numeric(as.factor(Test$education))
Test$maritalstatus <- as.numeric(as.factor(Test$maritalstatus))
Test$occupation<- as.numeric(as.factor(Test$occupation))
Test$relationship <- as.numeric(as.factor(Test$relationship))
Test$race <- as.numeric(as.factor(Test$race))
Test$native <- as.numeric(as.factor(Test$native))
Test$sex <- as.numeric(as.factor(Test$sex))
View(Test)
Test_n <- as.data.frame(lapply(Test[c(1:13)], Norm))
View(Test_n)
Test <- data.frame(Test_n[1:13],Test[14])
View(Test)
summary(Test)
Pred <- predict(Model_1,  newdata=Test[-14])
mean(Pred==Test$Salary)
library(gmodels)
CrossTable(Pred,Test$Salary)

## Different kernel method
Model_2 <- ksvm(Salary~.,data=Train, kernel='besseldot')
Pred1 <- predict(Model_2,newdata=Test[-14])
mean(Pred1==Test$Salary)
CrossTable(Pred,Test$Salary)