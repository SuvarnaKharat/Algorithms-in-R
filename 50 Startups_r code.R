#Build a Neural Network model for 50_startups data to predict profit 

install.packages("readr")
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Neural Network")
getwd()
library(readr)
startups <- read.csv("50_Startups.csv")
View(startups)
sum(is.na.data.frame(startups))
table(startups$State)
summary(startups)
str(startups)
startups$State <- as.numeric(as.factor(startups$State))
View(startups)
var(startups)

install.packages("moments")
library(moments)
skewness(startups$R.D.Spend)
skewness(startups$Administration)
skewness(startups$Marketing.Spend)
skewness(startups$Profit)
kurtosis(startups$R.D.Spend)
kurtosis(startups$Administration)
kurtosis(startups$Marketing.Spend)
kurtosis(startups$Profit)
hist(startups$R.D.Spend)
hist(startups$Profit)
hist(startups$Marketing.Spend)
hist(startups$Administration)
boxplot(startups)

#Normalization of data by create a custom norm function
Norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

#Testing of Norm function
Norm(c(1,5,-9,-859647))
startups_N <- as.data.frame(lapply(startups, Norm))
View(startups_N)
summary(startups_N)
table(startups_N$State)

#Create data partition
library(readr)
library(caret)
Traininglocal <- createDataPartition(startups_N$Profit, p=0.70, list = F)
Train <- startups_N[Traininglocal,]
Test <- startups_N[-Traininglocal,]
View(Train)
View(Test)

#Model building by using package "neuralnet", "nnet"
library(neuralnet)
library(nnet)
Model_1 <- neuralnet(Profit~., data = Train)
plot(Model_1)
Pred <- compute(Model_1, Test[1:4])
Pred_profit <- Pred$net.result
Pred$neurons
cor(Pred_profit, Test$Profit)

#By using hidden neuron
Model_2 <- neuralnet(Profit~.,data = startups_N, hidden = c(3,2))
plot(Model_2)
Pred_1 <- compute(Model_2,Test[1:4])
Pred1_profit <- Pred_1$net.result
Pred_1$neurons
cor(Pred1_profit,Test$Profit)

