#Problem Statement : Prepare a model for strength of concrete data using Neural Networks

install.packages("readr")
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Neural Network")
getwd()
library(readr)
Concrete <- read.csv("concrete.csv")
View(Concrete)
sum(is.na.data.frame(Concrete))
table(Concrete$age)
summary(Concrete)
str(Concrete)
Concrete$age <- as.numeric(as.factor(Concrete$age))
View(Concrete)
var(Concrete)

install.packages("moments")
library(moments)
skewness(Concrete)
kurtosis(Concrete)
pairs(Concrete)
hist(Concrete$water)
hist(Concrete$superplastic)
hist(Concrete$fineagg)
boxplot(Concrete)


#Normalization of data by create a custom norm function
Norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

#Testing of Norm function
Norm(c(1,5,-9,-859647))
Concrete_N <- as.data.frame(lapply(Concrete, Norm))
View(Concrete_N)
summary(Concrete_N)
table(Concrete_N$age)

#Create data partition
library(readr)
library(caret)
Traininglocal <- createDataPartition(Concrete_N$strength, p=0.70, list = F)
Train <- Concrete_N[Traininglocal,]
Test <- Concrete_N[-Traininglocal,]
View(Train)
View(Test)

#Model building by using package "neuralnet", "nnet"
library(neuralnet)
library(nnet)
Model_1 <- neuralnet(strength~., data = Train)
plot(Model_1)
Pred <- compute(Model_1, Test[1:8])
Pred_strength <- Pred$net.result
Pred$neurons
cor(Pred_strength, Test$strength)

#By using hidden neuron
Model_2 <- neuralnet(strength~.,data = Concrete_N, hidden = c(3,2))
plot(Model_2)
Pred_1 <- compute(Model_2,Test[1:8])
Pred1_strength <- Pred_1$net.result
Pred_1$neurons
cor(Pred1_strength,Test$strength)
