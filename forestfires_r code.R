#Problem Statement : PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS

install.packages("readr")
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Neural Network")
getwd()
library(readr)
forest <- read.csv("forestfires.csv")
View(forest)
str(forest)
summary(forest)
sum(is.na.data.frame(forest))
var(forest)


#Create a custom function "norm" to Normalize the data
Norm <- function(x){return((x-min(x))/(max(x)-min(x)))}
Norm(c(1,5,6,8,-9))
View(forest)
str(forest)
forest$month <- as.numeric(as.factor(forest$month))
forest$day<- as.numeric(as.factor(forest$day))
forest$size_category <- as.numeric(as.factor(forest$size_category))

View(forest)
table(forest$size)

forest<- forest[,c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,11)]

View(forest)
summary(forest)
boxplot(forest)
hist(forest$month)
hist(forest$day)
hist(forest$rain)

#Create a norm function to normalize the data
norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

#Testing of norm function
norm(c(1,2,3,-85))

#Apply Norm function to the "forest" data set.
forest_N <- as.data.frame(lapply(forest,norm))
View(forest_N)
str(forest_N)
summary(forest_N)
#Data partition of area column
library(caret)
Traininglocal <- createDataPartition(forest_N$area, p=.70, list=F)
Train <- forest_N[Traininglocal,]
Test <- forest_N[-Traininglocal,]
View(Train)
View(Test)
library(nnet)
library(neuralnet)
Model_1 <- neuralnet(area~., data = Test)
str(Model_1)
plot(Model_1)
Predict_1 <- compute(Model_1, Test[1:30])
Area_burned <- Predict_1$net.result
Predict_1$neurons
cor(Area_burned, Test$area)

#New model using hidden neurons startegy
Model_2 <- neuralnet(area~., data =forest_N, hidden = c(5,4))
plot(Model_2)
Predict_area <- compute(Model_2, Test[1:30])
areaburned <- Predict_area$net.result
Predict_area$neurons
cor(areaburned, Test$area)

