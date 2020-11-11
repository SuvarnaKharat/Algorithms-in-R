#Classify the Size_Categorie using SVM

#month	month of the year: 'jan' to 'dec'
#day	day of the week: 'mon' to 'sun'
#FFMC	FFMC index from the FWI system: 18.7 to 96.20
#DMC	DMC index from the FWI system: 1.1 to 291.3
#DC	DC index from the FWI system: 7.9 to 860.6
#ISI	ISI index from the FWI system: 0.0 to 56.10
#temp	temperature in Celsius degrees: 2.2 to 33.30
#RH	relative humidity in %: 15.0 to 100
#wind	wind speed in km/h: 0.40 to 9.40
#rain	outside rain in mm/m2 : 0.0 to 6.4
#Size_Categorie 	the burned area of the forest ( Small , Large)

install.packages("readr")
library(readr)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//SVM")
getwd()
Forest <- read.csv("forestfires.csv")
View(Forest)
str(Forest)
Forest$month <- as.numeric(as.factor(Forest$month))
Forest$day <- as.numeric(as.factor(Forest$day))
View(Forest)
str(Forest)
summary(Forest)
var(Forest)
sd(Forest$month)
sd(Forest$day)
sd(Forest$FFMC)
sd(Forest$DMC)
sd(Forest$DC)
sd(Forest$ISI)
sd(Forest$temp)
sd(Forest$RH)
sd(Forest$wind)
sd(Forest$rain)
sd(Forest$area)
install.packages("moments")
library(moments)
skewness(Forest[,-31])
kurtosis(Forest[,-31])
hist(Forest$month)
hist(Forest$day)
hist(Forest$FFMC)
hist(Forest$DMC)
hist(Forest$DC)
hist(Forest$ISI)
hist(Forest$temp)
hist(Forest$RH)
hist(Forest$wind)
hist(Forest$rain)
hist(Forest$area)
barplot(Forest$month)
barplot(Forest$day)
barplot(Forest$FFMC)
barplot(Forest$DMC)
barplot(Forest$DC)
barplot(Forest$ISI)
barplot(Forest$temp)
barplot(Forest$RH)
barplot(Forest$wind)
barplot(Forest$rain)
barplot(Forest$area)
boxplot(Forest[,-31])
sum(is.na(Forest))

library(kernlab)
library(caret)
names(Forest)[29] <- 'unknownname'
names(Forest)[31] <- 'Size'
View(Forest)
Traininglocal <- createDataPartition(Forest$Size, p=.70, list = F)
Train <- Forest[Traininglocal,]
prop.table(table(Train$Size))
table(Train$Size)
Test <- Forest[-Traininglocal,]
table(Test$Size)
prop.table(table(Test$Size))
View(Train)
View(Test)
#I remove the "area" column because this column justify the burn area is small or large in "Size"
#column. 
Forest <- Forest[-11]
View(Forest)
#building a model
Model_1 <- ksvm(Size~., data=Train, kernel="rbfdot")
pred <- predict(Model_1, newdata=Test)                 
mean(pred==Test$Size)
library(gmodels)
CrossTable(pred,Test$Size)