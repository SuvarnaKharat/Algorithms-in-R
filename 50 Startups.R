#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and
#make a table containing R^2 value for each prepared model.

#R&D Spend -- Research and devolop spend in the past few years
#Administration -- spend on administration in the past few years
#Marketing Spend -- spend on Marketing in the past few years
#State -- states from which data is collected
#Profit  -- profit of each state in the past few years

install.packages("readr")
library(readr)
setwd("C://Users//Lenovo//Desktop//ExcelR//Assignments//Multi Linear Regression")
getwd()
MLR <- read.csv("50_Startups.csv")
View(MLR)
install.packages("dummies")
library(dummies)
State1 <- dummy(MLR$State)
View(State1)
MLR <- cbind(MLR,State1)
View(MLR)
MLR <- MLR[,-4]
View(MLR)
is.na(MLR)
sum(is.na(MLR))
colnames(MLR)
colnames(MLR)[which(names(MLR)=="StateNew York")] <- 'StateNewYork'
View(MLR)
plot(MLR)
Model1 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+StateCalifornia+StateFlorida+StateNewYork, data=MLR)
summary(Model1)
predict(Model1,intinterval = "predict")
confint(Model1,level = .95)
summary(Model1)
summary(MLR)