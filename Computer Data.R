#Predict Price of the computer

#A dataframe containing :
#price : price in US dollars of 486 PCs
#speed : clock speed in MHz
#hd : size of hard drive in MB
#ram : size of Ram in in MB
#screen : size of screen in inches
#cd : is a CD-ROM present ?
#multi : is a multimedia kit (speakers, sound card) included ?
#premium : is the manufacturer was a "premium" firm (IBM, COMPAQ) ?
#ads : number of 486 price listings for each month
#trend : time trend indicating month starting from January of 1993 to November of 1995.

install.packages("readr")
library(readr)
setwd("C://Users//Lenovo//Desktop//ExcelR//Assignments//Multi Linear Regression")
getwd()
MLR <- read.csv("Computer_Data.csv")
View(MLR)
install.packages("dummies")
library(dummies)
Premium1 <- dummy(MLR$premium)
View(Premium1)
MLR <- cbind(MLR,Premium1)
View(MLR)
MLR <- MLR[,-4]
View(MLR)
is.na(MLR)
sum(is.na(MLR))
colnames(MLR)
plot(MLR)
Model1 <- lm(price~speed+ram+screen+cd+multi+ads+trend+premiumno+premiumyes, data=MLR)
summary(Model1)
predict(Model1,intinterval = "predict")
confint(Model1,level = .95)
summary(Model1)
summary(MLR)