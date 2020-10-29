#Forecasting Assignment : 
#Forecast the CocaCola prices data set. Prepare a document for each model 
#explaining how many dummy variables you have created and RMSE value 
#for each model. Finally which model you will use for Forecasting.

install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
install.packages("smooth")
library(smooth)
install.packages("readxl")
library(readxl)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Forcasting")
getwd()

CocaCola <- read_excel("CocaCola_Sales_Rawdata.xlsx") 
View(CocaCola) 
summary(CocaCola)
str(CocaCola)
sum(is.na(CocaCola))
table(CocaCola$Sales)
var(CocaCola$Sales)
sd(CocaCola$Sales)
install.packages("moments")
library(moments)
skewness(CocaCola$Sales)
kurtosis(CocaCola$Sales)
hist(CocaCola$Sales)
boxplot(CocaCola$Sales)
barplot(CocaCola$Sales)
plot(CocaCola$Sales)

install.packages("tseries")
library(tseries)
class(CocaCola)
Cola<-ts(CocaCola$Sales,frequency = 4,start=c(86))
View(Cola)
train<-Cola[1:38]
test<-Cola[39:42]
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)
plot(train)

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F) # simple exponential smooting#
hw_a
hwa_pred<-forecast(hw_a)
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape
# with alpha = 0.2, beta = 0.1

hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape
# with alpha = 0.2, beta = 0.1, gamma = 0.1 

hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape
# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape
hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape
############################## STOP HERE ###############################

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)
########################################################################

############## USING ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 

ses_a<-ses(train,alpha = 0.2) # 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape
# with alpha = 0.2, beta = 0.1

holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape
# with alpha = 0.2, beta = 0.1, gamma = 0.1 

hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new
# With out optimum values 

# simple exponential method

ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
sesna_mape
# Holts winter method 

holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape
# Holts winter Exponential method

hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape_new
df_mapes_new<-data.frame(c("sesa_mape","holtnab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtnab_mape,hwnabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)


#final Model
final_model <- HoltWinters(Cola)
plot(forecast(final_model))
final_pred<-data.frame(predict(Cola,h=4))
final_new<-MAPE(final_pred$Point.Forecast,test)*100
final_new
final_model$gamma


# MOVING AVERAGE 

ma_model1<-sma(train)
ma_pred<-data.frame(predict(ma_model1,h=4))
ma_pred
plot(forecast(ma_model1))
ma_mape<-MAPE(ma_pred$Point.Forecast,test)*100
ma_mape

# ARIMA MODEL
plot(train)
acf(train)
pacf(train)
a <- arima(train,order = c(1,1,8),method = "ML")

#Auto.Arima model on the price agg data
library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))

acf(model_AA$residuals)
pacf(model_AA$residuals)
plot(forecast(model_AA,h=12),xaxt="n")