#Forecasting Assignment : 
#Forecast the Airlines Passengers data set. Prepare a document for each model 
#explaining how many dummy variables you have created and RMSE value 
#for each model. Finally which model you will use for Forecasting.


install.packages("readxl")
library(readxl)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Forcasting")
getwd()
Airlines <- read_excel("Airlines+Data.xlsx") 
View(Airlines) # Seasonality 12 months 
summary(Airlines)
str(Airlines)
sum(is.na(Airlines))
table(Airlines$Passengers)
var(Airlines$Passengers)
sd(Airlines$Passengers)
install.packages("moments")
library(moments)
skewness(Airlines$Passengers)
kurtosis(Airlines$Passengers)
hist(Airlines$Passengers)
boxplot(Airlines)
barplot(Airlines$Passengers)
plot(Airlines)

# So creating 12 dummy variables 
install.packages("dummies")
library(dummies)
# Creating dummies for 12 months
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
# Assigning month names 
colnames(X)<-month.abb 
View(X)
Airlinesdata<-cbind(Airlines,X)
View(Airlinesdata)
Airlinesdata["t"]<-c(1:96)
View(Airlinesdata)

Airlinesdata["log_Passengers"]<-log(Airlinesdata["Passengers"])
Airlinesdata["t_square"]<-Airlinesdata["t"]*Airlinesdata["t"]
attach(Airlinesdata)
train<-Airlinesdata[1:80,]
test<-Airlinesdata[81:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model<-lm(log_Passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Additive Seasonality ##########################

multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#As per observation, multi_add_sea_model has least RMSE Value (9.46900)
#Hence, Multiplicative Additive Seasonality gives the best Model.

# Additive seasonality with Quadratic has least RMSE value

write.csv(Airlinesdata,file="Airlinesdata.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Forcasting")
test_data<-read.csv("Airlinesdata.csv")
View(test_data)
pred_new<-predict(Add_sea_Quad_model,newdata=test_data,interval = 'predict')
pred_new
