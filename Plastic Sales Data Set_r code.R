#Forecasting Assignment : 
#Forecast the Plastic Sales data set. Prepare a document for each model 
#explaining how many dummy variables you have created and RMSE value 
#for each model. Finally which model you will use for Forecasting.


library(readr)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Forcasting")
getwd()
Plastic <- read.csv("PlasticSales.csv") 
View(Plastic) # Seasonality 12 months 
summary(Plastic)
str(Plastic)
sum(is.na(Plastic))
table(Plastic$Sales)
var(Plastic$Sales)
sd(Plastic$Sales)
install.packages("moments")
library(moments)
skewness(Plastic$Sales)
kurtosis(Plastic$Sales)
hist(Plastic$Sales)
boxplot(Plastic$Sales)
barplot(Plastic$Sales)
plot(Plastic$Sales)

# So creating 12 dummy variables 
install.packages("dummies")
library(dummies)
# Creating dummies for 12 months
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )
# Assigning month names 
colnames(X)<-month.abb 
View(X)
PlasticSales<-cbind(Plastic,X)
View(PlasticSales)
PlasticSales["t"]<-c(1:60)
View(PlasticSales)

PlasticSales["log_Sales"]<-log(PlasticSales["Sales"])
PlasticSales["t_square"]<-PlasticSales["t"]*PlasticSales["t"]
attach(PlasticSales)
train<-PlasticSales[1:45,]
test<-PlasticSales[46:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Additive Seasonality ##########################

multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#As per observation, 	multi_add_sea_model has least RMSE Value (133.2998)
#Hence, Multiplicative Additive Seasonality gives the best Model.

write.csv(PlasticSales,file="PlasticSales.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Forcasting")
test_data<-read.csv("PlasticSales.csv")
View(test_data)
pred_new<-predict(Add_sea_Quad_model,newdata=test_data,interval = 'predict')
pred_new
