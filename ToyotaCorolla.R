#Consider only the below columns and prepare a prediction model for predicting Price.

#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

#Model -- model of the car
#Price  -- Offer Price in EUROs	
#Age_08_04 -- Age in months as in August 2004	
#Mfg_Month -- Manufacturing month (1-12)	
#Mfg_Year	-- Manufacturing Year
#KM -- Accumulated Kilometers on odometer
#Fuel_Type	 -- Fuel Type (Petrol, Diesel, CNG)
#HP -- Horse Power
#Met_Color	 -- Metallic Color?  (Yes=1, No=0)
#Color -- Color (Blue, Red, Grey, Silver, Black, etc.)
#Automatic	-- Automatic ( (Yes=1, No=0)
                        # cc -- Cylinder Volume in cubic centimeters
                        #Doors -- Number of doors
                        #Cylinders	-- Number of cylinders
                        #Gears -- Number of gear positions
                        #Quarterly_Tax -- Quarterly road tax in EUROs
                        #Weight -- Weight in Kilograms
                        #Mfr_Guarantee -- Within Manufacturer's Guarantee period  (Yes=1, No=0)
#BOVAG_Guarantee -- BOVAG (Dutch dealer network) Guarantee  (Yes=1, No=0)
#Guarantee_Period -- 	Guarantee period in months
#ABS -- Anti-Lock Brake System (Yes=1, No=0)
#Airbag_1 -- Driver_Airbag  (Yes=1, No=0)
#Airbag_2 -- Passenger Airbag  (Yes=1, No=0)
#Airco -- Airconditioning  (Yes=1, No=0)
#Automatic_airco -- Automatic Airconditioning  (Yes=1, No=0)
#Boardcomputer -- Boardcomputer  (Yes=1, No=0)
#CD_Player -- CD Player  (Yes=1, No=0)
#Central_Lock -- Central Lock  (Yes=1, No=0)
#Powered_Windows -- Powered Windows  (Yes=1, No=0)
#Power_Steering -- Power Steering  (Yes=1, No=0)
#Radio -- Radio  (Yes=1, No=0)
#Mistlamps	-- Mistlamps  (Yes=1, No=0)
#Sport_Model -- Sport Model  (Yes=1, No=0)
#Backseat_Divider -- Backseat Divider  (Yes=1, No=0)
#Metallic_Rim --Metallic Rim  (Yes=1, No=0)
#Radio_cassette -- Radio Cassette  (Yes=1, No=0)
#Tow_Bar -- Tow Bar  (Yes=1, No=0)


install.packages("readr")
library(readr)
setwd("C://Users//Lenovo//Desktop//ExcelR//Assignments//Multi Linear Regression")
getwd()
MLR <- read.csv("ToyotaCorolla.csv")
View(MLR)
install.packages("dummies")
library(dummies)
Gears1 <- dummy(MLR$Gears)
View(Gears1)
MLR <- cbind(MLR,Gears1)
View(MLR)
MLR <- MLR[,-4]
View(MLR)
is.na(MLR)
sum(is.na(MLR))
colnames(MLR)
plot(MLR)
Model1 <- lm(Price~KM+HP+cc+Doors+Gears3+Gears4+Gears5+Gears6+Quarterly_Tax+Weight, data=MLR)
summary(Model1)
predict(Model1,intinterval = "predict")
confint(Model1,level = .95)
summary(Model1)
summary(MLR)