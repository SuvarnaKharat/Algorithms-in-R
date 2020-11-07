#Hypothesis Testing Exercise

#A F&B manager wants to determine whether there is any significant difference in
#the diameter of the cutlet between two units. A randomly selected sample of cutlets
#was collected from both units and measured? Analyze the data and draw inferences 
#at 5% significance level. Please state the assumptions and tests that you carried 
#out to check validity of the assumptions

#Data Set - Cutlets.mtw

install.packages("readr")
library(readr)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Hypothesis")
getwd()
Cutlets<-read.csv("Cutlets.csv")
View(Cutlets)
str(Cutlets)
summary(Cutlets)
var(Cutlets)
sd(Cutlets$Unit.A)
sd(Cutlets$Unit.B)
range(Cutlets)
install.packages("moments")
library(moments)
skewness(Cutlets)
kurtosis(Cutlets)
hist(Cutlets$Unit.A)
hist(Cutlets$Unit.B)
plot(Cutlets$Unit.A,Cutlets$Unit.B)
barplot(Cutlets$Unit.A)
barplot(Cutlets$Unit.B)
boxplot(Cutlets$Unit.A,Cutlets$Unit.B)
sum(is.na(Cutlets))
attach(Cutlets)
colnames(Cutlets)<-c("UnitA","UnitB")
colnames(Cutlets)
View(Cutlets)
str(Cutlets)
UnitA<-as.numeric(as.factor(Cutlets$UnitA))
UnitB<-as.numeric(as.factor(Cutlets$UnitB))

#############Normality test###############

shapiro.test(UnitA) 
# p-value = 0.182 >0.05 so p high null fly => It follows normal distribution

shapiro.test(UnitB)
# p-value = 0.182 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(UnitA,UnitB)#variance test
# p-value = 1 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################

t.test(UnitA, UnitB, alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal means
# p-value = 1 > 0.05 accept null Hypothesis 
# Equal means

t.test(UnitA, UnitB, alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (UnitA, UnitB) < 0
# Alternative Hypothesis -> (UnitA) > (UnitB)0
# p-value = 0.5 > 0.05 => p high alternate go => accept null hypothesis
# Unit B better than Unit A