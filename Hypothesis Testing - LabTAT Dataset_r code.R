#Hypothesis Testing Exercise

#A hospital wants to determine whether there is any difference in the average 
#Turn Around Time (TAT) of reports of the laboratories on their preferred list. 
#They collected a random sample and recorded TAT for reports of 4 laboratories. 
#TAT is defined as sample collected to report dispatch.

#Analyze the data and determine whether there is any difference in average TAT
#among the different laboratories at 5% significance level.

#Minitab File: LabTAT.mtw

install.packages("readr")
library(readr)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Hypothesis")
getwd()
LabTAT<-read.csv("LabTAT.csv")
View(LabTAT)
str(LabTAT)
summary(LabTAT)
var(LabTAT)
sd(LabTAT$Laboratory.1)
sd(LabTAT$Laboratory.2)
sd(LabTAT$Laboratory.3)
sd(LabTAT$Laboratory.4)
range(LabTAT)
install.packages("moments")
library(moments)
skewness(LabTAT)
kurtosis(LabTAT)
hist(LabTAT$Laboratory.1)
hist(LabTAT$Laboratory.2)
hist(LabTAT$Laboratory.3)
hist(LabTAT$Laboratory.4)
pairs(LabTAT)
barplot(LabTAT$Laboratory.1)
barplot(LabTAT$Laboratory.2)
barplot(LabTAT$Laboratory.3)
barplot(LabTAT$Laboratory.4)
boxplot(LabTAT$Laboratory.1,LabTAT$Laboratory.2,LabTAT$Laboratory.3,LabTAT$Laboratory.4)
sum(is.na(LabTAT))
attach(LabTAT)

#############Normality test###############

shapiro.test(LabTAT$Laboratory.1) 
# p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution

shapiro.test(LabTAT$Laboratory.2)
# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution

shapiro.test(LabTAT$Laboratory.3) 
# p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution

shapiro.test(LabTAT$Laboratory.4)
# p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(LabTAT$Laboratory.1,LabTAT$Laboratory.2)#variance test
 # p-value = 0.1675 > 0.05 so p high null fly => Equal variances

var.test(LabTAT$Laboratory.1,LabTAT$Laboratory.3)#variance test
# p-value = 0.01366 < 0.05 so p is low null fly => UnEqual variances

var.test(LabTAT$Laboratory.1,LabTAT$Laboratory.4)#variance test
# p-value = 0.1408 > 0.05 p high null fly => Equal variances

var.test(LabTAT$Laboratory.2,LabTAT$Laboratory.3)#variance test
# p-value = 0.2742 > 0.05 p high null fly => Equal variances

var.test(LabTAT$Laboratory.2,LabTAT$Laboratory.4)#variance test
# p-value = 0.9261 > 0.05 p high null fly => Equal variances

var.test(LabTAT$Laboratory.3,LabTAT$Laboratory.4)#variance test
# p-value = 0.3168 > 0.05 p high null fly => Equal variances

#############Anova##########

Anova_results <- aov(Laboratory.1~Laboratory.2,data = LabTAT)
summary(Anova_results)
# p-value = 0.168 > 0.05 accept null hypothesis 

Anova_results <- aov(Laboratory.1~Laboratory.3,data = LabTAT)
summary(Anova_results)
# p-value = 0.243 > 0.05 accept null hypothesis 

Anova_results <- aov(Laboratory.1~Laboratory.4,data = LabTAT)
summary(Anova_results)
# p-value = 0.315 > 0.05 accept null hypothesis 

Anova_results <- aov(Laboratory.2~Laboratory.3,data = LabTAT)
summary(Anova_results)
# p-value = 0.474 > 0.05 accept null hypothesis 

Anova_results <- aov(Laboratory.2~Laboratory.4,data = LabTAT)
summary(Anova_results)
# p-value = 0.5 > 0.05 accept null hypothesis 

Anova_results <- aov(Laboratory.3~Laboratory.4,data = LabTAT)
summary(Anova_results)
# p-value = 0.173 > 0.05 accept null hypothesis 
# All Proportions all equal