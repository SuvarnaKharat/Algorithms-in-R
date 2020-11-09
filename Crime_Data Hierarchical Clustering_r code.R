#Hierarchical Clustering for Crime Data
#Perform Clustering for the crime data and identify the number of clusters formed 
#and draw inferences.

#Data Description:
#Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States

install.packages("readr")
library(readr)
setwd("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//clustering")
getwd()
input <- read.csv("crime_data.csv")
View(input)
mydata<- input[,c(2:5)]
#mydata<- input[,-1]
View(mydata)
summary(mydata)
str(mydata)
var(mydata)
sd(mydata$Murder)
sd(mydata$Assault)
sd(mydata$UrbanPop)
sd(mydata$Rape)
install.packages("moments")
library(moments)
skewness(mydata)
kurtosis(mydata)
hist(mydata$Murder)
hist(mydata$Assault)
hist(mydata$UrbanPop)
hist(mydata$Rape)
pairs(mydata)
barplot(mydata$Murder)
barplot(mydata$Assault)
barplot(mydata$UrbanPop)
barplot(mydata$Rape)
boxplot(mydata)
sum(is.na(mydata))
attach(mydata)


#Normalizing data
normalized_data<-scale(mydata) 
View(normalized_data)

# Distance matrix
d <- dist(normalized_data, method = "euclidean") 
d

# Model Building
fit <- hclust(d, method="complete")

# Display dendrogram
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red")

Crime_rate<-as.matrix(groups)
table(Crime_rate)

final <- data.frame(input, Crime_rate)
#final <- cbind(input, Crime_rate)
View(final)

#explore setcolorder for repositioning the columns in R
# Also install the package "data.table"
install.packages("data.table")
library(data.table)
setcolorder(final,c("Crime_rate"))
View(final)
