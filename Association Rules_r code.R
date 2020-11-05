#Association Rule Problem Statement for book, groceries and my_movies Data sets

#Prepare rules for all the data sets 
#1) Try different values of support and confidence. Observe the change in number
#of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots

install.packages("readr")
library(readr)
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)

# Data Set : Book 
book<-read.csv("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Association Rule//book.csv")
View(book)
class(book)
str(book)
summary(book)
var(book)
sd(book$ChildBks)
sd(book$YouthBks)
sd(book$CookBks)
sd(book$DoItYBks)
sd(book$RefBks)
sd(book$ArtBks)
sd(book$GeogBks)
sd(book$ItalCook)
sd(book$ItalAtlas)
sd(book$ItalArt)
sd(book$Florence)
range(book)
install.packages("moments")
library(moments)
skewness(book)
kurtosis(book)
sum(is.na(book))
attach(book)

book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 2000 rows ..so we should get 2000 transactions 
# Each row represents one transaction
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 
rules<-apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.7,minlen=4))
inspect(rules[1:100])
plot(rules)
 
rules<-apriori(as.matrix(book),parameter = list(support=0.003,confidence=0.6,minlen=3))
inspect(rules[1:100])
plot(rules)

rules<-apriori(as.matrix(book),parameter = list(support=0.004,confidence=0.8,minlen=2))
inspect(rules[1:100])
plot(rules)

# Data Set : Groceries 

G1<-read.csv("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Association Rule//groceries.csv")
View(G1)
str(G1)
summary(G1)
var(G1)
sum(is.na(G1))
attach(G1)


groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)

groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.07,minlen=2))
inspect(groceries_rules[1:10])
plot(groceries_rules)

groceries_rules<-apriori(groceries,parameter = list(support = 0.003,confidence = 0.06,minlen=3))
inspect(groceries_rules[1:10])
plot(groceries_rules)

groceries_rules<-apriori(groceries,parameter = list(support = 0.004,confidence = 0.08,minlen=2))
inspect(groceries_rules[1:10])
plot(groceries_rules)

#Data Set : my_movies 
my_movies<-read.csv("C://Users//mona//Desktop//DATA SCIENCE//ASSIGNMENTS//Association Rule//my_movies.csv")
View(my_movies)
class(my_movies)
str(my_movies)
summary(my_movies)
var(my_movies)
sd(my_movies$Sixth.Sense)
sd(my_movies$Gladiator)
sd(my_movies$LOTR1)
sd(my_movies$Harry.Potter1)
sd(my_movies$Patriot)
sd(my_movies$LOTR2)
sd(my_movies$Harry.Potter2)
sd(my_movies$LOTR)
sd(my_movies$Braveheart)
sd(my_movies$Green.Mile)
range(my_movies[,6:15])
install.packages("moments")
library(moments)
skewness(my_movies[,6:15])
kurtosis(my_movies[,6:15])
sum(is.na(my_movies))
attach(my_movies)

#Character data from column 1 to 5(V1, V2,V3,V4,V5) is been converted to binary
#format and already attached to the given data set. Hence, we will consider binary 
# data for extracting rules.
movies_trans<-as(as.matrix(my_movies[,6:15]),"transactions")
inspect(movies_trans[1:10])

# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 
rules<-apriori(movies_trans,parameter = list(support=0.002,confidence=0.7,minlen=4))
inspect(rules[1:10])
plot(rules)

rules<-apriori(movies_trans,parameter = list(support=0.003,confidence=0.6,minlen=3))
inspect(rules[1:10])
plot(rules)

rules<-apriori(movies_trans,parameter = list(support=0.004,confidence=0.8,minlen=2))
inspect(rules[1:10])
plot(rules)
