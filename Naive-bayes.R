#  Course          : CS513-Midterm
#  First Name      : Fan
#  Last Name       : Luo
#  Id              : 10442682
#  question 6      : Naïve Bayes
rm(list=ls())
data <- read.csv('E://breast-cancer-wisconsin.data.csv',na.string="?")
View(data)

# convert all the integer data types to factor data type
data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)],as.factor)
data1 <- na.omit(data)
View(data1)

# Remove any row with a missing value in any of the columns.
data1[data1 == "?"] <- NA
data2 <- na.omit(data1)

train_ind = sort(sample(nrow(data2),nrow(data2)*0.7))
training <- data2[train_ind, ]
test <- data2[-train_ind, ]

# Perform Naïve Bayes 
library(class) 
library(e1071)

nBayes_all <- naiveBayes(Class~., data=training)
category_all <- predict(nBayes_all, test)

# Score the test dataset
table(NBayes_all=category_all,Class=test$Class)
NB_wrong<-sum(category_all!=test$Class)
NB_errorRate<-NB_wrong/length(category_all)
NB_errorRate

# Measure the error rate. 
accuracy <- 1-NB_errorRate
accuracy
