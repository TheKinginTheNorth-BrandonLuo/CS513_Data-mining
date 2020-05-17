####################CS513HW3_KNN######################
#Fan Luo
#10442682
#Load dataset from current file
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

# importing "class" package
library(kknn)

# Using knn model and calculate prediction when k=3
predict3 <- kknn(formula = Class~.,training, test,k = 3, kernel = "rectangular")
fit3 <-fitted(predict3)
table(test$Class,fit3)

#k=5
predict5 <- kknn(formula = Class~.,training, test,k = 5, kernel = "rectangular")
fit5 <-fitted(predict5)
table(test$Class,fit5)

#k=10
predict10 <- kknn(formula = Class~.,training, test,k = 10, kernel = "rectangular")
fit10 <-fitted(predict10)
table(test$Class,fit10)