#  Course          : CS513
#  First Name      : Fan
#  Last Name       : Luo
#  Id              : 10442682
#  purpose         : HW07_RandomForest

#clean work environment
rm(list=ls())

#install.packages('randomForest')
library(randomForest)

bc<-read.csv("E://breast-cancer-wisconsin.data.csv",na.strings = "?")
bc2<-na.omit(bc)

# split training and test data
index <- seq (1,nrow(bc2),by=5)
test<-bc2[index,]
training<-bc2[-index,]

# perform randomForest method
fit <- randomForest( factor(Class)~., data=training[,-1], importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test[,-1])
table(actual=test$Class ,Prediction)

# perform error rate
wrong<- (test$Class!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 
# from the importance plot, we can conclude that F2, F6 and F7 are the top 3 important features of the data set since their accuracies are the highest.



