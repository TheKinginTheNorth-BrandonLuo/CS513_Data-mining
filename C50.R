####################CS513HW6_C50######################
#Fan Luo
#10442682
#Load dataset from current file
# clear environment
rm(list=ls())

# install.packages("C50")
library('C50')

# read data
dat<-read.csv("E:\\breast-cancer-wisconsin.data.csv")

# categories are represented by the ¡°factor¡± data type in R
categories<-factor(dat[,11])
dat<-cbind(dat[,2:10], categories)

# generate training and test set
index<-sort(sample(nrow(dat), round(.3*nrow(dat))))
training<-dat[-index,]
test<-dat[index,]

# run algorithms
C50_class<-C5.0(categories~., data=training)
summary(C50_class)
plot(C50_class)

C50_predict<-predict(C50_class, test, type="class")
table(actual=test[,10], C50=C50_predict)
wrong<-(test[,10]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,10])
c50_rate
