####################CS513HW3_KNN######################
#Fan Luo
#10442682
#Load dataset from current file
rm(list=ls())
data<-read.csv('E://breast-cancer-wisconsin.data.csv',na.string="?")
View(data)
str(data)

#Define data type
data$Class[data$Class==2]<-"Benign"
data$Class[data$Class==4]<-"Malignant"
data<-data[-1]

#split training and test data
set.seed(1234)
train<-sample(nrow(data),0.7*nrow(data))
tdata<-data[train,] 
vdata<-data[-train,]

#use rpart to print d_tree
library(rpart)
dtree<-rpart(Class~.,data=tdata,method="class", parms=list(split="information"))
printcp(dtree)

rpart(formula = Class ~ ., data = tdata, method = "class", parms = list(split = "information"))
opar<-par(no.readonly = T)
par(mfrow=c(1,2))

#plot the figure
library(rpart.plot)
rpart.plot(dtree,branch=1,type=2, fallen.leaves=T,cex=0.8, sub="BeforePrune")
par(opar)
