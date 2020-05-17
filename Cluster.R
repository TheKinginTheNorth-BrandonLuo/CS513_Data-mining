####################CS513HW9_Cluster######################
#Fan Luo
#10442682

# remove working environment
rm(list=ls())

# reading file
BCD <- read.csv('E://wisc_bc_ContinuousVar.csv',na.string="?")
str(BCD)

# remove the rows with missing values
BCD[BCD == "?"] <- NA
BCD2 <- na.omit(BCD)
BCD3<-na.omit(BCD)

# hclust method
bc2_dist<-dist( BCD2[,-c(1,32)])
hclust_resutls<-hclust(bc2_dist)
hclust_2<-cutree(hclust_resutls,2)
hclust_2
table(hclust_2,BCD2$diagnosis)

# K-means method
km<-kmeans(BCD3[,3:32], centers = 2);
str(km)
km$cluster
table(km$cluster,BCD3$diagnosis)
