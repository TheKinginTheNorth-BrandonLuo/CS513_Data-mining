####################CS513HW2_EDA######################
#Fan Luo
#10442683
################### QUESTION 1 ###################
rm(list=ls())
data <- read.csv('E://breast-cancer-wisconsin.data.csv',na.string="?")
View(data)

#I.	Summarizing each column (e.g. min, max, mean )
summary(data)

#II.	Identifying missing values
df<- data.frame(data)
View(df)
summary(df)

#III.	Replacing the missing values with the ¡°mean¡± of the column.
installed.packages()
install.packages("modeest")
library(modeest)
F6_mlv<-mlv(df$F6,method = "mfv",na.rm = TRUE)
F6_mlv
F6_mlv$M
df$F6[is.na(df$F6)] <- F6_mlv$M
View(df)

#IV.	Displaying the frequency table of ¡°Class¡± vs. F6
ftable(df$Class,df$F6)

#V.	Displaying the scatter plot of F1 to F6, one pair at a time
pairs(df[2:7],main = "Breast Cancer Wisconsin Data -- 2 Classes",
      pch = 21,bg =c("red","blue")[factor(df$Class)])

#VI.	Show histogram box plot for columns F7 to F9
boxplot(df[8:10])

#################QUESTION2################
# Delete all the objects from R- environment. 
rm(list=ls())

# Reload the "breast-cancer-wisconsin.data.csv" from canvas into R.
data1 <- read.csv('E://breast-cancer-wisconsin.data.csv',na.string="?")
View(data1)
nrow(data1)

# Remove any row with a missing value in any of the columns.
data1_missing<-na.omit(data1)
View(data1_missing)
nrow(data1_missing)