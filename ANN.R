#  Course          : CS513
#  First Name      : Fan
#  Last Name       : Luo
#  Id              : 10442682
#  purpose         : ANN

# remove working environment
remove(list=ls())

# reading file
filename<-file.choose() 
dsn<-  read.csv(filename)
summary(dsn)

# define columns in dataset
dsn<-as.data.frame (         
  cbind(  radius_mean=dsn$radius_mean
         ,diagnosis=dsn$diagnosis
         ,texture_mean=dsn$texture_mean
         ,perimeter_mean=dsn$perimeter_mean
         ,area_mean=dsn$area_mean
         ,smoothness_mean=dsn$smoothness_mean
         ,compactness_mean=dsn$compactness_mean
         ,concavity_mean=dsn$concavity_mean,
         concave.points_mean=dsn$concave.points_mean,
         symmetry_mean=dsn$symmetry_mean,
         fractal_dimension_mean=dsn$fractal_dimension_mean
         ,radius_se=dsn$radius_se
         ,texture_se=dsn$texture_se
         ,perimeter_se=dsn$perimeter_se
         ,area_se=dsn$area_se
         ,smoothness_se=dsn$smoothness_se
         ,compactness_se=dsn$compactness_se
         ,concavity_se=dsn$concavity_se
         ,concave.points_se=dsn$concave.points_se
         ,symmetry_se=dsn$symmetry_se
         ,fractal_dimension_se=dsn$fractal_dimension_se
         ,radius_worst=dsn$radius_worst
         ,texture_worst=dsn$texture_worst
         ,perimeter_worst=dsn$perimeter_worst
         ,area_worst=dsn$area_worst
         ,smoothness_worst=dsn$smoothness_worst
         ,compactness_worst=dsn$compactness_worst
         ,concavity_worst=dsn$concavity_worst
         ,concave.points_worst=dsn$concave.points_worst
         ,symmetry_worst=dsn$symmetry_worst
         ,fractal_dimension_worst=dsn$fractal_dimension_worst
  )
)


# split training and test dataset
train_ind = sort(sample(nrow(dsn),nrow(dsn)*0.7))
training <- dsn[train_ind, ]
test <- dsn[-train_ind, ]

# use neuralnet librarary 
library("neuralnet")
?neuralnet()

# fit neural network to dataset
net_cancer  <- neuralnet(diagnosis ~radius_mean+texture_mean+perimeter_mean+area_mean+
                           smoothness_mean+compactness_mean+concavity_mean+concave.points_mean+
                           symmetry_mean+fractal_dimension_mean+radius_se+texture_se+perimeter_se+
                           area_se+smoothness_se+compactness_se+concavity_se+concave.points_se+symmetry_se+
                           fractal_dimension_se+radius_worst+texture_worst+perimeter_worst+area_worst+
                           smoothness_worst+compactness_worst+concavity_worst+concave.points_worst+
                           symmetry_worst+fractal_dimension_worst
                      ,data=training
                      , hidden=5, threshold=0.01)



#Plot the neural network
plot(net_cancer)

net_cancer_results <-compute(net_cancer, test[,-2])
ANN=as.numeric(net_cancer_results$net.result)


ANN_round<-round(ANN)
ANN_cat<-ifelse(ANN<2.5,2,4)




table(Actual=test$diagnosis,ANN_cat)

# see the accuracy and error rate
wrong<- (test$Class!=ANN_cat)
rate<-sum(wrong)/length(wrong)


