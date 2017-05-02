#machine learning project
getwd()
setwd("/Users/grant/R_Polina/ExplDataAnalysis/Homework")
library(caret)

library(gbm)
training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")

str(training)
str(testing)
dim(training)
dim(testing)

#select 9 pred variables
train10<-data.frame(training$gyros_belt_x,training$gyros_belt_y,training$gyros_belt_z,
                  training$accel_belt_x,training$accel_belt_y,training$accel_belt_z,
                  training$accel_arm_x, training$accel_arm_y, training$accel_arm_z,
                  training$classe)

str(train10)


test10<-data.frame(testing$gyros_belt_x,testing$gyros_belt_y,testing$gyros_belt_z,
                   testing$accel_belt_x,testing$accel_belt_y,testing$accel_belt_z,
                   testing$accel_arm_x, testing$accel_arm_y, testing$accel_arm_z,
                   testing$classe)

head(test10)
modlda <- train(training.classe ~.,data=train10,method="lda")
modnb = train(training.classe ~ ., data=train10,method="nb")

print(modlda)
print(modnb)
plda = predict(modlda,test10); pnb = predict(modnb,test10)
table(plda,pnb)

#Using random forest
modrf <- train(training.classe~ .,data=train10,method="rf",prox=TRUE)
predrf <- predict(modrf,test10)
test10$predRightrf <- predrf==test10$testing.classe
table(predrf,test10$testing.classe)

#prediction accuracy of RF model on a testing set
mean(test10$predRightrf)
