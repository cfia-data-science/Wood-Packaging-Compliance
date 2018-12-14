library(caret)
library(e1071)
library(ROSE)
library(naivebayes)
set.seed(7267166)

#data partition
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]

#undersampled dataset
data_under<-ovun.sample(Compliant..Y.N.~., data=data_training, p=0.5, seed=1,  method="under")$data

#Create model 
NBclassfier <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                         data=data)

#make predictions on testing set
pred_NB <- predict(NBclassfier, newdata= cart_data_testing, type = "class")

#confusion matrix
confusionMatrix(pred_NB, cart_data_testing$Compliant..Y.N.)

#undersampled model
NBclassfier_under <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                          data=data_under)

#Making predictions on undersampled model
pred_NB_under <- predict(NBclassfier_under, newdata= cart_data_testing, type = "class")

#confusion matrix
confusionMatrix(pred_NB_under, cart_data_testing$Compliant..Y.N.)

#oversampled dataset
data_over<-ovun.sample(Compliant..Y.N.~., data=data_training,  p=0.5, seed=1,  method="over")$data

#oversampled model
NBclassfier_over <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                                data=data_over)

#Making predictions on oversampled model
pred_NB_over <- predict(NBclassfier_over, newdata= cart_data_testing, type = "class")

#confusion matrix
confusionMatrix(pred_NB_over, cart_data_testing$Compliant..Y.N.)
