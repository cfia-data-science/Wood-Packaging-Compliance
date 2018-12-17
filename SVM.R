#data partitions
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]
#undersampled dataset
data_under<-ovun.sample(Compliant..Y.N.~., data=data_training, p=0.5, seed=1,  method="under")$data
#oversampled dataset
data_over<-ovun.sample(Compliant..Y.N.~., data=data_training,  p=0.5, seed=1,  method="over")$data

#SVM model without data balancing
svm_model <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                                       data=data)
pred_SVM <- predict(svm_model, newdata= data_testing, type = "class")

confusionMatrix(pred_SVM, data_testing$Compliant..Y.N.)

#SVM model with undersampling
svm_model_under <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                 data=data_under)
pred_SVM_under <- predict(svm_model_under, newdata= data_testing, type = "class")

confusionMatrix(pred_SVM, data_testing$Compliant..Y.N.)


#SVM model with oversampling
svm_model_over <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                       data=data_over)
pred_SVM_over <- predict(svm_model_over, newdata= data_testing, type = "class")

confusionMatrix(pred_SVM, data_testing$Compliant..Y.N.)
