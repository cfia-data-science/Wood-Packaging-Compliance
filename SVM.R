#data partition
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]


#SVM model
svm_model <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                                       data=data)
pred_SVM <- predict(svm_model, newdata= data_testing, type = "class")

confusionMatrix(pred_SVM, data_testing$Compliant..Y.N.)
