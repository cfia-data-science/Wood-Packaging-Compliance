#data partition
training_index <- createDataPartition(data$Compliant..Y.N., p = 1/3, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]

#train model
model <- OneR(data_training[,c(12,1:11)], verbose = TRUE)

#make predictions
predictions <- predict(model, data_testing)

predictions


#confusion Matrix of Predictions
confusionMatrix(predictions, data_testing$Compliant..Y.N.)
