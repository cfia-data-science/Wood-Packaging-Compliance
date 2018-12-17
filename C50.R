library(C50)

#data partition
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]

#create model
model<- C5.0(x = data_training[3], y = data_training$Compliant..Y.N.)

#show model error message
summary(model)

predictions <- predict(model, data_testing)

confusionMatrix(predictions, data_testing$Compliant..Y.N.)

#C5.0 using CARET Does not work
model_c5 <- train (y = data_training$Compliant..Y.N., 
              x = data_training[,c(3,8,9,10)], 
              method = "C5.0", 
              allowParallel = TRUE,
              trControl = trainControl(method = "cv", number = 10))


