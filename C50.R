library(C50)

#data partition
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]


model<- C5.0(x = data_training[3], y = data_training$Compliant..Y.N.)

summary(model)

predictions <- predict(model, data_testing)

confusionMatrix(predictions, data_testing$Compliant..Y.N.)