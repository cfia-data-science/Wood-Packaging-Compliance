library(OneR)

#data partition
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]

#train model
model <- OneR(data_training[,c(1:10,12)], verbose = TRUE)

#make predictions
predictions <- predict(model, data_testing)


#confusion Matrix of Predictions
table(predictions,data_testing$Compliant..Y.N.)
