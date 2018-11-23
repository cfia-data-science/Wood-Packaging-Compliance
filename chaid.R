# data preprocessing
data$Compliant..Y.N. <- factor(data$Compliant..Y.N., levels = c("Y", "N"))
data <- data[!(is.na(data$Goods.Category) | data$Goods.Category == ""), ]

# stratified sampling
set.seed(123)
chaid_training_index <- createDataPartition(data$Compliant..Y.N., p = 0.75, list = FALSE)

# create training data
chaid_data_training <- data[chaid_training_index, ]
chaid_data_testing <- data[-chaid_training_index, ]

# chaid decision tree
set.seed(1)
chaid_up <- train(x = chaid_data_training[, c(3, 8, 9, 10, 15, 21)],
                  y = chaid_data_training$Compliant..Y.N.,
                  method = "chaid",
                  tuneLength = 10,
                  metric = "ROC",
                  na.action = na.pass,
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary,
                                           sampling = "up"))
importance_chaid_up <- varImp(chaid_up, scale = FALSE)
plot(importance_chaid_up, main = "Variable Importance in Weighted Model")
plot(chaid_up$finalModel)

# weighted model predictions
predictions_chaid_up <- predict(chaid_up, chaid_data_testing)
cm_chaid_up <- confusionMatrix(predictions_chaid_up, chaid_data_testing$Compliant..Y.N.)