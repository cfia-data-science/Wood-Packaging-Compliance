# stratified sampling
set.seed(123)
nnet_training_index <- createDataPartition(data$Compliant..Y.N., p = 0.75, list = FALSE)

# create training data
nnet_data_training <- data[nnet_training_index, ]
nnet_data_testing <- data[-nnet_training_index, ]

dummy <- dummyVars(" ~ .", data = nnet_data_training[, c(3, 8, 9, 10, 15, 21)], fullRank = TRUE)
nnet_data_training_transformed <- data.frame(predict(dummy, newdata = nnet_data_training[, c(3, 8, 9, 10, 15, 21)]))
nnet_data_training_transformed <- cbind(nnet_data_training_transformed, Compliant..Y.N. = nnet_data_training[, 12])
nnet_data_testing_transformed <- data.frame(predict(dummy, newdata = nnet_data_testing[, c(3, 8, 9, 10, 15, 21)]))
nnet_data_testing_transformed <- cbind(nnet_data_testing_transformed, Compliant..Y.N. = nnet_data_testing[, 12])

# create model weights (they sum to 1)
nnet_weights <- ifelse(nnet_data_training$Compliant..Y.N. == "Y",
                       (1/table(nnet_data_training$Compliant..Y.N.)[1]) * 0.5 * 10,
                       (1/table(nnet_data_training$Compliant..Y.N.)[2]) * 0.5 * 10)

nnet_grid <-  expand.grid(size = seq(from = 1, to = 30, by = 1),
                          decay = seq(from = 0.1, to = 0.5, by = 0.1))

nnet_weighted <- train(Compliant..Y.N. ~ .,
                       data = nnet_data_training_transformed,
                       weights = nnet_weights,
                       method = "nnet",
                       metric = "ROC",
                       na.action = na.pass,
                       tuneGrid = nnet_grid,
                       trControl = trainControl(method = "repeatedcv",
                                                number = 10,
                                                repeats = 5,
                                                classProbs = TRUE,
                                                summaryFunction = twoClassSummary))

#import the function from Github
source('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

plot.nnet(nnet_weighted$finalModel)

predictions_nnet_weighted <- predict(nnet_weighted, nnet_data_testing_transformed)
cm_nnet_weighted <- confusionMatrix(predictions_nnet_weighted, nnet_data_testing_transformed$Compliant..Y.N.)