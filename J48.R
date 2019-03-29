# J48 decision tree
set.seed(1)
J48_original <- train(x = data_training[, c(3, 8, 9, 10)],
                      y = data_training$Compliant..Y.N.,
                      method = "J48",
                      tuneLength = 10,
                      metric = "ROC",
                      na.action = na.pass,
                      trControl = trainControl(method = "repeatedcv",
                                               number = 10,
                                               repeats = 5,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary))

# original model predictions
predictions_J48_original <- predict(J48_original, data_testing)
cm_J48_original <- confusionMatrix(predictions_J48_original, data_testing$Compliant..Y.N.)
cm_J48_original
cm_J48_original$byClass["F1"]
gmean_J48_original <- unname((cm_J48_original$byClass["Specificity"] * cm_J48_original$byClass["Sensitivity"]) ^ 0.5)
gmean_J48_original

# J48 decision tree using under-sampled data
set.seed(3)
J48_under <- train(x = data_under[, c(3, 8, 9, 10)],
                   y = data_under$Compliant..Y.N.,
                   method = "J48",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))

# under-sampled model predictions
predictions_J48_under <- predict(J48_under, data_testing)
cm_J48_under <- confusionMatrix(predictions_J48_under, data_testing$Compliant..Y.N.)
cm_J48_under
cm_J48_under$byClass["F1"]
gmean_J48_under <- unname((cm_J48_under$byClass["Specificity"] * cm_J48_under$byClass["Sensitivity"]) ^ 0.5)
gmean_J48_under

# J48 decision tree using over-sampled data
set.seed(4)
J48_over <- train(x = data_over[, c(3, 8, 9, 10)],
                  y = data_over$Compliant..Y.N.,
                  method = "J48",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# over-sampled model predictions
predictions_J48_over <- predict(J48_over, data_testing)
cm_J48_over <- confusionMatrix(predictions_J48_over, data_testing$Compliant..Y.N.)
cm_J48_over
cm_J48_over$byClass["F1"]
gmean_J48_over <- unname((cm_J48_over$byClass["Specificity"] * cm_J48_over$byClass["Sensitivity"]) ^ 0.5)
gmean_J48_over

# J48 decision tree using rose data
set.seed(5)
J48_rose <- train(x = data_rose[, c(3, 8, 9, 10)],
                  y = data_rose$Compliant..Y.N.,
                  method = "J48",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# rose model predictions
predictions_J48_rose <- predict(J48_rose, data_testing)
cm_J48_rose <- confusionMatrix(predictions_J48_rose, data_testing$Compliant..Y.N.)
cm_J48_rose
cm_J48_rose$byClass["F1"]
gmean_J48_rose <- unname((cm_J48_rose$byClass["Specificity"] * cm_J48_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_J48_rose

# J48 decision tree using smote data
set.seed(6)
J48_smote <- train(x = data_smote[, c(3, 8, 9, 10)],
                   y = data_smote$Compliant..Y.N.,
                   method = "J48",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))

# smote model predictions
predictions_J48_smote <- predict(J48_smote, data_testing)
cm_J48_smote <- confusionMatrix(predictions_J48_smote, data_testing$Compliant..Y.N.)
cm_J48_smote
cm_J48_smote$byClass["F1"]
gmean_J48_smote <- unname((cm_J48_smote$byClass["Specificity"] * cm_J48_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_J48_smote