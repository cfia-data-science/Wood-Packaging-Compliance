# naive bayes model
set.seed(1)
rf_original <- train(x = data_training[, c(3, 8, 9, 10)],
                     y = data_training$Compliant..Y.N.,
                     method = "ranger",
                     tuneLength = 10,
                     metric = "ROC",
                     trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5,
                                              classProbs = TRUE,
                                              summaryFunction = twoClassSummary))

# original model predictions
predictions_rf_original <- predict(rf_original, data_testing)
cm_rf_original <- confusionMatrix(predictions_rf_original, data_testing$Compliant..Y.N.)
cm_rf_original
cm_rf_original$byClass["F1"]
gmean_rf_original <- unname((cm_rf_original$byClass["Specificity"] * cm_rf_original$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_original

# naive bayes using under-sampled data
set.seed(3)
rf_under <- train(x = data_under[, c(3, 8, 9, 10)],
                  y = data_under$Compliant..Y.N.,
                  method = "ranger",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# under-sampled model predictions
predictions_rf_under <- predict(rf_under, data_testing)
cm_rf_under <- confusionMatrix(predictions_rf_under, data_testing$Compliant..Y.N.)
cm_rf_under
cm_rf_under$byClass["F1"]
gmean_rf_under <- unname((cm_rf_under$byClass["Specificity"] * cm_rf_under$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_under

# naive bayes using over-sampled data
set.seed(4)
rf_over <- train(x = data_over[, c(3, 8, 9, 10)],
                 y = data_over$Compliant..Y.N.,
                 method = "ranger",
                 tuneLength = 10,
                 metric = "ROC",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 5,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary))

# over-sampled model predictions
predictions_rf_over <- predict(rf_over, data_testing)
cm_rf_over <- confusionMatrix(predictions_rf_over, data_testing$Compliant..Y.N.)
cm_rf_over
cm_rf_over$byClass["F1"]
gmean_rf_over <- unname((cm_rf_over$byClass["Specificity"] * cm_rf_over$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_over

# naive bayes using rose data
set.seed(5)
rf_rose <- train(x = data_rose[, c(3, 8, 9, 10)],
                 y = data_rose$Compliant..Y.N.,
                 method = "ranger",
                 tuneLength = 10,
                 metric = "ROC",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 5,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary))

# rose model predictions
predictions_rf_rose <- predict(rf_rose, data_testing)
cm_rf_rose <- confusionMatrix(predictions_rf_rose, data_testing$Compliant..Y.N.)
cm_rf_rose
cm_rf_rose$byClass["F1"]
gmean_rf_rose <- unname((cm_rf_rose$byClass["Specificity"] * cm_rf_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_rose

# naive bayes using smote data
set.seed(6)
rf_smote <- train(x = data_smote[, c(3, 8, 9, 10)],
                  y = data_smote$Compliant..Y.N.,
                  method = "ranger",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# smote model predictions
predictions_rf_smote <- predict(rf_smote, data_testing)
cm_rf_smote <- confusionMatrix(predictions_rf_smote, data_testing$Compliant..Y.N.)
cm_rf_smote
cm_rf_smote$byClass["F1"]
gmean_rf_smote <- unname((cm_rf_smote$byClass["Specificity"] * cm_rf_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_smote