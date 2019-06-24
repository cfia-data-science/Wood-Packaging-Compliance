# naive bayes model
set.seed(1)
nb_original <- train(x = data_training[, c(3, 8, 9, 10)],
                     y = data_training$Compliant..Y.N.,
                     method = "nb",
                     tuneLength = 10,
                     metric = "ROC",
                     na.action = na.pass,
                     trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5,
                                              classProbs = TRUE,
                                              summaryFunction = twoClassSummary))

# original model predictions
predictions_nb_original <- predict(nb_original, data_testing)
cm_nb_original <- confusionMatrix(predictions_nb_original, data_testing$Compliant..Y.N.)
cm_nb_original
cm_nb_original$byClass["F1"]
gmean_nb_original <- unname((cm_nb_original$byClass["Specificity"] * cm_nb_original$byClass["Sensitivity"]) ^ 0.5)
gmean_nb_original

# naive bayes using under-sampled data
set.seed(3)
nb_under <- train(x = data_under[, c(3, 8, 9, 10)],
                  y = data_under$Compliant..Y.N.,
                  method = "nb",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# under-sampled model predictions
predictions_nb_under <- predict(nb_under, data_testing)
cm_nb_under <- confusionMatrix(predictions_nb_under, data_testing$Compliant..Y.N.)
cm_nb_under
cm_nb_under$byClass["F1"]
gmean_nb_under <- unname((cm_nb_under$byClass["Specificity"] * cm_nb_under$byClass["Sensitivity"]) ^ 0.5)
gmean_nb_under

# naive bayes using over-sampled data
set.seed(4)
nb_over <- train(x = data_over[, c(3, 8, 9, 10)],
                 y = data_over$Compliant..Y.N.,
                 method = "nb",
                 tuneLength = 10,
                 metric = "ROC",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 5,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary))

# over-sampled model predictions
predictions_nb_over <- predict(nb_over, data_testing)
cm_nb_over <- confusionMatrix(predictions_nb_over, data_testing$Compliant..Y.N.)
cm_nb_over
cm_nb_over$byClass["F1"]
gmean_nb_over <- unname((cm_nb_over$byClass["Specificity"] * cm_nb_over$byClass["Sensitivity"]) ^ 0.5)
gmean_nb_over

# naive bayes using rose data
set.seed(5)
nb_rose <- train(x = data_rose[, c(3, 8, 9, 10)],
                 y = data_rose$Compliant..Y.N.,
                 method = "nb",
                 tuneLength = 10,
                 metric = "ROC",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 5,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary))

# rose model predictions
predictions_nb_rose <- predict(nb_rose, data_testing)
cm_nb_rose <- confusionMatrix(predictions_nb_rose, data_testing$Compliant..Y.N.)
cm_nb_rose
cm_nb_rose$byClass["F1"]
gmean_nb_rose <- unname((cm_nb_rose$byClass["Specificity"] * cm_nb_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_nb_rose

# naive bayes using smote data
set.seed(6)
nb_smote <- train(x = data_smote[, c(3, 8, 9, 10)],
                  y = data_smote$Compliant..Y.N.,
                  method = "nb",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# smote model predictions
predictions_nb_smote <- predict(nb_smote, data_testing)
cm_nb_smote <- confusionMatrix(predictions_nb_smote, data_testing$Compliant..Y.N.)
cm_nb_smote
cm_nb_smote$byClass["F1"]
gmean_nb_smote <- unname((cm_nb_smote$byClass["Specificity"] * cm_nb_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_nb_smote