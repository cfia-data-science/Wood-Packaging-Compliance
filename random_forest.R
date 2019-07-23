# random forest model
set.seed(1)
rf_original <- train(x = data_training[, c(3, 8, 9, 10)],
                     y = data_training$Compliant..Y.N.,
                     method = "ranger",
                     importance = "impurity",
                     tuneLength = 10,
                     metric = "ROC",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = TRUE,
                                              summaryFunction = twoClassSummary))

# original model predictions
predictions_rf_original <- predict(rf_original, data_testing)
cm_rf_original <- confusionMatrix(predictions_rf_original, data_testing$Compliant..Y.N.)
cm_rf_original
cm_rf_original$byClass["F1"]
gmean_rf_original <- unname((cm_rf_original$byClass["Specificity"] * cm_rf_original$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_original

# random forest using weighted data
set.seed(2)
rf_weighted <- train(x = data_training[, c(3, 8, 9, 10)],
                     y = data_training$Compliant..Y.N.,
                     method = "ranger",
                     importance = "impurity",
                     tuneLength = 10,
                     weights = class_weights,
                     metric = "ROC",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = TRUE,
                                              summaryFunction = twoClassSummary))

# weighted model predictions
predictions_rf_weighted <- predict(rf_weighted, data_testing)
cm_rf_weighted <- confusionMatrix(predictions_rf_weighted, data_testing$Compliant..Y.N.)
cm_rf_weighted
cm_rf_weighted$byClass["F1"]
gmean_rf_weighted <- unname((cm_rf_weighted$byClass["Specificity"] * cm_rf_weighted$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_weighted

# random forest using under-sampled data
set.seed(3)
rf_under <- train(x = data_under[, c(3, 8, 9, 10)],
                  y = data_under$Compliant..Y.N.,
                  method = "ranger",
                  importance = "impurity",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# under-sampled model predictions
predictions_rf_under <- predict(rf_under, data_testing)
cm_rf_under <- confusionMatrix(predictions_rf_under, data_testing$Compliant..Y.N.)
cm_rf_under
cm_rf_under$byClass["F1"]
gmean_rf_under <- unname((cm_rf_under$byClass["Specificity"] * cm_rf_under$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_under

# random forest using over-sampled data
set.seed(4)
rf_over <- train(x = data_over[, c(3, 8, 9, 10)],
                 y = data_over$Compliant..Y.N.,
                 method = "ranger",
                 importance = "impurity",
                 tuneLength = 10,
                 metric = "ROC",
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary))

# over-sampled model predictions
predictions_rf_over <- predict(rf_over, data_testing)
cm_rf_over <- confusionMatrix(predictions_rf_over, data_testing$Compliant..Y.N.)
cm_rf_over
cm_rf_over$byClass["F1"]
gmean_rf_over <- unname((cm_rf_over$byClass["Specificity"] * cm_rf_over$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_over

# random forest using rose data
set.seed(5)
rf_rose <- train(x = data_rose[, c(3, 8, 9, 10)],
                 y = data_rose$Compliant..Y.N.,
                 method = "ranger",
                 importance = "impurity",
                 tuneLength = 10,
                 metric = "ROC",
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary))

# rose model predictions
predictions_rf_rose <- predict(rf_rose, data_testing)
cm_rf_rose <- confusionMatrix(predictions_rf_rose, data_testing$Compliant..Y.N.)
cm_rf_rose
cm_rf_rose$byClass["F1"]
gmean_rf_rose <- unname((cm_rf_rose$byClass["Specificity"] * cm_rf_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_rose

# random forest using smote data
set.seed(6)
rf_smote <- train(x = data_smote[, c(3, 8, 9, 10)],
                  y = data_smote$Compliant..Y.N.,
                  method = "ranger",
                  importance = "impurity",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# smote model predictions
predictions_rf_smote <- predict(rf_smote, data_testing)
cm_rf_smote <- confusionMatrix(predictions_rf_smote, data_testing$Compliant..Y.N.)
cm_rf_smote
cm_rf_smote$byClass["F1"]
gmean_rf_smote <- unname((cm_rf_smote$byClass["Specificity"] * cm_rf_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_rf_smote

# comparison between different random forest models
rf_models <- list(original = rf_original,
                  weighted = rf_weighted,
                  under = rf_under,
                  over = rf_over,
                  rose = rf_rose,
                  smote = rf_smote)
rf_models_resampling <- resamples(rf_models)
summary(rf_models_resampling)
bwplot(rf_models_resampling)

rf_models_roc <- rf_models %>%
  map(test_roc, data = data_testing)
rf_models_roc %>%
  map(auc)

rf_results_roc <- list(NA)
num_model <- 1
for(roc in rf_models_roc){
  rf_results_roc[[num_model]] <- 
    data_frame(TPR = roc$sensitivities,
               FPR = 1 - roc$specificities,
               model = names(rf_models)[num_model])
  num_model <- num_model + 1
}
rf_results_roc <- bind_rows(rf_results_roc)

# plot ROC curve for all 6 random forest models
ggplot_rf_roc_curve <- ggplot(aes(x = FPR,  y = TPR, groover = model), data = rf_results_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00")) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
plot(ggplot_rf_roc_curve)