# j48 decision tree
set.seed(1)
j48_original <- train(x = data_training[, c(3, 8, 9, 10)],
                      y = data_training$Compliant..Y.N.,
                      method = "J48",
                      tuneLength = 10,
                      metric = "ROC",
                      trControl = trainControl(method = "cv",
                                               number = 10,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary))

# original model predictions
predictions_j48_original <- predict(j48_original, data_testing)
cm_j48_original <- confusionMatrix(predictions_j48_original, data_testing$Compliant..Y.N.)
cm_j48_original
cm_j48_original$byClass["F1"]
gmean_j48_original <- unname((cm_j48_original$byClass["Specificity"] * cm_j48_original$byClass["Sensitivity"]) ^ 0.5)
gmean_j48_original

# j48 decision tree using under-sampled data
set.seed(3)
j48_under <- train(x = data_under[, c(3, 8, 9, 10)],
                   y = data_under$Compliant..Y.N.,
                   method = "J48",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))

# under-sampled model predictions
predictions_j48_under <- predict(j48_under, data_testing)
cm_j48_under <- confusionMatrix(predictions_j48_under, data_testing$Compliant..Y.N.)
cm_j48_under
cm_j48_under$byClass["F1"]
gmean_j48_under <- unname((cm_j48_under$byClass["Specificity"] * cm_j48_under$byClass["Sensitivity"]) ^ 0.5)
gmean_j48_under

# j48 decision tree using over-sampled data
set.seed(4)
j48_over <- train(x = data_over[, c(3, 8, 9, 10)],
                  y = data_over$Compliant..Y.N.,
                  method = "J48",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# over-sampled model predictions
predictions_j48_over <- predict(j48_over, data_testing)
cm_j48_over <- confusionMatrix(predictions_j48_over, data_testing$Compliant..Y.N.)
cm_j48_over
cm_j48_over$byClass["F1"]
gmean_j48_over <- unname((cm_j48_over$byClass["Specificity"] * cm_j48_over$byClass["Sensitivity"]) ^ 0.5)
gmean_j48_over

# j48 decision tree using rose data
set.seed(5)
j48_rose <- train(x = data_rose[, c(3, 8, 9, 10)],
                  y = data_rose$Compliant..Y.N.,
                  method = "J48",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# rose model predictions
predictions_j48_rose <- predict(j48_rose, data_testing)
cm_j48_rose <- confusionMatrix(predictions_j48_rose, data_testing$Compliant..Y.N.)
cm_j48_rose
cm_j48_rose$byClass["F1"]
gmean_j48_rose <- unname((cm_j48_rose$byClass["Specificity"] * cm_j48_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_j48_rose

# j48 decision tree using smote data
set.seed(6)
j48_smote <- train(x = data_smote[, c(3, 8, 9, 10)],
                   y = data_smote$Compliant..Y.N.,
                   method = "J48",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))

# smote model predictions
predictions_j48_smote <- predict(j48_smote, data_testing)
cm_j48_smote <- confusionMatrix(predictions_j48_smote, data_testing$Compliant..Y.N.)
cm_j48_smote
cm_j48_smote$byClass["F1"]
gmean_j48_smote <- unname((cm_j48_smote$byClass["Specificity"] * cm_j48_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_j48_smote

# comparison between different j48 models
j48_models <- list(original = j48_original,
                    weighted = j48_weighted,
                    under = j48_under,
                    over = j48_over,
                    rose = j48_rose,
                    smote = j48_smote)
j48_models_resampling <- resamples(j48_models)
summary(j48_models_resampling)
bwplot(j48_models_resampling)

j48_models_roc <- j48_models %>%
  map(test_roc, data = data_testing)
j48_models_roc %>%
  map(auc)

j48_results_roc <- list(NA)
num_model <- 1
for(roc in j48_models_roc){
  j48_results_roc[[num_model]] <- 
    data_frame(TPR = roc$sensitivities,
               FPR = 1 - roc$specificities,
               model = names(j48_models)[num_model])
  num_model <- num_model + 1
}
j48_results_roc <- bind_rows(j48_results_roc)

# plot ROC curve for all 6 j48 models
ggplot_j48_roc_curve <- ggplot(aes(x = FPR,  y = TPR, groover = model), data = j48_results_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00")) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
plot(ggplot_j48_roc_curve)