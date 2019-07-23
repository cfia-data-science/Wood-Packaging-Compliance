# c50 decision tree
set.seed(1)
c50_original <- train(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                      data = data_training,
                      method = "C5.0",
                      tuneLength = 10,
                      metric = "ROC",
                      trControl = trainControl(method = "cv",
                                               number = 10,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary))

# original model predictions
predictions_c50_original <- predict(c50_original, data_testing)
cm_c50_original <- confusionMatrix(predictions_c50_original, data_testing$Compliant..Y.N.)
cm_c50_original
cm_c50_original$byClass["F1"]
gmean_c50_original <- unname((cm_c50_original$byClass["Specificity"] * cm_c50_original$byClass["Sensitivity"]) ^ 0.5)
gmean_c50_original

# c50 decision tree using weighted data
set.seed(2)
c50_weighted <- train(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                      data = data_training,
                      method = "C5.0",
                      tuneLength = 10,
                      weights = class_weights,
                      metric = "ROC",
                      trControl = trainControl(method = "cv",
                                               number = 10,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary))

# weighted model predictions
predictions_c50_weighted <- predict(c50_weighted, data_testing)
cm_c50_weighted <- confusionMatrix(predictions_c50_weighted, data_testing$Compliant..Y.N.)
cm_c50_weighted
cm_c50_weighted$byClass["F1"]
gmean_c50_weighted <- unname((cm_c50_weighted$byClass["Specificity"] * cm_c50_weighted$byClass["Sensitivity"]) ^ 0.5)
gmean_c50_weighted

# c50 decision tree using under-sampled data
set.seed(3)
c50_under <- train(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                   data = data_under,
                   method = "C5.0",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))

# under-sampled model predictions
predictions_c50_under <- predict(c50_under, data_testing)
cm_c50_under <- confusionMatrix(predictions_c50_under, data_testing$Compliant..Y.N.)
cm_c50_under
cm_c50_under$byClass["F1"]
gmean_c50_under <- unname((cm_c50_under$byClass["Specificity"] * cm_c50_under$byClass["Sensitivity"]) ^ 0.5)
gmean_c50_under

# c50 decision tree using over-sampled data
set.seed(4)
c50_over <- train(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                  data = data_over,
                  method = "C5.0",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# over-sampled model predictions
predictions_c50_over <- predict(c50_over, data_testing)
cm_c50_over <- confusionMatrix(predictions_c50_over, data_testing$Compliant..Y.N.)
cm_c50_over
cm_c50_over$byClass["F1"]
gmean_c50_over <- unname((cm_c50_over$byClass["Specificity"] * cm_c50_over$byClass["Sensitivity"]) ^ 0.5)
gmean_c50_over

# c50 decision tree using rose data
set.seed(5)
c50_rose <- train(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                  data = data_rose,
                  method = "C5.0",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

# rose model predictions
predictions_c50_rose <- predict(c50_rose, data_testing)
cm_c50_rose <- confusionMatrix(predictions_c50_rose, data_testing$Compliant..Y.N.)
cm_c50_rose
cm_c50_rose$byClass["F1"]
gmean_c50_rose <- unname((cm_c50_rose$byClass["Specificity"] * cm_c50_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_c50_rose

# c50 decision tree using smote data
set.seed(6)
c50_smote <- train(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                   data = data_smote,
                   method = "C5.0",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))

# smote model predictions
predictions_c50_smote <- predict(c50_smote, data_testing)
cm_c50_smote <- confusionMatrix(predictions_c50_smote, data_testing$Compliant..Y.N.)
cm_c50_smote
cm_c50_smote$byClass["F1"]
gmean_c50_smote <- unname((cm_c50_smote$byClass["Specificity"] * cm_c50_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_c50_smote

# comparison between different c50 models
c50_models <- list(original = c50_original,
                   weighted = c50_weighted,
                   under = c50_under,
                   over = c50_over,
                   rose = c50_rose,
                   smote = c50_smote)
c50_models_resampling <- resamples(c50_models)
summary(c50_models_resampling)
bwplot(c50_models_resampling)

c50_models_roc <- c50_models %>%
  map(test_roc, data = data_testing)
c50_models_roc %>%
  map(auc)

c50_results_roc <- list(NA)
num_model <- 1
for(roc in c50_models_roc){
  c50_results_roc[[num_model]] <- 
    data_frame(TPR = roc$sensitivities,
               FPR = 1 - roc$specificities,
               model = names(c50_models)[num_model])
  num_model <- num_model + 1
}
c50_results_roc <- bind_rows(c50_results_roc)

# plot ROC curve for all 6 c50 models
ggplot_c50_roc_curve <- ggplot(aes(x = FPR,  y = TPR, groover = model), data = c50_results_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00")) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
plot(ggplot_c50_roc_curve)