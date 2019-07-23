# linear svm
set.seed(1)
lsvm_original <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                     data = data_training,
                     kernel = "linear",
                     cross = 10)

# original model predictions
predictions_lsvm_original <- predict(lsvm_original, data_testing)
cm_lsvm_original <- confusionMatrix(predictions_lsvm_original, data_testing$Compliant..Y.N.)
cm_lsvm_original
cm_lsvm_original$byClass["F1"]
gmean_lsvm_original <- unname((cm_lsvm_original$byClass["Specificity"] * cm_lsvm_original$byClass["Sensitivity"]) ^ 0.5)
gmean_lsvm_original

# linear svm using weighted data
set.seed(2)
lsvm_weighted <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                     data = data_training,
                     kernel = "linear",
                     class.weights = c(weight_Y, weight_N),
                     cross = 10)

# weighted model predictions
predictions_lsvm_weighted <- predict(lsvm_weighted, data_testing)
cm_lsvm_weighted <- confusionMatrix(predictions_lsvm_weighted, data_testing$Compliant..Y.N.)
cm_lsvm_weighted
cm_lsvm_weighted$byClass["F1"]
gmean_lsvm_weighted <- unname((cm_lsvm_weighted$byClass["Specificity"] * cm_lsvm_weighted$byClass["Sensitivity"]) ^ 0.5)
gmean_lsvm_weighted

# linear svm using under-sampled data
set.seed(3)
lsvm_under <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                  data = data_under,
                  kernel = "linear",
                  cross = 10)

# under-sampled model predictions
predictions_lsvm_under <- predict(lsvm_under, data_testing)
cm_lsvm_under <- confusionMatrix(predictions_lsvm_under, data_testing$Compliant..Y.N.)
cm_lsvm_under
cm_lsvm_under$byClass["F1"]
gmean_lsvm_under <- unname((cm_lsvm_under$byClass["Specificity"] * cm_lsvm_under$byClass["Sensitivity"]) ^ 0.5)
gmean_lsvm_under

# linear svm using over-sampled data
set.seed(4)
lsvm_over <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                 data = data_over,
                 kernel = "linear",
                 cross = 10)

# over-sampled model predictions
predictions_lsvm_over <- predict(lsvm_over, data_testing)
cm_lsvm_over <- confusionMatrix(predictions_lsvm_over, data_testing$Compliant..Y.N.)
cm_lsvm_over
cm_lsvm_over$byClass["F1"]
gmean_lsvm_over <- unname((cm_lsvm_over$byClass["Specificity"] * cm_lsvm_over$byClass["Sensitivity"]) ^ 0.5)
gmean_lsvm_over

# linear svm using rose data
set.seed(5)
lsvm_rose <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                 data = data_rose,
                 kernel = "linear",
                 cross = 10)

# rose model predictions
predictions_lsvm_rose <- predict(lsvm_rose, data_testing)
cm_lsvm_rose <- confusionMatrix(predictions_lsvm_rose, data_testing$Compliant..Y.N.)
cm_lsvm_rose
cm_lsvm_rose$byClass["F1"]
gmean_lsvm_rose <- unname((cm_lsvm_rose$byClass["Specificity"] * cm_lsvm_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_lsvm_rose

# linear svm using smote data
set.seed(6)
lsvm_smote <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                  data = data_smote,
                  kernel = "linear",
                  cross = 10)

# smote model predictions
predictions_lsvm_smote <- predict(lsvm_smote, data_testing)
cm_lsvm_smote <- confusionMatrix(predictions_lsvm_smote, data_testing$Compliant..Y.N.)
cm_lsvm_smote
cm_lsvm_smote$byClass["F1"]
gmean_lsvm_smote <- unname((cm_lsvm_smote$byClass["Specificity"] * cm_lsvm_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_lsvm_smote

# comparison between different linear svm models
lsvm_models <- list(original = lsvm_original,
                    weighted = lsvm_weighted,
                    under = lsvm_under,
                    over = lsvm_over,
                    rose = lsvm_rose,
                    smote = lsvm_smote)
lsvm_models_resampling <- resamples(lsvm_models)
summary(lsvm_models_resampling)
bwplot(lsvm_models_resampling)

lsvm_models_roc <- lsvm_models %>%
  map(test_roc, data = data_testing)
lsvm_models_roc %>%
  map(auc)

lsvm_results_roc <- list(NA)
num_model <- 1
for(roc in lsvm_models_roc){
  lsvm_results_roc[[num_model]] <- 
    data_frame(TPR = roc$sensitivities,
               FPR = 1 - roc$specificities,
               model = names(lsvm_models)[num_model])
  num_model <- num_model + 1
}
lsvm_results_roc <- bind_rows(lsvm_results_roc)

# plot ROC curve for all 6 linear svm models
ggplot_lsvm_roc_curve <- ggplot(aes(x = FPR,  y = TPR, groover = model), data = lsvm_results_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00")) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
plot(ggplot_lsvm_roc_curve)