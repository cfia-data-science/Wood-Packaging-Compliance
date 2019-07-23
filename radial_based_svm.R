# radial based svm
set.seed(1)
rsvm_original <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                     data = data_training,
                     kernel = "radial",
                     cross = 10)

# original model predictions
predictions_rsvm_original <- predict(rsvm_original, data_testing)
cm_rsvm_original <- confusionMatrix(predictions_rsvm_original, data_testing$Compliant..Y.N.)
cm_rsvm_original
cm_rsvm_original$byClass["F1"]
gmean_rsvm_original <- unname((cm_rsvm_original$byClass["Specificity"] * cm_rsvm_original$byClass["Sensitivity"]) ^ 0.5)
gmean_rsvm_original

# radial based svm using weighted data
set.seed(2)
rsvm_weighted <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                     data = data_training,
                     kernel = "radial",
                     class.weights = c(weight_Y, weight_N),
                     cross = 10)

# weighted model predictions
predictions_rsvm_weighted <- predict(rsvm_weighted, data_testing)
cm_rsvm_weighted <- confusionMatrix(predictions_rsvm_weighted, data_testing$Compliant..Y.N.)
cm_rsvm_weighted
cm_rsvm_weighted$byClass["F1"]
gmean_rsvm_weighted <- unname((cm_rsvm_weighted$byClass["Specificity"] * cm_rsvm_weighted$byClass["Sensitivity"]) ^ 0.5)
gmean_rsvm_weighted

# radial based svm using under-sampled data
set.seed(3)
rsvm_under <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                  data = data_under,
                  kernel = "radial",
                  cross = 10)

# under-sampled model predictions
predictions_rsvm_under <- predict(rsvm_under, data_testing)
cm_rsvm_under <- confusionMatrix(predictions_rsvm_under, data_testing$Compliant..Y.N.)
cm_rsvm_under
cm_rsvm_under$byClass["F1"]
gmean_rsvm_under <- unname((cm_rsvm_under$byClass["Specificity"] * cm_rsvm_under$byClass["Sensitivity"]) ^ 0.5)
gmean_rsvm_under

# radial based svm using over-sampled data
set.seed(4)
rsvm_over <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                 data = data_over,
                 kernel = "radial",
                 cross = 10)

# over-sampled model predictions
predictions_rsvm_over <- predict(rsvm_over, data_testing)
cm_rsvm_over <- confusionMatrix(predictions_rsvm_over, data_testing$Compliant..Y.N.)
cm_rsvm_over
cm_rsvm_over$byClass["F1"]
gmean_rsvm_over <- unname((cm_rsvm_over$byClass["Specificity"] * cm_rsvm_over$byClass["Sensitivity"]) ^ 0.5)
gmean_rsvm_over

# radial based svm using rose data
set.seed(5)
rsvm_rose <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                 data = data_rose,
                 kernel = "radial",
                 cross = 10)

# rose model predictions
predictions_rsvm_rose <- predict(rsvm_rose, data_testing)
cm_rsvm_rose <- confusionMatrix(predictions_rsvm_rose, data_testing$Compliant..Y.N.)
cm_rsvm_rose
cm_rsvm_rose$byClass["F1"]
gmean_rsvm_rose <- unname((cm_rsvm_rose$byClass["Specificity"] * cm_rsvm_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_rsvm_rose

# radial based svm using smote data
set.seed(6)
rsvm_smote <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                  data = data_smote,
                  kernel = "radial",
                  cross = 10)

# smote model predictions
predictions_rsvm_smote <- predict(rsvm_smote, data_testing)
cm_rsvm_smote <- confusionMatrix(predictions_rsvm_smote, data_testing$Compliant..Y.N.)
cm_rsvm_smote
cm_rsvm_smote$byClass["F1"]
gmean_rsvm_smote <- unname((cm_rsvm_smote$byClass["Specificity"] * cm_rsvm_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_rsvm_smote

# comparison between different radial based svm models
rsvm_models <- list(original = rsvm_original,
                    weighted = rsvm_weighted,
                    under = rsvm_under,
                    over = rsvm_over,
                    rose = rsvm_rose,
                    smote = rsvm_smote)
rsvm_models_resampling <- resamples(rsvm_models)
summary(rsvm_models_resampling)
bwplot(rsvm_models_resampling)

rsvm_models_roc <- rsvm_models %>%
  map(test_roc, data = data_testing)
rsvm_models_roc %>%
  map(auc)

rsvm_results_roc <- list(NA)
num_model <- 1
for(roc in rsvm_models_roc){
  rsvm_results_roc[[num_model]] <- 
    data_frame(TPR = roc$sensitivities,
               FPR = 1 - roc$specificities,
               model = names(rsvm_models)[num_model])
  num_model <- num_model + 1
}
rsvm_results_roc <- bind_rows(rsvm_results_roc)

# plot ROC curve for all 6 radial based svm models
ggplot_rsvm_roc_curve <- ggplot(aes(x = FPR,  y = TPR, groover = model), data = rsvm_results_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00")) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
plot(ggplot_rsvm_roc_curve)