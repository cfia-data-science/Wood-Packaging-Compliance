# linear svm
set.seed(1)
svmLinear_original <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                          kernel = "linear",
                          data = data_training,
                          cross = 10)

# original model predictions
predictions_svmLinear_original <- predict(svmLinear_original, data_testing)
cm_svmLinear_original <- confusionMatrix(predictions_svmLinear_original, data_testing$Compliant..Y.N.)
cm_svmLinear_original
cm_svmLinear_original$byClass["F1"]
gmean_svmLinear_original <- unname((cm_svmLinear_original$byClass["Specificity"] * cm_svmLinear_original$byClass["Sensitivity"]) ^ 0.5)
gmean_svmLinear_original

# linear svm using under-sampled data
set.seed(3)
svmLinear_under <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                       kernel = "linear",
                       data = data_under,
                       cross = 10)

# under-sampled model predictions
predictions_svmLinear_under <- predict(svmLinear_under, data_testing)
cm_svmLinear_under <- confusionMatrix(predictions_svmLinear_under, data_testing$Compliant..Y.N.)
cm_svmLinear_under
cm_svmLinear_under$byClass["F1"]
gmean_svmLinear_under <- unname((cm_svmLinear_under$byClass["Specificity"] * cm_svmLinear_under$byClass["Sensitivity"]) ^ 0.5)
gmean_svmLinear_under

# linear svm using rose data
set.seed(5)
svmLinear_rose <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                      kernel = "linear",
                      data = data_rose,
                      cross = 10)

# rose model predictions
predictions_svmLinear_rose <- predict(svmLinear_rose, data_testing)
cm_svmLinear_rose <- confusionMatrix(predictions_svmLinear_rose, data_testing$Compliant..Y.N.)
cm_svmLinear_rose
cm_svmLinear_rose$byClass["F1"]
gmean_svmLinear_rose <- unname((cm_svmLinear_rose$byClass["Specificity"] * cm_svmLinear_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_svmLinear_rose

# radial based svm
set.seed(1)
svmRadial_original <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material, 
                          kernel = "radial",
                          data = data_training,
                          cross = 10)

# original model predictions
predictions_svmRadial_original <- predict(svmRadial_original, data_testing)
cm_svmRadial_original <- confusionMatrix(predictions_svmRadial_original, data_testing$Compliant..Y.N.)
cm_svmRadial_original
cm_svmRadial_original$byClass["F1"]
gmean_svmRadial_original <- unname((cm_svmRadial_original$byClass["Specificity"] * cm_svmRadial_original$byClass["Sensitivity"]) ^ 0.5)
gmean_svmRadial_original

# radial based svm using under-sampled data
set.seed(3)
svmRadial_under <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material, 
                       kernel = "radial",
                       data = data_under,
                       cross = 10)

# under-sampled model predictions
predictions_svmRadial_under <- predict(svmRadial_under, data_testing)
cm_svmRadial_under <- confusionMatrix(predictions_svmRadial_under, data_testing$Compliant..Y.N.)
cm_svmRadial_under
cm_svmRadial_under$byClass["F1"]
gmean_svmRadial_under <- unname((cm_svmRadial_under$byClass["Specificity"] * cm_svmRadial_under$byClass["Sensitivity"]) ^ 0.5)
gmean_svmRadial_under

# radial based svm using rose data
set.seed(5)
svmRadial_rose <- svm(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material, 
                      kernel = "radial",
                      data = data_rose,
                      cross = 10)

# rose model predictions
predictions_svmRadial_rose <- predict(svmRadial_rose, data_testing)
cm_svmRadial_rose <- confusionMatrix(predictions_svmRadial_rose, data_testing$Compliant..Y.N.)
cm_svmRadial_rose
cm_svmRadial_rose$byClass["F1"]
gmean_svmRadial_rose <- unname((cm_svmRadial_rose$byClass["Specificity"] * cm_svmRadial_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_svmRadial_rose