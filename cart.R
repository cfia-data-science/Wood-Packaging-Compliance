# cart decision tree
set.seed(1)
# train: fit predictive models over different tuning parameters
# train(x,
#       y,
#       method,
#       tuneLength,
#       weights,
#       metric,
#       na.action,
#       trControl)
# x: a data frame containing training data where samples are in rows and features are in columns
# y: a numeric or factor vector containing the outcome for each sample
# method: a string specifying which classification or regression model to use
# tuneLength: an integer denoting the number of levels for each tuning parameters
# weights: a numeric vector of case weights
# na.action: a function to specify the action to be taken if NAs are found
# trControl: a list of values that define how this function acts
cart_original <- train(x = data_training[, c(3, 8, 9, 10)],
                       y = data_training$Compliant..Y.N.,
                       method = "rpart",
                       tuneLength = 10,
                       metric = "ROC",
                       na.action = na.pass,
                       # trainControl: control parameters for train
                       # trainControl(method,
                       #              number,
                       #              repeats,
                       #              classProbs,
                       #              summaryFunction,
                       #              sampling)
                       # method: the resampling method
                       # number: the number of folds
                       # repeats: for repeated k-fold cross-validation only: the number of complete sets of folds to compute
                       # classProbs: a logical; should class probabilities be computed for classification models (along with predicted values) in each resample
                       # summaryFunction: a function to compute performance metrics across resamples
                       # sampling: a single character value describing the type of additional sampling that is conducted after resampling (usually to resolve class imbalances)
                       trControl = trainControl(method = "repeatedcv",
                                                number = 10,
                                                repeats = 5,
                                                classProbs = TRUE,
                                                summaryFunction = twoClassSummary))
# varImp: calculation of variable importance for regression and classification models
# varImp(object,
#        scale)
# object: an object corresponding to a fitted model
# scale: should the importance values be scaled to 0 and 100
importance_cart_original <- varImp(cart_original, scale = FALSE)
plot(importance_cart_original, main = "Variable Importance in Original Model")
rpart.plot(cart_original$finalModel, main = "CART Using Original Model", box.palette = "Reds")


# original model predictions
# predict: extract predictions and class probabilities from train objects
# predict(object,
#         newdata)
# object: a model object for which prediction is desired
# newdata: a new data frame for prediction
predictions_cart_original <- predict(cart_original, data_testing)
# confusionMatrix: create a confusion matrix
# confusionMatrix(data,
#                 reference)
# data: a factor of predicted classes
# reference a factor of classes to be used as the true results
cm_cart_original <- confusionMatrix(predictions_cart_original, data_testing$Compliant..Y.N.)
cm_cart_original
cm_cart_original$byClass["F1"]
gmean_cart_original <- unname((cm_cart_original$byClass["Specificity"] * cm_cart_original$byClass["Sensitivity"]) ^ 0.5)
gmean_cart_original

# cart decision tree using weighted data
set.seed(2)
cart_weighted <- train(x = data_training[, c(3, 8, 9, 10)],
                       y = data_training$Compliant..Y.N.,
                       method = "rpart",
                       tuneLength = 10,
                       weights = class_weights,
                       metric = "ROC",
                       na.action = na.pass,
                       trControl = trainControl(method = "repeatedcv",
                                                number = 10,
                                                repeats = 5,
                                                classProbs = TRUE,
                                                summaryFunction = twoClassSummary))
importance_cart_weighted <- varImp(cart_weighted, scale = FALSE)
plot(importance_cart_weighted, main = "Variable Importance in Weighted Model")
rpart.plot(cart_weighted$finalModel, main = "CART Using Weighted Model", box.palette = "Reds")

# weighted model predictions
predictions_cart_weighted <- predict(cart_weighted, data_testing)
cm_cart_weighted <- confusionMatrix(predictions_cart_weighted, data_testing$Compliant..Y.N.)
cm_cart_weighted
cm_cart_weighted$byClass["F1"]
gmean_cart_weighted <- unname((cm_cart_weighted$byClass["Specificity"] * cm_cart_weighted$byClass["Sensitivity"]) ^ 0.5)
gmean_cart_weighted

# cart decision tree using under-sampled data
set.seed(3)
cart_under <- train(x = data_under[, c(3, 8, 9, 10)],
                    y = data_under$Compliant..Y.N.,
                    method = "rpart",
                    tuneLength = 10,
                    metric = "ROC",
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10,
                                             repeats = 5,
                                             classProbs = TRUE,
                                             summaryFunction = twoClassSummary))
importance_cart_under <- varImp(cart_under, scale = FALSE)
plot(importance_cart_under, main = "Variable Importance in under-sampled Model")
rpart.plot(cart_under$finalModel, main = "CART Using under-sampled Model", box.palette = "Reds")

# under-sampled model predictions
predictions_cart_under <- predict(cart_under, data_testing)
cm_cart_under <- confusionMatrix(predictions_cart_under, data_testing$Compliant..Y.N.)
cm_cart_under
cm_cart_under$byClass["F1"]
gmean_cart_under <- unname((cm_cart_under$byClass["Specificity"] * cm_cart_under$byClass["Sensitivity"]) ^ 0.5)
gmean_cart_under

# cart decision tree using over-sampled data
set.seed(4)
cart_over <- train(x = data_over[, c(3, 8, 9, 10)],
                   y = data_over$Compliant..Y.N.,
                   method = "rpart",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))
importance_cart_over <- varImp(cart_over, scale = FALSE)
plot(importance_cart_over, main = "Variable Importance in over-sampled Model")
rpart.plot(cart_over$finalModel, main = "CART Using over-sampled Model", box.palette = "Reds")

# over-sampled model predictions
predictions_cart_over <- predict(cart_over, data_testing)
cm_cart_over <- confusionMatrix(predictions_cart_over, data_testing$Compliant..Y.N.)
cm_cart_over
cm_cart_over$byClass["F1"]
gmean_cart_over <- unname((cm_cart_over$byClass["Specificity"] * cm_cart_over$byClass["Sensitivity"]) ^ 0.5)
gmean_cart_over

# cart decision tree using rose data
set.seed(5)
cart_rose <- train(x = data_rose[, c(3, 8, 9, 10)],
                   y = data_rose$Compliant..Y.N.,
                   method = "rpart",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary))
importance_cart_rose <- varImp(cart_rose, scale = FALSE)
plot(importance_cart_rose, main = "Variable Importance in ROSE Model")
rpart.plot(cart_rose$finalModel, main = "CART Using ROSE Model", box.palette = "Reds")

# rose model predictions
predictions_cart_rose <- predict(cart_rose, data_testing)
cm_cart_rose <- confusionMatrix(predictions_cart_rose, data_testing$Compliant..Y.N.)
cm_cart_rose
cm_cart_rose$byClass["F1"]
gmean_cart_rose <- unname((cm_cart_rose$byClass["Specificity"] * cm_cart_rose$byClass["Sensitivity"]) ^ 0.5)
gmean_cart_rose

# cart decision tree using smote data
set.seed(6)
cart_smote <- train(x = data_smote[, c(3, 8, 9, 10)],
                    y = data_smote$Compliant..Y.N.,
                    method = "rpart",
                    tuneLength = 10,
                    metric = "ROC",
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10,
                                             repeats = 5,
                                             classProbs = TRUE,
                                             summaryFunction = twoClassSummary))
importance_cart_smote <- varImp(cart_smote, scale = FALSE)
plot(importance_cart_smote, main = "Variable Importance in SMOTE Model")
rpart.plot(cart_smote$finalModel, main = "CART Using SMOTE Model", box.palette = "Reds")

# smote model predictions
predictions_cart_smote <- predict(cart_smote, data_testing)
cm_cart_smote <- confusionMatrix(predictions_cart_smote, data_testing$Compliant..Y.N.)
cm_cart_smote
cm_cart_smote$byClass["F1"]
gmean_cart_smote <- unname((cm_cart_smote$byClass["Specificity"] * cm_cart_smote$byClass["Sensitivity"]) ^ 0.5)
gmean_cart_smote

# comparison between different models
models <- list(original = cart_original,
               weighted = cart_weighted,
               under = cart_under,
               over = cart_over,
               rose = cart_rose,
               smote = cart_smote)
models_resampling <- resamples(models)
summary(models_resampling)
bwplot(models_resampling)

test_roc <- function(model, data) {
  # roc: build a ROC curve
  # roc(response,
  #     predictor)
  # response: a factor, numeric or character vector of responses, typically encoded with 0 (controls) and 1 (cases)
  # predictor: a numeric vector of the same length than response, containing the predicted value of each observation
  roc(data$Compliant..Y.N.,
      predict(model, data, type = "prob")[, "N"])
}
models_roc <- models %>%
  map(test_roc, data = data_testing)
models_roc %>%
  map(auc)

results_roc <- list(NA)
num_model <- 1
for(roc in models_roc){
  results_roc[[num_model]] <- 
    data_frame(TPR = roc$sensitivities,
               FPR = 1 - roc$specificities,
               model = names(models)[num_model])
  num_model <- num_model + 1
}
results_roc <- bind_rows(results_roc)

# Plot ROC curve for all 6 models
ggplot_roc_curve <- ggplot(aes(x = FPR,  y = TPR, groover = model), data = results_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00")) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
plot(ggplot_roc_curve)

# plot(cart_original[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_original[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_original[["finalModel"]][["cptable"]][2:10, "CP"])),
#      type="o")
# plot(cart_weighted[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_weighted[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_weighted[["finalModel"]][["cptable"]][2:10, "CP"])),
#      type="o")
# plot(cart_under[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_under[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_under[["finalModel"]][["cptable"]][2:10, "CP"])),
#      type="o")
# plot(cart_over[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_over[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_over[["finalModel"]][["cptable"]][2:10, "CP"])),
#      type="o")
# plot(cart_rose[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_rose[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_rose[["finalModel"]][["cptable"]][2:10, "CP"])),
#      type="o")
# plot(cart_smote[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_smote[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_smote[["finalModel"]][["cptable"]][2:10, "CP"])),
#      type="o")

# open output file
pdf(file = "./diagrams/pdf/models.pdf", width = 12, height = 8.5)

plot(importance_cart_original, main = "Variable Importance in Original Model")
rpart.plot(cart_original$finalModel, main = "CART Using Original Model", box.palette = "Reds")

plot(importance_cart_weighted, main = "Variable Importance in Weighted Model")
rpart.plot(cart_weighted$finalModel, main = "CART Using Weighted Model", box.palette = "Reds")

plot(importance_cart_under, main = "Variable Importance in under-sampled Model")
rpart.plot(cart_under$finalModel, main = "CART Using under-sampled Model", box.palette = "Reds")

plot(importance_cart_over, main = "Variable Importance in over-sampled Model")
rpart.plot(cart_over$finalModel, main = "CART Using over-sampled Model", box.palette = "Reds")

plot(importance_cart_rose, main = "Variable Importance in ROSE Model")
rpart.plot(cart_rose$finalModel, main = "CART Using ROSE Model", box.palette = "Reds")

plot(importance_cart_smote, main = "Variable Importance in SMOTE Model")
rpart.plot(cart_smote$finalModel, main = "CART Using SMOTE Model", box.palette = "Reds")

bwplot(models_resampling)

plot(ggplot_roc_curve)

# close output file
dev.off()