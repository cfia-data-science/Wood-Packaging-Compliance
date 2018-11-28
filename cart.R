print("hello world")

# stratified sampling
set.seed(123)
# createDataPartition(y,
#                     p,
#                     list)
# y: a vector of outcomes
# p: the percentage of data that goes to training
# list: logical - should the results be in a list (TRUE) or a matrix with the number of rows equal to floor(p * length(y)) and times columns
cart_training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)

# create training data
cart_data_training <- reduced_data[cart_training_index, ]
cart_data_testing <- reduced_data[-cart_training_index, ]

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
cart_original <- train(x = cart_data_training[-6],
                       y = cart_data_training$Compliant..Y.N.,
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



predict(cart_original, data.frame(
    Packaging.Material = "WPM",
    Port.of.Entry..map. = "Halifax, Nova Scotia",
    Shipper.Country = "Albania",
    Goods.Category = "Building Materials",
    Month = "06",
    Shipper.Name = "MINOVA RSA"
))

# original model predictions
# predict: extract predictions and class probabilities from train objects
# predict(object,
#         newdata)
# object: a model object for which prediction is desired
# newdata: a new data frame for prediction
predictions_cart_original <- predict(cart_original, cart_data_testing)
# confusionMatrix: create a confusion matrix
# confusionMatrix(data,
#                 reference)
# data: a factor of predicted classes
# reference a factor of classes to be used as the true results
cm_cart_original <- confusionMatrix(predictions_cart_original, cart_data_testing$Compliant..Y.N.)
cm_cart_original

dim(cart_data_testing)

table(predictions_cart_original,cart_data_training$Compliant..Y.N.)
# create model weights (they sum to 1)
cart_weights <- ifelse(cart_data_training$Compliant..Y.N. == "Y",
                       (1/table(cart_data_training$Compliant..Y.N.)[1]) * 0.5,
                       (1/table(cart_data_training$Compliant..Y.N.)[2]) * 0.5)

# cart decision tree using weighted model
set.seed(2)
cart_weighted <- train(x = cart_data_training[, c(3, 8, 9, 10, 15, 21)],
                        y = cart_data_training$Compliant..Y.N.,
                        method = "blackboost",
                        tuneLength = 10,
                        weights = cart_weights,
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
predictions_cart_weighted <- predict(cart_weighted, cart_data_testing)
cm_cart_weighted <- confusionMatrix(predictions_cart_weighted, cart_data_testing$Compliant..Y.N.)

# cart decision tree using down-sampled model
set.seed(3)
cart_down <- train(x = cart_data_training[, c(3, 8, 9, 10, 15, 21)],
                    y = cart_data_training$Compliant..Y.N.,
                    method = "rpart",
                    tuneLength = 10,
                    metric = "ROC",
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10,
                                             repeats = 5,
                                             classProbs = TRUE,
                                             summaryFunction = twoClassSummary,
                                             sampling = "down"))
importance_cart_down <- varImp(cart_down, scale = FALSE)
plot(importance_cart_down, main = "Variable Importance in Down-sampled Model")
rpart.plot(cart_down$finalModel, main = "CART Using Down-sampled Model", box.palette = "Reds")

# down-sampled model predictions
predictions_cart_down <- predict(cart_down, cart_data_testing)
cm_cart_down <- confusionMatrix(predictions_cart_down, cart_data_testing$Compliant..Y.N.)

# cart decision tree using up-sampled model
set.seed(4)
cart_up <- train(x = cart_data_training[, c(3, 8, 9, 10, 15, 21)],
                  y = cart_data_training$Compliant..Y.N.,
                  method = "rpart",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary,
                                           sampling = "up"))
importance_cart_up <- varImp(cart_up, scale = FALSE)
plot(importance_cart_up, main = "Variable Importance in Up-sampled Model")
rpart.plot(cart_up$finalModel, main = "CART Using Up-sampled Model", box.palette = "Reds")

# up-sampled model predictions
predictions_cart_up <- predict(cart_up, cart_data_testing)
cm_cart_up <- confusionMatrix(predictions_cart_up, cart_data_testing$Compliant..Y.N.)

# rose sampling
rosest <- list(name = "ROSE",
               func = function(x, y) {
                 dat <- if (is.data.frame(x)) x else as.data.frame(x)
                 dat$.y <- y
                 dat <- ROSE(.y ~ ., data = dat, hmult.majo = 1, hmult.mino = 1)$data
                 list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                      y = dat$.y)
               },
               first = TRUE)

# cart decision tree using rose model
set.seed(5)
cart_rose <- train(x = cart_data_training[, c(3, 8, 9, 10, 15, 21)],
                    y = cart_data_training$Compliant..Y.N.,
                    method = "rpart",
                    tuneLength = 10,
                    metric = "ROC",
                    trControl = trainControl(method = "repeatedcv",
                                             number = 10,
                                             repeats = 5,
                                             classProbs = TRUE,
                                             summaryFunction = twoClassSummary,
                                             sampling = rosest))
importance_cart_rose <- varImp(cart_rose, scale = FALSE)
plot(importance_cart_rose, main = "Variable Importance in ROSE Model")
rpart.plot(cart_rose$finalModel, main = "CART Using ROSE Model", box.palette = "Reds")

# rose model predictions
predictions_cart_rose <- predict(cart_rose, cart_data_testing)
cm_cart_rose <- confusionMatrix(predictions_cart_rose, cart_data_testing$Compliant..Y.N.)

# smote sampling
smotest <- list(name = "SMOTE",
                func = function(x, y) {
                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y <- y
                  dat <- SMOTE(.y ~ ., data = dat, perc.over=100, k = 5)
                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                       y = dat$.y)
                },
                first = TRUE)

# cart decision tree using smote model
set.seed(6)
cart_smote <- train(x = cart_data_training[, c(3, 8, 9, 10, 15, 21)],
                     y = cart_data_training$Compliant..Y.N.,
                     method = "rpart",
                     tuneLength = 10,
                     metric = "ROC",
                     trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5,
                                              classProbs = TRUE,
                                              summaryFunction = twoClassSummary,
                                              sampling = smotest))
importance_cart_smote <- varImp(cart_smote, scale = FALSE)
plot(importance_cart_smote, main = "Variable Importance in SMOTE Model")
rpart.plot(cart_smote$finalModel, main = "CART Using SMOTE Model", box.palette = "Reds")

# smote model predictions
predictions_cart_smote <- predict(cart_smote, cart_data_testing)
cm_cart_smote <- confusionMatrix(predictions_cart_smote, cart_data_testing$Compliant..Y.N.)

# comparison between different models
models <- list(original = cart_original,
               weighted = cart_weighted,
               down = cart_down,
               up = cart_up,
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
  map(test_roc, data = cart_data_testing)
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
ggplot_roc_curve <- ggplot(aes(x = FPR,  y = TPR, group = model), data = results_roc) +
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
# plot(cart_down[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_down[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_down[["finalModel"]][["cptable"]][2:10, "CP"])),
#      type="o")
# plot(cart_up[["finalModel"]][["cptable"]][2:10, "CP"],
#      cart_up[["finalModel"]][["cptable"]][2:10, "rel error"],
#      xlim = rev(range(cart_up[["finalModel"]][["cptable"]][2:10, "CP"])),
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

plot(importance_cart_down, main = "Variable Importance in Down-sampled Model")
rpart.plot(cart_down$finalModel, main = "CART Using Down-sampled Model", box.palette = "Reds")

plot(importance_cart_up, main = "Variable Importance in Up-sampled Model")
rpart.plot(cart_up$finalModel, main = "CART Using Up-sampled Model", box.palette = "Reds")

plot(importance_cart_rose, main = "Variable Importance in ROSE Model")
rpart.plot(cart_rose$finalModel, main = "CART Using ROSE Model", box.palette = "Reds")

plot(importance_cart_smote, main = "Variable Importance in SMOTE Model")
rpart.plot(cart_smote$finalModel, main = "CART Using SMOTE Model", box.palette = "Reds")

bwplot(models_resampling)

plot(ggplot_roc_curve)

# close output file
dev.off()
