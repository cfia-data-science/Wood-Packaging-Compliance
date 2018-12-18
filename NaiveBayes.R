library(caret)
library(e1071)
library(ROSE)
library(naivebayes)
library(klaR)
set.seed(7267166)

#data partitions into training and testing sets
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]
#turning the training set into an undersampled dataset
data_under<-ovun.sample(Compliant..Y.N.~., data=data_training, p=0.5, seed=1,  method="under")$data
#turning the training set into an oversampled dataset
data_over<-ovun.sample(Compliant..Y.N.~., data=data_training,  p=0.5, seed=1,  method="over")$data


#Create NB model 
NBclassfier <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                         data=data)
#make predictions on testing set
pred_NB <- predict(NBclassfier, newdata= data_testing, type = "class")
#confusion matrix
confusionMatrix(pred_NB, data_testing$Compliant..Y.N.)


#create undersampled model
NBclassfier_under <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material,
                          data=data_under)
#Making predictions on undersampled model
pred_NB_under <- predict(NBclassfier_under, newdata= data_testing, type = "class")
#confusion matrix
confusionMatrix(pred_NB_under, data_testing$Compliant..Y.N.)

#Create oversampled model
NBclassfier_over <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                                data=data_over)
#Making predictions on oversampled model
pred_NB_over <- predict(NBclassfier_over, newdata= data_testing, type = "class")
#confusion matrix
confusionMatrix(pred_NB_over, data_testing$Compliant..Y.N.)


#Create naive bayes model with crossvalidation
NBclassfier_cv<- train(y = data_training$Compliant..Y.N., 
                        x = data_training[,c(3,8,9,10)], 
                        method = "nb",
                        tuneLength = 10,
                        metric = "ROC",
                        trControl = trainControl(method = "cv", number = 10, classProbs = TRUE))
#predictions
pred_NB_cv <- predict(NBclassfier_cv, newdata= data_testing, type = "raw")
#confusion matrix
confusionMatrix(pred_NB_cv, data_testing$Compliant..Y.N.)

#create NB model with cv
NBclassfier_cv_over<- train(y = data_training$Compliant..Y.N., 
                            x = data_training[,c(3,8,9,10)], 
                            method = "nb",  
                            trControl = trainControl(method = "cv", number = 10))

#Create NB oversampled model with CV
NBclassfier_cv_over<- train(y = data_over$Compliant..Y.N., 
                        x = data_over[,c(3,8,9,10)], 
                        tuneLength = 10,
                        metric = "ROC",
                        method = "nb",  
                        trControl = trainControl(method = "cv", number = 10, classProbs = TRUE))
#predictions
pred_NB_cv_over <- predict(NBclassfier_cv_over, newdata= data_testing, type = "raw")
#confusion matrix
confusionMatrix(pred_NB_cv_over, data_testing$Compliant..Y.N.)

#Create NB undersampled model with CV
NBclassfier_cv_under<- train(y = data_under$Compliant..Y.N., 
                            x = data_under[,c(3,8,9,10)], 
                            method = "nb",  
                            trControl = trainControl(method = "cv", number = 10))
#predictions
pred_NB_cv_under <- predict(NBclassfier_cv_under, newdata= data_testing, type = "raw")
#confusion matrix
confusionMatrix(pred_NB_cv_under, data_testing$Compliant..Y.N.)


# comparison between different models
models <- list(original = NBclassfier_cv,
               undersampled = NBclassfier_cv_under,
               oversampled = NBclassfier_cv_over)
models_resampling <- resamples(models)
summary(models_resampling)
bwplot(models_resampling)

NB_rose <- train(x = cart_data_training[, c(3,8,9,10)],
                   y = cart_data_training$Compliant..Y.N.,
                   method = "nb",
                   tuneLength = 10,
                   metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 5,
                                            classProbs = TRUE,
                                            summaryFunction = twoClassSummary,
                                            sampling = "rose"))

#predictions
pred_NB_rose<- predict(NB_rose, newdata= data_testing, type = "raw")
#confusion matrix
confusionMatrix(pred_NB_rose, data_testing$Compliant..Y.N.)

NB_smote <- train(x = cart_data_training[, c(3,8,9,10)],
                     y = cart_data_training$Compliant..Y.N.,
                     method = "rpart",
                     tuneLength = 10,
                     metric = "ROC",
                     trControl = trainControl(method = "repeatedcv",
                                              number = 10,
                                              repeats = 5,
                                              classProbs = TRUE,
                                              summaryFunction = twoClassSummary,
                                              sampling = "smote"))

#predictions
pred_NB_smote<- predict(NB_smote, newdata= data_testing, type = "raw")
#confusion matrix
confusionMatrix(pred_NB_smote, data_testing$Compliant..Y.N.)
