library(caret)
library(e1071)
library(ROSE)
library(naivebayes)
set.seed(7267166)

#data partition
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]

data_under<-ovun.sample(Compliant..Y.N.~., data=data_training, p=0.5, seed=1,  method="under")

#Create model
NBclassfier <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Packaging.Material , 
                         data=data_training)

#make predictions on testing set
testPred <- predict(NBclassfier, newdata= cart_data_testing, type = "class")

#confusion matrix
confusionMatrix(testPred, cart_data_testing$Compliant..Y.N.)


df_predicting <- data.frame(
  Packaging.Material = "MP",
  Port.of.Entry..map. = "Halifax, Nova Scotia",
  Shipper.Country = "Albania",
  Goods.Category = "Building Materials",
  Month = "06"
)

p <- predict(NBclassfier,newdata= df_predicting , type = "class")
p
