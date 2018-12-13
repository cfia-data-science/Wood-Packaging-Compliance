library(caret)
library(e1071)
library(naivebayes)
set.seed(7267166)

#Create model
NBclassfier <- naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Month + Packaging.Material , 
                         data=cart_data_training)

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
