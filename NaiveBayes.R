library(caret)
library(e1071)
set.seed(7267166)

NBclassfier = naiveBayes(Compliant..Y.N. ~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Month + Packaging.Material , 
                         data=cart_data_training)
print(NBclassfier)

testPred <- predict(NBclassfier, newdata= cart_data_testing, type = "class")

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
