rtree.fit <- rpart(Compliant..Y.N.~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Month + Packaging.Material , 
                   data=cart_data_training,
                   method="class",
                   parms = list(prior = c( 1-0.3 , 0.3), split = "gini"),
                   control=rpart.control(minsplit=30,cp=0.001, usesurrogate = 2))

rtree.fit2 <- rpart(Compliant..Y.N.~ Shipper.Country + Port.of.Entry..map. + Goods.Category + Month , 
                   data=cart_data_training,
                   method="class",
                   parms = list(prior = c( 1-0.25 , 0.25), split = "gini"),
                   control=rpart.control(minsplit=30,cp=0.001, usesurrogate = 2))


pred <- predict(rtree.fit,cart_data_testing, type="class")

confusionMatrix(pred, cart_data_testing$Compliant..Y.N.)

rpart.plot(rtree.fit, main = "CART Using Original Model", box.palette = "Reds")

df_predicting <- data.frame(
  Port.of.Entry..map. = "Halifax, Nova Scotia",
  Shipper.Country = "Albania",
  Goods.Category = "Building Materials",
  Month = "06"
)

p <- predict(rtree.fit, newdata = df_predicting, type = "class")

p

df_predictingTemp <- data.frame(
  Packaging.Material = "aedfsdf",
  Port.of.Entry..map. = "Halifax, Nova Scotia",
  Shipper.Country = "Albania",
  Goods.Category = "Building Materials",
  Month = "06"
)
df_predicting <- subset(df_predictingTemp, select = -Packaging.Material)
df_predicting

table(pred,cart_data_training$Compliant..Y.N.)

plot(rtree.fit, uniform=TRUE, 
     main="Regression Tree")

text(rtree.fit, use.n=TRUE, all=TRUE, cex=.8,minlength=4)

search()
library(help = "caret")
browseVignettes()
library("rpart")
getModelInfo(model = "rpart")

fit <- rpart(Price ~ Mileage + Type + Country, cu.summary)
plot(fit, compress = TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=.8)

printcp(rtree.fit)
plotcp(rtree.fit)
rsq.rpart(rtree.fit)
summary(rtree.fit)
