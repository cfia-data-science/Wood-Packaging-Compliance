# rename target values
data$Compliant..Y.N. <- factor(data$Compliant..Y.N., levels = c("Y", "N"))

# fill up missing goods category
goods_description_goods_category <- data.frame(Goods.Description = data$Goods.Description,
                                               Goods.Category = data$Goods.Category)
goods_description_goods_category <- goods_description_goods_category[which(goods_description_goods_category$Goods.Category != ""), ]
goods_description_goods_category <- goods_description_goods_category[!duplicated(goods_description_goods_category[, "Goods.Description"]), ]
# hash: associative array/dictionary data structure
# hash(keys,
#      values)
hash_goods_description_goods_category <- hash(keys = goods_description_goods_category$Goods.Description,
                                              values = goods_description_goods_category$Goods.Category)
for(i in seq(nrow(data))) {
  if(data$Goods.Category[i] == "") {
    if(has.key(as.character(data$Goods.Description[i]), hash_goods_description_goods_category)) {
      data$Goods.Category[i] <- as.character(hash_goods_description_goods_category[[as.character(data$Goods.Description[i])]])
    }
  }
}

# uniform country names
data$Shipper.Country <- as.character(data$Shipper.Country)
data$Shipper.Country[which(data$Shipper.Country == "West Bank And Gaza Strip")] <- "Palestinian Territories"
data$Shipper.Country[which(data$Shipper.Country == "Belarus (Russian Ruble)")] <- "Belarus"
# countrycode: 
# 1. Converts long country names into one of many different coding schemes.
# 2. Translates from one scheme to another.
# 3. Converts country name or coding scheme to the official short English country name.
# 4. Creates a new variable with the name of the continent or region to which each country belongs.
# countrycode(sourcevar,
#             origin,
#             destination)
# sourcevar: vector which contains the codes or country names to be converted (character or factor)
# origin: coding scheme of origin
# destination: coding scheme of destination
data$Shipper.Country[-which(data$Shipper.Country == "Excaps-Various Countries" | data$Shipper.Country == "Palestinian Territories" | data$Shipper.Country == "Netherlands Antilles")] <- countrycode(
  sourcevar = data$Shipper.Country[-which(data$Shipper.Country == "Excaps-Various Countries" | data$Shipper.Country == "Palestinian Territories" | data$Shipper.Country == "Netherlands Antilles")],
  origin = "country.name",
  destination = "fao.name"
)

# convert features into factor values
data$Shipper.Country <- factor(data$Shipper.Country)
data$Port.of.Entry..map. <- factor(data$Port.of.Entry..map.)
data$Packaging.Material <- factor(data$Packaging.Material)
data$Goods.Category <- factor(data$Goods.Category)
data$Goods.Description <- factor(data$Goods.Description)

# replace "Canada" in port of entry with "N/A"
data$Port.of.Entry..map. <- as.character(data$Port.of.Entry..map.)
data$Port.of.Entry..map.[(is.na(data$Port.of.Entry..map.) | data$Port.of.Entry..map. == "" | data$Port.of.Entry..map. == "Canada")] <- "N/A"
data$Port.of.Entry..map. <- factor(data$Port.of.Entry..map.)

# replace missing goods category with "N/A"
data$Goods.Category <- as.character(data$Goods.Category)
data$Goods.Category[(is.na(data$Goods.Category) | data$Goods.Category == "")] <- "N/A"
data$Goods.Category <- factor(data$Goods.Category)

# make lists for Rdata
data$Shipper.Country <- gsub("the ", "", data$Shipper.Country, ignore.case = TRUE)
data$Shipper.Country <- factor(data$Shipper.Country)
data$Port.of.Entry..map. <- factor(data$Port.of.Entry..map.)
list_shipper_country <- levels(data$Shipper.Country)
list_port_of_entry <- levels(data$Port.of.Entry..map.)
list_goods_category <- levels(data$Goods.Category)

list_port_of_entry <- list_port_of_entry[-which(list_port_of_entry == "N/A")]
list_port_of_entry <- c(list_port_of_entry, "N/A")

list_goods_category <- list_goods_category[-which(list_goods_category == "N/A")]
list_goods_category <- c(list_goods_category, "N/A")

# stratified sampling
set.seed(123)
# createDataPartition(y,
#                     p,
#                     list)
# y: a vector of outcomes
# p: the percentage of data that goes to training
# list: logical - should the results be in a list (TRUE) or a matrix with the number of rows equal to floor(p * length(y)) and times columns
training_index <- createDataPartition(data$Compliant..Y.N., p = 0.8, list = FALSE)

# data partition
data_training <- data[training_index, ]
data_testing <- data[-training_index, ]

# create class weights (they sum to 1)
class_weights <- ifelse(data_training$Compliant..Y.N. == "Y",
                       (1/table(data_training$Compliant..Y.N.)[1]) * 0.5,
                       (1/table(data_training$Compliant..Y.N.)[2]) * 0.5)

# undersampled dataset
data_under <- ovun.sample(Compliant..Y.N. ~ ., data = data_training, p = 0.5, seed = 1,  method = "under")$data

# oversampled dataset
data_over <- ovun.sample(Compliant..Y.N. ~ ., data = data_training,  p = 0.5, seed = 1,  method = "over")$data

# rose
data_rose <- ROSE(Compliant..Y.N. ~ ., data = data_training, seed = 1)$data

# smote
data_smote <- SMOTE(Compliant..Y.N. ~ ., data = data_training, perc.over = 100, seed = 1)