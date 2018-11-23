# rename target values
data$Compliant..Y.N. <- factor(data$Compliant..Y.N., levels = c("Y", "N"))

# create variable "Month"
if(!("Month" %in% colnames(data))) {
  month <- format(ymd_hms(data$Exam.Date), "%m")
  data <- data.frame(data, Month = month)
}

# create variable "Coast"
if(!("Coast" %in% colnames(data))) {
  coast <- c()
  for(i in seq(nrow(data))) {
    if(grepl("British Columbia", data$Port.of.Entry..map.[i])) {
      coast <- c(coast, "West")
    } else {
      coast <- c(coast, "East")
    }
  }
  data <- data.frame(data, Coast = coast)
}

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
data$Shipper.Country[-which(data$Shipper.Country == "Excaps-Various Countries" | data$Shipper.Country == "Palestinian Territories")] <- countrycode(
  sourcevar = data$Shipper.Country[-which(data$Shipper.Country == "Excaps-Various Countries" | data$Shipper.Country == "Palestinian Territories")],
  origin = "country.name",
  destination = "fao.name"
)

# create variable "Continent"
if(!("Continent" %in% colnames(data))) {
  continent <- factor(
    countrycode(
      sourcevar = data$Shipper.Country,
      origin = "country.name",
      destination = "continent"
    )
  )
  data <- data.frame(data, Continent = continent)
}

# convert features into factor values
data$Shipper.Country <- factor(data$Shipper.Country)
data$Port.of.Entry..map. <- factor(data$Port.of.Entry..map.)
data$Packaging.Material <- factor(data$Packaging.Material)
data$Goods.Category <- factor(data$Goods.Category)
data$Goods.Description <- factor(data$Goods.Description)
data$IPPC.Mark <- factor(data$IPPC.Mark)
data$Month <- factor(data$Month)
data$Continent <- factor(data$Continent)

# replace missing goods category with "Empty"
data$Goods.Category <- as.character(data$Goods.Category)
data$Goods.Category[(is.na(data$Goods.Category) | data$Goods.Category == "")] <- "Empty"
data$Goods.Category <- factor(data$Goods.Category)

# make lists for Rdata
data$Shipper.Country <- gsub("the ", "", data$Shipper.Country, ignore.case = TRUE)
data$Shipper.Country <- factor(data$Shipper.Country)
data$Port.of.Entry..map. <- gsub("Canada", "By Air", data$Port.of.Entry..map., ignore.case = TRUE)
data$Port.of.Entry..map. <- factor(data$Port.of.Entry..map.)
list_shipper_country <- levels(data$Shipper.Country)
list_port_of_entry <- levels(data$Port.of.Entry..map.)
list_goods_category <- levels(data$Goods.Category)
list_month <- levels(data$Month)

# relabel minority countries as "Continent Other"
shipper_countries <- as.data.frame(table(data$Shipper.Country, useNA = "ifany"))
shipper_countries_minor <- subset(shipper_countries,
                                  shipper_countries[, 2] <= 5)
minor_idx <- which(data$Shipper.Country %in% shipper_countries_minor$Var1)
list_africa_other <- levels(
  factor(
    data[which(
      (data$Shipper.Country %in% shipper_countries_minor$Var1)
      & data$Continent == "Africa"), ]$Shipper.Country
  )
)
list_americas_other <- levels(
  factor(
    data[which(
      (data$Shipper.Country %in% shipper_countries_minor$Var1)
      & data$Continent == "Americas"), ]$Shipper.Country
  )
)
list_asia_other <- levels(
  factor(
    data[which(
      (data$Shipper.Country %in% shipper_countries_minor$Var1)
      & data$Continent == "Asia"), ]$Shipper.Country
  )
)
list_europe_other <- levels(
  factor(
    data[which(
      (data$Shipper.Country %in% shipper_countries_minor$Var1)
      & data$Continent == "Europe"), ]$Shipper.Country
  )
)
list_oceania_other <- levels(
  factor(
    data[which(
      (data$Shipper.Country %in% shipper_countries_minor$Var1)
      & data$Continent == "Oceania"), ]$Shipper.Country
  )
)
data$Shipper.Country <- as.character(data$Shipper.Country)
data$Shipper.Country[data$Shipper.Country %in% list_africa_other] <- "Africa Other"
data$Shipper.Country[data$Shipper.Country %in% list_americas_other] <- "Americas Other"
data$Shipper.Country[data$Shipper.Country %in% list_asia_other] <- "Asia Other"
data$Shipper.Country[data$Shipper.Country %in% list_europe_other] <- "Europe Other"
data$Shipper.Country[data$Shipper.Country %in% list_oceania_other] <- "Oceania Other"
data$Shipper.Country <- factor(data$Shipper.Country)

reduced_data<- data[c(3, 5, 8, 9, 10, 12, 21)]

data_numeric <- as.data.frame(sapply(reduced_data, norminal_to_numeric))

cormat <- cor(data_numeric[, -12], use = "pairwise.complete.obs")
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot_pearsoncormat <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_fixed()
plot(ggplot_pearsoncormat)

corrplot(melted_cormat, method = "circle")

cor(rank(reduced_data$Shipper.Name), rank(reduced_data$Compliant..Y.N.))
  