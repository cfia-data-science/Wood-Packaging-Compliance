# targets of the current scenario
data_targeted <- data[data$Shipper.Country == "China"
                         | data$Shipper.Country == "India"
                         | data$Shipper.Country == "Hong Kong"
                         | data$Shipper.Country == "Turkey"
                         | data$Shipper.Country == "Pakistan"
                         | data$Shipper.Country == "Taiwan"
                         | data$Shipper.Country == "Republic of Korea"
                         | data$Shipper.Country == "Vietnam"
                         | data$Goods.Category == "Tiles"
                         | (data$Shipper.Country == "Italy" & data$Goods.Category == "Furniture")
                         | ((data$Shipper.Country == "France" | data$Shipper.Country == "Arab Emirats") & data$Goods.Category == "Machinery, Vehicles and Auto-parts")
                         | data$Goods.Category == "Quarry Products"
                         | (data$Goods.Category == "Building Material" & data$Shipper.Country == "Brazil"), ]
nrow(data_targeted[data_targeted$Compliant..Y.N. == "N", ])
(nrow(data[data$Compliant..Y.N. == "N", ]) - nrow(data_targeted[data_targeted$Compliant..Y.N. == "N", ])) / nrow(data[data$Compliant..Y.N. == "N", ])