# find redundancy in the dataset - pearson correlation matrix
data_numeric <- as.data.frame(sapply(data, norminal_to_numeric))
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

# find redundancy in the dataset - GKtau matrix
data_cor <- subset(data, select = c("IPPC.Mark", "Packaging.Material", "Port.of.Entry..map.", "Shipper.Country", "Goods.Category", "Month"))
data_cor <- data_cor[which(data_cor$Port.of.Entry..map. != "By Air"), ]
data_cor$Shipper.Country <- factor(data_cor$Shipper.Country)
data_cor$Port.of.Entry..map. <- factor(data_cor$Port.of.Entry..map.)
data_cor$IPPC.Mark <- factor(data_cor$IPPC.Mark)
GKmatrix <- GKtauDataframe(data_cor)
plot(GKmatrix)

# find compliance rate for wp packages
data_wp <- data %>% filter(Packaging.Material != "MP")
non_compliant_wp <- as.data.frame(table(data_wp$Compliant..Y.N., useNA = "ifany"))
ggplot(data = data_wp) + 
  geom_bar(width=0.4, mapping=aes(x= Compliant..Y.N., fill = Compliant..Y.N.))+
  theme(aspect.ratio = 2:5) + 
  scale_x_discrete(name = "Compliance")+
  scale_y_continuous("Count") +
  ggtitle("Compliance Rate for WP Packages", subtitle = "85.7% compliant") + 
  theme_minimal()


  # find compliance rate for non-wp packages
data_mp <- data %>% filter(Packaging.Material == "MP")
non_compliant_mp <- as.data.frame(table(data_mp$Compliant..Y.N., useNA = "ifany"))
ggplot(data = data_mp) + 
  geom_bar(mapping=aes(x= Compliant..Y.N., fill= Compliant..Y.N.), width=0.4 ) +
  theme(plot.background=element_blank()) +
  theme(panel.background=element_blank(), legend.position="none") + 
  theme(aspect.ratio = 3/100) + 
  scale_x_discrete(name = "Compliance")+
  scale_y_continuous("Count") +
  ggtitle("Compliance Rate for Non-WP Packages", subtitle = "99.678% compliant") + 
  theme_stata()

#find data distribution on shipper country
shipper_countries <- as.data.frame(table(data$Shipper.Country, useNA = "ifany"))
shipper_countries_major <- subset(shipper_countries,
                                  shipper_countries[, 2] > 300)
shipper_countries_minor <- subset(shipper_countries,
                                  shipper_countries[, 2] <= 300)

shipper_countries_major$Var1 <- as.character(shipper_countries_major$Var1)
shipper_countries <- rbind(shipper_countries_major,
                           list(Var1 = "Other",
                                Freq = sum(shipper_countries_minor$Freq)),
                           stringsAsFactors = FALSE)
shipper_countries$Var1 <- factor(shipper_countries$Var1)
# plot_ly(shipper_countries, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
#   layout(title = "Shipper Country",
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

shipper_countries_major<- shipper_countries_major[order(-shipper_countries_major$Freq),]

ggplot(data = shipper_countries) + 
  geom_bar(mapping=aes(x= reorder(Var1, -Freq), y = Freq, fill=Var1), width=0.8, stat = "identity") +
  theme(plot.background=element_blank()) +
  theme(panel.background=element_blank()) + 
  theme(aspect.ratio = 2/1.5) + 
  scale_x_discrete(name = "Country")+
  scale_y_continuous("Count") +
  ggtitle("Shipper Country", subtitle = "Count>300") + 
  theme_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")


# find data distribution on port of entry
port_of_entry <- as.data.frame(table(data$Port.of.Entry..map., useNA = "ifany"))
#plot_ly(port_of_entry, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  # layout(title = "Port of Entry",
  #        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
ggplot(data = port_of_entry) + 
  geom_bar(mapping=aes(x= reorder(Var1, -Freq) , y = Freq, fill = Var1), width=0.8, stat = "identity" ) +
  theme(plot.background=element_blank()) +
  theme(panel.background=element_blank()) + 
  theme(aspect.ratio = 2/1.5) + 
  scale_x_discrete(name = "Port of Entry")+
  scale_y_continuous("Count") +
  ggtitle("Port of Entry") + 
  theme_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")
  

# find data distribution on packaging material
packaging_material <- as.data.frame(table(data$Packaging.Material, useNA = "ifany"))
#plot_ly(packaging_material, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  # layout(title = "Packaging Material",
  #        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
ggplot(data = packaging_material, mapping = aes(reorder(Var1, -Freq) , y = Freq, fill = Var1)) + 
  geom_bar(width=0.5, stat = "identity") +
  theme(plot.background=element_blank()) +
  theme(panel.background=element_blank()) + 
  theme(aspect.ratio = 2/1.5) + 
  scale_x_discrete(name = "Packaging Material")+
  scale_y_continuous("Count") +
  ggtitle("Packaging Material") + 
  theme_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")

# find data distribution on goods category
goods_category <- as.data.frame(table(data$Goods.Category, useNA = "ifany"))
#plot_ly(goods_category, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  # layout(title = "Goods Category",
  #        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

ggplot(data = goods_category, mapping = aes(reorder(Var1, -Freq) , y = Freq, fill = Var1)) + 
  geom_bar(width=0.5, stat = "identity") +
  theme(plot.background=element_blank()) +
  theme(panel.background=element_blank()) + 
  theme(aspect.ratio = 2/1.5) + 
  scale_x_discrete(name = "Goods Category")+
  scale_y_continuous("Count") +
  ggtitle("Goods Category") + 
  theme_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")

ippc_mark <- as.data.frame(table(data$IPPC.Mark, useNA = "ifany"))
plot_ly(ippc_mark, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  layout(title = "IPPC Mark",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# find data distribution on month
month <- as.data.frame(table(data$Month, useNA = "ifany"))
# plot_ly(month, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
#   layout(title = "Month",
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

ggplot(data = month, mapping = aes(x = Var1 , y = Freq, fill= Var1 )) + 
  geom_bar(width=0.5, stat = "identity") +
  theme(plot.background=element_blank()) +
  theme(panel.background=element_blank()) + 
  theme(aspect.ratio = 2/1.5) + 
  scale_x_discrete(name = "Month")+
  scale_y_continuous("Count") +
  ggtitle("Shimpent in Month") + 
  theme_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

# find distribution of shipping country on each port of entry
data_halifax <- data[which(data$Port.of.Entry..map. == "Halifax, Nova Scotia"), ]
data_halifaX_countries <- as.data.frame(table(data_halifax$Shipper.Country, useNA = "ifany"))
data_halifaX_countries_major <- subset(data_halifaX_countries,
                                       data_halifaX_countries[, 2] > 200)
data_halifaX_countries_minor <- subset(data_halifaX_countries,
                                       data_halifaX_countries[, 2] <= 200)
data_halifaX_countries_major$Var1 <- as.character(data_halifaX_countries_major$Var1)
data_halifaX_countries <- rbind(data_halifaX_countries_major,
                                list(Var1 = "Other",
                                     Freq = sum(data_halifaX_countries_minor$Freq)),
                                stringsAsFactors = FALSE)
data_halifaX_countries$Var1 <- factor(data_halifaX_countries$Var1)
plot_ly(data_halifaX_countries, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  layout(title = "Halifax, Nova Scotia",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

data_montreal <- data[which(data$Port.of.Entry..map. == "Montreal, Quebec"), ]
data_montreal_countries <- as.data.frame(table(data_montreal$Shipper.Country, useNA = "ifany"))
data_montreal_countries_major <- subset(data_montreal_countries,
                                        data_montreal_countries[, 2] > 200)
data_montreal_countries_minor <- subset(data_montreal_countries,
                                        data_montreal_countries[, 2] <= 200)
data_montreal_countries_major$Var1 <- as.character(data_montreal_countries_major$Var1)
data_montreal_countries <- rbind(data_montreal_countries_major,
                                 list(Var1 = "Other",
                                      Freq = sum(data_montreal_countries_minor$Freq)),
                                 stringsAsFactors = FALSE)
data_montreal_countries$Var1 <- factor(data_montreal_countries$Var1)
plot_ly(data_montreal_countries, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  layout(title = "Montreal, Quebec",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

data_princerupert <- data[which(data$Port.of.Entry..map. == "Prince Rupert, British Columbia"), ]
data_princerupert_countries <- as.data.frame(table(data_princerupert$Shipper.Country, useNA = "ifany"))
data_princerupert_countries_major <- subset(data_princerupert_countries,
                                      data_princerupert_countries[, 2] > 100)
data_princerupert_countries_minor <- subset(data_princerupert_countries,
                                      data_princerupert_countries[, 2] <= 100)
data_princerupert_countries_major$Var1 <- as.character(data_princerupert_countries_major$Var1)
data_princerupert_countries <- rbind(data_princerupert_countries_major,
                               list(Var1 = "Other",
                                    Freq = sum(data_princerupert_countries_minor$Freq)),
                               stringsAsFactors = FALSE)
data_princerupert_countries$Var1 <- factor(data_princerupert_countries$Var1)
plot_ly(data_princerupert_countries, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  layout(title = "Prince Rupert, British Columbia",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

data_vancouver <- data[which(data$Port.of.Entry..map. == "Vancouver, British Columbia"), ]
data_vancouver_countries <- as.data.frame(table(data_vancouver$Shipper.Country, useNA = "ifany"))
data_vancouver_countries_major <- subset(data_vancouver_countries,
                                         data_vancouver_countries[, 2] > 100)
data_vancouver_countries_minor <- subset(data_vancouver_countries,
                                         data_vancouver_countries[, 2] <= 100)
data_vancouver_countries_major$Var1 <- as.character(data_vancouver_countries_major$Var1)
data_vancouver_countries <- rbind(data_vancouver_countries_major,
                                  list(Var1 = "Other",
                                       Freq = sum(data_vancouver_countries_minor$Freq)),
                                  stringsAsFactors = FALSE)
data_vancouver_countries$Var1 <- factor(data_vancouver_countries$Var1)
plot_ly(data_vancouver_countries, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  layout(title = "Vancouver, British Columbia",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

data_canada <- data[which(data$Port.of.Entry..map. == "By Air"), ]
data_canada_countries <- as.data.frame(table(data_canada$Shipper.Country, useNA = "ifany"))
data_canada_countries_major <- subset(data_canada_countries,
                                      data_canada_countries[, 2] > 5)
data_canada_countries_minor <- subset(data_canada_countries,
                                      data_canada_countries[, 2] <= 5)
data_canada_countries_major$Var1 <- as.character(data_canada_countries_major$Var1)
data_canada_countries <- rbind(data_canada_countries_major,
                               list(Var1 = "Other",
                                    Freq = sum(data_canada_countries_minor$Freq)),
                               stringsAsFactors = FALSE)
data_canada_countries$Var1 <- factor(data_canada_countries$Var1)
plot_ly(data_canada_countries, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  layout(title = "By Air",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# compliance rate
compliance_count <- ddply(data,
                          .(Compliant..Y.N.),
                          summarize,
                          count = length(Compliant..Y.N.),
                          percentage = percent(length(Compliant..Y.N.) / length(data$Compliant..Y.N.)),
                          .drop = FALSE)
ggplot_compliance_count <- ggplot(data = compliance_count,
                                  aes(x = Compliant..Y.N., y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 18)
  )
plot(ggplot_compliance_count)

# compliance rate for IPPC mark
compliance_count_ippc_mark <- ddply(data,
                                   .(IPPC.Mark, Compliant..Y.N.),
                                   summarize,
                                   count = length(IPPC.Mark),
                                   .drop = FALSE)
compliance_rate_ippc_mark <- ddply(compliance_count_ippc_mark,
                                   .(IPPC.Mark),
                                   transform,
                                   percentage = percent(count / sum(count)))
ggplot_compliance_rate_ippc_mark <- ggplot(data = compliance_rate_ippc_mark,
                                           aes(x = IPPC.Mark, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )
plot(ggplot_compliance_rate_ippc_mark)

# compliance rate for Packaging Material
compliance_count_packaging_material <- ddply(data,
                                            .(Packaging.Material, Compliant..Y.N.),
                                            summarize,
                                            count = length(Packaging.Material),
                                            .drop = FALSE)
compliance_rate_packaging_material <- ddply(compliance_count_packaging_material,
                                            .(Packaging.Material),
                                            transform,
                                            percentage = percent(count / sum(count)))
compliance_rate_packaging_material$Packaging.Material <- factor(
  compliance_rate_packaging_material$Packaging.Material,
  levels = compliance_rate_packaging_material[compliance_rate_packaging_material$Compliant..Y.N.=="N", ][order(
    compliance_rate_packaging_material[compliance_rate_packaging_material$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
    ), ]$Packaging.Material
)
ggplot_compliance_rate_packaging_material <- ggplot(data = compliance_rate_packaging_material,
                                                    aes(x = Packaging.Material, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
plot(ggplot_compliance_rate_packaging_material)

# compliance rate for Port of Entry
compliance_count_port_of_entry <- ddply(data,
                                       .(Port.of.Entry..map., Compliant..Y.N.),
                                       summarize,
                                       count = length(Packaging.Material),
                                       .drop = FALSE)
compliance_rate_port_of_entry <- ddply(compliance_count_port_of_entry,
                                       .(Port.of.Entry..map.),
                                       transform,
                                       percentage = percent(count / sum(count)))
compliance_rate_port_of_entry$Port.of.Entry..map. <- factor(
  compliance_rate_port_of_entry$Port.of.Entry..map.,
  levels = compliance_rate_port_of_entry[compliance_rate_port_of_entry$Compliant..Y.N.=="N", ][order(
    compliance_rate_port_of_entry[compliance_rate_port_of_entry$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Port.of.Entry..map.
)
ggplot_compliance_rate_port_of_entry <- ggplot(data = compliance_rate_port_of_entry,
                                               aes(x = Port.of.Entry..map., y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 14)
  )
plot(ggplot_compliance_rate_port_of_entry)

# compliance rate for most observed countries
most_observed_countries <- names(sort(table(data$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries <- data[which(data$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries$Shipper.Country <- factor(data_most_observed_countries$Shipper.Country,
                                                       levels = most_observed_countries)
compliance_count_most_observed_countries <- ddply(data_most_observed_countries,
                                                 .(Shipper.Country, Compliant..Y.N.),
                                                 summarize,
                                                 count = length(Shipper.Country),
                                                 .drop = FALSE)
compliance_rate_most_observed_countries <- ddply(compliance_count_most_observed_countries,
                                                 .(Shipper.Country),
                                                 transform,
                                                 percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries$Shipper.Country <- factor(
  compliance_rate_most_observed_countries$Shipper.Country,
  levels = compliance_rate_most_observed_countries[compliance_rate_most_observed_countries$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries[compliance_rate_most_observed_countries$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries$percentage[compliance_rate_most_observed_countries$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries <- ggplot(data = compliance_rate_most_observed_countries,
                                                         aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
plot(ggplot_compliance_rate_most_observed_countries)

# compliance rate for highest non-compliant countries
compliance_count_countries <- ddply(data,
                                    .(Shipper.Country, Compliant..Y.N.),
                                    summarize,
                                    count = length(Shipper.Country),
                                    .drop = FALSE)
compliance_rate_countries <- ddply(compliance_count_countries,
                                   .(Shipper.Country),
                                   transform,
                                   percentage = count / sum(count))
highest_noncompliant_countries <- 
  compliance_rate_countries[compliance_rate_countries$Compliant..Y.N. == "N", ][order(
    compliance_rate_countries[compliance_rate_countries$Compliant..Y.N. == "N", ]$percentage, decreasing = TRUE
    )[1:10], ]$Shipper.Country
compliance_count_highest_noncompliant_countries <- compliance_count_countries[compliance_count_countries$Shipper.Country %in% highest_noncompliant_countries, ]
compliance_count_highest_noncompliant_countries$Shipper.Country <- factor(compliance_count_highest_noncompliant_countries$Shipper.Country,
                                                                          levels = highest_noncompliant_countries)
compliance_rate_highest_noncompliant_countries <- ddply(compliance_count_highest_noncompliant_countries,
                                                        .(Shipper.Country),
                                                        transform,
                                                        percentage = percent(count / sum(count)))
ggplot_compliance_rate_highest_noncompliant_countries <- ggplot(data = compliance_rate_highest_noncompliant_countries,
                                                         aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
plot(ggplot_compliance_rate_highest_noncompliant_countries)

# compliance rate for goods category
compliance_count_goods_category <- ddply(data[!(is.na(data$Goods.Category) | data$Goods.Category == ""), ],
                                         .(Goods.Category, Compliant..Y.N.),
                                         summarize,
                                         count = length(Packaging.Material))
compliance_rate_goods_category <- ddply(compliance_count_goods_category,
                                        .(Goods.Category),
                                        transform,
                                        percentage = percent(count / sum(count)))
compliance_rate_goods_category$Goods.Category <- factor(
  compliance_rate_goods_category$Goods.Category,
  levels = compliance_rate_goods_category[compliance_rate_goods_category$Compliant..Y.N.=="N", ][order(
    compliance_rate_goods_category[compliance_rate_goods_category$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Goods.Category
)
ggplot_compliance_rate_goods_category <- ggplot(data = compliance_rate_goods_category,
                                                aes(x = Goods.Category, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 14)
  )
plot(ggplot_compliance_rate_goods_category)

# compliance rate for month
compliance_count_month <- ddply(data,
                                .(Month, Compliant..Y.N.),
                                summarize,
                                count = length(Month))
compliance_rate_month <- ddply(compliance_count_month,
                               .(Month),
                               transform,
                               percentage = percent(count / sum(count)))
ggplot_compliance_rate_month <- ggplot(data = compliance_rate_month,
                                                aes(x = Month, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
plot(ggplot_compliance_rate_month)

# compliance rate for most observed countries by port
data_east <- data[which(data$Coast == "East"), ]
data_west <- data[which(data$Coast == "West"), ]

data_most_observed_countries_east <- data_east[which(data_east$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries_east$Shipper.Country <- factor(data_most_observed_countries_east$Shipper.Country,
                                                            levels = most_observed_countries)
compliance_count_most_observed_countries_east <- ddply(data_most_observed_countries_east,
                                                       .(Shipper.Country, Compliant..Y.N.),
                                                       summarize,
                                                       count = length(Shipper.Country),
                                                       .drop = FALSE)
compliance_rate_most_observed_countries_east <- ddply(compliance_count_most_observed_countries_east,
                                                      .(Shipper.Country),
                                                      transform,
                                                      percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_east$percentage[compliance_rate_most_observed_countries_east$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_east <- ggplot(data = compliance_rate_most_observed_countries_east,
                                                              aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
plot(ggplot_compliance_rate_most_observed_countries_east)

data_most_observed_countries_west <- data_west[which(data_west$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries_west$Shipper.Country <- factor(data_most_observed_countries_west$Shipper.Country,
                                                            levels = most_observed_countries)
compliance_count_most_observed_countries_west <- ddply(data_most_observed_countries_west,
                                                       .(Shipper.Country, Compliant..Y.N.),
                                                       summarize,
                                                       count = length(Shipper.Country),
                                                       .drop = FALSE)
compliance_rate_most_observed_countries_west <- ddply(compliance_count_most_observed_countries_west,
                                                      .(Shipper.Country),
                                                      transform,
                                                      percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_west$percentage[compliance_rate_most_observed_countries_west$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_west <- ggplot(data = compliance_rate_most_observed_countries_west,
                                                              aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
plot(ggplot_compliance_rate_most_observed_countries_west)

data_most_observed_countries_halifax <- data_halifax[which(data_halifax$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries_halifax$Shipper.Country <- factor(data_most_observed_countries_halifax$Shipper.Country,
                                                               levels = most_observed_countries)
compliance_count_most_observed_countries_halifax <- ddply(data_most_observed_countries_halifax,
                                                          .(Shipper.Country, Compliant..Y.N.),
                                                          summarize,
                                                          count = length(Shipper.Country),
                                                          .drop = FALSE)
compliance_rate_most_observed_countries_halifax <- ddply(compliance_count_most_observed_countries_halifax,
                                                         .(Shipper.Country),
                                                         transform,
                                                         percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_halifax$percentage[compliance_rate_most_observed_countries_halifax$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_halifax <- ggplot(data = compliance_rate_most_observed_countries_halifax,
                                                                 aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
plot(ggplot_compliance_rate_most_observed_countries_halifax)

data_most_observed_countries_montreal <- data_montreal[which(data_montreal$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries_montreal$Shipper.Country <- factor(data_most_observed_countries_montreal$Shipper.Country,
                                                                levels = most_observed_countries)
compliance_count_most_observed_countries_montreal <- ddply(data_most_observed_countries_montreal,
                                                           .(Shipper.Country, Compliant..Y.N.),
                                                           summarize,
                                                           count = length(Shipper.Country),
                                                           .drop = FALSE)
compliance_rate_most_observed_countries_montreal <- ddply(compliance_count_most_observed_countries_montreal,
                                                          .(Shipper.Country),
                                                          transform,
                                                          percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_montreal$percentage[compliance_rate_most_observed_countries_montreal$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_montreal <- ggplot(data = compliance_rate_most_observed_countries_montreal,
                                                                  aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
plot(ggplot_compliance_rate_most_observed_countries_montreal)

data_most_observed_countries_princerupert <- data_princerupert[which(data_princerupert$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries_princerupert$Shipper.Country <- factor(data_most_observed_countries_princerupert$Shipper.Country,
                                                                    levels = most_observed_countries)
compliance_count_most_observed_countries_princerupert <- ddply(data_most_observed_countries_princerupert,
                                                               .(Shipper.Country, Compliant..Y.N.),
                                                               summarize,
                                                               count = length(Shipper.Country),
                                                               .drop = FALSE)
compliance_rate_most_observed_countries_princerupert <- ddply(compliance_count_most_observed_countries_princerupert,
                                                              .(Shipper.Country),
                                                              transform,
                                                              percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_princerupert$percentage[compliance_rate_most_observed_countries_princerupert$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_princerupert <- ggplot(data = compliance_rate_most_observed_countries_princerupert,
                                                                      aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
plot(ggplot_compliance_rate_most_observed_countries_princerupert)

data_most_observed_countries_vancouver <- data_vancouver[which(data_vancouver$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries_vancouver$Shipper.Country <- factor(data_most_observed_countries_vancouver$Shipper.Country,
                                                                 levels = most_observed_countries)
compliance_count_most_observed_countries_vancouver <- ddply(data_most_observed_countries_vancouver,
                                                            .(Shipper.Country, Compliant..Y.N.),
                                                            summarize,
                                                            count = length(Shipper.Country),
                                                            .drop = FALSE)
compliance_rate_most_observed_countries_vancouver <- ddply(compliance_count_most_observed_countries_vancouver,
                                                           .(Shipper.Country),
                                                           transform,
                                                           percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_vancouver$percentage[compliance_rate_most_observed_countries_vancouver$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_vancouver <- ggplot(data = compliance_rate_most_observed_countries_vancouver,
                                                                   aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
plot(ggplot_compliance_rate_most_observed_countries_vancouver)

data_most_observed_countries_canada <- data_canada[which(data_canada$Shipper.Country %in% most_observed_countries), ]
data_most_observed_countries_canada$Shipper.Country <- factor(data_most_observed_countries_canada$Shipper.Country,
                                                              levels = most_observed_countries)
compliance_rate_most_observed_countries_canada <- ddply(data_most_observed_countries_canada,
                                                        .(Shipper.Country, Compliant..Y.N.),
                                                        summarize,
                                                        count = length(Shipper.Country),
                                                        .drop = FALSE)
compliance_rate_most_observed_countries_canada <- ddply(compliance_rate_most_observed_countries_canada,
                                                        .(Shipper.Country),
                                                        transform,
                                                        percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_canada$percentage[compliance_rate_most_observed_countries_canada$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_canada <- ggplot(data = compliance_rate_most_observed_countries_canada,
                                                                aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
plot(ggplot_compliance_rate_most_observed_countries_canada)

# open output file
pdf(file = "./diagrams/pdf/profiling.pdf", width = 13.5, height = 10)

plot(ggplot_pearsoncormat)
plot(GKmatrix)
plot(ggplot_compliance_count +
       ggtitle("Compliant..Y.n") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_ippc_mark + 
       ggtitle("IPPC Mark") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_packaging_material + 
       ggtitle("Packaging Material") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_port_of_entry + 
       ggtitle("Port of Entry") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_goods_category + 
       ggtitle("Goods Category") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_month + 
       ggtitle("Month") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries + 
       ggtitle("10 Most Observed Countries") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_highest_noncompliant_countries + 
       ggtitle("10 Highest Non-compliant Countries") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_east + 
       ggtitle("10 Most Observed Countries, East Coast") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_west +
       ggtitle("10 Most Observed Countries, West Coast") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_halifax +
       ggtitle("10 Most Observed Countries, Halifax") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_montreal +
       ggtitle("10 Most Observed Countries, Montreal") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_princerupert +
       ggtitle("10 Most Observed Countries, Prince Rupert") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_vancouver +
       ggtitle("10 Most Observed Countries, Vancouver") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_canada +
       ggtitle("10 Most Observed Countries, By Air") +
       theme(plot.title = element_text(hjust = 0.5)))

# close output file
dev.off()

# targets of the current scenario
data_targeted <- data_wp[data_wp$Shipper.Country == "China"
                         | data_wp$Shipper.Country == "India"
                         | data_wp$Shipper.Country == "Hong Kong"
                         | data_wp$Shipper.Country == "Turkey"
                         | data_wp$Shipper.Country == "Pakistan"
                         | data_wp$Shipper.Country == "Taiwan"
                         | data_wp$Shipper.Country == "Republic of Korea"
                         | data_wp$Shipper.Country == "Vietnam"
                         | data_wp$Goods.Category == "Tiles"
                         | (data_wp$Shipper.Country == "Italy" & data_wp$Goods.Category == "Furniture")
                         | ((data_wp$Shipper.Country == "France" | data_wp$Shipper.Country == "Arab Emirats") & data_wp$Goods.Category == "Machinery, Vehicles and Auto-parts")
                         | data_wp$Goods.Category == "Quarry Products"
                         | (data_wp$Goods.Category == "Building Material" & data_wp$Shipper.Country == "Brazil"), ]
nrow(data_targeted[data_targeted$Compliant..Y.N. == "N", ])
(nrow(data_wp[data_wp$Compliant..Y.N. == "N", ]) - nrow(data_targeted[data_targeted$Compliant..Y.N. == "N", ])) / nrow(data_wp[data_wp$Compliant..Y.N. == "N", ])
