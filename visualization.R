# change long country name to fit the plot
data$Shipper.Country <- as.character(data$Shipper.Country)
data$Shipper.Country[data$Shipper.Country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
data$Shipper.Country <- factor(data$Shipper.Country)

# # find compliance rate for wp packages
# data_wp <- data %>% filter(Packaging.Material != "MP")
# non_compliant_wp <- as.data.frame(table(data_wp$Compliant..Y.N., useNA = "ifany"))
# ggplot(data = data_wp) + 
#   geom_bar(width=0.4, mapping=aes(x= Compliant..Y.N., fill = Compliant..Y.N.))+
#   theme(aspect.ratio = 2:5) + 
#   scale_x_discrete(name = "Compliance")+
#   scale_y_continuous("Count") +
#   ggtitle("Compliance Rate for WP Packages", subtitle = "85.7% compliant") + 
#   theme_minimal()
# 
# # find compliance rate for non-wp packages
# data_mp <- data %>% filter(Packaging.Material == "MP")
# non_compliant_mp <- as.data.frame(table(data_mp$Compliant..Y.N., useNA = "ifany"))
# ggplot(data = data_mp) + 
#   geom_bar(mapping=aes(x= Compliant..Y.N., fill= Compliant..Y.N.), width=0.4 ) +
#   theme(plot.background=element_blank()) +
#   theme(panel.background=element_blank(), legend.position="none") + 
#   theme(aspect.ratio = 3/100) + 
#   scale_x_discrete(name = "Compliance")+
#   scale_y_continuous("Count") +
#   ggtitle("Compliance Rate for Non-WP Packages", subtitle = "99.678% compliant") + 
#   theme_minimal()

# data distribution on IPPC mark
ippc_mark <- as.data.frame(table(data$IPPC.Mark, useNA = "ifany"))
ggplot_ippc_mark <- ggplot(data = ippc_mark, mapping = aes(x = Var1 , y = Freq, fill= Var1)) +
  geom_bar(width = 0.5, stat = "identity") +
  theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio = 2/1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# data distribution on packaging material
packaging_material <- as.data.frame(table(data$Packaging.Material, useNA = "ifany"))
ggplot_packaging_material <- ggplot(data = packaging_material, mapping = aes(reorder(Var1, -Freq) , y = Freq, fill = Var1)) +
  geom_bar(width = 0.5, stat = "identity") +
  theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio = 2/1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# data distribution on port of entry
port_of_entry <- as.data.frame(table(data$Port.of.Entry..map., useNA = "ifany"))
ggplot_port_of_entry <- ggplot(data = port_of_entry) +
  geom_bar(mapping = aes(x = reorder(Var1, -Freq) , y = Freq, fill = Var1), width = 0.8, stat = "identity" ) +
  theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio = 2/1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

# data distribution on goods category
goods_category <- as.data.frame(table(data$Goods.Category, useNA = "ifany"))
ggplot_goods_category <- ggplot(data = goods_category, mapping = aes(reorder(Var1, -Freq) , y = Freq, fill = Var1)) +
  geom_bar(width = 0.5, stat = "identity") +
  theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio = 2/1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

# data distribution on year
year <- as.data.frame(table(data$Year, useNA = "ifany"))
ggplot_year <- ggplot(data = year, mapping = aes(x = Var1 , y = Freq, fill = Var1)) +
  geom_bar(width = 0.5, stat = "identity") +
  theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio = 2/1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# data distribution on month
month <- as.data.frame(table(data$Month, useNA = "ifany"))
ggplot_month <- ggplot(data = month, mapping = aes(x = Var1 , y = Freq, fill = Var1)) +
  geom_bar(width = 0.5, stat = "identity") +
  theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio = 2/1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# data distribution on shipper country
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
ggplot_shipper_countries <- ggplot(data = shipper_countries) +
  geom_bar(mapping = aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1), width = 0.8, stat = "identity") +
  theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio = 2/1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

# distribution of shipping country on each port of entry
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

data_na <- data[which(data$Port.of.Entry..map. == "N/A"), ]
data_na_countries <- as.data.frame(table(data_na$Shipper.Country, useNA = "ifany"))
data_na_countries_major <- subset(data_na_countries,
                                  data_na_countries[, 2] > 5)
data_na_countries_minor <- subset(data_na_countries,
                                  data_na_countries[, 2] <= 5)
data_na_countries_major$Var1 <- as.character(data_na_countries_major$Var1)
data_na_countries <- rbind(data_na_countries_major,
                           list(Var1 = "Other",
                                Freq = sum(data_na_countries_minor$Freq)),
                           stringsAsFactors = FALSE)
data_na_countries$Var1 <- factor(data_na_countries$Var1)
plot_ly(data_na_countries, labels = ~ Var1, values = ~ Freq, type = "pie", textposition = "top center", textinfo = "label+percent") %>%
  layout(title = "N/A",
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
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plot(ggplot_compliance_count)

# compliance rate by IPPC mark
compliance_count_ippc_mark <- ddply(data,
                                    .(IPPC.Mark, Compliant..Y.N.),
                                    summarize,
                                    count = length(IPPC.Mark),
                                    .drop = FALSE)
compliance_rate_ippc_mark <- ddply(compliance_count_ippc_mark,
                                   .(IPPC.Mark),
                                   transform,
                                   percentage = percent(count / sum(count)))
compliance_rate_ippc_mark$IPPC.Mark <- as.factor(compliance_rate_ippc_mark$IPPC.Mark)
ggplot_compliance_rate_ippc_mark <- ggplot(data = compliance_rate_ippc_mark,
                                           aes(x = IPPC.Mark, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plot(ggplot_compliance_rate_ippc_mark)

# compliance rate by Packaging Material
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
compliance_rate_packaging_material$percentage[compliance_rate_packaging_material$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_packaging_material <- ggplot(data = compliance_rate_packaging_material,
                                                    aes(x = Packaging.Material, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plot(ggplot_compliance_rate_packaging_material)

# compliance rate by Port of Entry
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
compliance_rate_port_of_entry$percentage[compliance_rate_port_of_entry$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_port_of_entry <- ggplot(data = compliance_rate_port_of_entry,
                                               aes(x = Port.of.Entry..map., y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_port_of_entry)

# compliance rate by most observed countries
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
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries)

# compliance rate by highest non-compliant countries
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
compliance_rate_highest_noncompliant_countries$Shipper.Country <- factor(
  compliance_rate_highest_noncompliant_countries$Shipper.Country,
  levels = compliance_rate_highest_noncompliant_countries[compliance_rate_highest_noncompliant_countries$Compliant..Y.N.=="N", ][order(
    compliance_rate_highest_noncompliant_countries[compliance_rate_highest_noncompliant_countries$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_highest_noncompliant_countries$percentage[compliance_rate_highest_noncompliant_countries$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_highest_noncompliant_countries <- ggplot(data = compliance_rate_highest_noncompliant_countries,
                                                                aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_highest_noncompliant_countries)

# compliance rate by goods category
compliance_count_goods_category <- ddply(data[!(is.na(data$Goods.Category) | data$Goods.Category == ""), ],
                                         .(Goods.Category, Compliant..Y.N.),
                                         summarize,
                                         count = length(Packaging.Material),
                                         .drop = FALSE)
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
compliance_rate_goods_category$percentage[compliance_rate_goods_category$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_goods_category <- ggplot(data = compliance_rate_goods_category,
                                                aes(x = Goods.Category, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_goods_category)

# compliance rate by year
compliance_count_year <- ddply(data,
                               .(Year, Compliant..Y.N.),
                               summarize,
                               count = length(Year),
                               .drop = FALSE)
compliance_rate_year <- ddply(compliance_count_year,
                              .(Year),
                              transform,
                              percentage = percent(count / sum(count)))
compliance_rate_year$percentage[compliance_rate_year$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_year <- ggplot(data = compliance_rate_year,
                                      aes(x = Year, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plot(ggplot_compliance_rate_year)

# compliance rate by month
compliance_count_month <- ddply(data,
                                .(Month, Compliant..Y.N.),
                                summarize,
                                count = length(Month),
                                .drop = FALSE)
compliance_rate_month <- ddply(compliance_count_month,
                               .(Month),
                               transform,
                               percentage = percent(count / sum(count)))
compliance_rate_month$percentage[compliance_rate_month$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_month <- ggplot(data = compliance_rate_month,
                                       aes(x = Month, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plot(ggplot_compliance_rate_month)

# compliance rate by most observed countries at different ports
data_east <- data[which(data$Coast == "East"), ]
data_west <- data[which(data$Coast == "West"), ]

most_observed_countries_east <- names(sort(table(data_east$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries_east <- data_east[which(data_east$Shipper.Country %in% most_observed_countries_east), ]
data_most_observed_countries_east$Shipper.Country <- factor(data_most_observed_countries_east$Shipper.Country,
                                                            levels = most_observed_countries_east)
compliance_count_most_observed_countries_east <- ddply(data_most_observed_countries_east,
                                                       .(Shipper.Country, Compliant..Y.N.),
                                                       summarize,
                                                       count = length(Shipper.Country),
                                                       .drop = FALSE)
compliance_rate_most_observed_countries_east <- ddply(compliance_count_most_observed_countries_east,
                                                      .(Shipper.Country),
                                                      transform,
                                                      percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_east$Shipper.Country <- factor(
  compliance_rate_most_observed_countries_east$Shipper.Country,
  levels = compliance_rate_most_observed_countries_east[compliance_rate_most_observed_countries_east$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries_east[compliance_rate_most_observed_countries_east$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries_east$percentage[compliance_rate_most_observed_countries_east$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_east <- ggplot(data = compliance_rate_most_observed_countries_east,
                                                              aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries_east)

most_observed_countries_west <- names(sort(table(data_west$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries_west <- data_west[which(data_west$Shipper.Country %in% most_observed_countries_west), ]
data_most_observed_countries_west$Shipper.Country <- factor(data_most_observed_countries_west$Shipper.Country,
                                                            levels = most_observed_countries_west)
compliance_count_most_observed_countries_west <- ddply(data_most_observed_countries_west,
                                                       .(Shipper.Country, Compliant..Y.N.),
                                                       summarize,
                                                       count = length(Shipper.Country),
                                                       .drop = FALSE)
compliance_rate_most_observed_countries_west <- ddply(compliance_count_most_observed_countries_west,
                                                      .(Shipper.Country),
                                                      transform,
                                                      percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_west$Shipper.Country <- factor(
  compliance_rate_most_observed_countries_west$Shipper.Country,
  levels = compliance_rate_most_observed_countries_west[compliance_rate_most_observed_countries_west$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries_west[compliance_rate_most_observed_countries_west$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries_west$percentage[compliance_rate_most_observed_countries_west$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_west <- ggplot(data = compliance_rate_most_observed_countries_west,
                                                              aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries_west)

most_observed_countries_halifax <- names(sort(table(data_halifax$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries_halifax <- data_halifax[which(data_halifax$Shipper.Country %in% most_observed_countries_halifax), ]
data_most_observed_countries_halifax$Shipper.Country <- factor(data_most_observed_countries_halifax$Shipper.Country,
                                                               levels = most_observed_countries_halifax)
compliance_count_most_observed_countries_halifax <- ddply(data_most_observed_countries_halifax,
                                                          .(Shipper.Country, Compliant..Y.N.),
                                                          summarize,
                                                          count = length(Shipper.Country),
                                                          .drop = FALSE)
compliance_rate_most_observed_countries_halifax <- ddply(compliance_count_most_observed_countries_halifax,
                                                         .(Shipper.Country),
                                                         transform,
                                                         percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_halifax$Shipper.Country <- factor(
  compliance_rate_most_observed_countries_halifax$Shipper.Country,
  levels = compliance_rate_most_observed_countries_halifax[compliance_rate_most_observed_countries_halifax$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries_halifax[compliance_rate_most_observed_countries_halifax$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries_halifax$percentage[compliance_rate_most_observed_countries_halifax$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_halifax <- ggplot(data = compliance_rate_most_observed_countries_halifax,
                                                                 aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries_halifax)

most_observed_countries_montreal <- names(sort(table(data_montreal$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries_montreal <- data_montreal[which(data_montreal$Shipper.Country %in% most_observed_countries_montreal), ]
data_most_observed_countries_montreal$Shipper.Country <- factor(data_most_observed_countries_montreal$Shipper.Country,
                                                                levels = most_observed_countries_montreal)
compliance_count_most_observed_countries_montreal <- ddply(data_most_observed_countries_montreal,
                                                           .(Shipper.Country, Compliant..Y.N.),
                                                           summarize,
                                                           count = length(Shipper.Country),
                                                           .drop = FALSE)
compliance_rate_most_observed_countries_montreal <- ddply(compliance_count_most_observed_countries_montreal,
                                                          .(Shipper.Country),
                                                          transform,
                                                          percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_montreal$Shipper.Country <- factor(
  compliance_rate_most_observed_countries_montreal$Shipper.Country,
  levels = compliance_rate_most_observed_countries_montreal[compliance_rate_most_observed_countries_montreal$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries_montreal[compliance_rate_most_observed_countries_montreal$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries_montreal$percentage[compliance_rate_most_observed_countries_montreal$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_montreal <- ggplot(data = compliance_rate_most_observed_countries_montreal,
                                                                  aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries_montreal)

most_observed_countries_princerupert <- names(sort(table(data_princerupert$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries_princerupert <- data_princerupert[which(data_princerupert$Shipper.Country %in% most_observed_countries_princerupert), ]
data_most_observed_countries_princerupert$Shipper.Country <- factor(data_most_observed_countries_princerupert$Shipper.Country,
                                                                    levels = most_observed_countries_princerupert)
compliance_count_most_observed_countries_princerupert <- ddply(data_most_observed_countries_princerupert,
                                                               .(Shipper.Country, Compliant..Y.N.),
                                                               summarize,
                                                               count = length(Shipper.Country),
                                                               .drop = FALSE)
compliance_rate_most_observed_countries_princerupert <- ddply(compliance_count_most_observed_countries_princerupert,
                                                              .(Shipper.Country),
                                                              transform,
                                                              percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_princerupert$Shipper.Country <- factor(
  compliance_rate_most_observed_countries_princerupert$Shipper.Country,
  levels = compliance_rate_most_observed_countries_princerupert[compliance_rate_most_observed_countries_princerupert$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries_princerupert[compliance_rate_most_observed_countries_princerupert$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries_princerupert$percentage[compliance_rate_most_observed_countries_princerupert$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_princerupert <- ggplot(data = compliance_rate_most_observed_countries_princerupert,
                                                                      aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries_princerupert)

most_observed_countries_vancouver <- names(sort(table(data_vancouver$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries_vancouver <- data_vancouver[which(data_vancouver$Shipper.Country %in% most_observed_countries_vancouver), ]
data_most_observed_countries_vancouver$Shipper.Country <- factor(data_most_observed_countries_vancouver$Shipper.Country,
                                                                 levels = most_observed_countries_vancouver)
compliance_count_most_observed_countries_vancouver <- ddply(data_most_observed_countries_vancouver,
                                                            .(Shipper.Country, Compliant..Y.N.),
                                                            summarize,
                                                            count = length(Shipper.Country),
                                                            .drop = FALSE)
compliance_rate_most_observed_countries_vancouver <- ddply(compliance_count_most_observed_countries_vancouver,
                                                           .(Shipper.Country),
                                                           transform,
                                                           percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_vancouver$Shipper.Country <- factor(
  compliance_rate_most_observed_countries_vancouver$Shipper.Country,
  levels = compliance_rate_most_observed_countries_vancouver[compliance_rate_most_observed_countries_vancouver$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries_vancouver[compliance_rate_most_observed_countries_vancouver$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries_vancouver$percentage[compliance_rate_most_observed_countries_vancouver$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_vancouver <- ggplot(data = compliance_rate_most_observed_countries_vancouver,
                                                                   aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries_vancouver)

most_observed_countries_na <- names(sort(table(data_na$Shipper.Country), decreasing = TRUE)[1:10])
data_most_observed_countries_na <- data_na[which(data_na$Shipper.Country %in% most_observed_countries_na), ]
data_most_observed_countries_na$Shipper.Country <- factor(data_most_observed_countries_na$Shipper.Country,
                                                          levels = most_observed_countries_na)
compliance_rate_most_observed_countries_na <- ddply(data_most_observed_countries_na,
                                                    .(Shipper.Country, Compliant..Y.N.),
                                                    summarize,
                                                    count = length(Shipper.Country),
                                                    .drop = FALSE)
compliance_rate_most_observed_countries_na <- ddply(compliance_rate_most_observed_countries_na,
                                                    .(Shipper.Country),
                                                    transform,
                                                    percentage = percent(count / sum(count)))
compliance_rate_most_observed_countries_na$Shipper.Country <- factor(
  compliance_rate_most_observed_countries_na$Shipper.Country,
  levels = compliance_rate_most_observed_countries_na[compliance_rate_most_observed_countries_na$Compliant..Y.N.=="N", ][order(
    compliance_rate_most_observed_countries_na[compliance_rate_most_observed_countries_na$Compliant..Y.N.=="N", ]$count,
    decreasing = TRUE
  ), ]$Shipper.Country
)
compliance_rate_most_observed_countries_na$percentage[compliance_rate_most_observed_countries_na$percentage == "NaN%"] <- "0.00%"
ggplot_compliance_rate_most_observed_countries_na <- ggplot(data = compliance_rate_most_observed_countries_na,
                                                            aes(x = Shipper.Country, y = count, fill = Compliant..Y.N.)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = paste0(count, "\n", "(", percentage, ")")), vjust = -0.3, position = position_dodge(0.9), size = 2.5) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
plot(ggplot_compliance_rate_most_observed_countries_na)

# output to pdf
pdf(file = "./diagrams/pdf/visualization.pdf", width = 11, height = 8.5)
plot(ggplot_ippc_mark +
       ggtitle("Shipments", subtitle = "IPPC Mark") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_packaging_material +
       ggtitle("Shipments", subtitle = "Packaging Material") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_port_of_entry +
       ggtitle("Shipments", subtitle = "Port of Entry") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_goods_category +
       ggtitle("Shipments", subtitle = "Goods Category") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_year +
       ggtitle("Shipments", subtitle = "Year") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_month +
       ggtitle("Shipments", subtitle = "Month") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_shipper_countries +
       ggtitle("Shipments", subtitle = "Shipper Country, Count>300") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_count +
       ggtitle("Shipment Compliance") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_ippc_mark + 
       ggtitle("Shipment Compliance", subtitle = "IPPC Mark") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_packaging_material + 
       ggtitle("Shipment Compliance", subtitle = "Packaging Material") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_port_of_entry + 
       ggtitle("Shipment Compliance", subtitle = "Port of Entry") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_goods_category + 
       ggtitle("Shipment Compliance", subtitle = "Goods Category") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_year + 
       ggtitle("Shipment Compliance", subtitle = "Year") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_month + 
       ggtitle("Shipment Compliance", subtitle = "Month") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries + 
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_highest_noncompliant_countries + 
       ggtitle("Shipment Compliance", subtitle = "10 Highest Non-compliant Countries") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_east + 
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries, East Coast") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_west +
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries, West Coast") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_halifax +
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries, Halifax") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_montreal +
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries, Montreal") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_princerupert +
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries, Prince Rupert") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_vancouver +
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries, Vancouver") +
       theme(plot.title = element_text(hjust = 0.5)))
plot(ggplot_compliance_rate_most_observed_countries_na +
       ggtitle("Shipment Compliance", subtitle = "10 Most Observed Countries, N/A") +
       theme(plot.title = element_text(hjust = 0.5)))
dev.off()

# output to pdf individually
pdf(file = "./diagrams/pdf/shipments_ippc_mark.pdf", width = 11, height = 8.5)
plot(ggplot_ippc_mark)
dev.off()
pdf(file = "./diagrams/pdf/shipments_packaging_material.pdf", width = 11, height = 8.5)
plot(ggplot_packaging_material)
dev.off()
pdf(file = "./diagrams/pdf/shipments_port_of_entry.pdf", width = 11, height = 8.5)
plot(ggplot_port_of_entry)
dev.off()
pdf(file = "./diagrams/pdf/shipments_goods_category.pdf", width = 11, height = 8.5)
plot(ggplot_goods_category)
dev.off()
pdf(file = "./diagrams/pdf/shipments_year.pdf", width = 11, height = 8.5)
plot(ggplot_year)
dev.off()
pdf(file = "./diagrams/pdf/shipments_month.pdf", width = 11, height = 8.5)
plot(ggplot_month)
dev.off()
pdf(file = "./diagrams/pdf/shipments_shipper_countries.pdf", width = 11, height = 8.5)
plot(ggplot_shipper_countries)
dev.off()
pdf(file = "./diagrams/pdf/compliance_count.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_count)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_ippc_mark.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_ippc_mark)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_packaging_material.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_packaging_material)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_port_of_entry.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_port_of_entry)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_goods_category.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_goods_category)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_year.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_year)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_month.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_month)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_highest_noncompliant_countries.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_highest_noncompliant_countries)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries_east.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries_east)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries_west.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries_west)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries_halifax.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries_halifax)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries_montreal.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries_montreal)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries_princerupert.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries_princerupert)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries_vancouver.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries_vancouver)
dev.off()
pdf(file = "./diagrams/pdf/compliance_rate_most_observed_countries_na.pdf", width = 11, height = 8.5)
plot(ggplot_compliance_rate_most_observed_countries_na)
dev.off()

# output to png
png(file = "./diagrams/shipments_ippc_mark.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_ippc_mark)
dev.off()
png(file = "./diagrams/shipments_packaging_material.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_packaging_material)
dev.off()
png(file = "./diagrams/shipments_port_of_entry.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_port_of_entry)
dev.off()
png(file = "./diagrams/shipments_goods_category.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_goods_category)
dev.off()
png(file = "./diagrams/shipments_year.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_year)
dev.off()
png(file = "./diagrams/shipments_month.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_month)
dev.off()
png(file = "./diagrams/shipments_shipper_countries.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_shipper_countries)
dev.off()
png(file = "./diagrams/compliance_count.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_count)
dev.off()
png(file = "./diagrams/compliance_rate_ippc_mark.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_ippc_mark)
dev.off()
png(file = "./diagrams/compliance_rate_packaging_material.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_packaging_material)
dev.off()
png(file = "./diagrams/compliance_rate_port_of_entry.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_port_of_entry)
dev.off()
png(file = "./diagrams/compliance_rate_goods_category.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_goods_category)
dev.off()
png(file = "./diagrams/compliance_rate_year.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_year)
dev.off()
png(file = "./diagrams/compliance_rate_month.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_month)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries)
dev.off()
png(file = "./diagrams/compliance_rate_highest_noncompliant_countries.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_highest_noncompliant_countries)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries_east.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries_east)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries_west.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries_west)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries_halifax.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries_halifax)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries_montreal.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries_montreal)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries_princerupert.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries_princerupert)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries_vancouver.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries_vancouver)
dev.off()
png(file = "./diagrams/compliance_rate_most_observed_countries_na.png", width = 11, height = 8.5, units = "in", res = 500)
plot(ggplot_compliance_rate_most_observed_countries_na)
dev.off()