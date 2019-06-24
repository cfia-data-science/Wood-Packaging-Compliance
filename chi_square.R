# create contingency tables
yearly_summary <- as.data.frame.matrix(table(data$Year, data$Compliant..Y.N.))
monthly_summary <- as.data.frame.matrix(table(data$Month, data$Compliant..Y.N.))
yearmonthly_summary <- as.data.frame.matrix(table(data$Year.Month, data$Compliant..Y.N.))

# chi square test
chisq.test(yearly_summary)
chisq.test(monthly_summary)
chisq.test(yearmonthly_summary)

# subset data for 2014 - 2018
data_2014_2018 <- subset(data, data$Year == c(2014: 2018))
monthly_summary_2014_2018 <- as.data.frame.matrix(table(data_2014_2018$Year.Month, data_2014_2018$Compliant..Y.N.))
monthly_summary_2014_2018 <- na.omit(subset(monthly_summary_2014_2018, monthly_summary_2014_2018 != 0))

# chi square test
chisq.test(monthly_summary_2014_2018)

# data reduced to relevant variables
data_reduced <- subset(data, select = c("Compliant..Y.N.", "Packaging.Material", "Port.of.Entry..map.", "Shipper.Country", "Goods.Category", "Year", "Month"))

# function to find chi square values between all variables in a dataset
chisqmatrix <- function(x) {
  names = colnames(x)
  num = length(names)
  m = matrix(nrow = num, ncol = num, dimnames = list(names, names))
  for (i in 1: (num - 1)) {
    for (j in (i + 1): num) {
      m[i, j] = try(chisq.test(x[, i], x[, j])$p.value)
    }
  }
  return(m)
}

# plot of chi square p-values (they are all zeroes so there is nothing meaningful from it)
names = colnames(data_reduced)
num = length(names)
m_chi = matrix(nrow = num, ncol = num, dimnames = list(names, names))
m_chi <- chisqmatrix(data_reduced)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(m_chi,
         method = "color",
         col = col(200),
         addCoef.col = "black",
         mar = c(0,0,1,0))

# table of chi square values
table_chi <- gtable_add_rows(
  tableGrob(m_chi),
  heights = grobHeight(textGrob("Chi Square p-Values")) + unit(10, "mm"),
  pos = 0)
table_chi <- gtable_add_grob(
  table_chi,
  textGrob("Chi Square p-Values", gp = gpar(fontsize = 22)), 1, 1, 1, ncol(table_chi))

# output chi square p-value table
png(file = "./diagrams/chi_table.png", width = 13, height = 3, units = "in", res = 500)
grid.draw(table_chi)
dev.off()

pdf(file = "./diagrams/pdf/chi_table.pdf", width = 13, height = 3)
grid.draw(table_chi)
dev.off()