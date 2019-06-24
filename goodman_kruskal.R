# dataset reduced to relevant variables
data_cor <- subset(data, select = c("Compliant..Y.N.", "Packaging.Material", "Port.of.Entry..map.", "Shipper.Country", "Goods.Category", "Year", "Month"))

# create GKtau correlation matrix for selected variables
m_cor = matrix(nrow = length(colnames(data_cor)),
               ncol = length(colnames(data_cor)),
               dimnames = list(colnames(data_cor),
                               colnames(data_cor)))
for(i in 1: ncol(data_cor)){
  for (j in 1: ncol(data_cor)){
    m_cor[i, j] = ifelse(i == j, 1, try(GKtau(data_cor[, i], data_cor[, j])$tauxy))
  }
}

# dataset reduced to binary variables
data_bin <- data[c(12: 19)]

# create GKtau correlation matrix for binary variables
m_bin = matrix(nrow = length(colnames(data_bin)),
               ncol = length(colnames(data_bin)),
               dimnames = list(colnames(data_bin),
                               colnames(data_bin)))
for(i in 1: ncol(data_bin)){
  for (j in 1: ncol(data_bin)){
    m_bin[i, j] = ifelse(i == j, 1, try(GKtau(data_bin[, i], data_bin[, j])$tauxy))
  }
}

# color scheme
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# plot of GKtau correlation matrix for selected variables
png(file = "./diagrams/gk_selected_variables.png", width = 11, height = 8.5, units = "in", res = 500)
corrplot_cor <- corrplot(m_cor,
                         method="color",
                         col = col(200),
                         addCoef.col = "black",
                         mar = c(0, 0, 1, 0))
dev.off()

pdf(file = "./diagrams/pdf/gk_selected_variables.pdf", width = 11, height = 8.5)
corrplot_cor <- corrplot(m_cor,
                         method="color",
                         col = col(200),
                         addCoef.col = "black",
                         mar = c(0, 0, 1, 0))
dev.off()

# plot of GKtau correlation matrix for binary variables
png(file = "./diagrams/gk_binary_variables.png", width = 11, height = 8.5, units = "in", res = 500)
corrplot_bin <- corrplot(m_bin,
                         method="color",
                         col = col(200),
                         addCoef.col = "black",
                         mar = c(0, 0, 1, 0))
dev.off()

pdf(file = "./diagrams/pdf/gk_binary_variables.pdf", width = 11, height = 8.5)
corrplot_bin <- corrplot(m_bin,
                         method="color",
                         col = col(200),
                         addCoef.col = "black",
                         mar = c(0, 0, 1, 0))
dev.off()