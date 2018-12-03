data_reduced <- data[,c(2,3,4,7,8,10,12)]
data_reduced_chimatrix<- data_reduced[,c(1,2,5)]

chisq.test(data_reduced_chimatrix$Shipper.Country,data_reduced_chimatrix$Shipper.City)

chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) {
      m[i,j] = round(chisq.test(x[,i],x[,j],)$p.value, digits=2
    }
  }
  return (m)
}
mat = as.data.frame.matrix(chisqmatrix(data_reduced_chimatrix))
mat

library(GoodmanKruskal)

GKtau(data_reduced$Shipper.Country,data_reduced$Shipper.City)

GKMatrix <- GKtauDataframe(data_reduced_chimatrix)
plot(GKMatrix)
