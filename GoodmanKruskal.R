data_reduced <- data[,c(2,3,4,6,7,8,10,12)]

chisq.test(data_reduced_chimatrix$Shipper.Country,data_reduced_chimatrix$Shipper.City)

chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:num) {
    for (j in 1:num) {
      m[i,j] = GKtau(x[,i],x[,j])$tauxy
    }
  }
  return (m)
}



# m=matrix(nrow = ncol(data_reduced), ncol=ncol(data_reduced))
# for(i in 1:ncol(data_reduced)){
#   for (j in 1:ncol(data_reduced)){
#     m[i,j]=GKtau(data_reduced[,i],data_reduced[,j])$tauxy
#   }
# }

library(GoodmanKruskal)

names = colnames(data_reduced);  num = length(names)
m = matrix(nrow=num,ncol=num,dimnames=list(names,names))

m[1,1]<-GKtau(data_reduced[,1],data_reduced[,1])$tauxy
m[1,2]<-GKtau(data_reduced[,1],data_reduced[,2])$tauxy
m[1,3]<-GKtau(data_reduced[,1],data_reduced[,3])$tauxy
m[1,4]<-GKtau(data_reduced[,1],data_reduced[,4])$tauxy
m[1,5]<-GKtau(data_reduced[,1],data_reduced[,5])$tauxy
m[1,6]<-GKtau(data_reduced[,1],data_reduced[,6])$tauxy
m[1,7]<-GKtau(data_reduced[,1],data_reduced[,7])$tauxy
m[1,8]<-GKtau(data_reduced[,1],data_reduced[,8])$tauxy

m[2,1]<-GKtau(data_reduced[,2],data_reduced[,1])$tauxy
m[2,2]<-GKtau(data_reduced[,2],data_reduced[,2])$tauxy
m[2,3]<-GKtau(data_reduced[,2],data_reduced[,3])$tauxy
m[2,4]<-GKtau(data_reduced[,2],data_reduced[,4])$tauxy
m[2,5]<-GKtau(data_reduced[,2],data_reduced[,5])$tauxy
m[2,6]<-GKtau(data_reduced[,2],data_reduced[,6])$tauxy
m[2,7]<-GKtau(data_reduced[,2],data_reduced[,7])$tauxy
m[2,8]<-GKtau(data_reduced[,2],data_reduced[,8])$tauxy

m[3,1]<-GKtau(data_reduced[,3],data_reduced[,1])$tauxy
m[3,2]<-GKtau(data_reduced[,3],data_reduced[,2])$tauxy
m[3,3]<-GKtau(data_reduced[,3],data_reduced[,3])$tauxy
m[3,4]<-GKtau(data_reduced[,3],data_reduced[,4])$tauxy
m[3,5]<-GKtau(data_reduced[,3],data_reduced[,5])$tauxy
m[3,6]<-GKtau(data_reduced[,3],data_reduced[,6])$tauxy
m[3,7]<-GKtau(data_reduced[,3],data_reduced[,7])$tauxy
m[3,8]<-GKtau(data_reduced[,3],data_reduced[,8])$tauxy

m[4,1]<-GKtau(data_reduced[,4],data_reduced[,1])$tauxy
m[4,2]<-GKtau(data_reduced[,4],data_reduced[,2])$tauxy
m[4,3]<-GKtau(data_reduced[,4],data_reduced[,3])$tauxy
m[4,4]<-GKtau(data_reduced[,4],data_reduced[,4])$tauxy
m[4,5]<-GKtau(data_reduced[,4],data_reduced[,5])$tauxy
m[4,6]<-GKtau(data_reduced[,4],data_reduced[,6])$tauxy
m[4,7]<-GKtau(data_reduced[,4],data_reduced[,7])$tauxy
m[4,8]<-GKtau(data_reduced[,4],data_reduced[,8])$tauxy

m[5,1]<-GKtau(data_reduced[,5],data_reduced[,1])$tauxy
m[5,2]<-GKtau(data_reduced[,5],data_reduced[,2])$tauxy
m[5,3]<-GKtau(data_reduced[,5],data_reduced[,3])$tauxy
m[5,4]<-GKtau(data_reduced[,5],data_reduced[,4])$tauxy
m[5,5]<-GKtau(data_reduced[,5],data_reduced[,5])$tauxy
m[5,6]<-GKtau(data_reduced[,5],data_reduced[,6])$tauxy
m[5,7]<-GKtau(data_reduced[,5],data_reduced[,7])$tauxy
m[5,8]<-GKtau(data_reduced[,5],data_reduced[,8])$tauxy

m[6,1]<-GKtau(data_reduced[,6],data_reduced[,1])$tauxy
m[6,2]<-GKtau(data_reduced[,6],data_reduced[,2])$tauxy
m[6,3]<-GKtau(data_reduced[,6],data_reduced[,3])$tauxy
m[6,4]<-GKtau(data_reduced[,6],data_reduced[,4])$tauxy
m[6,5]<-GKtau(data_reduced[,6],data_reduced[,5])$tauxy
m[6,6]<-GKtau(data_reduced[,6],data_reduced[,6])$tauxy
m[6,7]<-GKtau(data_reduced[,6],data_reduced[,7])$tauxy
m[6,8]<-GKtau(data_reduced[,6],data_reduced[,8])$tauxy

m[7,1]<-GKtau(data_reduced[,7],data_reduced[,1])$tauxy
m[7,2]<-GKtau(data_reduced[,7],data_reduced[,2])$tauxy
m[7,3]<-GKtau(data_reduced[,7],data_reduced[,3])$tauxy
m[7,4]<-GKtau(data_reduced[,7],data_reduced[,4])$tauxy
m[7,5]<-GKtau(data_reduced[,7],data_reduced[,5])$tauxy
m[7,6]<-GKtau(data_reduced[,7],data_reduced[,6])$tauxy
m[7,7]<-GKtau(data_reduced[,7],data_reduced[,7])$tauxy
m[7,8]<-GKtau(data_reduced[,7],data_reduced[,8])$tauxy

m[8,1]<-GKtau(data_reduced[,8],data_reduced[,1])$tauxy
m[8,2]<-GKtau(data_reduced[,8],data_reduced[,2])$tauxy
m[8,3]<-GKtau(data_reduced[,8],data_reduced[,3])$tauxy
m[8,4]<-GKtau(data_reduced[,8],data_reduced[,4])$tauxy
m[8,5]<-GKtau(data_reduced[,8],data_reduced[,5])$tauxy
m[8,6]<-GKtau(data_reduced[,8],data_reduced[,6])$tauxy
m[8,7]<-GKtau(data_reduced[,8],data_reduced[,7])$tauxy
m[8,8]<-GKtau(data_reduced[,8],data_reduced[,8])$tauxy


title <- "Goodman Kurskal Associations"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(m, method="color", col=col(200), 
         title=title, 
         addCoef.col = "black", # Add coefficient of correlation
         mar=c(0,0,1,0)
) 
