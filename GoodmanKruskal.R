#dataset reduced to relevant variables
data_reduced <- data[,c(2,3,4,5,6,7,8,10,12,23,24)]


#GK Associations for all possible pairs
names = colnames(data_reduced);  num = length(names)
m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
#iterate
for(i in 1:ncol(data_reduced)){
   for (j in 1:ncol(data_reduced)){
     m[i,j]=try(GKtau(data_reduced[,i],data_reduced[,j])$tauxy) #"try" because certain pairs require too much memory to process
   }
}

#Change non numeric values to NA
m<- as.numeric(m.test)
m[!is.numeric(m.test)] <- NA
m<-matrix(m.test,ncol = length(names), nrow = length(names), dimnames=list(names,names))


#Plot of GK associations 
title <- "Goodman Kurskal Associations"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(m.test, method="color", col=col(200), 
         title=title,
         addCoef.col = "black", # Add coefficient of correlation
         mar=c(0,0,1,0) # fix location of title
)

#alternate plot
corrplot(m,method = "number")
