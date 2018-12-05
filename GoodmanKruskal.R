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

#fixing error values
m[3,3]<- 1
m[4,3]<- NA 
m[6,3]<- NA
m[3,4]<- NA
m[4,4]<- 1
m[6,4]<- NA
m[3,6]<- NA
m[4,6]<- NA
m[6,6]<- 1

# change to numeric
m <- apply(m,c(1, 2),as.numeric) 


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
