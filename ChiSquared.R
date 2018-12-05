#Adding year to end of data and creating contingency table
year <- as.numeric(format(as.Date(data$Exam.Date), "%y"))
data<-cbind(data,year)

#Add variable with year and month
yearMonth<- format(as.Date(data$Exam.Date),"%y, %m")
data<-cbind(data,yearMonth)

#Create contingency tables
yearlySummary<-table(data$year, data$Compliant..Y.N.)
yearlySummary<- as.data.frame.matrix(yearlySummary)
yearlySummary$year <- c(2009:2018)
monthlySummary<-table(data$yearMonth,data$Compliant..Y.N.)
monthlySummary<-as.data.frame.matrix(monthlySummary)

#chi squared test
chisq.test(monthlySummary)
chisq.test(yearlySummary)

#subset data for 2014-2017
data2014_17<-subset(data, data$year>13 & data$year<18)
monthlySummary2014_17<-table(data2014_17$yearMonth,data2014_17$Compliant..Y.N.)
monthlySummary2014_17<-as.data.frame.matrix(monthlySummary2014_17)
monthlySummary2014_17<- na.omit(subset(monthlySummary2014_17, monthlySummary2014_17!=0))

#chi squared test
chisq.test(monthlySummary2014_17)

#Creating table with rate of compliance
yearlySummaryRate<- yearlySummary
yearlySummaryRate$Rate <- yearlySummary$Y/(yearlySummary$Y+yearlySummary$N)
yearlySummaryRate$Y<-NULL
yearlySummaryRate$N<-NULL
yearlySummaryRate$year<- c(2009:2018)
yearlySummaryRate<- yearlySummaryRate[,c(2,1)]

monthlySummaryRate<-monthlySummary
monthlySummaryRate$Rate <- monthlySummary$Y/(monthlySummary$Y+monthlySummary$N)
monthlySummaryRate$yearMonth<- row.names(monthlySummaryRate)

#plot compliance rate of each year
ggplot(data=yearlySummaryRate)+
  geom_line(mapping=aes(x=year, y= Rate), stat="identity", fill = "cadetblue")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  ggtitle("Compliance Rate by Year", subtitle = "2009-2018")+
  theme_minimal()

#melt yearly summary
yearlySummary.m <- melt(yearlySummary, id.vars='year')

#plot compliance each year
ggplot()+
  geom_bar(data = yearlySummary.m, mapping = aes(year, value, fill=variable), stat="identity", position = position_dodge())+
  ggtitle("Compliance by Year", subtitle = "2009-2018")
  #geom_line(data=yearlySummaryRate, aes(x=year, y=Rate))
  
# plot of compliance rate each month
ggplot(data=monthlySummaryRate)+
  geom_bar(mapping=aes(x=yearMonth, y= Rate), stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")

data_reduced <- data[,c(2,3,4,6,7,8,10,12)]

chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) {
      m[i,j] = chisq.test(x[,i],x[,j],)$p.value
    }
  }
  return (m)
}

m<- as.data.frame.matrix(chisqmatrix(data_reduced))

chisq.test(data$Compliant..Y.N., data$Compliant..Y.N.)

names = colnames(data_reduced);  num = length(names)
m = matrix(nrow=num,ncol=num,dimnames=list(names,names))

m[1,1]<-chisq.test(data_reduced[,1],data_reduced[,1])$p.value
m[1,2]<-chisq.test(data_reduced[,1],data_reduced[,2])$p.value
m[1,3]<-chisq.test(data_reduced[,1],data_reduced[,3])$p.value
m[1,4]<-chisq.test(data_reduced[,1],data_reduced[,4])$p.value
m[1,5]<-chisq.test(data_reduced[,1],data_reduced[,5])$p.value
m[1,6]<-chisq.test(data_reduced[,1],data_reduced[,6])$p.value
m[1,7]<-chisq.test(data_reduced[,1],data_reduced[,7])$p.value
m[1,8]<-chisq.test(data_reduced[,1],data_reduced[,8])$p.value

m[2,1]<-chisq.test(data_reduced[,2],data_reduced[,1])$p.value
m[2,2]<-chisq.test(data_reduced[,2],data_reduced[,2])$p.value
m[2,3]<-chisq.test(data_reduced[,2],data_reduced[,3])$p.value
m[2,4]<-chisq.test(data_reduced[,2],data_reduced[,4])$p.value
m[2,5]<-chisq.test(data_reduced[,2],data_reduced[,5])$p.value
m[2,6]<-chisq.test(data_reduced[,2],data_reduced[,6])$p.value
m[2,7]<-chisq.test(data_reduced[,2],data_reduced[,7])$p.value
m[2,8]<-chisq.test(data_reduced[,2],data_reduced[,8])$p.value

m[3,1]<-chisq.test(data_reduced[,3],data_reduced[,1])$p.value
m[3,2]<-chisq.test(data_reduced[,3],data_reduced[,2])$p.value
m[3,3]<-chisq.test(data_reduced[,3],data_reduced[,3])$p.value
m[3,4]<-chisq.test(data_reduced[,3],data_reduced[,4])$p.value
m[3,5]<-chisq.test(data_reduced[,3],data_reduced[,5])$p.value
m[3,6]<-chisq.test(data_reduced[,3],data_reduced[,6])$p.value
m[3,7]<-chisq.test(data_reduced[,3],data_reduced[,7])$p.value
m[3,8]<-chisq.test(data_reduced[,3],data_reduced[,8])$p.value

m[4,1]<-chisq.test(data_reduced[,4],data_reduced[,1])$p.value
m[4,2]<-chisq.test(data_reduced[,4],data_reduced[,2])$p.value
m[4,3]<-chisq.test(data_reduced[,4],data_reduced[,3])$p.value
m[4,4]<-chisq.test(data_reduced[,4],data_reduced[,4])$p.value
m[4,5]<-chisq.test(data_reduced[,4],data_reduced[,5])$p.value
m[4,6]<-chisq.test(data_reduced[,4],data_reduced[,6])$p.value
m[4,7]<-chisq.test(data_reduced[,4],data_reduced[,7])$p.value
m[4,8]<-chisq.test(data_reduced[,4],data_reduced[,8])$p.value

m[5,1]<-chisq.test(data_reduced[,5],data_reduced[,1])$p.value
m[5,2]<-chisq.test(data_reduced[,5],data_reduced[,2])$p.value
m[5,3]<-chisq.test(data_reduced[,5],data_reduced[,3])$p.value
m[5,4]<-chisq.test(data_reduced[,5],data_reduced[,4])$p.value
m[5,5]<-chisq.test(data_reduced[,5],data_reduced[,5])$p.value
m[5,6]<-chisq.test(data_reduced[,5],data_reduced[,6])$p.value
m[5,7]<-chisq.test(data_reduced[,5],data_reduced[,7])$p.value
m[5,8]<-chisq.test(data_reduced[,5],data_reduced[,8])$p.value

m[6,1]<-chisq.test(data_reduced[,6],data_reduced[,1])$p.value
m[6,2]<-chisq.test(data_reduced[,6],data_reduced[,2])$p.value
m[6,3]<-chisq.test(data_reduced[,6],data_reduced[,3])$p.value
m[6,4]<-chisq.test(data_reduced[,6],data_reduced[,4])$p.value
m[6,5]<-chisq.test(data_reduced[,6],data_reduced[,5])$p.value
m[6,6]<-chisq.test(data_reduced[,6],data_reduced[,6])$p.value
m[6,7]<-chisq.test(data_reduced[,6],data_reduced[,7])$p.value
m[6,8]<-chisq.test(data_reduced[,6],data_reduced[,8])$p.value

m[7,1]<-chisq.test(data_reduced[,7],data_reduced[,1])$p.value
m[7,2]<-chisq.test(data_reduced[,7],data_reduced[,2])$p.value
m[7,3]<-chisq.test(data_reduced[,7],data_reduced[,3])$p.value
m[7,4]<-chisq.test(data_reduced[,7],data_reduced[,4])$p.value
m[7,5]<-chisq.test(data_reduced[,7],data_reduced[,5])$p.value
m[7,6]<-chisq.test(data_reduced[,7],data_reduced[,6])$p.value
m[7,7]<-chisq.test(data_reduced[,7],data_reduced[,7])$p.value
m[7,8]<-chisq.test(data_reduced[,7],data_reduced[,8])$p.value

m[8,1]<-chisq.test(data_reduced[,8],data_reduced[,1])$p.value
m[8,2]<-chisq.test(data_reduced[,8],data_reduced[,2])$p.value
m[8,3]<-chisq.test(data_reduced[,8],data_reduced[,3])$p.value
m[8,4]<-chisq.test(data_reduced[,8],data_reduced[,4])$p.value
m[8,5]<-chisq.test(data_reduced[,8],data_reduced[,5])$p.value
m[8,6]<-chisq.test(data_reduced[,8],data_reduced[,6])$p.value
m[8,7]<-chisq.test(data_reduced[,8],data_reduced[,7])$p.value
m[8,8]<-chisq.test(data_reduced[,8],data_reduced[,8])$p.value

# plot of chi squared values. they are all zeroes so there is nothing meaningful from it
title <- "Chi Squared Values"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(m, method="color", col=col(200), 
         title=title, 
         addCoef.col = "black", # Add coefficient of correlation
         mar=c(0,0,1,0)
) 

library(grid)
library(gridExtra)
library(gtable)

t1 <- tableGrob(m)
title <- textGrob("Chi Squared P Values",gp=gpar(fontsize=30))
padding <- unit(10,"mm")

table <- gtable_add_rows(
  t1, 
  heights = grobHeight(title) + padding,
  pos = 0)
table <- gtable_add_grob(
  table, 
  title, 
  1, 1, 1, ncol(table))

grid.newpage()
grid.draw(table)

#chisquared test for months
monthBreakdown<-as.data.frame.matrix(table(data$Month, data$Compliant..Y.N.))
monthBreakdown$Rate <- monthBreakdown$N/(monthBreakdown$N+monthBreakdown$Y)
chisq.test(monthBreakdown)

