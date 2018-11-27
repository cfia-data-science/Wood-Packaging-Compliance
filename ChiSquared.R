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
