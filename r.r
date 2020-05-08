#Importing date library
library(lubridate)

#Main dataset to be imported from the same library
DF= read.csv("data.csv")
#India dataset
IDF=DF[DF$Country.Region=="India" & month(mdy(DF$ObservationDate)) >=4,]
#World dataset 
WDF=DF[month(mdy(DF$ObservationDate)) >=4,]
#Listing out unique dates
dates = unique(IDF$ObservationDate)

#Grouping based on observation date
#Data regarding deaths, confirmed cases and recovered cases of both nationn and world
IConfData = rowsum(IDF$Confirmed, IDF$ObservationDate)
IDeaData = rowsum(IDF$Deaths, IDF$ObservationDate)
IRecData = rowsum(IDF$Recovered, IDF$ObservationDate)
WConfData = rowsum(WDF$Confirmed, WDF$ObservationDate)
WDeaData = rowsum(WDF$Deaths, WDF$ObservationDate)
WRecData = rowsum(WDF$Recovered, WDF$ObservationDate)

no_days = seq(1:length(IConfData))

#dataframe with required columns
Idf = data.frame(no_days,dates,conf = log10(IConfData), dea = log10(IDeaData), rec = log10(IRecData))
Wdf = data.frame(no_days,dates,conf = log10(WConfData), dea = log10(WDeaData), rec = log10(WRecData))

#Different models
ICmodel = lm(Idf$conf~Idf$no_days)
IDmodel = lm(Idf$dea~Idf$no_days)
IRmodel = lm(Idf$rec~Idf$no_days)
WCmodel = lm(Wdf$conf~Wdf$no_days)
WDmodel = lm(Wdf$dea~Wdf$no_days)
WRmodel = lm(Wdf$rec~Wdf$no_days)


#Initial date
init_date = mdy(IDF[1,2])
#Predicting for next 120 days
xdays=seq(1,120)
#Creating an empty dataframe
resdf=data.frame(Date=c(), I_Confirmed=c(),I_Death=c(),I_Recovered=c(), W_Confirmed=c(),W_Death=c(), W_Recovered=c())

#Each day is predicted for next 120 days
for (i in xdays){
  IC=10^(ICmodel$coefficients[1]+i*ICmodel$coefficients[2])
  ID=10^(IDmodel$coefficients[1]+i*IDmodel$coefficients[2])
  IR=10^(IRmodel$coefficients[1]+i*IRmodel$coefficients[2])
  
  WC=10^(WCmodel$coefficients[1]+i*WCmodel$coefficients[2])
  WD=10^(WDmodel$coefficients[1]+i*WDmodel$coefficients[2])
  WR=10^(WRmodel$coefficients[1]+i*WRmodel$coefficients[2])
  
  kl=data.frame(as.Date(as.Date(init_date)+(i-1)),round(IC,0),round(ID,0),round(IR,0),round(WC,0),round(WD,0),round(WR,0))
  names(kl) = c("Date", "I_Confirmed", "I_Death","I_Recovered", "W_Confirmed", "W_Death","W_Recovered")
  resdf<-rbind(resdf, kl)
}

#Creating a csv file to export our predicted data
write.csv(resdf,paste(getwd(),"Prediction.csv",sep="/"))
