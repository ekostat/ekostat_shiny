rm(list = ls())

source("ReadMonitoringData.R")

wb<-c("danafjord","gullmarn","koljoefjord","byfjorden")
pd<-c("2001_2006","2007_2012","2013_2016")

dflist<-list()
n<-0
for (i in 1:length(wb)){
  for (j in 1:length(pd)){
    n<-n+1
    dftemp<-ReadMonitoringDataSMHI(paste0("data/",wb[i],"_",pd[j],".sas7bdat"))
    if (n==1){
      df<-dftemp
    }else{
      df<-bind_rows(df,dftemp)
    }
    dflist[n]<-dftemp
  }
}

write.table(df,file="data/data.txt",row.names=F,na="",sep=";")


