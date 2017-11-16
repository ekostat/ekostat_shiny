rm(list = ls())
library(tidyverse)
source("ReadMonitoringData.R")

FixNames<-function(df){
  
  if(length(names(df)[names(df)=="BQI"])>0){
    df$obspoint<-as.character(df$obspoint)
  }
  
  if(length(names(df)[names(df)=="region"])>0){
    df$region<-paste0("SE_",df$region)
    names(df)[names(df)=="region"]<-"typology"
  }
  
  if(length(names(df)[names(df)=="waterbody"])>0){
    names(df)[names(df)=="waterbody"]<-"WB_name"
  }
  
  if(length(names(df)[names(df)=="waterbody_id"])>0){
    df$waterbody_id<-paste0("SE",df$waterbody_id)
    names(df)[names(df)=="waterbody_id"]<-"WB_ID"
  }
  if(length(names(df)[names(df)=="djup"])>0){
    names(df)[names(df)=="djup"]<-"station_depth"
  }
  
  if(length(names(df)[names(df)=="Period_no"])>0){
    df$Period_no<-NULL
    }
  if(length(names(df)[names(df)=="transect"])>0){
    df$transect<-NULL
  }
  if(length(names(df)[names(df)=="Season"])>0){
    df$Season<-NULL
  }
  if(length(names(df)[names(df)=="Unders_kning"])>0){
    df$Unders_kning<-NULL
  }
  
  names(df)[names(df)=="Year"]<-"year"
  names(df)[names(df)=="Month"]<-"month"
  
  return(df)
}

wb<-c("danafjord","gullmarn","koljoefjord","byfjorden","vblekinge","mblekinge","halse","marstrand","havsten")
#wb<-c("halse","marstrand","havsten")
pd<-c("2001_2006","2007_2012","2013_2016")

#wb<-c("danafjord","halse")
#pd<-c("2007_2012")

dflist<-list()
n<-0
for (i in 1:length(wb)){
  for (j in 1:length(pd)){
    sasfile<-paste0("data/",wb[i],"_",pd[j],".sas7bdat")
    cat(paste0(sasfile,"\n"))
    if (file.exists(sasfile)) {
      n<-n+1
      dftemp<-ReadMonitoringDataSMHI(sasfile)
      dftemp<-FixNames(dftemp)
      if (n==1){
        df<-dftemp
      }else{
        df<-bind_rows(df,dftemp)
      }
      dflist[n]<-dftemp
    }
  }
}

#write.table(df,file="data/data.txt",row.names=F,na="",sep=";")
#write.table(df,file="data/data_bqi.txt",row.names=F,na="",sep=";")


