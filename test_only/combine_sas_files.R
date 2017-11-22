rm(list = ls())
library(haven)
library(tidyverse)
source("test_only/ReadMonitoringData.R")

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

LoadSASlist<-function(wblist,pdlist){
  
  dflist<-list()
  n<-0
  for (i in 1:length(wblist)){
    for (j in 1:length(pdlist)){
      sasfile<-paste0("data/",wblist[i],"_",pdlist[j],".sas7bdat")
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
  return(df)
}


#wb<-c("halse","marstrand","havsten")
wb1<-c("danafjord","gullmarn","koljoefjord","byfjorden","vblekinge","mblekinge","halse","marstrand","havsten")
wb2<-c("gullmarno2x","byfjordeno2x")
pd<-c("2001_2006","2007_2012","2013_2016")

df1<-LoadSASlist(wb1,pd)
df2<-LoadSASlist(wb2,pd)

#wb<-c("danafjord","halse")
#pd<-c("2007_2012")

WBname<-df1 %>% distinct(station,x_utm,y_utm,WB_ID,WB_name,type,TYP_NFS06,typology,country,institution)
Periods<-df1 %>% distinct(year,period)

df2 <- df2 %>% select(-WB_name) %>% left_join(WBname) %>% left_join(Periods)
#,by=c("station"="station")


df<-bind_rows(df1,df2)

write.table(df,file="data/data.txt",row.names=F,na="",sep=";")



grouplist<-c("station","x_utm","y_utm","WB_name","TYP_NFS06","WB_ID","area",
"institution","type","typology","country","obspoint","station_depth","date",
"year","month","period")

varlist<-c("chla","sali","TP","TN","secchi","temp","MSMDI","logitMSMDI","BQI","springlag","dens_dif")

for (i in 1:length(varlist)){
  collist<-c(grouplist,varlist[i])
  dftemp<-df1 %>% select(collist)
  names(dftemp)[names(dftemp)==varlist[i]]<-"value"
  dftemp$param<-varlist[i]
  if (i==1){
    df<-dftemp
  }else{
    df<-bind_rows(df,dftemp)
  }
}

df$depth<-NA
df2<-df2 %>% rename(value=O2) %>% mutate(param="O2")

df<-bind_rows(df,df2)

df<-df %>% filter(!is.na(value))

df<-df %>% select(-c(TYP_NFS06,x_utm,y_utm,area,country))

#write.table(df,file="data/data.txt",row.names=F,na="",sep=";")
#write.table(df,file="data/data_bqi.txt",row.names=F,na="",sep=";")

write.table(df,file="data/data_long.txt",row.names=F,na="",sep=";")

