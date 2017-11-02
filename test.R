

rm(list=ls())
source("IndicatorFunctions.R")
source("ReadIndicatorType.R")
source("CalculateIndicatorSupport.R")
source("IndicatorFunctions.R")
#source("Assessment_test.R")
source("Assessment.R")
source("ReadBounds.R")
source("ReadVariances.R")
source("ReadIndicatorParms.R")
source("ReadMonitoringData.R")
source("Aggregation.R")


library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(shiny)
library(dplyr)
library(prodlim)

#----------------------------------------------------------------------

df<-read.table("data/data.txt", fileEncoding = "UTF-8", sep=";", stringsAsFactors=F, header=T)
#df<-filter(df,!is.na(sali))
#df<-filter(df,WB_ID=="SE621688-144133") #Västra Blekinge
#df<-filter(df,WB_ID=="SE574000-114230") #Danafjord


df.wb<-read.table("data/waterbodies.txt", fileEncoding = "UTF-8", sep="\t", stringsAsFactors=F, header=T)
df[df$WB_name=="Halse/Askeröfj","WB_name"]<-"Halsefjorden"
df<-df %>% left_join(select(df.wb,WaterbodyName,WaterbodyID,DistrictID), by=c("WB_name"="WaterbodyName")) 
df$WB<-paste0(df$WaterbodyID," ",df$WB_name)  
df$obspoint<-df$station
df$typology<-ifelse(is.na(df$typology),df$typology,"SE_2") # was missing for BQI stations
#df.select <- filter(df, period=="2001-2006")
#df.select <- filter(df, WB %in% c("SE574000-114230 Dana fjord"), period=="2001-2006")
#df.select <- filter(df, WB %in% c("SE574000-114230 Dana fjord"),period=="2001-2006")
#Indicators<-c("ChlaEQR","TNsummer","TNwinter","Secchi")
df.select <- filter(df, !is.na(BQI),period=="2007-2012", WB=="SE581740-114820 Havstensfjorden")
df.select$obspoint<-df.select$station
Indicators<-c("BQI")

#df.select<-df

nSimMC <- 100

#Indicators<-c("ChlaEQR","TNsummer","TNwinter","Secchi","MSMDI","BQI")
bReload<-FALSE
#bReload<-TRUE
bSave<-FALSE

if(bReload==TRUE){
  load('data/SavedResults.Rda')
  
}else{
  AssessmentResults<-Assessment(df.select,nsim=nSimMC,
                                IndicatorList=Indicators)
}
if(bSave==TRUE){
  save(AssessmentResults,file='data/SavedResults.Rda')
}

df.resultsAvg<-AssessmentResults[[1]]
df.resultsMC<-AssessmentResults[[2]]
df.err<-AssessmentResults[[3]]

source("Aggregation.R")
test1<-Aggregate(df.resultsAvg,Groups=c("WB","Period","Type"),level=1)
test2<-Aggregate(df.resultsAvg,Groups=c("WB","Period","Type"),level=2)
test3<-Aggregate(df.resultsAvg,Groups=c("WB","Period","Type"),level=3)
test4<-Aggregate(df.resultsAvg,Groups=c("WB","Period","Type"),level=4)


#test2<-Aggregate(df.resultsAvg,Groups=c("WB","Period","Type"))
ford1<-Aggregate(df.resultsMC,Groups=c("WB","Period","Type","sim"))

ford2<-Frequency(ford1,Groups=c("WB","Period","Type"))
    



