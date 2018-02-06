# Offline processing of indicators
# Do Monte Carlo simulations based on variance components
# Save results to be used by shiny app as R data file 
# AssessmentResults.Rda

rm(list = ls())

library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(prodlim)
library(matrixStats)


source("ReadIndicatorParms.R")
source("IndicatorFunctions.R")
source("CalculateIndicatorSupport.R")
source("Assessment.R")
source("ReadBounds.R")
source("ReadBathymetry.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")
source("Aggregation.R")
source("classoutputtable.R")

start_time <- Sys.time()
nSimMC <- 3 #1000 # number of Monte Carlo simulations

df_water<-read_sas("data/coast_watersamples.sas7bdat") #19824
df_o2<-read_sas("data/coast_oxygen.sas7bdat")          #94747
df_bqi<-read_sas("data/coast_bqi.sas7bdat")            # 7142


#remove problematic O2 profiles
distinct_O2 <- df_o2 %>% group_by(station,date,time,station_depth,O2) %>% summarise() %>%
  ungroup() %>% group_by(station,date,time,station_depth) %>% summarise(n_O2=n()) %>% filter(n_O2==1)

df_o2 <- df_o2 %>% left_join(distinct_O2,by=c("station","date","time","station_depth"))
df_o2 <- df_o2 %>% mutate(O2=ifelse(n_O2==1,NA,O2)) %>% select(-n_O2)

df <- bind_rows(df_water,df_o2,df_bqi)
df <- df %>% mutate(WB_ID=paste0("SE",WB_ID))

#Read waterbody data
df_wb <- read.table("data/waterbodies.txt",fileEncoding = "UTF-8",
                    sep = "\t",stringsAsFactors = F,header = T)
df_wb <- df_wb %>% select(WaterbodyName, WaterbodyID, DistrictID, TypeID) %>%
  arrange(WaterbodyName)

#Fix observation data
df[df$WB_name=="Dragviksfjärden", "WB_name"] <- "Dragsviksfjärden"
df[df$WB_ID=="SE590020-114520", "WB_ID"] <- "SE0101010201-C" #Inre Idefjorden
df[df$WB_ID=="SE590670-111380", "WB_ID"] <- "SE0101010301-C" #Singlefjorden
df[df$WB_ID=="SE673283-158060", "WB_ID"] <- "SE604200-171765" #Yttre Fjärden

#Join waterbody district information to obs data
df <- df %>%
  left_join(df_wb,by = c("WB_name"="WaterbodyName","WB_ID"="WaterbodyID")) %>% 
  mutate(type=ifelse(type=="",TypeID,type))

df[df$WB_ID=="SE641840-211540", "DistrictID"] <- "SE1" #Inre Lövselefjärden
df[df$WB_ID=="SE634350-202000", "DistrictID"] <- "SE1" #Inre Österfjärden
df[df$WB_ID=="SE641720-211520", "DistrictID"] <- "SE1" #Yttre Lövselefjärden
df[df$WB_ID=="SE634110-201920", "DistrictID"] <- "SE1" #Yttre Österfjärden
df[df$WB_ID=="SE647050-213980", "DistrictID"] <- "SE1" #S m Bottenvikens kustvatten
df[df$WB_ID=="SE634210-202020", "DistrictID"] <- "SE1" #Holmsund*
df[df$WB_ID=="SE634210-202020", "type"] <- "22"        #Holmsund*
# *In VISS, I can only find vatttendrag with this name but it is SE1 and Luleå kommun

#Fix records with missing type, using other records for the same waterbody
type<-df %>% filter(!type=="") %>% group_by(WB_ID,type) %>% summarise() %>%
  rename(type2=type)

df <- df %>% left_join(type,by=c("WB_ID"="WB_ID")) %>% mutate(type=ifelse(type=="",type2,type)) %>% select(-type2)

df <- df %>% select(DistrictID,type,station,WB_name,WB_ID,institution,station_depth,
                    date,time,temp,sali,depth,secchi,
                    DIN,DIP,TN,TP,chla,biovol,O2,BQI)

df <- df %>% mutate(MSMDI=NA) # Find out what happened to MSMDI values! 

df$WB <- paste0(df$WB_ID, " ", df$WB_name)
df$obspoint <- df$station

df <- df %>% 
  mutate(type=ifelse(substr(type,1,1)=="0",substr(type,2,4),type)) %>% 
  rename(typology=type)


df <- df %>% mutate(year=year(date),month=month(date)) %>% 
  mutate(period=ifelse(year<2001,NA,ifelse(year<2007,"2001-2006",ifelse(year<2013,"2007-2012","2013-2017"))))

IndList<-c("CoastChla",         #Chlorophyll a
           "CoastChlaEQR",      #Chlorophyll a (EQR)
           "CoastTNsummer",     #Summer TN
           "CoastTNsummerEQR",  #Summer TN (EQR)
           "CoastTNwinter",     #Winter TN
           "CoastTNwinterEQR",  #Winter TN (EQR)
           "CoastTPsummer",     #Summer TP
           "CoastTPsummerEQR",  #Summer TP (EQR)
           "CoastTPwinter",     #Winter TP
           "CoastTPwinterEQR",  #Winter TP (EQR)
           "CoastDINsummer",    #Summer DIN 
           "CoastDINsummerEQR", #Summer DIN (EQR) 
           "CoastDIPsummer",    #Summer DIP 
           "CoastDIPsummerEQR", #Summer DIP (EQR) 
           "CoastSecchi",       #Secchi Depth 
           "CoastSecchiEQR",    #Secchi Depth (EQR) 
           "CoastBQI",          #Benthic Quality Index (BQI) 
           "CoastMSMDI",        #Multi Species Maximum Depth Index (MSMDI) 
           "CoastOxygen")       #Dissolved Oxygen (O2) 

IndList<-c("CoastChla","CoastChlaEQR",
           "CoastTNsummer","CoastTNsummerEQR",
           "CoastTNwinter","CoastTNwinterEQR",
           "CoastTPsummer","CoastTPsummerEQR",
           "CoastTPwinter","CoastTPwinterEQR",
           "CoastSecchi","CoastSecchiEQR",
           "CoastBQI","CoastMSMDI","CoastOxygen") 


#Problem data - to be analysed later. Removing these is just a quick fix!



#df$O2<-ifelse(df$WB=="SE552500-124461 S Öresunds kustvatten",NA,df$O2)
#df<-df %>% filter(WB=="SE554800-142001 V Hanöbuktens kustvatten")

AssessmentResults <- Assessment(df, nsim = nSimMC, IndList)

resAvg <- AssessmentResults[[1]]
resMC <- AssessmentResults[[2]]
resErr <- AssessmentResults[[3]]

cat(paste0("Time elapsed: ",Sys.time() - start_time))

save(AssessmentResults,file="AssessmentResultsX.Rda")

# 
# df2 <- read.table("data/data.txt",fileEncoding = "UTF-8",
#                  sep = ";",stringsAsFactors = F,header = T)
# df2 <- df2 %>% select(-c(TYP_NFS06, x_utm, y_utm, area))
# 
# names1<-data.frame(name=names(df))
# names2<-data.frame(name=names(df2))
# names2$X<-1
# names1<-names1 %>% left_join(names2)
