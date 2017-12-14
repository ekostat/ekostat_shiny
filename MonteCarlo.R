# Offline processing of indicators
# Do Monte Carlo simulations based on variance components
# Save results to be used by shiny app

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
nSimMC <- 1000 # number of Monte Carlo simulations

#Read observation data
df <- read.table("data/data.txt",fileEncoding = "UTF-8",
                 sep = ";",stringsAsFactors = F,header = T)
df <- df %>% select(-c(TYP_NFS06, x_utm, y_utm, area))

#Read waterbody data
df.wb <- read.table("data/waterbodies.txt",fileEncoding = "UTF-8",
                    sep = "\t",stringsAsFactors = F,header = T)

#Fix observation data
df[df$WB_name == "Halse/Askeröfj", "WB_name"] <- "Halsefjorden"
df <-
  df %>% left_join(
    select(df.wb, WaterbodyName, WaterbodyID, DistrictID),
    by = c("WB_name" = "WaterbodyName")
  )

df$WB <- paste0(df$WaterbodyID, " ", df$WB_name)
df$obspoint <- df$station
df$typology <-
  ifelse(is.na(df$typology), df$typology, "SE_2") # was missing for BQI stations

if(is.null(df$DIN)){df$DIN<-NA}
if(is.null(df$DIP)){df$DIP<-NA}

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

AssessmentResults <- Assessment(df, nsim = nSimMC, IndList)

resAvg <- AssessmentResults[[1]]
resMC <- AssessmentResults[[2]]
resErr <- AssessmentResults[[3]]

cat(paste0("Time elapsed: ",Sys.time() - start_time))

save(AssessmentResults,file="AssessmentResults.Rda")
