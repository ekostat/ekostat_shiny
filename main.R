library(tidyverse)
library(haven)
library(lme4)
library(lubridate)

library(extrafont)
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

# source("R/extract_modis.R") # ATTENTION: takes few minutes

# Clean the workspace
rm(list = ls())

source("ReadMonitoringData.R")
source("ReadIndicatorParms.R")
source("CalculateIndicator.R")

# Read data set for specific waterbody and period
df<-ReadMonitoringDataSMHI("data/Gullmarn_2007_2012.sas7bdat")

# Read general parameters for the indicator
parmlist <- ReadParms_chla()
# Calculate the indicator
CalculateIndicator_Chla(df,parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                        parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                        parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"])




