library(tidyverse)
library(haven)
library(lme4)
library(lubridate)

library(extrafont)
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))


# Clean the workspace
rm(list = ls())

source("ReadMonitoringData.R")
source("ReadIndicatorParms.R")
source("CalculateIndicator.R")

# Read data set for specific waterbody and period
#df <- ReadMonitoringDataSMHI("data/Gullmarn_2007_2012.sas7bdat")
df <- ReadMonitoringDataSMHI("data/danafjord_2013_2016.sas7bdat")


# Read general parameters for the indicator
parmlist <- ReadParms_chla()
# Calculate the indicator
CalculateIndicator_Chla(df,c(6,7,8),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                        parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                        parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"])

# type with no salinity correction, e.g. type=6
RefCond_sali <- c(rep(0.9,36))
# type with salinity correction, e.g. type=8
RefCond_sali <- c(15.7,12.4,9.5,6.9,4.8,3.0,1.7,rep(1.29,29))

CalculateIndicator_ChlaEQR(df,RefCond_sali,c(6,7,8),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                        parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                        parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"])

# type with salinity correction, e.g. type=8 in summer
RefCond_sali <- c(56,50,43,37,31,24,18,rep(15,29))
CalculateIndicator_nutrientEQR("TNsummer",df,RefCond_sali,c(6,7,8),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                           parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                           parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"])
# type with salinity correction, e.g. type=8 in winter
RefCond_sali <- c(56,50,44,38,32,26,20,rep(17,29))
CalculateIndicator_nutrientEQR("TNwinter",df,RefCond_sali,c(11,12,1,2),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                               parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                               parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"])
