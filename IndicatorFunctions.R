# Aggregation principle used for Chlorophyll
Aggregate_year_station <- function(df) {
  yearmeans <- df %>%    group_by(year,station) %>%
                         summarise(xvar = mean(xvar)) %>%
                         group_by(year) %>%
                         summarise(xvar = mean(xvar))
                         
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for nutrients
Aggregate_year <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvar))
  
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for Secchi depth
Aggregate_period <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvar))
  
  periodmean <- mean(df$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for Chlorophyll
AggregateEQR_year_station <- function(df) {
  
  df <- mutate(df,xvarEQR = ifelse(xvar<RefCond,1,RefCond/xvar))

  yearmeans <- df %>%    group_by(year,station) %>%
    summarise(xvarEQR = mean(xvarEQR)) %>%
    group_by(year) %>%
    summarise(xvar = mean(xvarEQR))   # should be returned in xvar
  
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# Aggregation principle used for nutrients
AggregateEQR_year <- function(df) {
  
  df <- mutate(df,xvarEQR = RefCond/xvar)
  
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvarEQR))   # should be returned in xvar
  
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# Indicator response negative to degradation, i.e. Secchi depth
AggregateEQR_N_period <- function(df) {
  
  df <- mutate(df,xvarEQR = xvar/RefCond)
  
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvarEQR))   # should be returned in xvar
  
  periodmean <- mean(df$xvarEQR)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# Calculation of BQI indicator according to Handbook
BQIbootstrap <- function(df) {
  yearstatmeans <- df %>%    group_by(year,station) %>%
    summarise(xvar = mean(xvar))
  Nyearstat <- yearstatmeans %>% group_by(year) %>% summarise(n_station = length(xvar))
  BQIsimyear <- mat.or.vec(length(Nyearstat$n_station), 1)
  for(i in 1:length(Nyearstat$n_station)) {
     BQIsim <- trunc(runif(9999,1,Nyearstat$n_station[i]+1))
     BQIsim <- yearstatmeans$xvar[yearstatmeans$year == Nyearstat$year[i]][BQIsim]
     BQIsimyear[i] <- quantile(BQIsim,probs=0.2)
  }

  periodmean <- mean(BQIsimyear)
  yearmeans <- data.frame(year=Nyearstat$year,xvar = BQIsimyear)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# Calculation of Oxygen indicator according to Handbook
# First test calculates the average of O2 observations below the 25%-percentile - threshold is 3.5 ml/l
# Second test calculates the average of O2 observations (Jan-May) below the 25%-percentile 
OxygenTest1 <- function(df) {
  years <- df %>% group_by(year) %>% summarise()
  df1 <- filter(df,xvar<=quantile(xvar,na.rm=TRUE)[2])
  O2_test1 <- mean(df1$xvar)
  O2_test1_yearmeans <- df1 %>% group_by(year) %>% summarise(xvar = mean(xvar))
  O2_test1_yearmeans <- left_join(years,O2_test1_yearmeans,c("year"))
  df2 <- df1 %>% filter(month %in% c(1,2,3,4,5)) 
  O2_test2 <- mean(df2$xvar)
  O2_test2_yearmeans <- df2 %>% group_by(year) %>% summarise(xvar = mean(xvar))
  O2_test2_yearmeans <- left_join(years,O2_test2_yearmeans,c("year"))
  yearmeans <- data.frame(year=O2_test1_yearmeans$year,xvar = O2_test1_yearmeans$xvar)
  res <- list(periodmean=O2_test1,yearmeans=yearmeans,error_code=0)
  return(res)
}

# df contains oxygen profiles
OxygenTest2 <- function(df) {
  df <- filter(df,!is.na(xvar))
  # create list of years for producing vectors of similar length in the returned list
  years <- df %>% group_by(year) %>% summarise()
  # find depth for 3.5 ml/l by extrapolation, when this threshold is not in the profile
  O2bottom_ext <- df %>% group_by(station,date,time,station_depth) %>% summarise(O2bottom1 = xvar[which.max(depth)],O2bottom2 = xvar[which.max(depth)-1],depth1 = depth[which.max(depth)],depth2 = depth[which.max(depth)-1],n_obs =n(),
                   O2clinedepth_max=ifelse(n_obs>1,ifelse(O2bottom1>3.5 && O2bottom2-O2bottom1>0,depth1+(3.5-O2bottom1)/(O2bottom2-O2bottom1)*(depth2-depth1),1000),NaN))
  # find profile statistics for tests and indicator calculation
  O2bottom <- df %>% group_by(station,date,time,station_depth) %>% summarise(max_depth=max(depth),O2bottom = xvar[which.max(depth)],O2clinedepth = approx(xvar,depth,c(3.5))$y)
  # If O2clinedepth is not in the profile then use the extrapolated value
  O2bottom <- full_join(O2bottom,O2bottom_ext,c("station","date","time","station_depth"))
  O2bottom <- O2bottom %>% mutate(O2clinedepth = ifelse(is.na(O2clinedepth),O2clinedepth_max,O2clinedepth),depth1 = NULL, depth2 = NULL, O2bottom1 = NULL, O2bottom2 = NULL, O2clinedepth_max = NULL)
  # Find the percent area affected by O2 concentrations <3.5 ml/l
  O2bottom <- O2bottom %>% mutate(area_hyp = 100-approx(WB_bathymetry$depth,WB_bathymetry$area_pct,O2clinedepth,yleft=0,yright=100)$y)
  # Calculate test1 as average of O2 observations at bottom (<1.5 m from bottom depth) below the 25-percentile for Jan-Dec
  lower_quantile <- quantile(O2bottom$O2bottom,na.rm=TRUE)[2]
  df1 <- O2bottom %>% filter(station_depth-max_depth<1.5) %>% filter(O2bottom<=lower_quantile) %>% mutate(year=lubridate::year(date))
#  df1 <- df %>% filter(station_depth-depth<1.5) %>% filter(xvar<=quantile(xvar,na.rm=TRUE)[2])
  O2_test1 <- mean(df1$O2bottom)
  O2_test1_yearmeans <- df1 %>% group_by(year) %>% summarise(O2bottom = mean(O2bottom))
  O2_test1_yearmeans <- left_join(years,O2_test1_yearmeans,c("year"))
  # Calculate EQR from Table 7.1 in Handbook
  EQR_test1 <- approx(c(-5.0,0.0,1.0,2.1,3.5,7.0),c(0,0.2,0.4,0.6,0.8,1),O2_test1,yleft=0,yright=1)$y
  EQR_test1_yearmeans <- approx(c(-10.0,0.0,1.0,2.1,3.5,7.0),c(0,0.2,0.4,0.6,0.8,1),O2_test1_yearmeans$O2bottom,yleft=0,yright=1)$y
  # Calculate test2 as average of O2 concentrations below the 25-percentile for Jan-May
  df2 <- O2bottom %>% mutate(month = lubridate::month(date),year = lubridate::year(date)) %>% filter(month %in% c(1,2,3,4,5))
  # Return from function if no observations available (Jan-May) for calculation of O2_test2
  if (nrow(df2) == 0) {
    return(list(error_code=-91))
  }
  O2_test2 <- mean(df2$O2bottom)
  O2_test2_yearmeans <- df2 %>% group_by(year) %>% summarise(O2bottom = mean(O2bottom))
  O2_test2_yearmeans <- left_join(years,O2_test2_yearmeans,c("year"))
  # Calculate indicator for percent area affected by <3.5 ml/l
  df2 <- O2bottom %>% mutate(month = lubridate::month(date),year = lubridate::year(date)) %>% filter(month %in% c(6,7,8,9,10,11,12))
  hyparea <- mean(df2$area_hyp)
  hyparea_yearmeans <- df2 %>% group_by(year) %>% summarise(area_hyp = mean(area_hyp))
  # Calculate EQR from Table 7.1 in Handbook
  EQR_test2 <- approx(BoundariesHypoxicArea,c(0,0.2,0.4,0.6,0.8,1),hyparea,yleft=0,yright=1)$y
  EQR_test2_yearmeans <- approx(BoundariesHypoxicArea,c(0,0.2,0.4,0.6,0.8,1),hyparea_yearmeans$area_hyp,yleft=0,yright=1)$y
  if (O2_test1>3.5 || O2_test2>3.5) {
    res <- list(periodmean=EQR_test1,yearmeans=data.frame(year=O2_test1_yearmeans$year,xvar = EQR_test1_yearmeans),error_code=0)
  } else {
    res <- list(periodmean=EQR_test2,yearmeans=data.frame(year=O2_test1_yearmeans$year,xvar = EQR_test2_yearmeans),error_code=0)
  }
  return(res)
}


#' Generic routine for calculating indicator statistics
#' 
#' @param df A dataframe with monitoring data from the Swedish Monitoring program. 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \describe{ 
#'   \item{station}{An identifier for the monitoring station.} 
#'   \item{date}{Date of the observation.} 
#'   \item{institution}{Provider of the observation.} 
#'   \item{chla}{Chlorophyll a concentration in sample.} 
#'   
#' @param MonthInclude A list of months to be included in the indicator
#' @param var_list List of variance components
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples 

CalculateIndicator <-
  function(Indicator,df,RefCond_sali,var_list,MonthInclude,startyear,endyear,n_iter=1000,confidence_lvl=0.95) {
    # Set flag to zero and change it for error handling below
    flag <- 0
# Select the observation variable for the indicator
    xvar <- switch(Indicator,
                   CoastChla         = df$chla,
                   CoastChlaEQR      = df$chla,
                   CoastBiovol       = df$biovol,
                   CoastBiovolEQR    = df$biovol,
                   CoastSecchi       = df$secchi,
                   CoastSecchiEQR    = df$secchi,
                   CoastDINwinter    = df$DIN,
                   CoastDINwinterEQR = df$DIN,
                   CoastDIPwinter    = df$DIP,
                   CoastDIPwinterEQR = df$DIP,
                   CoastTNsummer     = df$TN,
                   CoastTNsummerEQR  = df$TN,
                   CoastTNwinter     = df$TN,
                   CoastTNwinterEQR  = df$TN,
                   CoastTPsummer     = df$TP,
                   CoastTPsummerEQR  = df$TP,
                   CoastTPwinter     = df$TP,
                   CoastTPwinterEQR  = df$TP,
                   CoastOxygen       = df$O2,
                   CoastBQI          = df$BQI,
                   CoastMSMDI        = df$MSMDI)
    df <- mutate(df,xvar=xvar)
# Associating indicators with transformation from observations
    f_fun <- switch(Indicator,
                    CoastChla         = Aggregate_year_station,
                    CoastChlaEQR      = AggregateEQR_year_station,
                    CoastBiovol       = Aggregate_year_station,
                    CoastBiovolEQR    = AggregateEQR_year_station,
                    CoastSecchi       = Aggregate_period,
                    CoastSecchiEQR    = AggregateEQR_N_period,
                    CoastDINwinter    = Aggregate_year,
                    CoastDINwinterEQR = AggregateEQR_year,
                    CoastDIPwinter    = Aggregate_year,
                    CoastDIPwinterEQR = AggregateEQR_year,
                    CoastTNsummer     = Aggregate_year,
                    CoastTNsummerEQR  = AggregateEQR_year,
                    CoastTNwinter     = Aggregate_year,
                    CoastTNwinterEQR  = AggregateEQR_year,
                    CoastTPsummer     = Aggregate_year,
                    CoastTPsummerEQR  = AggregateEQR_year,
                    CoastTPwinter     = Aggregate_year,
                    CoastTPwinterEQR  = AggregateEQR_year,
                    CoastOxygen       = OxygenTest2,
                    CoastBQI          = BQIbootstrap,
                    CoastMSMDI        = Aggregate_period)
# Assigning transformations for measurements to obtain normal distributed variates
    g_fun <- switch(Indicator,
                    CoastChla         = log,
                    CoastChlaEQR      = log,
                    CoastBiovol       = log,
                    CoastBiovolEQR    = log,
                    CoastSecchi       = log,
                    CoastSecchiEQR    = log,
                    CoastDINwinter    = log,
                    CoastDINwinterEQR = log,
                    CoastDIPwinter    = log,
                    CoastDIPwinterEQR = log,
                    CoastTNsummer     = log,
                    CoastTNsummerEQR  = log,
                    CoastTNwinter     = log,
                    CoastTNwinterEQR  = log,
                    CoastTPsummer     = log,
                    CoastTPsummerEQR  = log,
                    CoastTPwinter     = log,
                    CoastTPwinterEQR  = log,
                    CoastOxygen       = identity,
                    CoastBQI          = identity,
                    CoastMSMDI        = logit_w_replace)    
# Assigning inverse transformations of g_fun
    g_fun_inv <- switch(Indicator,
                    CoastChla         = exp,
                    CoastChlaEQR      = exp,
                    CoastBiovol       = exp,
                    CoastBiovolEQR    = exp,
                    CoastSecchi       = exp,
                    CoastSecchiEQR    = exp,
                    CoastDINwinter    = exp,
                    CoastDINwinterEQR = exp,
                    CoastDIPwinter    = exp,
                    CoastDIPwinterEQR = exp,
                    CoastTNsummer     = exp,
                    CoastTNsummerEQR  = exp,
                    CoastTNwinter     = exp,
                    CoastTNwinterEQR  = exp,
                    CoastTPsummer     = exp,
                    CoastTPsummerEQR  = exp,
                    CoastTPwinter     = exp,
                    CoastTPwinterEQR  = exp,
                    CoastOxygen       = identity,
                    CoastBQI          = identity,
                    CoastMSMDI        = plogis) 
# Switch year for winter months (Nov+Dec) to include together with (Jan+Feb)
    if (Indicator %in% c("TNwinter","TNwinterEQR","TPwinter","TPwinterEQR")) {
      df <- mutate(df,year=ifelse(month %in% c(11,12),year+1,year))
    }
# Filter dataframe to include observations used in indicator only
    df <- Filter_df(df,MonthInclude,startyear,endyear)    
# setting RefCond depending on salinity for indicators with salinity correction
    RefCond <- mat.or.vec(nrow(df), 1)
    if (Indicator %in% c("ChlaEQR","SecchiEQR","DINsummerEQR","DIPsummerEQR","TNsummerEQR","TPsummerEQR","TNwinterEQR","TPwinterEQR")) {
       df <- filter(df,!is.na(sali))
       RefCond <- mat.or.vec(nrow(df), 1)
       sali_class <- findInterval(df$sali, c(seq(0, 35)))
       for (i in 1:nrow(df)) {RefCond[i] <- RefCond_sali[sali_class[i]]}
       }
    df <- mutate(df,RefCond = RefCond) 
# Calculate number of years, stations, months, institutions and combinations thereof in df 
    ndf <- DF_Ncalculation(df)
    # Return from function if no observations for calculation
    if (ndf$n_obs == 0) return(list(result_code=-90))
# Estimate mean of the transformed observation for simulation
    alpha <- df %>% group_by(year) %>% summarise(mean = mean(g_fun(xvar),na.rm=TRUE))
# Calculate indicator
    mu_indicator <- f_fun(df)
# Simulate system with random variables for estimating the variance of the indicator
    simres <- vector("numeric",n_iter)
    simresyear <- matrix(nrow=ndf$n_year,ncol=n_iter)
    simrescode <- vector("numeric",n_iter)
# simulation loop - simres contains the residuals from n_iter simulations
    for (isim in 1:n_iter) {
      # simulate variations in the random factors using the data structure
      if (Indicator == "Oxygen") {
         simulobs <- SetVector_IndicatorSimO2(alpha$mean,ndf,var_list,df,length(MonthInclude))
      } else {
         simulobs <- SetVector_IndicatorSim(alpha$mean,ndf,var_list,df,length(MonthInclude))
      }
      # backtransform simulations from log domain to original domain
      simulobs <- g_fun_inv(simulobs)
      # add simulated observation to df
      simul_df <- df %>% mutate(xvar = NULL, xvar=simulobs)
      # Calculate indicator value for each year and period
      simul_indicator <- f_fun(simul_df)
      # Check for errors in simulations
      simrescode[isim] <- simul_indicator$error_code
      if (simul_indicator$error_code == 0) {
         simresyear[,isim]=simul_indicator$yearmeans$xvar
         simres[isim] <- simul_indicator$periodmean
      } else{
        simresyear[,isim]= c(rep(NA,6))
        simresyear[isim]= NA
      }
    } # end simulation loop
    
# Adjust simulations to have zero mean and then add indicator means - bias correction
    simres <- g_fun_inv(g_fun(simres)-g_fun(mean(simres))+g_fun(mu_indicator$periodmean))
    simresyear <- g_fun_inv(g_fun(simresyear)-g_fun(apply(simresyear,1,mean))+g_fun(mu_indicator$yearmeans$xvar))
    
# Calculate statistics
    period <- data.frame(mean=mean(simres),stderr=sd(simres),lower = quantile(simres,probs=c((1-confidence_lvl)/2)),upper = quantile(simres,probs=c(1-(1-confidence_lvl)/2)),row.names = NULL)
    annual <- data.frame(year = mu_indicator$yearmeans$year,
                         mean = apply(simresyear,1,mean),
                         stderr = apply(simresyear,1,sd),
                         lower = apply(simresyear,1,quantile,probs=c((1-confidence_lvl)/2),na.rm=TRUE),
                         upper = apply(simresyear,1,quantile,probs=c(1-(1-confidence_lvl)/2),na.rm=TRUE))
    flag <- ifelse(length(annual$mean)<3,-1,flag)
    obs_sim <- as.numeric(simres)
    
    res <- list(period=period,annual=annual,indicator_sim=simres,n_list=ndf,result_code=flag)
    return(res)
  }