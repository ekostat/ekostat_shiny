#' LIBRARY OF ROUTINES FOR CALCULATING SWEDISH WFD INDICATORS
#'
#'
#' Swedish indicator for Chla
#' 
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
#' @param var_year Variance for the interannual variation
#' @param var_station Variance for the variation among stations
#' @param var_statyear Variance for the interaction bw years and stations
#' @param var_inst Variance for the variation among institutions
#' @param var_res Variance for the residual variation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples
CalculateIndicator_Chla <-
  function(df,MonthInclude,var_list,n_iter=1000) {
# Set flag to zero and change it for error handling below
    flag <- 0
# Filter dataframe to include observations used in indicator only
    df <- Filter_df(df,MonthInclude,"chla")
# Calculate number of years, stations, months, institutions and combinations thereof in df 
    ndf <- DF_Ncalculation(df)
# Estimate mean of the log-transformed chla obs
    alpha <- mean(log(df$chla))
# Simulate system with random variables for estimating the variance of the indicator
    simres <- vector("numeric",n_iter)
    simresyear <- matrix(nrow=n_iter,ncol=ndf$n_year)
# simulation loop - simres contains the residuals from n_iter simulations
    for (isim in 1:n_iter) {
    # simulate variations in the random factors using the data structure
          simulobs <- SetVector_IndicatorSim(alpha,ndf,var_list,df)
    # backtransform simulations from log domain to original domain
          simulobs <- exp(simulobs)
    # add simulated observation to df
          simul_df <- data.frame(df$year,df$station,simulobs)
    # Calculate indicator value for each year
          simresyearx <- simul_df %>%
            group_by(df.year,df.station) %>%
            summarise(simulobs = mean(simulobs)) %>%
            group_by(df.year) %>%
            summarise(simulobs = mean(simulobs))
          simresyear[isim,]=simresyearx$simulobs
    # Calculate indicator for period
          simres[isim] <- mean(simresyearx$simulobs)
         } # end simulation loop

# Estimate indicator median from original data
    median <- as.numeric(df %>%
                         group_by(year,station) %>%
                         summarise(chla = mean(chla)) %>%
                         group_by(year) %>%
                         summarise(chla = mean(chla)) %>%
                         summarise(chla = mean(chla)))
# Estimate other indicator statistics from MC simulations
    mean <- mean(as.numeric(simres))
    stderr <- sd(as.numeric(simres))
    yearmean <- data.frame(year = simresyearx$df.year,mean = apply(simresyear,2,mean),stderr = apply(simresyear,2,sd))
    flag <- ifelse(length(yearmean$mean)<3,-1,flag)
    obs_sim <- as.numeric(simres)

    res <- list(median=median,mean=mean,stderr=stderr,yearmean=yearmean,indicator_sim=obs_sim,n_list=ndf,result_code=flag)
    return(res)
  }

#' Swedish indicator for Chla, based on EQR calculations
#' 
#' 
#' @param df A dataframe with monitoring data from the Swedish Monitoring program. 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \describe{ 
#'   \item{station}{An identifier for the monitoring station.} 
#'   \item{date}{Date of the sampling.} 
#'   \item{depth}{Depth of the observation in meter.} 
#'   \item{chla}{Chlorophyll a concentration in sample.} 
#'   \item{sali}{Salinity for the measured chla observation.} 
#'   
#' @param RefCond_sali Vector of RC values (n=36) for different salinity levels
#' @param MonthInclude A list of months to be included in the indicator
#' @param var_year Variance for the interannual variation
#' @param var_station Variance for the variation among stations
#' @param var_statyear Variance for the interaction bw years and stations
#' @param var_inst Variance for the variation among institutions
#' @param var_res Variance for the residual variation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples
CalculateIndicator_ChlaEQR <-
  function(df,RefCond_sali,MonthInclude,var_list,n_iter=1000) {
# Set flag to zero and change it for error handling below
    flag <- 0
# Filter dataframe to include observations used in indicator only
    df <- Filter_df(df,MonthInclude,"chla")
# Calculate number of years, stations, months, institutions and combinations thereof in df 
    ndf <- DF_Ncalculation(df)
# setting RefCond depending on salinity and calculate chlaEQR
    RefCond_chla <- mat.or.vec(ndf$n_obs, 1)
    sali_class <- findInterval(df$sali, c(seq(0, 35)))
    for (i in 1:ndf$n_obs) {RefCond_chla[i] <- RefCond_sali[sali_class[i]]}
    df <- mutate(df,RefCond_chla = RefCond_chla)
    df <- mutate(df,chlaEQR = ifelse(chla<RefCond_chla,1,RefCond_chla/chla))
# Estimate mean of the log-transformed chla obs
    alpha <- mean(log(df$chla))
# Calculate number of years, stations, year*stations, institutions in df 
    ndf <- DF_Ncalculation(df)
# Simulate system with random variables for estimating the variance of the indicator
    simres <- vector("numeric",n_iter)
    simresyear <- matrix(nrow=n_iter,ncol=ndf$n_year)
# simulation loop - simres contains the residuals from n_iter simulations
    for (isim in 1:n_iter) {
    # simulate variations in the random factors using the data structure
      simulobs <- SetVector_IndicatorSim(alpha,ndf,var_list,df)
    # backtransform simulations from log domain to original domain
      simulobs <- exp(simulobs)
    # transform simulations to EQR scale
      simulobs <- ifelse(simulobs<RefCond_chla,1,RefCond_chla/simulobs)
    # add simulated observation to df
      simul_df <- data.frame(df$year,df$station,simulobs)
    # Calculate indicator value for each year
      simresyearx <- simul_df %>%
        group_by(df.year,df.station) %>%
        summarise(simulobs = mean(simulobs)) %>%
        group_by(df.year) %>%
        summarise(simulobs = mean(simulobs))
      simresyear[isim,]=simresyearx$simulobs
    # Calculate indicator for period
      simres[isim] <- mean(simresyearx$simulobs)
     } # end simulation loop

# Estimate indicator median from original data
    median <- as.numeric(df %>%
                           group_by(year,station) %>%
                           summarise(chlaEQR = mean(chlaEQR)) %>%
                           group_by(year) %>%
                           summarise(chlaEQR = mean(chlaEQR)) %>%
                           summarise(chlaEQR = mean(chlaEQR)))
# Estimate other indicator statistics from MC simulations
    mean <- mean(as.numeric(simres))
    stderr <- sd(as.numeric(simres))
    yearmean <- data.frame(year = simresyearx$df.year,mean = apply(simresyear,2,mean),stderr = apply(simresyear,2,sd))
    flag <- ifelse(length(yearmean$mean)<3,-1,flag)
    obs_sim <- as.numeric(simres)
    
    res <- list(median=median,mean=mean,stderr=stderr,yearmean=yearmean,indicator_sim=obs_sim,n_list=ndf,result_code=flag)
    return(res)
  }

#' Swedish indicator for nutrient concentrations, based on EQR calculations
#' 
#' 
#' @param NutrientIndicator A string giving the name of the nutrient indicator to be calculated
#' @param df A dataframe with monitoring data from the Swedish Monitoring program. 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \describe{ 
#'   \item{station}{An identifier for the monitoring station.} 
#'   \item{date}{Date of the sampling.} 
#'   \item{depth}{Depth of the observation in meter.} 
#'   \item{din}{DIN concentration in sample.} 
#'   \item{din}{TN concentration in sample.} 
#'   \item{din}{DIP concentration in sample.} 
#'   \item{din}{TP concentration in sample.} 
#'   \item{sali}{Salinity for the measured nutrient observation.} 
#'   
#' @param RefCond_sali Vector of RC values (n=36) for different salinity levels
#' @param MonthInclude A list of months to be included in the indicator
#' @param var_year Variance for the interannual variation
#' @param var_station Variance for the variation among stations
#' @param var_statyear Variance for the interaction bw years and stations
#' @param var_inst Variance for the variation among institutions
#' @param var_res Variance for the residual variation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples
CalculateIndicator_nutrientEQR <-
  function(NutrientIndicator,df,RefCond_sali,MonthInclude,var_list,n_iter=1000) {
# Set flag to zero and change it for error handling below
    flag <- 0
# Select the indicator response variable
    nutrient <- switch(NutrientIndicator,
                       DINsummer = df$DIN,
                       DIPsummer = df$DIP,
                       TNsummer = df$TN,
                       TNwinter = df$TN,
                       TPsummer = df$TP,
                       TPwinter = df$TP)
    df <- mutate(df,nutrient=nutrient)  
# Filter dataframe to include observations used in indicator only
    df <- Filter_df(df,MonthInclude,"nutrient")
# Switch year for winter months (Nov+Dec) to include in winter indicators
    df <- mutate(df,year=ifelse(month %in% c(11,12),year+1,year))
# Calculate number of years and stations in df 
    ndf <- DF_Ncalculation(df)
# setting RefCond depending on salinity and calculate chlaEQR
    RefCond_nutrient <- mat.or.vec(ndf$n_obs, 1)
    sali_class <- findInterval(df$sali, c(seq(0, 35)))
    for (i in 1:ndf$n_obs) {RefCond_nutrient[i] <- RefCond_sali[sali_class[i]]}
    df <- mutate(df,RefCond_nutrient = RefCond_nutrient)
    df <- mutate(df,nutrientEQR = ifelse(nutrient<RefCond_nutrient,1,RefCond_nutrient/nutrient))
# Estimate mean of the log-transformed nutrient obs
    alpha <- mean(log(df$nutrient))
# Simulate system with random variables for estimating the variance of the indicator
    simres <- vector("numeric",n_iter)
    simresyear <- matrix(nrow=n_iter,ncol=ndf$n_year)
# simulation loop - simres contains the residuals from n_iter simulations
    for (isim in 1:n_iter) {
      # simulate variations in the random factors using the data structure
      simulobs <- SetVector_IndicatorSim(alpha,ndf,var_list,df)
      # backtransform simulations from log domain to original domain
      simulobs <- exp(simulobs)
      # transform simulations to EQR scale
      simulobs <- ifelse(simulobs<RefCond_nutrient,1,RefCond_nutrient/simulobs)
      # add simulated observation to df
      simul_df <- data.frame(df$year,df$station,simulobs)
      # Calculate indicator value for each year
      simresyearx <- simul_df %>%
        group_by(df.year,df.station) %>%
        summarise(simulobs = mean(simulobs)) %>%
        group_by(df.year) %>%
        summarise(simulobs = mean(simulobs))
      simresyear[isim,]=simresyearx$simulobs
      # Calculate indicator for period
      simres[isim] <- mean(simresyearx$simulobs)
    } # end simulation loop

# Estimate indicator median from original data
    median <- as.numeric(df %>%
                           group_by(year,station) %>%
                           summarise(nutrientEQR = mean(nutrientEQR)) %>%
                           group_by(year) %>%
                           summarise(nutrientEQR = mean(nutrientEQR)) %>%
                           summarise(nutrientEQR = mean(nutrientEQR)))
# Estimate other indicator statistics from MC simulations
    mean <- mean(as.numeric(simres))
    stderr <- sd(as.numeric(simres))
    yearmean <- data.frame(year = simresyearx$df.year,mean = apply(simresyear,2,mean),stderr = apply(simresyear,2,sd))
    flag <- ifelse(length(yearmean$mean)<3,-1,flag)
    obs_sim <- as.numeric(simres)
    
    res <- list(median=median,mean=mean,stderr=stderr,yearmean=yearmean,indicator_sim=obs_sim,n_list=ndf,result_code=flag)
    return(res)
  }
