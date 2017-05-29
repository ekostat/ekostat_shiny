#' Swedish indicator for Chla
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
#'   
#' @param MonthInclude A list of months to be included in the indicator
#' @param var_year Variance for the interannual variation
#' @param var_station Variance for the variation among stations
#' @param var_res Variance for the residual variation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples
CalculateIndicator_Chla <-
  function(df,MonthInclude,var_year,var_station,var_res,n_iter=1000) {
    
# Add month and year to df
    df <- mutate(df,month=month(date))
    df <- mutate(df,year=month(year))
# Use only June-August data    
    df <- filter(df,month %in% MonthInclude)
# Remove observations with missing values    
    df <-drop_na(df,chla)
# Calculate number of years and stations in df 
    n_obs <- length(df$chla)
    n_year <- length(unique(df$year))
    n_station <- length(unique(df$station))
# Simulate system with random variables for estimating the variance of the indicator
    Random_year <- mat.or.vec(n_year, 1)
    Random_station <- mat.or.vec(n_station, 1)
    Random_res <- mat.or.vec(n_obs, 1)
    
   # simres <- mat.or.vec(n_iter, 1)
    simres <- vector("numeric",n_iter)
    for (isim in 1:n_iter) {
    # simulate variations in the random factors using the data structure
        for (i in 1:n_year) {Random_year[i] <- rnorm(1) * sqrt(var_year)}
        for (i in 1:n_station) {Random_station[i] <- rnorm(1) * sqrt(var_station)}
        for (i in 1:n_obs) {Random_res[i] <- rnorm(1) * sqrt(var_res)}
        simulvector_year <- Random_year[match(df$year,unique(df$year))]
        simulvector_station <- Random_station[match(df$station,unique(df$station))]
        simulvector_obs <- simulvector_year+simulvector_station+Random_res
        simul_df <- data.frame(df$year,df$station,simulvector_obs)
        simres[isim] <- simul_df %>%
          group_by(df.year,df.station) %>%
          summarise(simulvector_obs = mean(simulvector_obs)) %>%
          group_by(df.year) %>%
          summarise(simulvector_obs = mean(simulvector_obs)) %>%
          summarise(simulvector_obs = mean(simulvector_obs))
         }
    
# Aggregate by year and station    
    dfres <- df %>%
      group_by(year,station) %>%
      summarise(chla = mean(chla))
# Aggregate by year
    dfres <- dfres %>%
      group_by(year) %>%
      summarise(chla = mean(chla))
# Aggregate for entire period
# Only use indicator if there are at least 3 years of data  
    mean <- ifelse(length(dfres$chla)>=3,mean(dfres$chla),NA)
    stderr <- ifelse(length(dfres$chla)>=3,sd(as.numeric(simres)),NA)
    error_sim <- c(rep(mean,n_iter))+as.numeric(simres)
    res <- list(mean=mean,stderr=stderr,error_sim=error_sim)
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
#' @param var_res Variance for the residual variation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples
CalculateIndicator_ChlaEQR <-
  function(df,RefCond_sali,MonthInclude,var_year,var_station,var_res,n_iter=1000) {
    
    # Add month and year to df
    df <- mutate(df,month=month(date))
    df <- mutate(df,year=month(year))
    # Use only June-August data    
    df <- filter(df,month %in% MonthInclude)
    # Remove observations with missing values    
    df <-drop_na(df,chla)
    # Calculate number of years and stations in df 
    n_obs <- length(df$chla)
    n_year <- length(unique(df$year))
    n_station <- length(unique(df$station))
    # Simulate system with random variables for estimating the variance of the indicator
    Random_year <- mat.or.vec(n_year, 1)
    Random_station <- mat.or.vec(n_station, 1)
    Random_res <- mat.or.vec(n_obs, 1)
    RefCond_chla <- mat.or.vec(n_obs, 1)
    chlaEQR <- mat.or.vec(n_obs, 1)
    sali_class <- findInterval(df$sali, c(seq(0, 35)))
    for (i in 1:n_obs) {RefCond_chla[i] <- RefCond_sali[sali_class[i]]}
    
    # simres <- mat.or.vec(n_iter, 1)
    simres <- vector("numeric",n_iter)
    for (isim in 1:n_iter) {
      # simulate variations in the random factors using the data structure
      for (i in 1:n_year) {Random_year[i] <- rnorm(1) * sqrt(var_year)}
      for (i in 1:n_station) {Random_station[i] <- rnorm(1) * sqrt(var_station)}
      for (i in 1:n_obs) {Random_res[i] <- rnorm(1) * sqrt(var_res)}
      simulvector_year <- Random_year[match(df$year,unique(df$year))]
      simulvector_station <- Random_station[match(df$station,unique(df$station))]
      simulvector_obs <- df$chla+simulvector_year+simulvector_station+Random_res
      # Calculate the EQR value for the Chla observation and set to 1 if lower than RC value
      for (i in 1:n_obs) {chlaEQR[i] <- ifelse(simulvector_obs[i]<RefCond_chla[i],1,RefCond_chla[i]/simulvector_obs[i])}
      
      simul_df <- data.frame(df$year,df$station,df$sali,chlaEQR)
      simres[isim] <- simul_df %>%
        group_by(df.year,df.station) %>%
        summarise(chlaEQR = mean(chlaEQR)) %>%
        group_by(df.year) %>%
        summarise(chlaEQR = mean(chlaEQR)) %>%
        summarise(chlaEQR = mean(chlaEQR))
    }
    
    # Recalculate chlaEQR for the original data    
    for (i in 1:n_obs) {chlaEQR[i] <- ifelse(df$chla[i]<RefCond_chla[i],1,RefCond_chla[i]/df$chla[i])}
    simul_df <- data.frame(df$year,df$station,df$sali,chlaEQR)
    # Aggregate by year and station    
    dfres <- df %>%
      group_by(year,station) %>%
      summarise(chlaEQR = mean(chlaEQR))
    # Aggregate by year
    dfres <- dfres %>%
      group_by(year) %>%
      summarise(chlaEQR = mean(chlaEQR))
    # Aggregate for entire period
    # Only use indicator if there are at least 3 years of data  
    mean <- ifelse(length(dfres$chlaEQR)>=3,mean(dfres$chlaEQR),NA)
    stderr <- ifelse(length(dfres$chlaEQR)>=3,sd(as.numeric(simres)),NA)
    error_sim <- as.numeric(simres)
    res <- list(mean=mean,stderr=stderr,error_sim=error_sim)
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
#' @param var_res Variance for the residual variation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples
CalculateIndicator_nutrientEQR <-
  function(NutrientIndicator,df,RefCond_sali,MonthInclude,var_year,var_station,var_res,n_iter=1000) {
    
    # Add month and year to df
    df <- mutate(df,month=month(date))
    df <- mutate(df,year=month(year))
    
    # Use only June-August data    
    df <- filter(df,month %in% MonthInclude)
    # Switch year for winter months (Nov+Dec) to include in winter indicators
    df <- mutate(df,year=ifelse(month %in% c(11,12),year+1,year))
    # Select the indicator response variable
    nutrient <- switch(NutrientIndicator,
                       DINsummer = df$DIN,
                       DIPsummer = df$DIP,
                       TNsummer = df$TN,
                       TNwinter = df$TN,
                       TPsummer = df$TP,
                       TPwinter = df$TP)
    df <- mutate(df,nutrient=nutrient)                   
    # Remove observations with missing values    
    df <-drop_na(df,nutrient)
    # Calculate number of years and stations in df 
    n_obs <- length(df$nutrient)
    n_year <- length(unique(df$year))
    n_station <- length(unique(df$station))
    # Simulate system with random variables for estimating the variance of the indicator
    Random_year <- mat.or.vec(n_year, 1)
    Random_station <- mat.or.vec(n_station, 1)
    Random_res <- mat.or.vec(n_obs, 1)
    RefCond_nutrient <- mat.or.vec(n_obs, 1)
    nutrientEQR <- mat.or.vec(n_obs, 1)
    sali_class <- findInterval(df$sali, c(seq(0, 35)))
    for (i in 1:n_obs) {RefCond_nutrient[i] <- RefCond_sali[sali_class[i]]}
    
    # simres <- mat.or.vec(n_iter, 1)
    simres <- vector("numeric",n_iter)
    for (isim in 1:n_iter) {
      # simulate variations in the random factors using the data structure
      for (i in 1:n_year) {Random_year[i] <- rnorm(1) * sqrt(var_year)}
      for (i in 1:n_station) {Random_station[i] <- rnorm(1) * sqrt(var_station)}
      for (i in 1:n_obs) {Random_res[i] <- rnorm(1) * sqrt(var_res)}
      simulvector_year <- Random_year[match(df$year,unique(df$year))]
      simulvector_station <- Random_station[match(df$station,unique(df$station))]
      simulvector_obs <- df$nutrient+simulvector_year+simulvector_station+Random_res
      # Calculate the EQR value for the nutrient observation
      nutrientEQR <- RefCond_nutrient/simulvector_obs
      
      simul_df <- data.frame(df$year,df$station,df$sali,nutrientEQR)
      simres[isim] <- simul_df %>%
        group_by(df.year) %>%
        summarise(nutrientEQR = mean(nutrientEQR)) %>%
        summarise(nutrientEQR = mean(nutrientEQR))
    }
    
    # Recalculate nutrientEQR for the original data    
    nutrientEQR <- RefCond_nutrient/df$nutrient
    simul_df <- data.frame(df$year,df$station,df$sali,nutrientEQR)
    # Aggregate by year   
    dfres <- df %>%
      group_by(year) %>%
      summarise(nutrientEQR = mean(nutrientEQR))
    # Aggregate for entire period
    # Only use indicator if there are at least 3 years of data  
    mean <- ifelse(length(dfres$nutrientEQR)>=3,mean(dfres$nutrientEQR),NA)
    stderr <- ifelse(length(dfres$nutrientEQR)>=3,sd(as.numeric(simres)),NA)
    error_sim <- as.numeric(simres)
    res <- list(mean=mean,stderr=stderr,error_sim=error_sim)
    return(res)
  }
