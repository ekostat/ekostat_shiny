#' Swedish indicator for Chla
#' 
#' @param indicator Name of the indicator to be calculated. Possible values are
#'   "CumulativeCover", "PropOpportunist", and "NPerennials"
#' 
#' @param df A dataframe with monitoring data from the Swedish Monitoring program. 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \describe{ 
#'   \item{station}{An identifier for the monitoring station.} 
#'   \item{date}{Date of the sampling.} 
#'   \item{depth}{Depth of the observation in meter.} 
#'   
#' @param Indicator_pred_pct Percentile for indicator
#'   
#' @return
#' @export
#' 
#' @examples
CalculateIndicator_Chla <-
  function(df,var_year,var_station,var_res,n_iter=1000,Indicator_pred_pct = 0.5) {
    
# Add month and year to df
    df <- mutate(df,month=month(date))
    df <- mutate(df,year=month(year))
# Use only June-August data    
    df <- filter(df,month %in% 6:8)
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
    res <- list(mean=mean,stderr=stderr)
    return(res)
}