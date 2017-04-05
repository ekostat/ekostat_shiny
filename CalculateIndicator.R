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
  function(df,Indicator_pred_pct = 0.5) {
    
# Add month and year to df
    df <- mutate(df,month=month(date))
    df <- mutate(df,year=month(year))
# Use only June-August data    
    df <- filter(df,month %in% 6:8) 
# Aggregate by year and station    
    res <- df %>%
      group_by(year,station) %>%
      summarise(chla = mean(chla))
# Aggregate by year
    res <- res %>%
      group_by(year) %>%
      summarise(chla = mean(chla))
# Aggregate for entire period
# Only use indicator if there are at least 3 years of data    
    res <- ifelse(length(res$chla)>=3,mean(res$chla),na())
    return(res)
}