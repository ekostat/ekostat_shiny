#' Assessment
#' 
#' 
#' @param df A dataframe with monitoring data from the Swedish Monitoring program. 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \item{station}{An identifier for the monitoring station.} 
#'   \item{date}{Date of the sampling.} 
#'   \item{depth}{Depth of the observation in meter.} 
#'   \item{chla}{Chlorophyll a concentration in sample.} 
#'   
#'   The following variable is optional
#'   \item{WB}{Waterbody} 
#'   
#' @param nsim Number of iterations for Monte Carlo simulation 
#'   
#' 
#' @examples
Assessment <-
  function(df,nsim) {
    res<-list(data.frame)
    res[[1]]<-df %>% group_by(WB) %>%
      summarise(n=n())
    res[[2]]<-df %>% group_by(WB) %>%
      summarise(n=n())
    return(res)
  }