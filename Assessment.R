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
#'   \item{WB}{Waterbody} 
#'   
#' @param nsim Number of iterations for Monte Carlo simulation 
#'   
#' 
#' @examples
Assessment <-
  function(df.all,nsim=1000) {
    
    df.bounds<-ReadBounds()
    
    df.all$typology<-gsub("SE_", "", df.all$typology)
    
    wblist<-distinct(df.all,WB,typology)
    wbcount<-nrow(wblist)
    
    parmlist <- ReadParms_chla()
    #cat(paste0("ALL: ",nrow(df.all),"\n"))
    for(iWB in 1:wbcount){
      df <- df.all %>% filter(WB == wblist$WB[iWB])

      # Calculate the indicator
      res.chl<-CalculateIndicator_Chla(df,c(6,7,8),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                                       parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                                       parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"], 
                                       n_iter=nsim)
      
      # type with no salinity correction, e.g. type=6
      RefCond_sali <- c(rep(0.9,36))
      # type with salinity correction, e.g. type=8
      RefCond_sali <- c(15.7,12.4,9.5,6.9,4.8,3.0,1.7,rep(1.29,29))
      
      res.chlEQR<-CalculateIndicator_ChlaEQR(df,RefCond_sali,c(6,7,8),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                                          parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                                          parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"], 
                                          n_iter=nsim)
      
      # type with salinity correction, e.g. type=8 in summer
      RefCond_sali <- c(56,50,43,37,31,24,18,rep(15,29))
      res.TNsummer<-CalculateIndicator_nutrientEQR("TNsummer",df,RefCond_sali,c(6,7,8),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                                                   parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                                                   parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"], 
                                                   n_iter=nsim)
      
      # type with salinity correction, e.g. type=8 in winter
      RefCond_sali <- c(56,50,44,38,32,26,20,rep(17,29))
      res.TNwinter<-CalculateIndicator_nutrientEQR("TNwinter",df,RefCond_sali,c(11,12,1,2),parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],
                                                   parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],
                                                   parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"], 
                                                   n_iter=nsim)
      
      #Combine results of different indicators - Mean and StdErr
      df.temp<-data.frame(Mean=res.chl[[1]],StdErr=res.chl[[2]])
      df.temp$Indicator<-"chla"
      res.ind.temp<-df.temp
      df.temp<-data.frame(Mean=res.TNsummer[[1]],StdErr=res.TNsummer[[2]])
      df.temp$Indicator<-"TNsummer"
      res.ind.temp<-bind_rows(res.ind.temp,df.temp)
      df.temp<-data.frame(Mean=res.TNwinter[[1]],StdErr=res.TNwinter[[2]])
      df.temp$Indicator<-"TNwinter"
      res.ind.temp<-bind_rows(res.ind.temp,df.temp)
      res.ind.temp$WB<-wblist$WB[iWB]
      res.ind.temp$Type<-wblist$typology[iWB]
      
      #Combine results of different indicators - Random values
      df.temp<-data.frame(Estimate=res.chl[[3]])
      df.temp$Indicator<-"chla"
      df.temp$sim<-1:nsim
      res.rnd.temp<-df.temp
      df.temp<-data.frame(Estimate=res.TNsummer[[3]])
      df.temp$Indicator<-"TNsummer"
      df.temp$sim<-1:nsim
      res.rnd.temp<-bind_rows(res.rnd.temp,df.temp)
      df.temp<-data.frame(Estimate=res.TNwinter[[3]])
      df.temp$Indicator<-"TNwinter"
      df.temp$sim<-1:nsim
      res.rnd.temp<-bind_rows(res.rnd.temp,df.temp)
      res.rnd.temp$WB<-wblist$WB[iWB]
      res.rnd.temp$Type<-wblist$typology[iWB]
      
      if(iWB==1){
        res.ind<-res.ind.temp
        res.rnd<-res.rnd.temp
      }else{
        res.ind<-bind_rows(res.ind,res.ind.temp)
        res.rnd<-bind_rows(res.rnd,res.rnd.temp)
      }
    }      
    
    # Get indicator categories based on mean values
    res.ind<- res.ind %>% select(WB,Type,Indicator,Mean,StdErr)
    res.ind<- res.ind %>% left_join(df.bounds, by=c("Indicator"="Indicator","Type"="Type"))
    
    res.ind$Value<-ifelse(res.ind$UseEQR==1,(res.ind$Mean/res.ind$Ref),res.ind$Mean)
    res.ind<-GetClass(res.ind)
    
    # Get indicator categories for MC results
    res.rnd<- res.rnd %>% select(WB,Type,Indicator,sim,Estimate)
    res.rnd<- res.rnd %>% left_join(df.bounds, by=c("Indicator"="Indicator","Type"="Type"))
    res.rnd$Value<-ifelse(res.rnd$UseEQR==1,(res.rnd$Estimate/res.rnd$Ref),res.rnd$Estimate)
    res.rnd<-GetClass(res.rnd)
    cat(paste0("Sim results: ",nrow(res.rnd),"\n"))
    
    #Find counts for each category
    
    res.rnd.count <- res.rnd %>% filter(!is.na(ClassID)) %>%
      group_by(WB,Type,Indicator,ClassID) %>% summarise(n=n())
    res.rnd.count$ClassID<-paste0("C",res.rnd.count$ClassID)
    res.rnd.count$n<-res.rnd.count$n/nsim
    
    res.rnd.count<-spread(res.rnd.count, ClassID, n, fill = NA)
    res.ind<-left_join(res.ind,res.rnd.count)
    
    names(res.ind)[names(res.ind)=="C1"]<-"fBad"
    names(res.ind)[names(res.ind)=="C2"]<-"fPoor"
    names(res.ind)[names(res.ind)=="C3"]<-"fMod"
    names(res.ind)[names(res.ind)=="C4"]<-"fGood"
    names(res.ind)[names(res.ind)=="C5"]<-"fHigh"
    
    res<-list(data.frame)
    #Overall results
    res[[1]]<-df.all %>% group_by(WB) %>%
      summarise(n=n())
    
    #QE results
    res[[2]]<-res.ind %>% group_by(WB,QE,QualityElement) %>%
      summarise(n=n())
 
    #Indicators
    
    res[[3]]<-res.ind %>% select(-c(ClassID,QE)) #(WB,Type,Indicator,Mean,StdErr,UseEQR,Ref,HG,GM,MP,PB)
    return(res)
  }

#' Assessment
#' 
#' 
#' @param df A dataframe 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \item{Value} 
#'   \item{Ref}{ } 
#'   \item{HG}{ } 
#'   \item{GM}{ } 
#'   \item{MP}{ } 
#'   \item{PB}{ } 
#'   \item{Resp}{ } 
#'   #'   
#' 
#' @examples
GetClass<-function(df){
  Categories<-c("Bad","Poor","Mod","Good","High","Ref")
  df$Resp<-ifelse(df$HG > df$GM,-1,1)
  df$class1<-ifelse(df$Resp==1,df$Value<df$Ref,df$Value>df$Ref)
  df$class2<-ifelse(df$Resp==1,df$Value<df$HG,df$Value>df$HG)
  df$class3<-ifelse(df$Resp==1,df$Value<df$GM,df$Value>df$GM)
  df$class4<-ifelse(df$Resp==1,df$Value<df$MP,df$Value>df$MP)
  df$class5<-ifelse(df$Resp==1,df$Value<df$PB,df$Value>df$PB)
  df$ClassID<-df$class1+df$class2+df$class3+df$class4+df$class5+1
  df$Class<-Categories[df$ClassID]
  df$Bnd1<-df$MP-2*(df$MP-df$PB)
  df$Bnd2<-df$PB
  df$Bnd1<-ifelse(df$ClassID==2,df$PB,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==2,df$MP,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==3,df$MP,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==3,df$GM,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==4,df$GM,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==4,df$HG,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==5,df$HG,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==5,df$Ref,df$Bnd2)
  df$EQR<-0.2*((df$ClassID-1)+(df$Value-df$Bnd1)/(df$Bnd2-df$Bnd1))
  df$EQR<-ifelse(df$ClassID>5,1,df$EQR)
  df$ClassID<-ifelse(df$ClassID>5,5,df$ClassID)
  df<-select(df,-c(Resp,class1,class2,class3,class4,class5,Bnd1,Bnd2))
  return(df)
}

