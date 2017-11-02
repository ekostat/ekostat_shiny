
#' Assessment
#' 
#' 
#' @param nsim Number of iterations for Monte Carlo simulation 
#'   
#' @param df.all 
#' @param IndicatorList 
#' 
#' @examples
Assessment <-
  function(df.all,nsim=1000,IndicatorList) {
    
    df.bounds<-ReadBounds()
    df.indicators<-ReadIndicatorType()
    df.variances<-ReadVariances()
    
    df.all$typology<-gsub("SE_", "", df.all$typology)
    # V_stationdate
    
    df.months<- df.bounds %>% distinct(Indicator,Type,Months)
    
    wblist<-distinct(df.all,WB,typology)
    wbcount<-nrow(wblist)
    
    # Loop through distinct waterbodies and periods in the data
    for(iWB in 1:wbcount){
      df.temp<-df.all %>% filter(WB == wblist$WB[iWB])
      plist<-distinct(df.all,period)
      pcount<-nrow(plist)
      typology<-df.temp[1,"typology"]
      
      for(iPeriod in 1:pcount){
        df <- df.all %>% filter(WB == wblist$WB[iWB],period == plist$period[iPeriod])
        cat(paste0("WB: ",wblist$WB[iWB],"  Period: ",plist$period[iPeriod],"\n"))
        
        # Get start and end years from the period text (e.g. "2001-2006")
        startyear<-as.numeric(substr(as.character(plist$period[iPeriod]),1,4))
        endyear<-as.numeric(substr(as.character(plist$period[iPeriod]),6,9))
        
        # Loop through selected indicators
        for(iInd in IndicatorList){
          BoundsList<-df.bounds %>% filter(Type==typology,Indicator==iInd)
          IndSubtypes<-distinct(BoundsList,Depth_stratum)
          subcount<-nrow(IndSubtypes)
          dfsubs<-df
          
          for(iSub in 1:subcount){
            
            subtype<-IndSubtypes[iSub,1]
            if(subtype!=""){
              df<-FilterDepth(dfsubs,subtype)
            }
            
            res<-IndicatorResults(df,typology,df.bounds,df.indicators,df.variances,iInd,startyear,endyear,nsim)
          
            if(res$result_code!=-90){
              rm(df.temp)
              df.temp<-data.frame(Mean=res$period$mean,StdErr=res$period$stderr,Code=res$result_code)
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$Code<-res$result_code
              cat(paste0("Indicator: ",iInd,"  Result: ",res$result_code,"\n"))
              
              if(exists("res.ind")){
                res.ind<-bind_rows(res.ind,df.temp)
              }else{
                res.ind<-df.temp
              }
              
              rm(df.temp)
              df.temp<-data.frame(Estimate=res$indicator_sim,Code=res$result_code)
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$sim<-1:nsim
              df.temp$Code<-res$result_code
              
              if(exists("res.rnd")){
                res.rnd<-bind_rows(res.rnd,df.temp)
              }else{
                res.rnd<-df.temp
              }
            }else{ #res$result_code!=0
              #Add to the list of errors
              ErrDesc <- "unspecified"
              if(res$result_code==-1) ErrDesc<-"data <3years"
              if(res$result_code==-90) ErrDesc<-"no data"
              
              rm(df.temp)
              df.temp<-data.frame(WB=wblist$WB[iWB],
                                  Type=wblist$typology[iWB],
                                  Period=plist$period[iPeriod],
                                  Indicator=iInd,
                                  IndSubtype=subtype,
                                  Code=res$result_code,
                                  Error=ErrDesc)
              if(exists("res.err")){
                res.err<-bind_rows(res.err,df.temp)
              }else{
                res.err<-df.temp
              }
            }
          }
        } #for(iSub in 1:subcount)
      }  #for(iPeriod in 1:pcount) 
    }    #for(iWB in 1:wbcount)
    #---------------------- Summarise results --------------------------
    # Get indicator categories based on mean values
    
    res.ind<- res.ind %>% select(WB,Type,Period,Indicator,IndSubtype,Mean,StdErr)
    res.ind<- res.ind %>% left_join(df.bounds, by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    res.ind$Value<-res.ind$Mean
    
    #We now have some duplicates for BQI because there are different
    
    # Do we show mean concentrations where the indicator is EQR value?
    #res.ind$Value<-ifelse(res.ind$UseEQR==1,(res.ind$Mean/res.ind$Ref),res.ind$Mean)
    res.ind<-GetClass(res.ind)
    
    # Get indicator categories for MC results
    res.rnd<- res.rnd %>% select(WB,Type,Period,Indicator,IndSubtype,sim,Estimate,Code)
    
    res.rnd<- res.rnd %>% left_join(df.bounds, by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    names(res.rnd)[names(res.rnd)=="Estimate"]<-"Value"
    
    #res.rnd$Value<-ifelse(res.rnd$UseEQR==1,(res.rnd$Estimate/res.rnd$Ref),res.rnd$Estimate)
    res.rnd<-GetClass(res.rnd)
    cat(paste0("Sim results: ",nrow(res.rnd),"\n"))
    
    res.rnd <- res.rnd %>% left_join(select(df.indicators,Indicator,QualityElement,QualitySubelement,QEtype))
    
    #Find counts for each category
   
    res.rnd.count <- res.rnd %>% filter(!is.na(ClassID)) %>%
      group_by(WB,Period,Type,Indicator,IndSubtype,ClassID) %>% summarise(n=n())
    
    # Here we add zeros for ClassIDs which don't occur
    # this ensures that all 5 columns are present after transposing
    
    ClassID<-c(1,2,3,4,5)
    ClassID<-data.frame(ClassID)
    ClassID$X<-1
    res.rnd.count$X<-1
    
    res.rnd.count <- res.rnd.count %>% 
      distinct(WB,Period,Type,Indicator,IndSubtype) %>% 
      mutate(X=1) %>%
      left_join(ClassID) %>%
      left_join(res.rnd.count) %>%
      select(-X)
    
    res.rnd.count$ClassID<-paste0("C",res.rnd.count$ClassID)
    res.rnd.count$n<-res.rnd.count$n/nsim
    
    res.rnd.count<-spread(res.rnd.count, ClassID, n, fill = NA)
    res.ind<-left_join(res.ind,res.rnd.count)
    
    res.ind <- res.ind %>% left_join(select(df.indicators,Indicator,QualityElement,QualitySubelement,QEtype))
    
    names(res.ind)[names(res.ind)=="C1"]<-"fBad"
    names(res.ind)[names(res.ind)=="C2"]<-"fPoor"
    names(res.ind)[names(res.ind)=="C3"]<-"fMod"
    names(res.ind)[names(res.ind)=="C4"]<-"fGood"
    names(res.ind)[names(res.ind)=="C5"]<-"fHigh"
    
    res<-list(data.frame)
    
    #Overall results
    #res[[1]]<-df.all %>% group_by(WB,period) %>%
    #  summarise(n=n())
    
    #QE results
    #res[[2]]<-res.ind %>% group_by(WB,Period,QualityElement) %>%
    #  summarise(n=n())
    
    #Indicators
    res[[1]] <-res.ind #%>% select(WB,Type,Period,QualityElement,QualitySubelement,Indicator,Mean,StdErr,EQR,Class,fBad,fPoor,fMod,fGood,fHigh )
    res[[2]]<-res.rnd
    if(!exists("res.err")){
      res.err<-data.frame(WB=NA,Type=NA,Period=NA,Indicator=NA,
                          IndSubtype=NA,Code=NA,Error=NA)
    }
    res[[3]]<-res.err
    
    return(res)
    
  }

#' GetClass
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
#'   \item{Worst}{ } 
#'   \item{Resp}{ } 
#'   #'   
#' 
#' @examples
GetClass<-function(df){
  Categories<-c("Bad","Poor","Mod","Good","High","Ref")
  names(df)[names(df)=="RefCond"]<-"Ref"
  names(df)[names(df)=="H.G"]<-"HG"
  names(df)[names(df)=="G.M"]<-"GM"
  names(df)[names(df)=="M.P"]<-"MP"
  names(df)[names(df)=="P.B"]<-"PB"
  #names(df)[names(df)==""]<-""
  
  df$Resp<-ifelse(df$HG > df$GM,-1,1)
  df$class1<-ifelse(df$Resp==1,df$Value<df$Ref,df$Value>df$Ref)
  df$class2<-ifelse(df$Resp==1,df$Value<df$HG,df$Value>df$HG)
  df$class3<-ifelse(df$Resp==1,df$Value<df$GM,df$Value>df$GM)
  df$class4<-ifelse(df$Resp==1,df$Value<df$MP,df$Value>df$MP)
  df$class5<-ifelse(df$Resp==1,df$Value<df$PB,df$Value>df$PB)
  df$ClassID<-df$class1+df$class2+df$class3+df$class4+df$class5+1
  df$Class<-Categories[df$ClassID]
  df$Bnd1<-df$Worst
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

#' SalinityReferenceValues
#' 
#' 
SalinityReferenceValues <- function(df.data,df.bounds,indicator,missing=1){
  typology <- substr(as.character(distinct(df.data,typology)[1,1]),4,5)
  refcond<-filter(df.bounds,Type==typology,Indicator==indicator)
  refcond<-refcond[,grep("Sali_", names(refcond), value=TRUE)]
  refcond<-as.numeric(refcond[1,])
  refcond[is.na(refcond)]<-missing
  return(refcond)
}

#' IndicatorMonths
#' 
#' 
IndicatorMonths <- function(df.months,typology,indicator){
  
  df.months<-df.months %>% filter(Indicator==indicator)
  #Are there different combinations of months for this indicator
  n <- nrow(summarise(group_by(df.months,Months),n=n()))
  if(n>1){
    #If so, then filter the boundary data by typology
    df.months<-df.months %>% filter(Indicator==indicator,Type==typology)
  }
  months<-df.months[1,"Months"]
  if(is.na(months)){months<-"1,2,3,4,5,6,7,8,9,10,11,12"}
  if(months=="1,2,..,12"){months<-"1,2,3,4,5,6,7,8,9,10,11,12"}
  months<-lapply(strsplit(months, ","),function(x) as.numeric(x))[[1]]
  return(months)
}

#' IndicatorMonths
#' 
#' 
VarianceComponents<-function(df.indicators,df.variances,typology,indicator){
  measurement<-df.indicators[df.indicators$Indicator==indicator,"Measurement"]
  df.variances<-df.variances %>% filter(Type==typology, Measurement==measurement)
  
  variance_list <- list(V_station=df.variances$V_station[1],
                        V_obspoint=df.variances$V_station[1],
                        V_year=df.variances$V_year[1],
                        V_yearmonth=df.variances$V_yearmonth[1],
                        V_stationdate=df.variances$V_stationdate[1],
                        V_stationyear=df.variances$V_stationyear[1],
                        V_stationmonth=df.variances$V_stationmonth[1],
                        V_institution=df.variances$V_institution[1],
                        V_replication=df.variances$V_replication[1])
  variance_list <- lapply(variance_list, function(x) ifelse(is.na(x),0,x))
  return(variance_list)
}



#' IndicatorResults
#' 
#' 
IndicatorResults<-function(df,typology,df.bounds,df.indicators,df.variances,indicator,startyear,endyear,nsim){
  missing <- switch(indicator,0,
                    ChlaEQR      = 0.9,
                    TNsummer     = 50,
                    TNwinter     = 50
  )
  df.months<- df.bounds %>% distinct(Indicator,Type,Months)
  RefCond_sali<-SalinityReferenceValues(df,df.bounds,indicator,missing)
  MonthInclude <- IndicatorMonths(df.months,typology,indicator)
  variance_list<- VarianceComponents(df.indicators,df.variances,typology,indicator)
  res<-CalculateIndicator(indicator,df,RefCond_sali,variance_list,MonthInclude,startyear,endyear,n_iter=nsim)
  
}

#' FilterDepth
#' 
#'
FilterDepth<-function(df,depths){
  # e.g. >20 m, 5-20 m, 5-60 m,  >5 m
  depths<-gsub(' m', '', depths)
  pos = regexpr('-', depths)
  if(pos>0){
    z1<-as.numeric(substr(depths,1,pos-1))
    z2<-as.numeric(substr(depths,pos+1,99))
    df<-df %>% filter(station_depth > z1) %>% filter(station_depth < z2)
  }else{
    z<-as.numeric(substr(depths,2,99))
    if(substr(depths,1,1)=="<"){
      df<-df %>% filter(station_depth <= z)
    }
    if(substr(depths,1,1)==">"){
      df<-df %>% filter(station_depth >= z)
    }
  }
  return(df)
}