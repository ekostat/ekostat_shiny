

library(shiny)
library(DT)
library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(dplyr)
library(prodlim)
library(sparkline)

source("ReadIndicatorParms.R")
source("CalculateIndicator.R")
source("IndicatorFunctions.R")
source("CalculateIndicatorSupport.R")
source("Assessment.R")
source("ReadBounds.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")
source("Aggregation.R")
source("classoutputtable.R")


shinyServer(function(input, output, session) {
  
  #Prepare data
  df<-read.table("data/data.txt", fileEncoding = "UTF-8", sep=";", stringsAsFactors=F, header=T)
  df.wb<-read.table("data/waterbodies.txt", fileEncoding = "UTF-8", sep="\t", stringsAsFactors=F, header=T)
  df[df$WB_name=="Halse/Askeröfj","WB_name"]<-"Halsefjorden"
  df<-df %>% left_join(select(df.wb,WaterbodyName,WaterbodyID,DistrictID), by=c("WB_name"="WaterbodyName")) 
  df$WB<-paste0(df$WaterbodyID," ",df$WB_name)  
  df$obspoint<-df$station
  df$typology<-ifelse(is.na(df$typology),df$typology,"SE_2") # was missing for BQI stations
  
  values <- reactiveValues(resMC=data.frame())
  
  RoundColList<-c("secchi","temp","sali","chla" ,"TP","TN","dens_dif","BQI","MSMDI","logitMSMDI","Oxygen")
  
  output$nText <- renderText({
    outText()
  })
  
  
  # Oberve changes in number of Monte Carlo simulations
  observeEvent(input$n, {
    nsim<-as.numeric(input$n)
    if(is.na(nsim)){nsim<-0}
    if(nsim<1){
      showModal(modalDialog(
        title = div(tags$b("MC Simulations", style = "color: red;")),
        "Number of simulations cannot be less than 1!"
      ))
      updateNumericInput(session,"n",value=1)
    }
  })
  
  observeEvent(input$dataButton, {
    #df.select <- filter(df, WB %in% input$waterbody, period %in% input$period)
    n<-nrow(df.select())
    output$nrows <- renderUI({
      tagList(p(renderText(paste0("Selected: ",n, " rows of data."))))
    })
    updateNavbarPage(session, "inTabset", selected="Assessment")
  })
  
  observeEvent(input$chkClassBnds,{
    if(nrow(values$resMC)>0){
      str(paste0("dfMC updated n=",nrow(values$resMC)))
      if(input$chkClassBnds==TRUE){
        grplist<-c("WB","Type","Period",
                   "QEtype","QualityElement","QualitySubelement",
                   "Indicator","IndSubtype",
                   "Months","Unit","Note", 
                   "Worst","PB","MP","GM","HG","Ref",
                   "Mean","StdErr","EQR","Class")
      }else{
        grplist<-c("WB","Type","Period",
                   "QEtype","QualityElement","QualitySubelement",
                   "Indicator","IndSubtype","Months",
                   "Unit","Note","Mean","StdErr","EQR","Class")
      }
      
      df<-values$resMC %>% rename(EQRMC=EQR,ClassMC=Class,Class=ClassAvg,EQR=EQRavg)
      if(!input$IgnoreErr){
        df<-ClearErrorValues(df,varList=c("EQR","Class","ClassMC","EQRMC"))
      }
      output$resTableMC <- ClassOutputTableDT(df,
                                              Groups=grplist,ClassVar="ClassMC",
                                              roundlist=c("Mean","StdErr","EQR"),colOK=11,sDOM="pl")
    }
  })
  
  
  observeEvent(values$resMC, {
    if(nrow(values$resMC)>0){
      str(paste0("dfMC updated n=",nrow(values$resMC)))
      if(input$chkClassBnds==TRUE){
        grplist<-c("WB","Type","Period",
                   "QEtype","QualityElement","QualitySubelement",
                   "Indicator","IndSubtype","Note",
                   "Unit","Months", "Worst","PB","MP","GM","HG","Ref",
                   "Mean","StdErr","EQR","Class")
      }else{
        grplist<-c("WB","Type","Period",
                   "QEtype","QualityElement","QualitySubelement",
                   "Indicator","IndSubtype","Note",
                   "Unit","Months","Mean","StdErr","EQR","Class")
      }
      
      df<-values$resMC %>% rename(EQRMC=EQR,ClassMC=Class,Class=ClassAvg,EQR=EQRavg)
      if(!input$IgnoreErr){
        df<-ClearErrorValues(df,varList=c("EQR","Class","ClassMC","EQRMC"))
      }
      output$resTableMC <- ClassOutputTableDT(df,
                                              Groups=grplist,ClassVar="ClassMC",
                                              roundlist=c("Mean","StdErr","EQR"),colOK=11,sDOM="pl")  
      
      resMC<-values$resMC
      resAvg<-values$resAvg

      if(!input$IgnoreErr){
        resMC<-ClearErrorValues(resMC,varList=c("EQR","Class","ClassAvg","EQRavg"))
        resAvg<-ClearErrorValues(resAvg,varList=c("EQR","Class")) 
      }
      res1MC<-Aggregate(resMC,Groups=c("Region","WB","Type","Typename","Period","sim"),level=1) %>% rename(ClassMC=Class)
      res1Avg<-Aggregate(resAvg,Groups=c("Region","WB","Type","Typename","Period"),level=1) %>%
        select(Region,WB,Type,Typename,Period,Class)
      values$res1MC<-res1MC %>% left_join(res1Avg)
    }
  })
  
  observeEvent(input$IgnoreErr, {
    if(nrow(values$resMC)>0){
      df<-values$resMC %>% rename(EQRMC=EQR,ClassMC=Class,Class=ClassAvg,EQR=EQRavg)
      #browser()
      if(!input$IgnoreErr){
        df<-ClearErrorValues(df,varList=c("EQR","Class","EQRMC","ClassMC"))
      }
      grplist<-c("WB","Type","Period","QEtype","QualityElement","QualitySubelement",
                 "Indicator","IndSubtype","Note","Unit","Months",#"Worst","PB","MP","GM","HG","Ref",
                 "Mean","StdErr","EQR","Class")
      output$resTableMC <- ClassOutputTableDT(df,
                                              Groups=grplist,ClassVar="ClassMC",
                                              roundlist=c("Mean","StdErr","EQR"),colOK=9,sDOM="pl")   
      resMC<-values$resMC
      resAvg<-values$resAvg
      if(!input$IgnoreErr){
        resMC<-ClearErrorValues(resMC,varList=c("EQR","Class","ClassAvg","EQRavg"))
        resAvg<-ClearErrorValues(resAvg,varList=c("EQR","Class")) 
      }
      res1MC<-Aggregate(resMC,Groups=c("Region","WB","Type","Typename","Period","sim"),level=1) %>% rename(ClassMC=Class)
      res1Avg<-Aggregate(resAvg,Groups=c("Region","WB","Type","Typename","Period"),level=1) %>%
        select(Region,WB,Type,Typename,Period,Class)
      values$res1MC<-res1MC %>% left_join(res1Avg)
    }
    values$res2MC<-""
    values$res3MC<-""
    values$res4MC<-""
    values$resInd<-""
    
  })  
  
  observeEvent(input$resTable1_rows_selected,{
    df<-values$resMC %>% group_by(WB,Period) %>% summarise() %>% ungroup()
    values$sWB<-df$WB[input$resTable1_rows_selected]
    values$sPeriod<-df$Period[input$resTable1_rows_selected]
    df<-filter(values$resMC,WB==values$sWB,Period==values$sPeriod)
    if(!input$IgnoreErr){
      df<-ClearErrorValues(df,varList=c("EQR","Class","ClassAvg","EQRavg"))
    }
    res2MC<-Aggregate(df,Groups=c("WB","Period","Type","sim"),level=2) %>% 
      rename(ClassMC=Class,EQRMC=EQR)
    df<-filter(values$resAvg,WB==values$sWB,Period==values$sPeriod)
    if(!input$IgnoreErr){
      df<-ClearErrorValues(df,varList=c("EQR","Class"))
    }
    res2Avg<-Aggregate(df,Groups=c("WB","Period","Type"),level=2) %>%
      select(WB,Type,Period,QEtype,EQR,Class)
    values$res2MC<-res2MC %>% left_join(res2Avg)
    values$res3MC<-""
    values$res4MC<-""
    values$resInd<-""
  })
  
  observeEvent(input$resTable2_rows_selected,{
    n<-input$resTable2_rows_selected
    df<- values$res2MC %>% group_by(QEtype) %>% summarise() %>% ungroup()
    values$sQEtype<-df$QEtype[input$resTable2_rows_selected]
    df<-filter(values$resMC,WB==values$sWB,Period==values$sPeriod,QEtype==values$sQEtype)
    if(!input$IgnoreErr){
      df<-ClearErrorValues(df,varList=c("EQR","Class","ClassAvg","EQRavg"))
    }
    res3MC<-Aggregate(df,Groups=c("WB","Period","Type","sim"),level=3) %>%
      rename(ClassMC=Class,EQRMC=EQR)
    
    df<-filter(values$resAvg,WB==values$sWB,Period==values$sPeriod,QEtype==values$sQEtype)
    if(!input$IgnoreErr){
      df<-ClearErrorValues(df,varList=c("EQR","Class"))
    }
    res3Avg<-Aggregate(df,Groups=c("WB","Period","Type"),level=3) %>%
      select(WB,Type,Period,QEtype,QualityElement,EQR,Class)
    values$res3MC<-res3MC %>% left_join(res3Avg)
    
    values$res4MC<-""
    values$resInd<-""
  })
  
  observeEvent(input$resTable3_rows_selected,{
    n<-input$resTable3_rows_selected
    df<- values$res3MC %>% group_by(QualityElement) %>% summarise() %>% ungroup()
    values$sQualityElement<-df$QualityElement[input$resTable3_rows_selected]
    df<-filter(values$resMC,WB==values$sWB,Period==values$sPeriod,
               QEtype==values$sQEtype,QualityElement==values$sQualityElement)
    if(!input$IgnoreErr){
      df<-ClearErrorValues(df,varList=c("EQR","Class","ClassAvg","EQRavg"))
    }
    res4MC<-Aggregate(df,Groups=c("WB","Period","Type","sim"),level=4) %>%
      rename(ClassMC=Class,EQRMC=EQR)
    
    df<-filter(values$resAvg,WB==values$sWB,Period==values$sPeriod,
               QEtype==values$sQEtype,QualityElement==values$sQualityElement)
    if(!input$IgnoreErr){
      df<-ClearErrorValues(df,varList=c("EQR","Class","ClassAvg","EQRavg"))
    }
    res4Avg<-Aggregate(df,Groups=c("WB","Period","Type"),level=4) %>%
      select(WB,Type,Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    values$res4MC<-res4MC %>% left_join(res4Avg)
    
    values$resInd<-""
  })
  
  observeEvent(input$resTable4_rows_selected,{
    n<-input$resTable4_rows_selected
    df<- values$res4MC %>% group_by(QualitySubelement) %>% summarise() %>% ungroup()
    values$sQualitySubelement<-df$QualitySubelement[input$resTable4_rows_selected]
    
    df<-values$resMC
    if(!input$IgnoreErr){
      df<-ClearErrorValues(df,varList=c("EQR","Class","ClassAvg","EQRavg"))
    }
    
    values$resInd<-filter(df,WB==values$sWB,Period==values$sPeriod,
                          QEtype==values$sQEtype,QualityElement==values$sQualityElement,
                          QualitySubelement==values$sQualitySubelement) %>%
      rename(EQRMC=EQR,ClassMC=Class,Class=ClassAvg,EQR=EQRavg)
  })
  

  
  
  observeEvent(values$res1MC, {
    output$resTable1 <- ClassOutputTableDT(values$res1MC,Groups=c("Region","WB","Type","Typename","Period","Class"),ClassVar="ClassMC")
    
  })
  
  
  observeEvent(values$res2MC, {
    grplist<-c("WB","Period","Type","QEtype","EQR","Class")
    rmlist=c("WB","Period","Type")
    
    output$resTable2 <- ClassOutputTableDT(values$res2MC,
                                           Groups=grplist,
                                           roundlist=c("EQR"),
                                           remove=rmlist,ClassVar="ClassMC")
  })
  
  
  observeEvent(values$res3MC, {
    grplist<-c("WB","Period","Type","QEtype","QualityElement","EQR","Class")
    rmlist=c("WB","Period","Type","QEtype")
    output$resTable3 <- ClassOutputTableDT(values$res3MC,roundlist=c("EQR"),
                                           Groups=grplist,remove=rmlist,ClassVar="ClassMC")
  })
  
  observeEvent(values$res4MC, {
    grplist<-c("WB","Period","Type","QEtype","QualityElement","QualitySubelement","EQR","Class")
    rmlist=c("WB","Period","Type","QEtype","QualityElement")
    output$resTable4 <- ClassOutputTableDT(values$res4MC,roundlist=c("EQR"),
                                           Groups=grplist,remove=rmlist,ClassVar="ClassMC")
  })
  
  observeEvent(values$resInd, {
    grplist<-c("WB","Type","Period",
               "QEtype","QualityElement","QualitySubelement",
               "Indicator","IndSubtype","Note",
               "Unit","Months","Worst","PB","MP","GM","HG","Ref",
               "Mean","StdErr","EQR","Class")
    rmlist<-c("WB","Type","Period",
              "QEtype","QualityElement","QualitySubelement")
    
    output$resTableInd <- ClassOutputTableDT(values$resInd,
                                             Groups=grplist,
                                             ClassVar="ClassMC",
                                             roundlist=c("Mean","StdErr","EQR"),
                                             remove=rmlist,colOK=3
    )
  })
  
  
  
  observeEvent(input$goButton,{
    
    nSimMC <- input$n
    IndList<- input$indSelect
    
    #Check that at least one indicator has been selected
    if(length(IndList)>0){
      df.select <- filter(df, WB %in% input$waterbody, period %in% input$period)
      n<-nrow(df.select)
      
      #Progress wrap Start
      style <- isolate(input$style)
      withProgress(message = 'Calculating', style = style, value = 0.0, {
        
      #Call the assessment calculations
      AssessmentResults<-Assessment(df.select,nsim=nSimMC,IndList)
      
      })
      
      resAvg<-AssessmentResults[[1]]
      resMC<-AssessmentResults[[2]]
      resErr<-AssessmentResults[[3]]
      
      values$resAvg <- resAvg
      values$resMC <- resMC
      values$resErr <- resErr
      values$res2MC <-""
      values$res3MC <-""
      values$res4MC <-""
      values$resInd<-""
      
      #browser()
      updateNavbarPage(session, "inTabset", selected="Results")
    }else{
      #no indicators selected
      showModal(modalDialog(
        title = div(tags$b("No indicators selected", style = "color: red;")),
        "You need to select at least one indicator!"
      ))
    }
    
  })
  
  
  datacount<- reactive({
    nrow(filter(df, WB %in% input$waterbody, period %in% input$period))
  })
  
  
  df.select <- reactive({
    df %>% filter(WB %in% input$waterbody, period %in% input$period)
  })
  
  
  output$downloadReport <- downloadHandler(
    
    #contentType="application/pdf",
    contentType="text",
    filename = function(){
      paste0("report-", Sys.Date(), ".pdf")
    },
    content = function(file){
      write("test",file)
    }
  )
  
  
  district_list <- reactive({
    sort(unique(df$DistrictID))
  })
  
  waterbody_list <- reactive({
    dfwb<-filter(df, DistrictID %in% input$district)
    res<- sort(unique(dfwb$WB))
    return(res)
  })
  
  period_list <- reactive({
    dfwb<-filter(df, WB %in% input$waterbody, DistrictID %in% input$district)
    res<- sort(unique(dfwb$period), decreasing = TRUE)
    return(res)
  })
  
  
  output$selectWaterDistrict <- renderUI({
    tagList(
      selectInput("district", "Select Water District", choices=district_list(), selected="")
    )})
  
  
  output$selectPeriod <- renderUI({
    tagList(
      selectInput("period", "Select Period(s)", choices=period_list(), multiple=TRUE)
    )})
  
  output$selectWaterBodies <- renderUI({
    tagList(
      selectInput("waterbody", "Select Waterbody(s)", choices=waterbody_list(), selected="ALL", multiple=TRUE)
    )})
  
  output$dataButton <- renderUI({
    if(datacount()>0){
      buttontext<-paste0("Get ", as.character(datacount()), " rows of data.")
      tagList(actionButton("dataButton",buttontext ))
    }
  })
  
  output$goButton <- renderUI({
    if(datacount()>0){
      tagList(actionButton("goButton", "Calculate Status"))
    }
  })
  
  output$resTableErr <- DT::renderDataTable({
    dt <- values$resErr %>% datatable(escape = F,rownames = F,selection = 'single',
              options = list(dom="pl"))
    dt
  })
  
  
  #Check box with list of available indicators
  output$chkIndicators <- renderUI({
    if(datacount()>0){
      sList<-NULL
      Choices<-NULL
      dfind <- df %>% filter(WB %in% input$waterbody, period %in% input$period)
      if(length(names(dfind)[names(dfind)=="DIN"])==0){
        dfind$DIN<-NA
      }
      if(length(names(dfind)[names(dfind)=="DIP"])==0){
        dfind$DIP<-NA
      }
      
      
      
      nchl<- dfind %>% filter(!is.na(chla)) %>% summarise(n=n())
      if(nchl[1,1]>0){
        sList = c(sList,"Chlorophyll a" = "Chla","Chlorophyll a (EQR)" = "ChlaEQR")
        Choices<-c(Choices,"Chla")
      }
      ntn<- dfind %>% filter(!is.na(TN)) %>% summarise(n=n())
      if(ntn[1,1]>0){
        sList = c(sList,"Summer TN" = "TNsummer","Summer TN (EQR)" = "TNsummerEQR",
                  "Winter TN" = "TNwinter","Winter TN (EQR)" = "TNwinterEQR")
        Choices<-c(Choices,"TNsummer","TNwinter")
      }
      ntp<- dfind %>% filter(!is.na(TP)) %>% summarise(n=n())
      if(ntp[1,1]>0){
        sList = c(sList,"Summer TP" = "TPsummer","Summer TP (EQR)" = "TPsummerEQR",
                  "Winter TP" = "TPwinter","Winter TP (EQR)" = "TPwinterEQR")
        Choices<-c(Choices,"TPsummer","TPwinter")
      }
      ndin<- dfind %>% filter(!is.na(DIN)) %>% summarise(n=n())
      if(ndin[1,1]>0){
        sList = c(sList,"Summer DIN" = "DINsummer","Summer DIN (EQR)" = "DINsummerEQR")
        Choices<-c(Choices,"DINsummer")
      }
      ndip<- dfind %>% filter(!is.na(DIP)) %>% summarise(n=n())
      if(ndip[1,1]>0){
        sList = c(sList,"Summer DIP" = "DIPsummer","Summer DIP (EQR)" = "DIPsummerEQR")
        Choices<-c(Choices,"DIPsummer")
      }
      nsec<- dfind %>% filter(!is.na(secchi)) %>% summarise(n=n())
      if(nsec[1,1]>0){
        sList = c(sList,"Secchi Depth" = "Secchi","Secchi Depth (EQR)" = "SecchiEQR")
        Choices<-c(Choices,"Secchi")
      }
      nbqi<- dfind %>% filter(!is.na(BQI)) %>% summarise(n=n())
      if(nbqi[1,1]>0){
        sList = c(sList,"Benthic Quality Index (BQI)" = "BQI")
        Choices<-c(Choices,"BQI")
      }
      nmsmdi<- dfind %>% filter(!is.na(MSMDI)) %>% summarise(n=n())
      if(nmsmdi[1,1]>0){
        sList = c(sList,"Multi Species Maximum Depth Index (MSMDI)" = "MSMDI")
        Choices<-c(Choices,"MSMDI")
      }
      
      #Choices<-c("Chla")
      tagList(
        checkboxGroupInput("indSelect", "Indicators:",
                           sList,selected=Choices ))
      
    }
  })
  
})


