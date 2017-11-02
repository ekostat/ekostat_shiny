

library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(shiny)
library(dplyr)

source("ReadIndicatorParms.R")
source("CalculateIndicator.R")
source("IndicatorFunctions.R")
source("CalculateIndicatorSupport.R")
source("Assessment.R")
source("ReadBounds.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")


shinyServer(function(input, output) {
  # missing code
  
#Prepare data
  df<-read.table("data/data.txt", fileEncoding = "UTF-8", sep=";", stringsAsFactors=F, header=T)
  df.wb<-read.table("data/waterbodies.txt", fileEncoding = "UTF-8", sep="\t", stringsAsFactors=F, header=T)
  
  df[df$WB_name=="Halse/Askeröfj","WB_name"]<-"Halsefjorden"
  df<-df %>% left_join(select(df.wb,WaterbodyName,WaterbodyID,DistrictID), by=c("WB_name"="WaterbodyName")) 
  df$WB<-paste0(df$WaterbodyID," ",df$WB_name)  
  
  RoundColList<-c("secchi","temp","sali","chla" ,"TP","TN","dens_dif","BQI","MSMDI","logitMSMDI")
  selection<-c("DistrictID","WB","station","typology","date",
               "year","period","station_depth","secchi","temp","sali",
               "chla" ,"TP","TN","dens_dif","springlag","BQI","MSMDI","logitMSMDI")
  
  df<-df[,selection]
  
  IndList<-c("ChlaEQR","TNsummer","TNwinter","Secchi","MSMDI","BQI")
  
  output$nText <- renderText({
    outText()
  })
  
  observeEvent(input$goButton, {
    df.select <- filter(df, WB %in% input$waterbody, period==input$period)
    n<-nrow(df.select)
    output$nrows <- renderUI({
      tagList(p(renderText(paste0("Loaded: ",n, " rows of data."))))
    })
    
    nSimMC <- input$n
    
    AssessmentResults<-Assessment(df.select,nsim=nSimMC,IndList)
    df.resultsOverall<-AssessmentResults[[1]]
    df.resultsQE<-AssessmentResults[[2]]
    df.resultsInd<-AssessmentResults[[3]]
    
    output$resTableQE <- renderUI({ 
      if(is.data.frame(df.resultsQE)){
        return(tagList(renderTable(df.resultsQE)))
      }
    })
    
    output$resTableOverall <- renderUI({ 
      if(is.data.frame(df.resultsOverall)){
        return(tagList(renderTable(df.resultsOverall)))
      }
    })  
    
    output$resTableInd <- renderUI({ 
      if(is.data.frame(df.resultsInd)){
        return(tagList(renderTable(df.resultsInd)))
      }
    })  
    
  })
  
  datacount<- reactive({
    nrow(filter(df, WB %in% input$waterbody))
  })
  
  
  outputdata <- reactive({
    df.select
  })
  
  
  #value <- reactiveValues(0)
  output$downloadReport <- downloadHandler(
    contentType="application/pdf",
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
  
  period_list <- reactive({
    sort(unique(df$period), decreasing = TRUE)
  })
  
  waterbody_list <- reactive({
    res<- sort(unique(df$WB))
    return(res)
  })
  
  output$selectWaterDistrict <- renderUI({
    tagList(
      selectInput("district", "Select Water District", choices=district_list(), selected="")
    )})
  
  
  output$selectPeriod <- renderUI({
    tagList(
      selectInput("period", "Select Period", choices=period_list(), multiple=FALSE)
    )})
  
  output$selectWaterBodies <- renderUI({
    tagList(
      selectInput("waterbody", "Select Waterbody(s)", choices=waterbody_list(), selected="ALL", multiple=TRUE)
    )})
  
  output$dataButton <- renderUI({
    if(datacount()>0){
      tagList(actionButton("goButton", "GO"))
    }
  })
  
  
  
})