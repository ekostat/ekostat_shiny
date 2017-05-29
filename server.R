

library(shiny)
library(dplyr)

shinyServer(function(input, output) {
  # missing code
  
  df.select <- reactiveValues(data = NULL)
  
  df<-read.table("data/data.txt", fileEncoding = "UTF-8", sep=";", stringsAsFactors=F, header=T)
  df.wb<-read.table("data/waterbodies.txt", fileEncoding = "UTF-8", sep="\t", stringsAsFactors=F, header=T)

  df<-df %>% left_join(select(df.wb,WaterbodyID,DistrictID), by=c("WB_ID"="WaterbodyID")) 
  df$WB<-paste0(df$WB_ID," ",df$WB_name)  
  
  output$nText <- renderText({
    outText()
  })
  
  observeEvent(input$goButton, {
    df.select <- filter(df, WB %in% input$waterbody)
    n<-nrow(df.select)
    output$nrows <- renderUI({
      tagList(p(renderText(paste0("Loaded: ",n, " rows of data."))))
    })
  })
  
  datacount<- reactive({
      nrow(filter(df, WB %in% input$waterbody))
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
      tagList(actionButton("goButton", "Get data"))
    }
  })
  
})

