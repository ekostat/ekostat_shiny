


library(shiny)
library(DT)
library(tidyr)
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
source("ReadBathymetry.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")
source("Aggregation.R")
source("classoutputtable.R")


shinyServer(function(input, output, session) {
  #Read observation data
  df <-
    read.table(
      "data/data.txt",
      fileEncoding = "UTF-8",
      sep = ";",
      stringsAsFactors = F,
      header = T
    )
  df <- df %>% select(-c(TYP_NFS06, x_utm, y_utm, area))
  
  #Read waterbody data
  df.wb <-
    read.table(
      "data/waterbodies.txt",
      fileEncoding = "UTF-8",
      sep = "\t",
      stringsAsFactors = F,
      header = T
    )
  
  #Fix observation data
  df[df$WB_name == "Halse/Askeröfj", "WB_name"] <- "Halsefjorden"
  df <-
    df %>% left_join(
      select(df.wb, WaterbodyName, WaterbodyID, DistrictID),
      by = c("WB_name" = "WaterbodyName")
    )
  df$WB <- paste0(df$WaterbodyID, " ", df$WB_name)
  df$obspoint <- df$station
  df$typology <-
    ifelse(is.na(df$typology), df$typology, "SE_2") # was missing for BQI stations
  
  
  
  values <- reactiveValues(resMC = data.frame())
  
  RoundColList <-
    c(
      "secchi",
      "temp",
      "sali",
      "chla" ,
      "biovol",
      "TP",
      "TN",
      "dens_dif",
      "BQI",
      "MSMDI",
      "logitMSMDI",
      "Oxygen"
    )
  
  output$nText <- renderText({
    outText()
  })
  
  
  # Oberve changes in number of Monte Carlo simulations
  observeEvent(input$n, {
    nsim <- as.numeric(input$n)
    if (is.na(nsim)) {
      nsim <- 0
    }
    if (nsim < 1) {
      showModal(modalDialog(
        title = div(tags$b("MC Simulations", style = "color: red;")),
        "Number of simulations cannot be less than 1!"
      ))
      updateNumericInput(session, "n", value = 1)
    }
  })
  
  observeEvent(input$dataButton, {
    #df.select <- filter(df, WB %in% input$waterbody, period %in% input$period)
    n <- nrow(df.select())
    output$nrows <- renderUI({
      tagList(p(renderText(
        paste0("Selected: ", n, " rows of data.")
      )))
    })
    updateNavbarPage(session, "inTabset", selected = "Assessment")
  })
  
  observeEvent(input$chkClassBnds, {
    if (nrow(values$resMC) > 0) {
      str(paste0("dfMC updated n=", nrow(values$resMC)))
      if (input$chkClassBnds == TRUE) {
        grplist <- c(
          "WB",
          "Type",
          "Period",
          "QEtype",
          "QualityElement",
          "QualitySubelement",
          "Indicator",
          "IndSubtype",
          "Months",
          "Unit",
          "Note",
          "Worst",
          "PB",
          "MP",
          "GM",
          "HG",
          "Ref",
          "Mean",
          "StdErr",
          "EQR",
          "Class"
        )
      } else{
        grplist <- c(
          "WB",
          "Type",
          "Period",
          "QEtype",
          "QualityElement",
          "QualitySubelement",
          "Indicator",
          "IndSubtype",
          "Months",
          "Unit",
          "Note",
          "Mean",
          "StdErr",
          "EQR",
          "Class"
        )
      }
      
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )
      if (!input$IgnoreErr) {
        df <- ClearErrorValues(df, varList = c("EQR", "Class", "ClassMC", "EQRMC"))
      }
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 11,
        sDOM = "pl"
      )
    }
  })
  
  
  observeEvent(values$resMC, {
    if (nrow(values$resMC) > 0) {
      str(paste0("dfMC updated n=", nrow(values$resMC)))
      if (input$chkClassBnds == TRUE) {
        grplist <- c(
          "WB",
          "Type",
          "Period",
          "QEtype",
          "QualityElement",
          "QualitySubelement",
          "Indicator",
          "IndSubtype",
          "Note",
          "Unit",
          "Months",
          "Worst",
          "PB",
          "MP",
          "GM",
          "HG",
          "Ref",
          "Mean",
          "StdErr",
          "EQR",
          "Class"
        )
      } else{
        grplist <- c(
          "WB",
          "Type",
          "Period",
          "QEtype",
          "QualityElement",
          "QualitySubelement",
          "Indicator",
          "IndSubtype",
          "Note",
          "Unit",
          "Months",
          "Mean",
          "StdErr",
          "EQR",
          "Class"
        )
      }
      
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )
      if (!input$IgnoreErr) {
        df <- ClearErrorValues(df, varList = c("EQR", "Class", "ClassMC", "EQRMC"))
      }
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 11,
        sDOM = "pl"
      )
      
      resMC <- values$resMC
      resAvg <- values$resAvg
      
      if (!input$IgnoreErr) {
        resMC <-
          ClearErrorValues(resMC, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
        resAvg <- ClearErrorValues(resAvg, varList = c("EQR", "Class"))
      }
      res1MC <-
        Aggregate(
          resMC,
          Groups = c("Region", "WB", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = Class)
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB, Type, Typename, Period, Class)
      values$res1MC <- res1MC %>% left_join(res1Avg)
    }
  })
  
  observeEvent(input$IgnoreErr, {
    if (nrow(values$resMC) > 0) {
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )
      #browser()
      if (!input$IgnoreErr) {
        df <- ClearErrorValues(df, varList = c("EQR", "Class", "EQRMC", "ClassMC"))
      }
      grplist <-
        c(
          "WB",
          "Type",
          "Period",
          "QEtype",
          "QualityElement",
          "QualitySubelement",
          "Indicator",
          "IndSubtype",
          "Note",
          "Unit",
          "Months",
          #"Worst","PB","MP","GM","HG","Ref",
          "Mean",
          "StdErr",
          "EQR",
          "Class"
        )
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 9,
        sDOM = "pl"
      )
      resMC <- values$resMC
      resAvg <- values$resAvg
      if (!input$IgnoreErr) {
        resMC <-
          ClearErrorValues(resMC, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
        resAvg <- ClearErrorValues(resAvg, varList = c("EQR", "Class"))
      }
      res1MC <-
        Aggregate(
          resMC,
          Groups = c("Region", "WB", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = Class)
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB, Type, Typename, Period, Class)
      values$res1MC <- res1MC %>% left_join(res1Avg)
    }
    values$res2MC <- ""
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
    
  })
  
  observeEvent(input$resTable1_rows_selected, {
    df <-
      values$resMC %>% group_by(WB, Period) %>% summarise() %>% ungroup()
    values$sWB <- df$WB[input$resTable1_rows_selected]
    values$sPeriod <- df$Period[input$resTable1_rows_selected]
    df <- filter(values$resMC, WB == values$sWB, Period == values$sPeriod)
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res2MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 2) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    df <- filter(values$resAvg, WB == values$sWB, Period == values$sPeriod)
    if (!input$IgnoreErr) {
      df <- ClearErrorValues(df, varList = c("EQR", "Class"))
    }
    res2Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 2) %>%
      select(WB, Type, Period, QEtype, EQR, Class)
    values$res2MC <- res2MC %>% left_join(res2Avg)
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  })
  
  observeEvent(input$resTable2_rows_selected, {
    n <- input$resTable2_rows_selected
    df <-
      values$res2MC %>% group_by(QEtype) %>% summarise() %>% ungroup()
    values$sQEtype <- df$QEtype[input$resTable2_rows_selected]
    df <-
      filter(values$resMC,
             WB == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res3MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 3) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <-
      filter(values$resAvg,
             WB == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)
    if (!input$IgnoreErr) {
      df <- ClearErrorValues(df, varList = c("EQR", "Class"))
    }
    res3Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 3) %>%
      select(WB, Type, Period, QEtype, QualityElement, EQR, Class)
    values$res3MC <- res3MC %>% left_join(res3Avg)
    
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  })
  
  observeEvent(input$resTable3_rows_selected, {
    #n<-input$resTable3_rows_selected
    df <-
      values$res3MC %>% group_by(QualityElement) %>% summarise() %>% ungroup()
    values$sQualityElement <-
      df$QualityElement[input$resTable3_rows_selected]
    df <- filter(
      values$resMC,
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res4MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 4) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <- filter(
      values$resAvg,
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res4Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 4) %>%
      select(WB,
             Type,
             Period,
             QEtype,
             QualityElement,
             QualitySubelement,
             EQR,
             Class)
    values$res4MC <- res4MC %>% left_join(res4Avg)
    
    values$resInd <- ""
    values$resObs <- ""
  })
  
  observeEvent(input$resTable4_rows_selected, {
    #n<-input$resTable4_rows_selected
    df <-
      values$res4MC %>% group_by(QualitySubelement) %>% summarise() %>% ungroup()
    values$sQualitySubelement <-
      df$QualitySubelement[input$resTable4_rows_selected]
    
    df <- values$resMC
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    
    values$resInd <- filter(
      df,
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement,
      QualitySubelement == values$sQualitySubelement
    ) %>%
      rename(
        EQRMC = EQR,
        ClassMC = Class,
        Class = ClassAvg,
        EQR = EQRavg
      )
  })
  
  observeEvent(input$resTableInd_rows_selected, {
    #n<-input$resTableInd_rows_selected
    df <-
      values$resInd %>% group_by(Indicator) %>% summarise() %>% ungroup()
    values$sIndicator <-
      df$Indicator[input$resTable4_rows_selected]
    
    df <- SelectObs(
        df.select(),
        indicator = values$sIndicator,
        sWB = values$sWB,
        sPeriod = values$sPeriod
      )
    if(nrow(df)>0){
      values$resObs <- df
    }else{
      values$resObs <- ""
    }
   
  })
  
  output$titleTable1 <- renderText({
    if (is.null(values$res1MC)) {
      "<h3>No results</h3>"
    } else{
      if (values$res1MC == "") {
        "<h3>No results</h3>" # style='color:#FF0000';
      } else{
        "<h3>Overall Results:</h3>"
      }
    }
  })
  
  
  observeEvent(values$res1MC, {
    output$resTable1 <-
      ClassOutputTableDT(
        values$res1MC,
        Groups = c("Region", "WB", "Type", "Typename", "Period", "Class"),
        ClassVar = "ClassMC"
      )
    
  })
  
  
  observeEvent(values$res2MC, {
    grplist <- c("WB", "Period", "Type", "QEtype", "EQR", "Class")
    rmlist = c("WB", "Period", "Type")
    
    output$resTable2 <- ClassOutputTableDT(
      values$res2MC,
      Groups = grplist,
      roundlist = c("EQR"),
      remove = rmlist,
      ClassVar = "ClassMC"
    )
    
    output$titleTable2 <- renderText({
      if (values$res2MC == "") {
        ""
      } else{
        "<h3>Biological/Supporting:</h3>"
      }
    })
    
  })
  
  
  observeEvent(values$res3MC, {
    grplist <-
      c("WB",
        "Period",
        "Type",
        "QEtype",
        "QualityElement",
        "EQR",
        "Class")
    rmlist = c("WB", "Period", "Type", "QEtype")
    output$resTable3 <-
      ClassOutputTableDT(
        values$res3MC,
        roundlist = c("EQR"),
        Groups = grplist,
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
    output$titleTable3 <- renderText({
      if (values$res3MC == "") {
        ""
      } else{
        "<h3>QualityElement:</h3>"
      }
    })
  })
  
  observeEvent(values$res4MC, {
    grplist <-
      c(
        "WB",
        "Period",
        "Type",
        "QEtype",
        "QualityElement",
        "QualitySubelement",
        "EQR",
        "Class"
      )
    rmlist = c("WB", "Period", "Type", "QEtype", "QualityElement")
    output$resTable4 <-
      ClassOutputTableDT(
        values$res4MC,
        roundlist = c("EQR"),
        Groups = grplist,
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
    output$titleTable4 <- renderText({
      if (values$res4MC == "") {
        ""
      } else{
        "<h3>Subelement:</h3>"
      }
    })
  })
  
  observeEvent(values$resInd, {
    grplist <- c(
      "WB",
      "Type",
      "Period",
      "QEtype",
      "QualityElement",
      "QualitySubelement",
      "Indicator",
      "IndSubtype",
      "Note",
      "Unit",
      "Months",
      "Worst",
      "PB",
      "MP",
      "GM",
      "HG",
      "Ref",
      "Mean",
      "StdErr",
      "EQR",
      "Class"
    )
    rmlist <- c("WB",
                "Type",
                "Period",
                "QEtype",
                "QualityElement",
                "QualitySubelement")
    
    output$resTableInd <- ClassOutputTableDT(
      values$resInd,
      Groups = grplist,
      ClassVar = "ClassMC",
      roundlist = c("Mean", "StdErr", "EQR"),
      remove = rmlist,
      colOK = 3
    )
    output$titleTableInd <- renderText({
      if (values$resInd == "") {
        ""
      } else{
        "<h3>Indicators:</h3>"
      }
    })
  })
  
  observeEvent(values$resObs, {
    if (!is.null(values$sIndicator)) {
      vars = GetVarNames(values$sIndicator)
    } else{
      vars <- ""
    }
    
    output$resTableObs <-
      ClassObsTableDT(values$resObs, sDOM = "pl", roundlist = vars)
    
    output$titleTableObs <- renderText({
      if (values$resObs == "") {
        ""
      } else{
        "<h3>Observations:</h3>"
      }
    })
    
    
    output$plotObs <- renderPlot({
      if (values$resObs == "") {
        p <- 0
      } else{
        yvar <- vars[length(vars)]
    
        df <- values$resObs
        df$date <- as.Date(df$date)
        df$station <- as.factor(df$station)
       
        p <- ggplot(df, aes_string(x = "date", y = yvar, colour="station")) + geom_point(size=2) 
        p <- p + theme_minimal(base_size = 16) #+ scale_fill_manual(values = c("red", "blue", "green", "black")) #+scale_colour_tableau("station") #,
        
      }
      return(p)
    }, height = 400, width = 600)
    
    
    
  })
  
  
  
  observeEvent(input$goButton, {
    nSimMC <- input$n
    IndList <- input$indSelect
    
    #Check that at least one indicator has been selected
    if (length(IndList) > 0) {
      df.select <-
        filter(df, WB %in% input$waterbody, period %in% input$period)
      n <- nrow(df.select)
      
      #Progress wrap Start
      style <- isolate(input$style)
      withProgress(message = 'Calculating',
                   style = style,
                   value = 0.0,
                   {
                     #Call the assessment calculations
                     AssessmentResults <- Assessment(df.select, nsim = nSimMC, IndList)
                     
                   })
      
      resAvg <- AssessmentResults[[1]]
      resMC <- AssessmentResults[[2]]
      resErr <- AssessmentResults[[3]]
      
      #save(AssessmentResults,file="results.Rda")
      
      values$resAvg <- resAvg
      values$resMC <- resMC
      values$resErr <- resErr
      values$res2MC <- ""
      values$res3MC <- ""
      values$res4MC <- ""
      values$resInd <- ""
      values$resObs <- ""
      #browser()
      updateNavbarPage(session, "inTabset", selected = "Results")
    } else{
      #no indicators selected
      showModal(modalDialog(
        title = div(tags$b("No indicators selected", style = "color: red;")),
        "You need to select at least one indicator!"
      ))
    }
    
  })
  
  
  datacount <- reactive({
    nrow(filter(df, WB %in% input$waterbody, period %in% input$period))
  })
  
  
  df.select <- reactive({
    df %>% filter(WB %in% input$waterbody, period %in% input$period)
  })
  
  
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0("report-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0("report-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  district_list <- reactive({
    sort(unique(df$DistrictID))
  })
  
  waterbody_list <- reactive({
    dfwb <- filter(df, DistrictID %in% input$district)
    res <- sort(unique(dfwb$WB))
    return(res)
  })
  
  period_list <- reactive({
    dfwb <-
      filter(df, WB %in% input$waterbody, DistrictID %in% input$district)
    res <- sort(unique(dfwb$period), decreasing = TRUE)
    return(res)
  })
  
  
  output$selectWaterDistrict <- renderUI({
    tagList(
      selectInput(
        "district",
        "Select Water District",
        choices = district_list(),
        selected = ""
      )
    )
  })
  
  
  output$selectPeriod <- renderUI({
    tagList(selectInput(
      "period",
      "Select Period(s)",
      choices = period_list(),
      multiple = TRUE
    ))
  })
  
  output$selectWaterBodies <- renderUI({
    tagList(
      selectInput(
        "waterbody",
        "Select Waterbody(s)",
        choices = waterbody_list(),
        selected = "ALL",
        multiple = TRUE
      )
    )
  })
  
  output$dataButton <- renderUI({
    if (datacount() > 0) {
      buttontext <-
        paste0("Get ", as.character(datacount()), " rows of data.")
      tagList(actionButton("dataButton", buttontext))
    }
  })
  
  output$goButton <- renderUI({
    if (datacount() > 0) {
      tagList(actionButton("goButton", "Calculate Status"))
    }
  })
  
  output$resTableErr <- DT::renderDataTable({
    dt <-
      values$resErr %>% datatable(
        escape = F,
        rownames = F,
        selection = 'single',
        options = list(dom = "pl")
      )
    dt
  })
  
  
  #Check box with list of available indicators
  output$chkIndicators <- renderUI({
    if (datacount() > 0) {
      sList <- NULL
      Choices <- NULL
      dfind <-
        df %>% filter(WB %in% input$waterbody, period %in% input$period)
      if (length(names(dfind)[names(dfind) == "DIN"]) == 0) {
        dfind$DIN <- NA
      }
      if (length(names(dfind)[names(dfind) == "DIP"]) == 0) {
        dfind$DIP <- NA
      }
      
      
      
      nchl <- dfind %>% filter(!is.na(chla)) %>% summarise(n = n())
      if (nchl[1, 1] > 0) {
        sList = c(sList,
                  "Chlorophyll a" = "CoastChla",
                  "Chlorophyll a (EQR)" = "CoastChlaEQR")
        Choices <- c(Choices, "CoastChla")
      }
      ntn <- dfind %>% filter(!is.na(TN)) %>% summarise(n = n())
      if (ntn[1, 1] > 0) {
        sList = c(
          sList,
          "Summer TN" = "CoastTNsummer",
          "Summer TN (EQR)" = "CoastTNsummerEQR",
          "Winter TN" = "CoastTNwinter",
          "Winter TN (EQR)" = "CoastTNwinterEQR"
        )
        Choices <- c(Choices, "CoastTNsummer", "CoastTNwinter")
      }
      ntp <- dfind %>% filter(!is.na(TP)) %>% summarise(n = n())
      if (ntp[1, 1] > 0) {
        sList = c(
          sList,
          "Summer TP" = "CoastTPsummer",
          "Summer TP (EQR)" = "CoastTPsummerEQR",
          "Winter TP" = "CoastTPwinter",
          "Winter TP (EQR)" = "CoastTPwinterEQR"
        )
        Choices <- c(Choices, "CoastTPsummer", "CoastTPwinter")
      }
      ndin <- dfind %>% filter(!is.na(DIN)) %>% summarise(n = n())
      if (ndin[1, 1] > 0) {
        sList = c(sList,
                  "Summer DIN" = "CoastDINsummer",
                  "Summer DIN (EQR)" = "CoastDINsummerEQR")
        Choices <- c(Choices, "CoastDINsummer")
      }
      ndip <- dfind %>% filter(!is.na(DIP)) %>% summarise(n = n())
      if (ndip[1, 1] > 0) {
        sList = c(sList,
                  "Summer DIP" = "CoastDIPsummer",
                  "Summer DIP (EQR)" = "CoastDIPsummerEQR")
        Choices <- c(Choices, "CoastDIPsummer")
      }
      nsec <- dfind %>% filter(!is.na(secchi)) %>% summarise(n = n())
      if (nsec[1, 1] > 0) {
        sList = c(sList,
                  "Secchi Depth" = "CoastSecchi",
                  "Secchi Depth (EQR)" = "CoastSecchiEQR")
        Choices <- c(Choices, "CoastSecchi")
      }
      nbqi <- dfind %>% filter(!is.na(BQI)) %>% summarise(n = n())
      if (nbqi[1, 1] > 0) {
        sList = c(sList, "Benthic Quality Index (BQI)" = "CoastBQI")
        Choices <- c(Choices, "CoastBQI")
      }
      nmsmdi <- dfind %>% filter(!is.na(MSMDI)) %>% summarise(n = n())
      if (nmsmdi[1, 1] > 0) {
        sList = c(sList, "Multi Species Maximum Depth Index (MSMDI)" = "CoastMSMDI")
        Choices <- c(Choices, "CoastMSMDI")
      }
      
      no2 <- dfind %>% filter(!is.na(O2)) %>% summarise(n = n())
      if (no2[1, 1] > 0) {
        sList = c(sList, "Dissolved Oxygen (O2)" = "CoastOxygen")
        Choices <- c(Choices, "CoastOxygen")
      }
      
      
      
      tagList(checkboxGroupInput("indSelect", "Indicators:",
                                 sList, selected = Choices))
      
    }
  })
  
})
