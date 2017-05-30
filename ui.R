library(shiny)

# Define UI for random distribution application 
shinyUI(
  navbarPage(
    windowTitle="WATERS",
    #title=div(img(src="waters_1.gif"), "WATERS Assessment"), 
    title= "WATERS Assessment", 
    tabPanel("Data",
             navlistPanel(
               widths=c(2,10),well=F,
               tabPanel(
                 "New",
                 p(strong("New Assessment")),
                 #p("Select waterbodies and download data from database(s)."),
                 p("Select waterbodies"),
                 uiOutput("selectWaterDistrict"),
                 uiOutput("selectPeriod"),
                 uiOutput("selectWaterBodies"),
                 uiOutput("dataButton"),
                 uiOutput("nrows")
                 #actionButton("goButton", "Get data")
                 #conditionalPanel(condition = "input.waterbody != ''",)
               ),
               tabPanel("Open",
                        fileInput('file1', 'Load existing assessment',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv'))  
               ),
               tabPanel("Save",
                        p(strong("Save the current assessment")),
                        downloadButton('downloadData', label="Save") 
               )
             )
             
    ),
    tabPanel(
      "Options",
      navlistPanel(
        widths=c(2,10),well=F,
        tabPanel("Monte Carlo",
                 "Options for Monte Carlo simulations.",
                 numericInput("n",
                              label = "Number of simulations", 
                              value = 100)   
                 ),
        tabPanel("Uncertainty",
                 "Uncertainty Library"),
        tabPanel("Aggregation",
                 "Aggregation methods")
      )
    ),
    
    tabPanel(
      "Results",
      navlistPanel(
        widths=c(2,10),well=F,
        tabPanel("Overall",
                 "Overall Results",
                 uiOutput("resTableOverall")),
        tabPanel("Quality Element",
                 "QE Results",
                 uiOutput("resTableQE")),
        tabPanel("Indicator",
                 "Indicator Data",
                 uiOutput("resTableInd"))
      ),
      
      fluidRow(column(width=12,
        downloadButton('downloadReport', label="Download report")
      ))
    )
  ))


