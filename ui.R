library(shiny)

# Define UI for random distribution application 
shinyUI(
  navbarPage(
    title="WATERS Assessment", 
    tabPanel("Data",
             navlistPanel(
               widths=c(2,10),well=F,
               tabPanel(
                 "New",
                 p(strong("New Assessment")),
                 p("Select waterbodies and dowload data from database(s)."),
                 p("Include own data?"),
                 uiOutput("selectWaterDistrict"),
                 uiOutput("selectWaterBodies"),
                 actionButton("goButton", "Download data")
               ),
               tabPanel("Load",
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
        tabPanel("Uncertainty",
                 "Uncertainty Library"),
        tabPanel("Aggregation",
                 "Aggregation methods"),
        tabPanel("Monte Carlo",
                 "Options for Monte Carlo simulations.")
      )
    ),
    
    tabPanel(
      "Results",
      navlistPanel(
        widths=c(2,10),well=F,
        tabPanel("Indicator",
                 "Indicator Results"),
        tabPanel("Quality Element",
                 "QE Results"),
        tabPanel("Overall",
                 "Overall Results")
      ),
      
      fluidRow(column(width=12,
        downloadButton('downloadReport', label="Download report")
      ))
    )
  ))


