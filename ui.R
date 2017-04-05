library(shiny)

shinyUI(fluidPage(title="WATERS",
                  
                  titlePanel(
                    a(href="http://waters.gu.se", target="_blank", img(src="1530704_waters_webheader2.jpg"))),
                  
                  navlistPanel(
                    "Input",
                    tabPanel("Data",
                             actionButton("goButton", "Get data"),
                             fileInput('file1', 'Load existing assessment',
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv'))
                    ),
                    "Options",
                    tabPanel("Fixed and Variance Parameters"),
                    tabPanel("Indicator Models"),
                    tabPanel("Boundaries"),
                    "Results",
                    tabPanel("Assessment"),
                    "Output",
                    tabPanel("Assessment",
                             fluidRow(
                               column(2,
                                      "sidebar"
                               ),
                               column(10,
                                      "main"
                               )
                             )
                    )
                  ))
)