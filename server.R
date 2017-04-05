

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   # missing code
  outText <- eventReactive(input$goButton, {
    input$n
  })
  
  output$nText <- renderText({
    outText()
  })
  })
  
