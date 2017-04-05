library(shiny)

shinyUI(fluidPage(title="WATERS",
  
  titlePanel(
    a(href="http://waters.gu.se", target="_blank", img(src="1530704_waters_webheader2.jpg"))),
  
  navlistPanel(
    "Header A",
    tabPanel("Component 1"),
    tabPanel("Component 2"),
    "Header B",
    tabPanel("Component 3"),
    tabPanel("Component 4"),
    "-----",
    tabPanel("Component 5")
  )
))
