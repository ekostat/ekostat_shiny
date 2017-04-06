

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
    c("xxxx","yyyy","zzzz")
  })
  waterbody_list <- reactive({
    c("Inre Bråviken","Inre Slätbaken","Kaggebofjärden","Yttre Bråviken","Trännöfjärden","Lindödjupet","Kärrfjärden","Ryssbysjön","Stensjön","Vässledasjön","Krön","Lilla Nätaren","Rocksjön","Munksjön","Ören 1","Ören 2","Stora Nätaren","Försjön","Landsjön","Glimmingen","Ralången","Noen","Allgjuttern","Bunn","Säbysjön","Åsunden","Sommen-Västra","Sommen-Östra","Roxen","Vättern - Alsen","Vättern - Storvättern","Vättern - Kärrafjärden","Glan","Fagertärn","Bleklången","Bosjön","Anten","Avern","Skeppsjön","Boren","Bönnern","Hargsjön","Hövern","Asplången","Kisasjön","Nimmern","Drögen")
  })
  
  output$selectWaterDistrict <- renderUI({
    tagList(
      selectInput("district", "Select Water District", choices=district_list(), selected="ALL")
    )})
  
  output$selectWaterBodies <- renderUI({
    tagList(
      selectInput("waterbody", "Select Waterbody(s)", choices=waterbody_list(), selected="ALL", multiple=TRUE)
    )})
  
})

