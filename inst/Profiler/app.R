#
# CLIVAR Profile Plotter
#
# Pulls cruises and stations and plots

library(shiny)
library(dplyr)
library(clivartools)

    #Get and order available cruises, most recent first
    cruises <- getCruises()


# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("CLIVAR Profile Plotter"),

   # Controls
   sidebarLayout(
      sidebarPanel(
        selectInput("cruiseSelect",
                    label = h3("Cruise"), choices = cruises ),
        #uiOutput("stationSelect")
        selectInput("stationSelect",
                    label = h3("Station"), 42)
      ),

      # Plot and data table
      mainPanel(
        plotOutput("profilePlot"),
        # dataTableOutput(outputId="table"),
        tableOutput("profileTable")
      )
   )
)

# Define server logic
server <- function(session, input, output) {

   # Produce station list based on cruise selection
   #output$stationSelect<- renderUI({
   #  validate(need(input$cruiseSelect, message = FALSE))
   #  stations <- getStations(input$cruiseSelect)
   #  selectInput("stations", "Choose station", stations)
   #})

    observe({

      # If no cruise is selected, don't do anything
      validate(need(input$cruiseSelect, message = FALSE))

      #Get and order available stations within cruise
      stations <- getStations(input$cruiseSelect)

      # Change values for input$stationSelect
      updateSelectInput(session, "stationSelect",
                        choices = stations)
    })

   profile <- reactive({
     getProfile(input$cruiseSelect, input$stationSelect)
   })

   output$profilePlot <- renderPlot({
     plotProfile(profile())
   })

   output$profileTable <- renderTable({
     select(profile(), whpid, station, cast, depth_corr, f_modern, f_ext_error)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

