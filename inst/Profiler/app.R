#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(clivartools)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("CLIVAR Profile Plotter"),

   # Controls
   sidebarLayout(
      sidebarPanel(
        selectInput("cruiseSelect",
                    label = h3("Cruise"),"I08S"),
        selectInput("stationSelect",
                    label = h3("Station"),"42")
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
     observe({

    #Get and order available cruises, most recent first
    cruises <- getCruises()

    # Change values for input$cruiseSelect
    updateSelectInput(session, "cruiseSelect",
                      choices = cruises)

  })

     observe({

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

