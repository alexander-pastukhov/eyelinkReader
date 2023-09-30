library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          checkboxInput(inputId = "show_fixations",
                        label = "Fixations",
                        value = TRUE),

          checkboxInput(inputId = "show_saccades",
                        label = "Saccades",
                        value = TRUE),

          selectInput(inputId = "trials",
                      label = "Trial",
                      choices = NULL,
                      multiple = TRUE,
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # initialize with number of trials
    trials <- sort(unique(gaze$saccades$trial))
    updateSelectInput(session, "trials", choices = trials)

    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      plot(gaze,
           trial = input$trials,
           show_fixations = input$show_fixations,
           show_saccades = input$show_saccades) +
        theme(legend.position = "bottom")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
