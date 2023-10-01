library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          fileInput("file", "Choose an EDF file", accept = ".edf"),
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

    data <- reactive({
      req(input$file)
      read_edf(input$file$datapath)
    })

    output$distPlot <- renderPlot({
      # getting samples for the relevant trials
      samples_df <-
        gaze$samples |>
        select(trial, gxR, gyR) |>
        drop_na()
      if (!is.null(input$trials)) samples_df <- filter(samples_df, trial %in% input$trials)


      # generate bins based on input$bins from ui.R
      plot(gaze,
           trial = input$trials,
           show_fixations = input$show_fixations,
           show_saccades = input$show_saccades) +

        # theme(legend.position = "bottom") +
        geom_density_2d_filled(data = samples_df,
                        aes(x =  gxR, y = gyR)) +
        scale_size(breaks = round(seq(min(gaze$fixations$duration), max(gaze$fixations$duration), length.out = 10))) +
        scale_colour_binned(breaks = round(seq(min(gaze$saccades$sttime_rel), max(gaze$saccades$sttime_rel), length.out = 10)))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
