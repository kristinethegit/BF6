#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Oregon Wildfires"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Overview", plotOutput("plot"), 
                     tabPanelBody("panel2", "Panel 2 content")),
            tabPanel("Table", tableOutput("table"), 
                     tabPanelBody("panel3", "Panel 3 content")),
            tabPanel("Page", plotOutput("plot"),
                     tabPanelBody("panel4", "Panel 4 conent")),
            tabPanel("Page", plotOutput("plot"),
                     tabPanelBody("panel5", "Panel 5 conent")),
            tabPanel("Conclusion", textOutput("Conclusion"), 
                     tabPanelBody("panel1", "Panel 1 content"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)