library(shiny)
library(tidyverse)
library(ggplot2)

# create sample data
causes <- c("Lightning", "Human Activity")
counts <- c(500, 250)

ui <- fluidPage(
  
  #Title   
  titlePanel("Wildfires in Oregon"),
  
  # Tabs
  tabsetPanel(
    # Introduction tab with short information about the purpose of this app and dataset
    tabPanel("Introduction",
             mainPanel(
               imageOutput("Image"),
               h2(strong("Project Overview")),
               p("This report will provide a broad summary of wildfires in the state of Oregon and its causes."),
               h2(strong("Data Set")),
               p("This app uses wild fire data from DATA.GOV. The data set from the Oregon Department of Forestry (ODF) and it spans from 2000 through 2022."),
               a("Access link here", href='https://catalog.data.gov/dataset/odf-fire-occurrence-data-2000-2022'),
               h2(strong("Audience")),
               p("")
             )
    ),
    
    # Causes of wildfires tab
    tabPanel("Causes of wildfires in Oregon",
             sidebarPanel(
             ),
             mainPanel(
               plotOutput("causes_plot")
             )
    ),
    
    # Human fires tab
    tabPanel("How frequent are human induced fires?",
             sidebarPanel(
             ),
             mainPanel(
             )
    ),
    
    # Early vs later fires tab
    tabPanel("How have wildfires in earlier years differed from later years?",
             sidebarPanel(
             ),
             mainPanel(
             )
    )
  )
)

server <- function(input, output){
  
  # Image for the first page
  output$Image <- renderImage({
    list(src = "image.jpeg", width = "600", height = "300")
  })
  
  # Causes of wildfires plot (Kristine Chang)
  output$causes_plot <- renderPlot({
    ggplot(data = data.frame(causes, counts), aes(x = causes, y = counts, fill = causes)) + 
      geom_bar(stat = "identity") +
      labs(title = "Causes of wildfires in Oregon",
           x = "Cause",
           y = "Number of wildfires")
  })
  
}

shinyApp(ui = ui, server = server)

