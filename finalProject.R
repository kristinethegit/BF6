#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#loading data

#OregonFires <- read.delim()
  

  
  
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
            p(""),
             )
    ),
    #causes of wildfires tab
    tabPanel("Causes of wildfires in Oregon",
             sidebarPanel(
               # Sidebar content for Tab 2
             ),
             mainPanel(
               # Main content for Tab 2
             )
    ),
    #human fires tab
    tabPanel("How frequent are human induced fires?",
              sidebarPanel(
                # Sidebar content for Tab 3
             ),
               mainPanel(
                   # Main content for Tab 3
             )
  ),
  #early vs later fires
  tabPanel("How have wildfires in earlier years differed from later years?",
           sidebarPanel(
             # Sidebar content for Tab 3
           ),
           mainPanel(
             # Main content for Tab 3
          )
)
)
)
#server
server <- function(input, output){
  
  #image for the first page
  output$Image <- renderImage({
    list(src = "image .jpeg", width = "600", height = "300")
  })
  
  
  
}


shinyApp(ui = ui, server = server)
