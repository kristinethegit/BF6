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
  
#Title   
titlePanel("Wildfires in Oregon")
  
  
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Overview",
             sidebarPanel(
               # Sidebar content for Tab 1
               
             ),
             mainPanel(
               # Main content for Tab 1
             )
    ),
    tabPanel("Tab 2",
             sidebarPanel(
               # Sidebar content for Tab 2
             ),
             mainPanel(
               # Main content for Tab 2
             )
    )
  )
)
#server
server <- function(input, output){
  
  
}


shinyApp(ui = ui, server = server)
