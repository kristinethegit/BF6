library(shiny)
library(tidyverse)
library(ggplot2)
OregonFires <- read_delim("ODF_Fire_Occurrence_Data.csv")

# create sample data
causes_data <- data.frame(
  year = c(rep(2000:2022, 2)),
  cause = c(rep("Lightning", 23), rep("Human Activity", 23)),
  count = c(sample(100:1000, 23), sample(50:500, 23))
)

ui <- fluidPage(
  
  # Title   
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
               p("This dataframe portrays the number of Oregon wildfires over the years. This will help the state government organize and put the wildfires under control now knowing the cause and location.")
             )
    ),
    
    # Causes of wildfires tab
    tabPanel("Causes of Wildfires in Oregon",
             sidebarPanel(
               # Sidebar content for Causes of wildfires tab
               h3("Select years to compare:"),
               sliderInput("year_range", "Year Range", min = 2000, max = 2022, value = c(2015, 2022), step = 1)
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
    ),
    tabPanel("Conclusion",
             mainPanel("• The main cause of Oregon wildfires is mostly from lightning, however, humans cause half of what lightning causes.",
                       imageOutput("Image2")
             )
    )
  )
)


server <- function(input, output){
  
  # Image for the first page
  output$Image <- renderImage({
    list(src = "image .jpeg", width = "600", height = "300")
  })
  
  # Causes of wildfires plot
  causes_plot_data <- reactive({
    filtered_data <- causes_data %>% 
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    ggplot(data = filtered_data, aes(x = year, y = count, fill = cause)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Causes of wildfires in Oregon",
           x = "Year",
           y = "Number of wildfires") +
      scale_fill_manual(values = c("#003f5c", "#ffa600")) # custom fill colors
  })
  
  output$causes_plot <- renderPlot({
    causes_plot_data()
  })
  

  # Image 2 for the conclusion page
  output$Image2 <- renderImage({
    list(src = "image2 .jpg", width = "600", height = "300")
  })
}

shinyApp(ui = ui, server = server)

