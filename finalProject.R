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
  
  #Title   
  titlePanel("Wildfires in Oregon"),
  
  # Tabs
  tabsetPanel(
    # Introduction tab with short information about the purpose of this app and dataset
    tabPanel("Introduction",
             mainPanel(
               #Add image to the intro page 
               imageOutput("Image"),
               #Add description to the intro page
               h2(strong("Project Overview")),
               p("Wildfires have so much impact on our human lives that why people can describe its 
               as one big national disaster. These wildfires take away many people's homes and lives in
               an instant, but just one case allows data analysts to gather a lot of information about.  
               Data obtained through a wildfire can predict and prepare various information such as the direction, 
               location, and reason of the fire in the future. We're going to use data on wildfires in Oregon among 
              many states of the United States. In addition, we will explain why forest fires occur mainly and analyze 
                 how  people affect wildfires."),
               h3(strong("Audience")),
               p("We consider anyone in general who is interested in the wildfire. 
                 However, our main target audience would be the people who live in Oregon firefighter departments 
                 both national and Oregon, scientists, researchers and land managers.
                 The scholar field can study and predict by using the data of wildfire in diverse aspects."),
               h3(strong("Data Set")),
               p("We will be working with the wildfires in Oregon dataset made by Oregon Department of Forestry (ODF)
                 at the government website(DATA.GOV.) This dataset includes data from 2000 to 2022 and records 23491 
                 cases of wildfired. As one case of  wildfire is happen in, there is a lot of information that can be
                 recorded such as where it occurred; DistrictName, which unit it was; UnitName, how size of it; Size_class, 
                 location and time data, and etc. These dataset can be used in various fields and aspects as well as simply 
                 answering the questions that we are currently focusing on for our target customer. 
                 There are many information variables to record based on the different cases of wildfires in Oregon; 
                 however, We narrowed down the datasat and only utilized the following cariabels: Fire Year, CauseBy, and HumanOrLightning."),
               a("Access the Dataset what we used", href='https://catalog.data.gov/dataset/odf-fire-occurrence-data-2000-2022'),
               h3(strong("Questions")),
               p("Some questions we focused on are : "),
               p("• What are the causes of wildfires  on the West Coast?"),
               p("• How frequently are fires started by humans?"),
               p("• How have wildfires in earlier years differed from later years?"),
               h3(strong("Creators")),
               p("• Kristine Chang"),
               p("• Simran Bhatti"),
               p("• Bella Kwon"),
               p("• Leire Gangoiti")
              
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

