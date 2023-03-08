
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
#wildfires per year for line plot
fire_counts <- OregonFires%>% 
  group_by(FireYear) %>% 
  summarise(FIRE_COUNT = n())

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
    tabPanel("Oregon Fires",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "acres",
                   "Select a range of estimated total acres burned:",
                   min = 0,
                   max = max(OregonFires$EstTotalAcres, na.rm = TRUE),
                   value = c(0, max(OregonFires$EstTotalAcres, na.rm = TRUE)),
                   sep = "",
                   pre = "Acres: "
                 ),
                 checkboxGroupInput(
                   "districts",
                   "Select districts:",
                   choices = unique(OregonFires$DistrictName),
                   selected = unique(OregonFires$DistrictName)
                 )
               ),
               mainPanel(
                 plotOutput("scatterplot")
               )
             )),
    
    # Early vs later fires tab
    tabPanel("How have wildfires in earlier years differed from later years?",
             sidebarPanel(h3("Earlier Years vs. Later Years:"),
                          #radio buttons
                          radioButtons("earlylater", "Select Early Years or Later Years",
                                       choices = c( "Early Years", "Later Years"),
                                       selected = "Early Years")
             ),
             mainPanel(plotOutput("line_plot")
             )  
    ), 
    
       tabPanel("Conclusion",
             mainPanel(
               p("• The main cause of Oregon wildfires is mostly from lightning, however, humans cause half of what lightning causes."),
                p("• Earlier years (200-2010) of the Oregon wildfires had a significant amount being 2006 with 1300 occurances of fires,
                       however, after a steep dip, the wild fires shot back up again in the later years (2011-2022)"),
                       imageOutput("Image2")
             )
    )
  ))


# Server
server <- function(input, output){
  
  # Image for the first page
  output$Image <- renderImage({
    list(src = "image .jpeg", width = "600", height = "300")
  })
  
  # Causes of wildfires plot
  causes_data <- data.frame(
    year = 2010:2020,
    cause = c("Human", "Lightning"),
    count = c(546, 1171)
  )
  
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
  
  # Create a reactive filter based on the user's radio button selection for line plot
  EarlyvsLater <- reactive({
    if (input$earlylater == "Early Years") {
      filter(fire_counts, FireYear >= 2000 & FireYear <= 2010)
    } else {
      filter(fire_counts, FireYear >= 2011 & FireYear <= 2022)
    }
  })
  #output for line plot
  output$line_plot <- renderPlot({
    ggplot(EarlyvsLater(), aes(x = FireYear, y = FIRE_COUNT)) + 
      geom_line() +
      xlab("Year") +
      ylab("Number of Fires") +
      ggtitle("Fire Occurrence Data") +
      scale_x_continuous(breaks = seq(2000, 2022)) 
    
  })
  
  # Image 2 for the conclusion page
  output$Image2 <- renderImage({
    list(src = "image2 .jpg", width = "600", height = "300")
  })
  
  output$scatterplot <- renderPlot({
    
    # Filter the data based on the selected estimated total acres burned and districts
    filter_data <- OregonFires %>%
      filter(
        EstTotalAcres >= input$acres[1] & EstTotalAcres <= input$acres[2],
        DistrictName %in% input$districts
      )
    
    # Group the data by EstTotalAcres, FireYear, and DistrictName, and count the number of rows in each group
    grouped_data <- filter_data %>%
      group_by(EstTotalAcres, FireYear, DistrictName) %>%
      summarise(count = n())
    
    # Create the scatterplot
    ggplot(
      data = grouped_data,
      aes(x = EstTotalAcres, y = count, color = DistrictName, group = FireYear)
    ) +
      geom_point() +
      labs(x = "Amount of Fires", y = "Total Acres", color = "Fire Year") +
      scale_color_discrete(name = "District") +
      guides(color = guide_legend(ncol = 1)) +
      theme(legend.position = "right")
  })
  
}

shinyApp(ui = ui, server = server)

