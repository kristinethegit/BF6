
library(shiny)
library(tidyverse)
library(ggplot2)
OregonFires <- read_delim("ODF_Fire_Occurrence_Data.csv")


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
               imageOutput("Image2"),
               h2(strong("Discoveries")),
               p("Based on the plots and tables that we have computed, 
               we can conclude that there is some correlation about wildfires in Oregon."),
               
               h3(strong("Data Quality")),
               p("In general, looking at the overall trend of the histograms in both causes of Wildfires
                 in Oregon which are the Human Activity and Lightning during the selected year(s) ;2015 to 2022
                 . These bars can appear in the trades more instantly which is more effective for the number of
                 wildfires in each year."),
               p("By using the Scatterplot, we are trying to show how much total Acres was burned by wildfires 
                 in Oregon by districts of Oregon. But if you look at the plot, most of the dots are located 
                 close to zero. This means a very small value. The reason is that the total case in dataset is
                 approximately 23491 such a lots. So the total acre's value becomes smaller; therefore; 
                 it doesn't show exactly relationship between the amount of fires and total acres. By the way, 
                 we're going to approximately both South Cascade and Southwest Oregon districts are the most burned 
                 by the Wildfire."),
              p("For the last plot, we selected the variables ; the number of wildfires and years 
                 to forecast the overall trend over the decade. We set 2000 year to 2010 year as the
                 early year(s) and then to 2022 year as the later year(s). Although there are potential
                 content to lead data bias in decade such as climate change or social growth, the dataset 
                 don't consider that aspect. So, it could a few aspects of data bias. "),
               
               
               h3(strong("Answering these questions and trend ")),
               p("• What are the causes of wildfires on the West Coast? "),
               p("• How frequently are fires started by humans? :"),
               p("Based on a dataset from 2015 to 2022, We can look for 
               that lightning is the main cause of wildfires in Oregon rather than
               human activities between these histograms. Due to human activities, 2018 
               is the most likely year to occur Wildfires in Oregon. Since then, the overall 
               trend has been gradually decreasing. However, we can still predict that at least
               100 wildfires are caused by human activities every year."),
               p("• How have wildfires in earlier years differed from later years? :"),
               p("Comparing the two line graphs, the overall number of wildfires has increased
                 in the later years compared to the early years. However, it is difficult to judge 
                 the stable increase or decrease correlation over years because there are no any 
                 obvious pattern. However, we can predict that the number of wildfires is rapidly
                 increasing and decreasing every five years."),
               
               h3(strong("Future Ideas ")),
               p("As the number of Wildfires due to the influence of lightning continues to increase,
                 we should think the Oregon state's  government and fire department should consider
                 ways to prevent forest fires from lightning. Compared to lightning, there are 
                 significantly fewer cases caused by human activities, and overall, that case number
                 is also decreasing. This shows that the marketings or campaigns of Wildfires 
                 prevention in Oregon strengthened citizenship and made them more conscious of
                 Wildfires."),
               p("In Oregon, South Cascade and Southwest Oregon are mainly areas where most wildfires occur.
                 Therefore, authorities need to keep a close eye on their fire department and more support 
                 from them to deal and care for fires quickly."),
               p("In addition, the overall number of wildfires has increased in recent years compared to the
                 last few years. Oregon State should continue to provide early education to their citizens 
                 on how to deal with wildfires. Although the predicted wildfires' cycle is to surge and decrease
                 repeatedly every five-year, the state governor should prepare the recovery costs to the victims
                 for financial problems caused by wildfires."),
            
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

