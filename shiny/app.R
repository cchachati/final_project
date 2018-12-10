
library(tidyverse)
library(stringr)
library(rebus)
library(lubridate)
library(leaflet)
library(htmltools)
library(leaflet.extras)
library(scales)
library(plotly)
library(stringi)
library(shinythemes)
library(vistime)
library(shiny)


# Read the cleaned data and death locations from the data folder.
# The data was cleaned in a separate r markdown file to keep
# this shiny app concise and neat. 

data <- read_rds("data/data_clean.rds")
death_location <- read_rds("data/death_location.rds")

# Read the blocs of texts that describe all the plots, graphics and timeline,
# and provide instructions for manipulating the different graphics.
# These chunks of text were written in a separate r markdown file and saved
# as rds files to keep this shiny app as neat and concse as possible. 

instructions_timeline <- read_rds("outputs/instructions_timeline.rds")
instructions_lineplot_a <- read_rds("outputs/instructions_lineplot_a.rds")
instructions_lineplot_b <- read_rds("outputs/instructions_lineplot_b.rds")
description_timeline <- read_rds("outputs/description_timeline.rds")
description_actors <- read_rds("outputs/description_actors.rds")
description_lineplot_a <- read_rds("outputs/description_lineplot_a.rds")
description_lineplot_b1 <- read_rds("outputs/description_lineplot_b1.rds")
description_lineplot_b2 <- read_rds("outputs/description_lineplot_b2.rds")
description_barplot <- read_rds("outputs/description_barplot.rds")

# Define the choices that the user will select.
# These choices will be used in the UI for the side panels. 

choices1 <- c("Cause of Death" = "Deathcause",
              "Adult/Child" = "Adult_Child",
              "Affiliation" = "Actor", "Status" = "Status",
              "Gender" = "Gender", "Province" = "Province")
choices2 <- c("Cause of Death" = "Deathcause",
              "Adult/Child" = "Adult_Child",
              "Affiliation" = "Actor", "Status" = "Status",
              "Gender" = "Gender")

## Since the application is focused on observing deaths over time and in
## in different provinces, I thought it would be useful to include 
## a timeline of the main events during the Syrian Civil War that the 
## user can refer back to if they would like more context on the data 
## they are presented with. 

## I initially created the timeline in a seperate rmd file and read it in
## as an rds file. 
## However, running into errors publishing the app. 
## The only way it worked was to directly create the timeline
## on the shiny app.

## To do that, I first had to create a data frame containing the events 
## and dates that I wanted to display on the application. 
## Since there were not many events, I decided to create the data frame
## directly on r instead of on excel. 
## From among the various timeline packages on r, I chose to use vistime 
## because it provided the option of removing the label or the description
## of the event, and only allowing it to appear if the user hovered
## over the data. 
## Given that the desriptions for some of my events were long, 
## I found that to be most appropriate. 

## The vistime package requires that I specify the start and end date 
## of each event, as well as the event itself. 
## Hence, I created the variables: content, start and end. 
## The end date was NA for all the events since the events
## were specific to a certain day rather than a time period. 

data_timeline <- data.frame(
  content = c("Protests in Daraa", 
              "US freezes Syrian assets",
              "Assad top officials killed in Damascus. Fighting in Aleppo and Damascus intensifies",
              "Gas attack in north Syria. 26 dead",
              "Hizbullah joins the fight in support of Assad", 
              "Chemical attack in Damascus suburbs. Hundreds killed", 
              "Increase in international humanitarian aid", 
              "Assad reelected",
              "ISIS declares Raqqa as the caliphate capital",
              "US led campaign against ISIS begins", 
              "Nusra Front controls Idlib",
              "Russia campaign in support of Assad begins",
              "200 airstrikes in Aleppo in one weekend", 
              "Assad takes eastern Aleppo", 
              "Nerve gas attack in Idlib. US responds with missiles", 
              "Assad takes Homs", 
              "ISIS loses Raqqa", 
              "Turkey attacks Kurds in northern Syria", 
              "Toxic gas in rebel-held Damascus suburbs. US, UK & France attack",
              "Assad gains full control in Damascus suburbs and southern Syria",
              "30 killed in US-led airstrike on Deir ez-Zor"),
  start = c("2011/03/01", "2011/08/18", 
            "2012/07/18", "2013/03/19",
            "2013/05/01", "2013/08/21",
            "2014/02/01", "2014/06/03",
            "2014/06/30", "2014/09/23",
            "2015/03/28", "2015/09/30",
            "2016/09/01", "2016/12/01",
            "2017/04/04", "2017/05/01",
            "2017/10/01", "2018/01/20",
            "2018/04/01", "2018/05/21",
            "2018/07/12"),
  end = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  
  ## Since I chose to have 21 events displayed on the timeline, 
  ## I wanted to distinguish them into categories. 
  ## Hence, I decided to group the events based on whether they pertained to
  ## domestic fighting, international politics, or an international intervention. 
  ## In this case, domestic fighting referred to all events that were related to
  ## fighting between the Assad regime and domestic opposition.
  ## International intervention related to all military campaigns 
  ## or single bombardments that were executed by international actors. 
  ## International politics referred to diplomatic negotiations or policies
  ## between the Assad regime and international actors 
  ## or among international actors on issues relating to Syria.
  ## I colored the categories accordingly. 
  
  group = c("Domestic Fighting", "International Politics",
            "Domestic Fighting", "Domestic Fighting", 
            "International Military Involvement", "Domestic Fighting",
            "International Politics", "Domestic Fighting",
            "Domestic Fighting", "International Military Involvement",
            "Domestic Fighting", "International Military Involvement", 
            "Domestic Fighting", "Domestic Fighting", 
            "Domestic Fighting", "Domestic Fighting",
            "Domestic Fighting", "International Military Involvement",
            "Domestic Fighting", "Domestic Fighting", 
            "International Military Involvement"),
  style = c("red", "blue", 
            "red", "red",
            "green", "red", 
            "blue", "red",
            "red", "green",
            "red", "green",
            "red", "red",
            "red", "red",
            "red", "green",
            "red", "red",
            "green"))

## As described above, I specified the arguments for the vistime function
## based on the variables I had created. 
## I decided not to display the labels since the descriptions 
## of the events were lengthy and it would thus not be aesthetically pleasing. 
## Since the code for creating the timeline is long, I creating an rds file
## that I will then read into my shiny application. 

timeline <- vistime(data_timeline, events = "content", groups = "group",
                    start = "start", end = "end", colors = "style", 
                    showLabels = FALSE)

# I was initially debating between using a dark or light background
# I eventually decided to use the "darkly" theme because it highlights
# the tabs that are currently open.
# Since I have tabs within tabs, I found it useful. 

ui <- fluidPage(theme = shinytheme("darkly"), 
                
                titlePanel(h1("The Syrian Civil War"),
                           windowTitle = "The Syrian Civil War"),
                titlePanel(h4("Cayanne Chachati")),
                
                # I chose to use the side bar and main panel layout because
                # for almost all my tabs, I give the user the option to select
                # certain variables, or I give the user instructions for how
                # to manipulate the graphics. 
                # Thus, I found that splitting the tabs into side and main panels 
                # would be most effective. 
                
                
                # In this code chunk, I created a tabsetPanel within another one.
                # This will allow the user to select between exploring
                # the main actors or a timeline of events.
                # I found this to be more visually appealing than having 
                # both options in one tab. 
                # The information in my side panel does not need to be split
                # into different wells or boxes. 
                # However, using well panel in this tab will keep 
                # the layout consistent since I use it throughout.
                # Instead of writing out a bloc of text explaining how to 
                # manipulate the timeline, I read it in from a rds file I 
                # previously created. 
                
                tabsetPanel(
                  tabPanel(
                    title = "Background on the Civil War",
                    tabsetPanel(
                      tabPanel(
                        title = "Timeline",
                        sidebarPanel(width = 3,
                                     wellPanel(
                                       h3("Manipulating the Timeline"),
                                       h5(instructions_timeline)
                                     ) 
                        ),
                        
                        # While the data itself is very informative, the user
                        # would learn a lot more about the war by knowing the
                        # context or main events that are causing such a high 
                        # death toll. 
                        # I thus decided to create a timeline.
                        # I wanted the timeline to be reactive, and for the
                        # user to be able to toggle between the different events.
                        # Thus, I decided against importing an image.
                        # I chose to use plotly because in the timeline,
                        # the points, representing the different events, overlap.
                        # Plotly will allow the user to zoom in thus displaying 
                        # each point independently and clearly. 
                        
                        mainPanel( 
                          titlePanel(h3("Timeline of the Syrian Civil War")),
                          plotlyOutput(outputId = "timeline",
                                       width = 900, height = 400),
                          br(),
                          h5(description_timeline), 
                          hr(),
                          helpText("Data from", tags$a("CNN", 
                                                       href = "https://www.cnn.com/2013/08/27/world/meast/syria-civil-war-fast-facts/index.html"), 
                                   "and", tags$a("AP News", 
                                                 href = "https://www.apnews.com/792a0bd7dd6a4006a78287f170165408"))
                        )
                      ),
                      
                      # To further understand the progression of the civil war,
                      # and the timeline, I thought it would be useful to
                      # display a diagram of the different actors. 
                      # Since creating the diagram on r is beyond the
                      # scope of my skills, I decided to import an image,
                      # and displayed the link of the source. 
                      
                      tabPanel(
                        title = "Primary Actors",
                        titlePanel(h3("The Primary Actors of the Syrian Civil War")),
                        br(),
                        HTML('<center><img src="actors.jpg" height = 350 width = 500 ></center>'),
                        br(),
                        h5(description_actors),
                        hr(),
                        helpText("Diagram from", tags$a("Business Insider", 
                                                        href = "https://www.businessinsider.com/who-is-involved-in-the-war-in-syria-2013-10"))
                      )
                    )
                  ),
                  
                  # To understand the progression of the Syrian civil war 
                  # and its periods of intense conflict, it is valuable
                  # to track the number of deaths over the months.
                  # I wanted the user to choose between seeing the total
                  # number of deaths over time or the total number of deaths
                  # over time while filtering for certain variables.
                  # Hence, I decided to create two tabs within
                  # the general tab of "Exploring Deaths Over Time".
                  
                  # In the first tab, I create a lineplot that shows
                  # the deaths over time. 
                  # I used plotly because of the flexibility of its axes.
                  # Since deaths are calculated per month, it would be 
                  # messy to display all the months and years.
                  # However, the user would also loose valuable information
                  # by only seeing the year.
                  # Plotly will allow the user to zoom in and out and
                  # accordingly to increase the specificity of the date. 
                  # As previously mentionned, I read in the instructions
                  # and descriptions of the lineplot from an rds file.
                  
                  tabPanel(
                    title = "Explore Deaths Over Time",
                    tabsetPanel(
                      tabPanel(
                        title = "Total Deaths Over Time",
                        sidebarPanel(width = 3,
                                     wellPanel(
                                       h3("Manipulating the Graph"),
                                       instructions_lineplot_a
                                     ) 
                        ),
                        mainPanel(
                          h3("Line Plot"),
                          br(),
                          plotlyOutput(outputId = "lineplot_a",
                                       width = 700, height = 450),
                          br(),
                          h5(description_lineplot_a),
                          hr(),
                          helpText("Data from", tags$a("Violations Documentation Center in Syria", 
                                                       href = "https://data.world/polymathic/casualties-of-the-syrian-civil-war/workspace/project-summary"))
                        )
                      ),
                      
                      # To get a more comprehensive understanding of the 
                      # nature of the Syrian civil war, its primary actors, 
                      # and victims, as well as its progression over time, 
                      # it is useful to compare the number of deaths over time
                      # across certain demographic, geographic and political
                      # factors.
                      
                      # The raw data already had information about the gender,
                      # status, province, deathcause, age and affiliation of 
                      # each person. 
                      # Hence, I gave the user the option of selecting between
                      # the different variables, as well as the option
                      # of exploring the data in more detail in the form of
                      # a table. 
                      # Plotly was used for the options stated above.
                      # In addition, Plotly allows the user to see the distinct
                      # observations for each variable by double clicking on 
                      # that observation in the legend. 
                      # Especially for a variable like province, that has many
                      # distinct observation, this option allows the user to 
                      # see the change in each observation over time. 
                      
                      tabPanel(
                        title = "Filtered Deaths Over Time",
                        sidebarPanel(width = 3, 
                                     wellPanel(
                                       h3("Plotting"), br(),
                                       selectInput(inputId = "color", 
                                                   label = "Choose a variable:",
                                                   choices = choices1,
                                                   selected = "Gender"),
                                       h5("Status refers to whether the deceased was a civilian or combatant"),
                                       h5("Affiliation refers to the group that the deceased supported 
                                          regardless of their status"),
                                       checkboxInput(inputId = "table", 
                                                     label = "Display Table of Results")
                                     ),
                                     wellPanel(
                                       h3("Manipulating the Graph"),
                                       instructions_lineplot_b
                                     )
                        ),
                        
                        # I used a conditionalPanel because, as opposed to 
                        # a reactive variable that only makes the display
                        # of the table conditional, the conditional panel will
                        # make the display of the explanations, the title and 
                        # the table all conditional. 
                        # This is especially useful because I am creating a 
                        # title independent of the table's caption. 
                        # It would be a lot more complicated to make each 
                        # text chunk into a separate reactive text output. 
                        
                        mainPanel(length = 10,
                                  h3("Line Plot"),    
                                  br(), 
                                  plotlyOutput(outputId = "lineplot_b", 
                                               width = 975, height = 500),
                                  br(),
                                  h5(description_lineplot_b1),
                                  h5(uiOutput(outputId = "subtitle_lineplot_b")),
                                  h5(description_lineplot_b2),
                                  br(), 
                                  conditionalPanel(condition = "input.table == true", 
                                                   hr(),
                                                   h3("Data Table")),
                                  br(),
                                  conditionalPanel(condition = "input.table == true",
                                                   h5(textOutput("description_table"))),
                                  br(),
                                  DT::dataTableOutput(outputId = "table"),
                                  hr(),
                                  helpText("Data from", 
                                           tags$a("Violations Documentation Center in Syria", 
                                                  href = "https://data.world/polymathic/casualties-of-the-syrian-civil-war/workspace/project-summary"))
                        )
                      )
                    )
                  ),
                  
                  # To gain a more holistic understanding of the
                  # regions and populations that were most affected
                  # by the war, it is valuable to analyze the total number
                  # of deaths in different provinces. 
                  # Hence, I created a bar plot that allows for a 
                  # comparative analysis of the total number of deaths across 
                  # the provinces, and across different demographic
                  # and political factors.
                  
                  # I wanted the user to contextualize the conflict
                  # based on the location of the different provinces.
                  # Hence, I created two tabs: one for the graph and the 
                  # other for a map. 
                  
                  # Since the user had to manipulate the axes and colors 
                  # of the graph, as well as subset the data by province,
                  # I divided the side panel into two wells or boxes. 
                  # I limited the user's selection to only four provinces
                  # because the barplot is facetted by province.
                  # If the user were to select all the provinces, it would
                  # be difficult to interpret the different facets. 
                  
                  tabPanel(
                    title = "Explore Deaths by Province",
                    sidebarPanel(width = 3, height = 15,
                                 wellPanel(
                                   h3("Subsetting"),
                                   br(),
                                   selectizeInput(inputId = "province", 
                                                  label = "Select up to Four Provinces:", 
                                                  choices = c(levels(death_location$province)),
                                                  multiple = TRUE,
                                                  selected = c("Aleppo", "Damascus", "Lattakia", "Homs"),
                                                  options = list(maxItems = 4))
                                 ),
                                 wellPanel(
                                   h3("Plotting"),
                                   br(),
                                   
                                   # I gave the user the option of selecting both
                                   # the axes and the color. 
                                   # In this case, the color allows the user to 
                                   # compare deaths across two different variables
                                   # (the variable selected for the axis and the
                                   # variable selected for the color). 
                                   
                                   selectInput(inputId = "x", 
                                               label = "Y-axis:",
                                               choices = choices2,
                                               selected = "Status"),
                                   selectInput(inputId = "y", 
                                               label = "Color by:",
                                               choices = choices2,
                                               selected = "Gender"),
                                   h5("Status refers to whether the deceased was a civilian or combatant"),
                                   h5("Affiliation refers to the group that the deceased supported 
                                      regardless of their status")
                                 )
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel(
                          title = "Data",
                          h3("Barplot"),  
                          br(),
                          plotOutput(outputId = "barplot",
                                     width = "900px", height = "500px"),
                          br(),
                          h5(description_barplot),
                          hr(),
                          helpText("Data from", 
                                   tags$a("Violations Documentation Center in Syria", 
                                          href = "https://data.world/polymathic/casualties-of-the-syrian-civil-war/workspace/project-summary"))
                        ),
                        tabPanel(
                          title = "Map",
                          h3("Map of Syria and its Provinces"), 
                          br(), 
                          leafletOutput(outputId = "my_map2", 
                                        width = "800px", height = "500px")
                        )
                      )
                    )
                  )
                )
)

server <- function(input, output) {
  
  # As previously described, for aesthetic purposes, I read in the timeline
  # from an rds file, and chose to use plotly. 
  
  output$timeline <- renderPlotly({
    timeline
  })
  
  # The raw data indicated the gender, status etc of each person that
  # had been killed, but did not calculate any statistics.
  # Hence, in order to create a display of the number of deaths
  # over time, I would have to calculate the sum of deaths for a given
  # time period. 
  # While cleaning the data, I had regrouped and recoded all the dates
  # into months. I thus only needed to find the sum per month. 
  
  lineplot <- 
    total_overtime <-  
    data %>%
    group_by(Deathdate) %>%
    count() %>%
    mutate(`Number of Deaths by Month` = n)
  
  # The following code chunk is to produce lineplot_a : 
  # "Total Deaths Over Time": 
  # In this code chunk I determine the style (font and size) that will be
  # used consistently in the rest of the application. 
  # I tried to manually enter the the breaks for the y axis, and to include 
  # a comma in the death counts. 
  # However, because dynamicTicks = TRUE, plotly automtaically determines the
  # breaks and does not allow me to add commas.
  
  output$lineplot_a <- renderPlotly({
    lineplot <- 
      ggplot(total_overtime, aes(x = Deathdate, y = `Number of Deaths by Month`)) + 
      geom_line() + 
      theme_minimal(base_size = 11, base_line_size = 1) + 
      theme(title = element_text(size = 12, family = "Arial", face = "bold"),
            axis.title = element_text(size = 11)) +
      labs(title = "Total Deaths Over Time ",
           x = "Year",
           y = "Number of Deaths by Month",
           align = "c")
    
    ggplotly(p = lineplot, width = 800, height = 450, dynamicTicks = TRUE) 
  })
  
  # The following code chunk is to produce lineplot_b: 
  # "Total Deaths Over Time by XXX" 
  
  output$lineplot_b <- renderPlotly({
    
    # In this code chunk, instead of just grouping by deathdate, I also
    # need to group by the variable that the user selects. 
    # This can only be done using the group_by_ function. 
    
    data_time <- 
      data %>%
      group_by_(.dots = input$color) %>%
      count(Deathdate) %>%
      mutate(`Number of Deaths by Month` = n)
    
    # Plotly does not have built in function to style its legend
    # Thus, in order to maintain the same style throughout the application, 
    # I manually coded the style for the legend.
    
    l <- list(
      font = list(
        family = "Arial",
        size = 12,
        color = "black"))
    
    # Aes() calls on non reactive variables whereas aes_string() is for 
    # reactive variables. 
    # Since my plot contains both, I had to seperate them and call
    # on both. 
    # I had to call on the function names and which in order to print the 
    # label of the variable instead of the variable name. 
    # This is done throughout the app for all legend, axis and title names. 
    
    lineplot_b <- 
      ggplot(data_time, aes(x = Deathdate, y = `Number of Deaths by Month`)) +
      geom_line(aes_string(color = input$color)) + 
      theme_minimal(base_size = 11, base_line_size = 1) + 
      theme(title = element_text(size = 12, family = "Arial", face = "bold"),
            axis.title = element_text(size = 11),
            legend.title = element_text(size = 11, face = "bold")) +
      ggtitle(paste("Total Deaths Over Time by", names(choices1[which(choices1 == input$color)]))) +
      labs(x = "Year",
           y = "Number of Deaths by Month",
           color = names(choices1[which(choices1 == input$color)]),
           align = "c")
    
    # Making the ticks dynamic is important because it allows the user to 
    # manipulate the specificity of the date based on how much they zoom in.
    
    ggplotly(p = lineplot_b, width = 900, height = 500, dynamicTicks = TRUE) %>%
      layout(legend = l)
  })
  
  # The following code chunk is to produce the subtitle for lineplot_b: 
  
  output$subtitle_lineplot_b <- renderUI({
    
    #  create a function that outputs a different
    # subtitle depending on the variable that the user selects. 
    # Each subtitle describes the key findings of the line plot. 
    # This will allow the user to interpret the line plot more easily. 
    
    if (input$color == "Gender") {
      HTML(paste(em("From the graph above, it is evident that across
                    every time point, more males have died than females. 
                    In addition, male death reached a maximum point between
                    2012 and 2013")))
    } else {
      if (input$color == "Affiliation") {
        HTML(paste(em("From the graph above, it is evident that from
                      2011 to 2016, the highest deaths were among non
                      identified actors. In addition, from 2016 to 2018
                      deaths among the Assad regime increased.")))
      } else {
        if (input$color == "Adult_Child") {
          HTML(paste(em("From the graph above, it is evident that across 
                        every time point, more adults have died than children. 
                        In addition, adult death reached a maximum point
                        between 2012 and 2013.")))
        } else {
          if (input$color == "Deathcause") {
            HTML(paste(em("From the graph above, it is evident that shelling
                          and shooting are leading causes of death from
                          2011 to 2018. In addition, the number of field 
                          executions increased between 2012 and 2013")))
          } else {
            if (input$color == "Status") {
              HTML(paste(em("From the graph above, it is evident that across
                            every time point, more civilians have died than
                            non-civilians. In addition, civilian death reached
                            a maximum point between 2012 and 2013.")))
            }else {
              HTML(paste(em("From the graph above, it is evident that in the
                            early years of the war the highest number of
                            deaths was in Homs. Then from 2012 to 2018, the
                            highest fatalities were  in Aleppo and the Damascus Suburbs.")))
            }
            }
            }
            }
          }
        })
  
  # The following code chunk is to produce the description or subtitle 
  # for the table that shows the detailed data on deaths over time: 
  
  output$description_table <- renderText({
    paste("The data table below shows the number of individuals that
          were killed every month from March 2011 to September 2018
          by", input$color, ".")
  })
  
  # As previously described, the raw data does not contain any summary statistis
  # Hence, in this code chunk, I create a reactive data table that will 
  # calculate the sum of deaths based on the variables that the user selects. 
  # In addition, the table is only displayed if the user chooses the option
  # to display the table. 
  
  table <- reactive({
    data_table <- 
      data %>%
      group_by_(.dots = input$color) %>%
      count(Deathdate) %>%
      mutate(`Death Date` = Deathdate,
             `Number of Deaths by Month` = n) %>%
      select(-2, -3)
    
    if (input$table) {
      data_table 
    }
  })
  
  # I chose to use renderDataTable as opposed to renderTable because the former
  # allows the user to search the table for specific observations.
  # Since my data set is large, and the data only displays 
  # 5 - 15 observations, I thought this option would be more user friendly. 
  # For aesthetic purposes, I limited the table to 10 observations,
  # however, the user can select the number of observations they want displayed.
  
  output$table <- DT::renderDataTable({
    DT::datatable(
      table(), options = list(
        lengthMenu = list(c(5, 10, 15), c('5', '10', '15')),
        pageLength = 10),
      rownames = FALSE, 
      style = "bootstrap"
    )
  })
  
  # In the tab "Exploring Deaths by Province", the user is given the option 
  # to filter and display the data by province. 
  # Hence, I created a reactive variable that would subset the data pertaining
  # to the provinces that the user had selected. 
  # I used the death_location data frame that contains the longitude and
  # latitude for each province. 
  # This reactive variable will be used to create the map. 
  
  death_province <- reactive({
    data_m <- death_location
    data_m <- subset(
      data_m, province %in% c(req(input$province))
    )
    data_m
  })
  
  
  # The following code chunk is to produce the map of Syria and its provinces.
  
  output$my_map2 <- renderLeaflet({
    
    # The reactive variable is used to determine which provinces will 
    # be marked on the map. 
    # The bounds of the map are made to include the "other" observations
    # whose coordinates are in the Mediterranean Sea. 
    
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addMarkers(lng = death_province()$lon, lat = death_province()$lat,
                 label = death_province()$province, popup = death_province()$province) %>%
      fitBounds(42.1855, 37.3323, 35.5751, 32.7069) %>%
      addSearchOSM()
  })
  
  # As describes above, I created another reactive variable to subset
  # the observations by province.
  # I had to recreate the reactive variable, since I am using a different
  # data frame. 
  # This reactive variable will be used to create the barplot. 
  
  data_province <- reactive({
    data_p <- data
    data_p <- subset(
      data_p,
      Province %in% c(req(input$province))
    )
  })
  
  # This code chunk produces the barplot: 
  # "Total Deaths in each Province from March 2011 to September 2018 by XXX and XXX"
  # I decided to manually code the theme for the barplot.
  # I had initially tried theme_minimal and theme_linedraw.
  # I liked the black background behind the title, but the title 
  # could not be clearly read.
  # In addition, I wanted the font and size of the graphic to be consistent
  # with the rest of the application. 
  # Since I wanted the user to be able to compare deaths by province, 
  # I facetted the graph by province. 
  
  output$barplot <- renderPlot({
    data <- data_province()
    ggplot(data, aes_string(input$x, fill = input$y)) + 
      geom_bar(position = "dodge") + 
      labs(y = "Number of Deaths",
           x = names(choices1[which(choices2 == input$x)]),
           fill = names(choices2[which(choices2 == input$y)])) + 
      ggtitle(label = paste("Total Deaths in each Province by",
                            names(choices2[which(choices2 == input$x)]), "and",
                            names(choices2[which(choices2 == input$y)])),
              subtitle = "March 2011 to September 2018") +
      coord_flip() + 
      scale_y_continuous(label = comma) +
      theme_linedraw(base_size = 14, base_line_size = 1) + 
      theme(title = element_text(size = 13, family = "Arial", face = "bold"),
            plot.subtitle = element_text(size = 13, family = "Arial", face = "bold"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", size = 13 , face = "bold"),
            legend.text = element_text(size = 13, colour = "black"),
            legend.title = element_text(face = "bold", size = 13),
            legend.justification = c("right", "top"),
            legend.position = "right",
            panel.spacing = unit(1, "lines"), 
            axis.title = element_text(size = 14)) +
      facet_wrap(~Province, shrink = TRUE)
  }) 
  }


# Run the application 
shinyApp(ui = ui, server = server)

# Note: While I did define the status and affiliation variables, 
# the definition of specific observations is not found in the raw data. 
# Many observations are self evident.
# However, and particularly for the Affiliation variable, 
# there may be some confusion regarding what can be organized under each
# observation. 
