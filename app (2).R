library(dplyr)
library(shiny)
library(plotly)

child_race_pov <- read.csv("child_race_pov.csv")
child_poverty <- read.csv("Children in poverty.csv")

#data cleaning for file 1
child_race_pov <- subset(child_race_pov, (DataFormat == "Number"))
child_race_pov <- subset(child_race_pov, !(Data %in% c("S", "N.A.")))
child_race_pov <- subset(child_race_pov, !(Race %in% c("Total")))
child_race_pov <- subset(child_race_pov, select = -c(DataFormat, LocationType))
child_race_pov <- na.omit(child_race_pov)

#Data cleaning for file 2
child_poverty <- subset(child_poverty, (DataFormat == "Number"))
child_poverty <- subset(child_poverty, select = -c(DataFormat, LocationType))
child_poverty <- subset(child_poverty, !(Data %in% c("S", "N.A.")))
child_poverty <- na.omit(child_poverty)

new_set <- child_poverty$TimeFrame >= 2000 & child_poverty$TimeFrame <= 2022
new_set_two <- child_race_pov$TimeFrame >= 2000 & child_race_pov$TimeFrame <= 2022
filtered_df_one <- child_poverty[new_set, ]
filtered_df_two <- child_race_pov[new_set_two, ]

# merged by both location column
both_df <- merge(filtered_df_one, filtered_df_two, by = c("Location","TimeFrame", "Data"), all = TRUE)
both_df <- na.omit(both_df)

ui <- navbarPage(
  title = "Poverty in Youth",
  tags$head(
    tags$style(HTML("
    body { 
    background-color: #b8bccc;
    font-family: 'cursive', 'Calibri'; 
    }
  h3 { 
  color: Black;
  }
                    "))
  ),
  # Home tab
  tabPanel("About",
           column(6, style = "padding-right: 50px;", 
                  div(style = "font-size: 16px;",
                      h3("Project Overview"),
                      p("The advice that many of us have heard is along the lines of, “Life is never a smooth journey, there will always be hurdles along the way.” As we agree with this statement, we also know that some people have more hurdles than others. Imagine a world where every young person, regardless of their circumstances, has easy access to resources and support."),
                      h3("Vision"),
                      p("Our vision is to create a world where every young person, regardless of their circumstances, has easy access to vital resources and unwavering support. We envision a digital platform that serves as a lifeline for youth and young adults seeking assistance with fundamental needs such as shelter, food, and more."),
                      h3("Problem Statement"),
                      p("Navigating the wealth of information available can be overwhelming, leading many to struggle in finding the help they urgently need. While support exists, it is often inefficient and unreliable, particularly for young individuals facing adversities. Research indicates that children living in poverty spend more time surviving than exploring the world around them, leaving them without the crucial assistance they require."),
                      h3("Key Findings"),
                      p("We discovered that child poverty youth rates have decreased over the past ten years throughout the United States. Although it's been decreasing, the progress has been slow, emphasizing the need for continued efforts and initiatives to further decrease this significant social challenge."),
                      p(a(href = "https://datacenter.aecf.org/data/tables/43-children-in-poverty?loc=1&loct=2#detailed/2/2-53/false/1095/any/322", "Child Poverty Statistics in the U.S.")),
                      p(a(href = "https://datacenter.aecf.org/data/tables/10931-adults-ages-18-to-24-who-sometimes-or-often-did-not-have-enough-food-to-eat-in-the-past-week?loc=1&loct=1#detailed/1/any/false/2501,2485,2475,2470,2460,2461,2421,2420,2102,2101/any/21240", "Food insecurity among adults ages 18 to 24"))
                  ),
           ),
           fluidRow(
             column(6,
                    tags$img(src = "https://www.hope4youthmn.org/wp-content/uploads/2023/05/services-1-1030x1030.png", 
                             height = 600, 
                             width = "100%", 
                             style = "padding-left: 20px; padding-right: 20px")
             )
           )
  ),
  
  # Interactive page 1
  tabPanel("Data Selection",
           sidebarLayout(
             sidebarPanel(
               div(
                 style = "background-color: whitesmoke; padding: 10px; border-radius: 5px; color: grey; text-align: left; margin-top: 10px;",
                 p("Explore youth in poverty across the United States over the last 22 years as it unfolds through the lens of a scatter plot, as you hover your mouse on eaceach point represents unique intersection of each state, year and youth poverty rates. This visual scatter plot allows to witness the triumps and challnges as some states lower youth poverty rates while other persistent the struggles. Hover your mouse sheds light to real-life experiences behind each data points."),
                 p(style = "font-style: italic")
               )
             ),
             mainPanel(
               plotlyOutput("scatterPlotData")
             )
           )
  ),
  
  # Interactive page 2
  tabPanel("Race",
           sidebarLayout(
             sidebarPanel(
               selectInput("raceFilter", "Select", choices = c(both_df$Race)),
               div(
                 style = "background-color: whitesmoke; padding: 10px; border-radius: 5px; color: grey; text-align: left; margin-top: 10px;",
                 p("This scatter plot illustrates a relationship between race and youth poverty. The data paints a vivid picture of the challenges faced by different racial communities and their youth from years starting 2000 to 2022. Use the drop-down to select and journey through the data points that mark the interaction between race and youth poverty. Each point on the graph reveals the disparities and hardships. These challenges are faced due to lack of resources but also other factors."),
                 p(style = "font-style: italic;")
               )
             ),
             mainPanel(
               plotlyOutput("scatterPlotRace")
             )
           )
  ),
  
  
  # Interactive page 3
  tabPanel("Location",
           sidebarLayout(
             sidebarPanel(
               selectInput("locationFilter", "Select", choices = c(unique(both_df$Location))),
               div(
                 style = "background-color: whitesmoke; padding: 10px; border-radius: 5px; color: grey; text-align: left; margin-top: 10px;",
                 p("Journey through the heart of America exploring the interaction of location and youth poverty. A canvas of scatter plots, where each point represents a state. Through the valleys of hardships, the scatter plot unravels socio-economic disparities. Use the drop-down menu select a state and witness the scatter plot of youth in poverty by each state.  
"),
                 p(style = "font-style: italic;"),
               )
             ),
             mainPanel(
               plotlyOutput("scatterPlotLocation")
             )
           )
  ),
  
  #analysis page 
  tabPanel("Analysis",
           p(strong("Authors: Arsima Sisay (asisay@uw.edu), Myky Ho (mho02@uw.edu), Anisha Bains (abains2@uw.edu)")),
           p(strong("Affliation: INFO-201: Technical Foundations of Informatics - The Information School - University of Washington")),
           p(strong("Date: Fall 2023")),
           h3("Keywords"),
           p("Children, Young Adults, Poverty, Rates, Race, United States, Data"),
           h3("Abstract"),
           p("Our website ultimately aims to provide insight about whether child and adult youth poverty in the United States has changed throughout years. Our data presents information from the years of 2012 to 2022, with emphasis on raising awareness about the necessary ways to improve this problem. The question we focused on answering throughout our project was how much has poverty changed over the course of 10 years. To help answer our research question, we will analyze datasets regarding child poverty, as well as poverty and food insecurity issues amongst young adults of age from 18-24. By reducing poverty for a large amount of individuals would help improve many issues such as food insecurity, quality of health, and overall quality of life"),
           h3("Design"),
           p("Our project is designed in a way that presents a clear indication about the topics we will be covering throughout our website. The about page, contains information about the importance of our topic and how there are many ways the issues of poverty and food insecurity can be addressed. The about page states our focus about how to further explain the issues, as well as provide helpful information and resources that could potentially positively impact individuals dealing with said issues. Further into our research, we wanted to create three interactive pages showing the rates in which child poverty and food The first interactive page named “Data Selection”, includes a scatterplot that demonstrates the percentage of youth experiencing poverty along with the corresponding year and state when the user hovers over the graph. The second interactive page. The second interactive with the name, “Race” includes a scatterplot that lets user pick a certain ethnic group to analyze in the drop down menu. The data shows children living in households with a high housing cost burden by race and ethnicity in the United States. The third interactive page named, “Location” that includes a scattered-plot with drop down menu to let user analyze the data by which state. We hoped to create graphs and informative data that could be well considered when identifying the ways in which these issues can be resolved or having the materials to analyze what is working versus what is not."),
           h3("Purpose"),
           p("We believe that life experiences, especially at a young age, shapes the upbringing of a person. This ignited our desire to ensure that individuals unable to access resources for everyday living are brought into light. Our goal is to not only raise awareness for people but also create a community of students, teachers, friends and family that can lift each other up in times of need. We hope that with the data provided on our site we can educate people about the real numbers of youths not receiving the help that they need. Advocating and spreading awareness of this issue is crucial."),
           h3("Conclusion"),
           p("During the process of completion of our project we learned about the many ways in which people are highly affected by poverty in the United States. Factors such as food insecurity, homelessness, and lack of education contribute to the well known issue of poverty. These consequences play a negative role on individuals, communities, and societies. Raising awareness about poverty will hopefully result in a more equitable world and drive the need to change these factors for people. After researching children and young adults, it was refreshing to see the data of people living in poverty decrease over the years, although it is still an ongoing issue that will optimistically continue to decline.")
  )
)

# Server Logic
server <- function(input, output) {
  # Data Selection
  filtered_data <- reactive({
    subset(both_df, Location == input$state & TimeFrame == input$time)
  })
  output$scatterPlotData <- renderPlotly({
    plot_data <- filtered_data()
    plot_ly(data = both_df, x = ~Data, y = ~TimeFrame, type = "scatter", mode = "markers", color= ~Location) %>%
     layout(title = 'Youth in Poverty ', xaxis = list(title = 'Data'), 
           yaxis = list(title = 'Time Frame'))
  })
  
  # Race 
  race_data <- reactive({
    subset(both_df, Race == input$raceFilter & Location == "United States")
  })
  output$scatterPlotRace <- renderPlotly({
    plot_ly(data = race_data(), x = ~TimeFrame, y = ~Data, type="scatter", mode="markers", color= ~Race) %>%
      layout(title = 'Youth in Poverty per Year', xaxis = list(title = 'Year'), 
             yaxis = list(title = 'Total Number Of People in Poverty'))
  })
  
  # Location
  location <- reactive({
    subset(both_df, Location == input$locationFilter)
  })
  output$scatterPlotLocation <- renderPlotly({
    location_data = location()
    plot_ly(data = location_data, x = ~TimeFrame, y = ~Data, type="scatter", mode="markers", color= ~Race) %>%
      layout(title = 'Youth in Poverty per Year', xaxis = list(title = 'Year'), 
             yaxis = list(title = 'Total Number Of People in Poverty'))
  })
  
}

# Run the application 
shinyApp(ui, server)