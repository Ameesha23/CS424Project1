library(shiny)
library(ggplot2)
library(lubridate)


#read in the data for the three stations we're investigating
UICHalsted <-read.csv("UIC_Halsted.csv", header=TRUE)
Ohare <-read.csv("Ohare.csv", header=TRUE)
Washington <-read.csv("Washington.csv", header=TRUE)

#fix dates using lubridate
UICHalsted$newDate = as_date(mdy(UICHalsted$date))
UICHalsted$year = year(UICHalsted$newDate)
UICHalsted$rides <- as.numeric(as.character(UICHalsted$rides))
#UICHalsted$Month = year(UICHalsted$newDate)

ui <- fluidPage(
  #Input Functions
  #Output Functions
  titlePanel("Project 1 - Subway"),
  print(str(UICHalsted)),
  #sliderInput(inputId = "num", 
              #label = "Choose a number", 
              #value = 25, min = 1, max = 100),
  #plotOutput("hist", height = 250),
  plotOutput("UICHalstedYears", height = 250)
)

server <- function(input, output) {
  #output objects
  output$hist <- renderPlot({
    title <- "100 random normal values"
    hist(rnorm(input$num), main = title)
  })
  
  output$UICHalstedYears <- renderPlot({
    ggplot(UICHalsted, aes(x = year, y = rides)) + geom_bar(stat = "identity") +
      labs(title = "UIC Halsted Rides Per Year", x = "Year", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "Helvetica-Bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
}

shinyApp(ui = ui, server = server)


#use lubridate to convert dates to usable formats
#bar chart total entries at UIC-Halsted for each year (2001, 2002, ... 2021)
#allow the user to choose to see each of the following charts (either 
#individually or all at the same time)
# bar chart entries at UIC-Halsted each day for 2021 (jan 1, jan 2, ... dec 31)
# bar chart total entries at UIC-Halsted for each month for 2021 (jan,... dec)
# bar chart total entries at UIC-Halsted for each day of the week for 2021 
# (mon, tue, ... sun)
#allow the user to use a menu to choose any of the years from 2001 - 2021 and 
#have all of the UIC-Halsted charts update for the chosen year
#allow the user to see the data for each chart as a table in the same order
#have an 'about page' in your app, perhaps as a separate tab in the shiny 
# interface, with appropriate credits (where the data is from, who wrote the 
# app, when, why, etc.)
