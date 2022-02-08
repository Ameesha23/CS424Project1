library(shiny)
library(ggplot2)
library(shinydashboard)
library(lubridate)

#read in the data for the three stations we're investigating
UICHalsted <-read.csv("UIC_Halsted.csv", header=TRUE)
Ohare <-read.csv("Ohare.csv", header=TRUE)
Washington <-read.csv("Washington.csv", header=TRUE)

#fix dates using lubridate
UICHalsted$newDate = as_date(mdy(UICHalsted$date))
Ohare$newDate = as_date(mdy(Ohare$date))
Washington$newDate = as_date(mdy(Washington$date))

#add year, month and day columns for the three datasets
UICHalsted$year = year(UICHalsted$newDate)
UICHalsted$month = month(UICHalsted$newDate)
UICHalsted$wday = weekdays(as.POSIXct(UICHalsted$newDate), abbreviate = T)

Ohare$year = year(Ohare$newDate)
Ohare$month = month(Ohare$newDate)
Ohare$wday = weekdays(as.POSIXct(Ohare$newDate), abbreviate = T)

Washington$year = year(Washington$newDate)
Washington$month = month(Washington$newDate)
Washington$wday = weekdays(as.POSIXct(Washington$newDate), abbreviate = T)

#changes rides from character to numeric
UICHalsted$rides <- as.numeric(gsub(",","",UICHalsted$rides))
Ohare$rides <- as.numeric(gsub(",","",Ohare$rides))
Washington$rides <- as.numeric(gsub(",","",Washington$rides))


#menu options for selecting years
years<-c("", "2001", "2002", "2003", "2004", "2005",
         "2006", "2007", "2008", "2009", "2010",
         "2011", "2012", "2013", "2014", "2015",
         "2016", "2017", "2018", "2019", "2020", "2021")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "CS 424 Project 1", titleWidth = 450),
  dashboardSidebar(disable = FALSE, collapsed = TRUE,
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                     menuItem("About", tabName = "About", icon = NULL)
                   
                   
  ),
  dashboardBody(
    sidebarPanel(
      selectInput("year", "Select the year to visualize", years, selected = ""),
      radioButtons("type", h5("View"),
                   choices = list("Visualizations" = 1, "Data Tables" = 2),
                   selected = 1),
      checkboxInput("checkType", "View Data Tables", value = FALSE),
      width = 2
    
    ),
    mainPanel(
        tags$style(HTML("
                .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:#233C67
                }
                
                .box.box-solid.box-primary{
                  border-bottom-color:#666666;
                  border-left-color:#666666;
                  border-right-color:#666666;
                  border-top-color:#666666;
                }
  
      ")),
        fluidRow(
          column(1,
          ),
          column(10,
                 fluidRow(
                   conditionalPanel(
                     condition = "input.year == ''",
                     box(title = "UIC Halsted Rides per Year", solidHeader = TRUE, status = "primary", width = 40,
                         plotOutput("UICHalstedYears", height = 200)
                     )
                     
                   )
                 )
          )
        ),
        
        fluidRow(
          column(3,
                 fluidRow(
                   conditionalPanel(
                     condition = "input.year != ''",
                     box(title = "UIC Halsted Rides per Day", solidHeader = TRUE, status = "primary", width = 40,
                         plotOutput("UICHalstedAllDays", height = 170)
                     )
                     
                   )
                 ),
                 
                 fluidRow(
                   conditionalPanel(
                     condition = "input.year != ''",
                     box(title = "UIC Halsted Rides per Month", solidHeader = TRUE, status = "primary", width = 40,
                         plotOutput("UICHalstedByMonth", height = 170)
                     )
                     
                   )
                 ),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.year != ''",
                     box(title = "UIC Halsted Rides per Weekday", solidHeader = TRUE, status = "primary", width = 40,
                         plotOutput("UICHalstedByWday", height = 170)
                     )
                   )
                 )
          ),
          column(3,
                 fluidRow(
                   conditionalPanel(
                     condition = "input.year != '' && input.checkType",
                     box(title = "UIC Halsted Rides per Day Data", solidHeader = TRUE, status = "primary", width = 40,
                         dataTableOutput("TableDay", height = 100)
                     )
                     
                   )
                 ),
                 
                 fluidRow(
                   conditionalPanel(
                     condition = "input.year != '' && input.checkType",
                     box(title = "UIC Halsted Rides per Month", solidHeader = TRUE, status = "primary", width = 40,
                         dataTableOutput("TableMonth", height = 100)
                     )
                     
                   )
                 ),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.year != '' && input.checkType",
                     box(title = "UIC Halsted Rides per Weekday", solidHeader = TRUE, status = "primary", width = 40,
                         dataTableOutput("TableWday", height = 100)
                     )
                     
                   )
                 )
          )
      ),
      
    )
    
  )
  
)


server <- function(input, output) {
  
  inputYear <- reactive({
    as.numeric(input$year)
  })
  
  #output objects
  output$hist <- renderPlot({
    title <- "100 random normal values"
    hist(rnorm(input$num), main = title)
  })
  
  output$UICHalstedYears <- renderPlot({
    ggplot(UICHalsted, aes(x = year, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$UICHalstedAllDays <- renderPlot({
    #inputYear = as.numeric(input$year)
    UICHalstedYearSub <- subset(UICHalsted, year == inputYear())
    ggplot(UICHalstedYearSub, aes(x = newDate, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$UICHalstedByMonth <- renderPlot({
    UICHalstedYearSub <- subset(UICHalsted, year == inputYear())
    ggplot(UICHalstedYearSub, aes(x = month, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$UICHalstedByWday <- renderPlot({
    UICHalstedYearSub <- subset(UICHalsted, year == inputYear())
    ggplot(UICHalstedYearSub, aes(x = wday, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  #output$TableDay <- renderDataTable({
  #  UICHalstedYearSub <- subset(UICHalsted, year == inputYear())
  #  ReturnData <- UICHalstedYearSub[,c(2, 6, 5)]
  #  ReturnData
  #})
  
  
  
  output$TableDay <- DT::renderDataTable(
    DT::datatable({ 
      UICHalstedYearSub <- subset(UICHalsted, year == inputYear())
      ReturnData <- UICHalstedYearSub[,c(4, 6)]
    }, 
    options = list(searching = FALSE, pageLength = 2, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableMonth <- DT::renderDataTable(
    DT::datatable({ 
      UICHalstedYearSub <- subset(UICHalsted, year == inputYear())
      if(nrow(UICHalstedYearSub)){
        ReturnData <- as.data.frame(aggregate(UICHalstedYearSub$rides, by=list(month=UICHalstedYearSub$month), FUN=sum))
      }
    }, 
    options = list(searching = FALSE, pageLength = 2, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableWday <- DT::renderDataTable(
    DT::datatable({ 
      UICHalstedYearSub <- subset(UICHalsted, year == inputYear())
      if (nrow(UICHalstedYearSub)){
        ReturnData <- as.data.frame(aggregate(UICHalstedYearSub$rides, by=list(wday=UICHalstedYearSub$wday), FUN=sum))
      }
    }, 
    options = list(searching = FALSE, pageLength = 2, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )

  
  
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
