library(shiny)
library(ggplot2)
library(shinydashboard)
library(lubridate)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)

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
UICHalsted$month = month(UICHalsted$newDate, abbr = TRUE, label = TRUE)
UICHalsted$wday = weekdays(as.POSIXct(UICHalsted$newDate), abbreviate = T)

#UICHalsted$wday <- factor(UICHalsted$wday, c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))

Ohare$year = year(Ohare$newDate)
Ohare$month = month(Ohare$newDate, abbr = TRUE, label = TRUE)
Ohare$wday = weekdays(as.POSIXct(Ohare$newDate), abbreviate = T)

Washington$year = year(Washington$newDate)
Washington$month = month(Washington$newDate, abbr = TRUE, label = TRUE)
Washington$wday = weekdays(as.POSIXct(Washington$newDate), abbreviate = T)

#changes rides from character to numeric
UICHalsted$rides <- as.numeric(gsub(",","",UICHalsted$rides))
Ohare$rides <- as.numeric(gsub(",","",Ohare$rides))
Washington$rides <- as.numeric(gsub(",","",Washington$rides))


#menu options for selecting years
years<-c("2001", "2002", "2003", "2004", "2005",
         "2006", "2007", "2008", "2009", "2010",
         "2011", "2012", "2013", "2014", "2015",
         "2016", "2017", "2018", "2019", "2020", "2021")
dataframe <- c("UICHalsted", "Ohare", "Washington")

# Create the shiny dashboard
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "CS 424 Spring 2022 Example Dashboard"),
  dashboardSidebar(disable = FALSE, collapsed = TRUE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", icon = NULL, href = "https://asaxen21.wixsite.com/ameesha/manufacturing")
                   )
                      
                   
                   #selectInput("Year", "Select the year to visualize", years, selected = 2021),
                   #selectInput("Room", "Select the room to visualize", listNamesGood, selected = "Machine Room")
  ),
  dashboardBody(
    tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#003d59
    }
    .box.box-solid.box-primary{
    border-bottom-color:#666666;
    border-left-color:#666666;
    border-right-color:#666666;
    border-top-color:#666666;
    }
    /* body */
    .content-wrapper, .right-side {
    background-color: #f5f5f5;
    }
                    ")
    ),
    fluidRow(
      column(1,
             fluidRow(
               style = "padding-left:20px",
               HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
               selectInput("yearLeft", h6("Select a year to visualize on left screen"), years, selected = "2001"),
               selectInput("dataLeft", h6("Select Data for left screen"), dataframe, selected = "UICHalsted"),
               selectInput("yearRight", h6("Select a year to visualize on right screen"), years, selected = "2001"),
               selectInput("dataRight", h6("Select Data for right screen"), dataframe, selected = "Ohare"),
               HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>")
             ),
      ),
      column(2,
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Day Data", solidHeader = TRUE, status = "primary", width = 60,
                   div(DT::dataTableOutput("TableDayLeft"), style = "font-size:103%")
               )
             ), 
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Month Data", solidHeader = TRUE, status = "primary", width = 60,
                   div(DT::dataTableOutput("TableMonthLeft"), style = "font-size:99%")
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Weekday Data", solidHeader = TRUE, status = "primary", width = 60,
                   div(DT::dataTableOutput("TableWdayLeft"), style = "font-size:98%")
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides over the years data", solidHeader = TRUE, status = "primary", width = 40,
                   div(DT::dataTableOutput("TableYearsLeft"), style = "font-size:100%")
               )
             )
      ),
      column(3,
             fluidRow(
               style = "padding-left:20px",
               box(title = paste("Rides per Day"), solidHeader = TRUE, status = "primary", width = 60,
                   plotOutput("AllDaysLeft", height = 300)
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Month", solidHeader = TRUE, status = "primary", width = 40,
                   plotOutput("MonthlyDataLeft", height = 300)
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Weekday", solidHeader = TRUE, status = "primary", width = 40,
                   plotOutput("WeeklyDataLeft", height = 300)
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides over the years", solidHeader = TRUE, status = "primary", width = 40,
                   plotOutput("AllYearsLeft", height = 300)
               )
             )
      ),
      column(3,
             fluidRow(
               style = "padding-left:20px",
               box(title = paste("Rides per Day"), solidHeader = TRUE, status = "primary", width = 40,
                   plotOutput("AllDaysRight", height = 300)
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Month", solidHeader = TRUE, status = "primary", width = 40,
                   plotOutput("MonthlyDataRight", height = 300)
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Weekday", solidHeader = TRUE, status = "primary", width = 40,
                   plotOutput("WeeklyDataRight", height = 300)
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Weekday", solidHeader = TRUE, status = "primary", width = 40,
                   plotOutput("AllYearsRight", height = 300)
               )
             )
      ),
      column(2,
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Day Data", solidHeader = TRUE, status = "primary", width = 40,
                   div(DT::dataTableOutput("TableDayRight"), style = "font-size:103%")
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Month Data", solidHeader = TRUE, status = "primary", width = 40,
                   div(DT::dataTableOutput("TableMonthRight"), style = "font-size:99%")
               )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides per Weekday Data", solidHeader = TRUE, status = "primary", width = 40,
                   div(DT::dataTableOutput("TableWdayRight"), style = "font-size:98%")
                   )
             ),
             fluidRow(
               style = "padding-left:20px",
               box(title = "Rides over the years data", solidHeader = TRUE, status = "primary", width = 40,
                   div(DT::dataTableOutput("TableYearsRight"), style = "font-size:100%")
               )
             )
      )
    )
  ))



#server functions
server <- function(input, output) {
  
  inputYearLeft <- reactive({
    as.numeric(input$yearLeft)
  })
  
  dataLeft <- reactive({
    if(input$dataLeft == "UICHalsted")
      UICHalsted
    else
      Ohare
  })
  
  inputYearRight <- reactive({
    as.numeric(input$yearRight)
  })
  
  dataRight <- reactive({
    if(input$dataRight == "UICHalsted")
      UICHalsted
    else if(input$dataRight == "Ohare")
      Ohare
    else
      Washington
  })
  
  
  
  output$AllYearsLeft <- renderPlot({
    ggplot(dataLeft(), aes(x = year, y = rides/100000)) + geom_bar(stat = "identity", fill = "#3e6a7f", width=0.8) +
      labs(x = "Year", y ="Rides (in hundred thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=25), axis.title=element_text(size=15)) 
  })
  
  output$AllDaysLeft <- renderPlot({
    #inputYear = as.numeric(input$year)
    col <- c("#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6")
    YearSub <- subset(dataLeft(), year == inputYearLeft())
    ggplot(YearSub, aes(x = newDate, y = rides/1000, fill = month(newDate,abbr = TRUE, label = TRUE))) + 
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12)) +
      scale_fill_manual(values = col)
  })
  
  output$MonthlyDataLeft <- renderPlot({
    YearSub <- subset(dataLeft(), year == inputYearLeft())
    ggplot(YearSub, aes(x = month, y = rides/1000)) + geom_bar(stat = "identity", fill = "#3e6a7f", width=0.8) +
      labs(x = "Month", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$WeeklyDataLeft <- renderPlot({
    YearSub <- subset(dataLeft(), year == inputYearLeft())
    YearSub$wday <- factor(YearSub$wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    ggplot(YearSub, aes(x = wday, y = rides/1000)) + geom_bar(stat = "identity", fill = "#3e6a7f", width=0.8) +
      labs(x = "Weekday", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$AllYearsRight <- renderPlot({
    ggplot(dataRight(), aes(x = year, y = rides/100000)) + geom_bar(stat = "identity", fill = "#91b3bb", width=0.8) +
      labs(x = "Year", y ="Rides (in hundred thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=25), axis.title=element_text(size=15)) 
  })
  
  output$AllDaysRight <- renderPlot({
    #inputYear = as.numeric(input$year)
    col <- c("#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6")
    YearSub <- subset(dataRight(), year == inputYearRight())
    ggplot(YearSub, aes(x = newDate, y = rides/1000, fill = month(newDate,abbr = TRUE, label = TRUE))) + 
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12)) +
      scale_fill_manual(values = col)
    })
  
  output$MonthlyDataRight <- renderPlot({
    YearSub <- subset(dataRight(), year == inputYearRight())
    ggplot(YearSub, aes(x = month, y = rides/1000)) + geom_bar(stat = "identity", fill = "#91b3bb", width=0.8) +
      labs(x = "Month", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$WeeklyDataRight <- renderPlot({
    YearSub <- subset(dataRight(), year == inputYearRight())
    YearSub$wday <- factor(YearSub$wday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    ggplot(YearSub, aes(x = wday, y = rides/1000)) + geom_bar(stat = "identity", fill = "#91b3bb", width=0.8) +
      labs(x = "Weekday", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  
  output$TableDayLeft <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(dataLeft(), year == inputYearLeft())
      ReturnData <- YearSub[,c(4, 6)]
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableMonthLeft <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(dataLeft(), year == inputYearLeft())
      if(nrow(YearSub)){
        ReturnData <- as.data.frame(aggregate(YearSub$rides, by=list(month=YearSub$month), FUN=sum))
      }
    }, 
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableWdayLeft <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(dataLeft(), year == inputYearLeft())
      if (nrow(YearSub)){
        ReturnData <- as.data.frame(aggregate(YearSub$rides, by=list(wday=YearSub$wday), FUN=sum))
      }
    }, 
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableYearsLeft <- DT::renderDataTable(
    DT::datatable({ 
      ReturnData <- as.data.frame(aggregate(dataLeft()$rides, by=list(year=dataLeft()$year), FUN=sum))
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableDayRight <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(dataRight(), year == inputYearRight())
      ReturnData <- YearSub[,c(4, 6)]
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableMonthRight <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(dataRight(), year == inputYearRight())
      if(nrow(YearSub)){
        ReturnData <- as.data.frame(aggregate(YearSub$rides, by=list(month=YearSub$month), FUN=sum))
      }
    }, 
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableWdayRight <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(dataRight(), year == inputYearRight())
      if (nrow(YearSub)){
        ReturnData <- as.data.frame(aggregate(YearSub$rides, by=list(wday=YearSub$wday), FUN=sum))
      }
    }, 
    options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableYearsRight <- DT::renderDataTable(
    DT::datatable({ 
      ReturnData <- as.data.frame(aggregate(dataRight()$rides, by=list(year=dataRight()$year), FUN=sum))
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
}

shinyApp(ui = ui, server = server)



