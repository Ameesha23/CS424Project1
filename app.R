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
UICHalsted$wday = weekdays(as.POSIXct(UICHalsted$newDate), abbreviate = F)
#UICHalsted$wday <- factor(UICHalsted$wday, c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))

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
dataframe <- c("UICHalsted", "Ohare", "Washington")

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
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      selectInput("yearLeft", h1("Select a year to visualize on left screen"), years, selected = ""),
      selectInput("dataLeft", h1("Select Data for left screen"), dataframe, selected = ""),
      checkboxInput("checkTypeLeft", h2("View Data Tables"), value = FALSE),
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      selectInput("yearRight", h1("Select a year to visualize on right screen"), years, selected = ""),
      selectInput("dataRight", h1("Select Data for right screen"), dataframe, selected = ""),
      checkboxInput("checkTypeRight", h2("View Data Tables"), value = FALSE),
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      
      width = 1,
      tags$style(HTML("
                .well {background-color:#99AFD7;}
                      ")
                 ),
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
                        ")
                   ),
        
        fluidRow(
          column(1),
          column(11,
                 fluidRow(
                   conditionalPanel(
                     condition = "input.yearLeft == ''",
                     box(title = "UIC Halsted Rides per Year", solidHeader = TRUE, status = "primary", width = 60,
                         plotOutput("UICHalstedYears", height = 500)
                         )
                     )
                   ),
                 align = "center"
                 )
          ),
        
        
        fluidRow(
          column(3,
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearLeft != '' && input.checkTypeLeft",
                            box(title = "Rides per Day Data", solidHeader = TRUE, status = "primary", width = 40,
                                #DT::dataTableOutput("TableDayLeft", height = 150)
                                div(DT::dataTableOutput("TableDayLeft"), style = "font-size:106%")
                            ),
                            br()
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearLeft != '' && input.checkTypeLeft",
                            box(title = "Rides per Month Data", solidHeader = TRUE, status = "primary", width = 40,
                                #DT::dataTableOutput("TableMonthLeft", height = 150)
                                div(DT::dataTableOutput("TableMonthLeft"), style = "font-size:107%")
                            ),
                            br()
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearLeft != '' && input.checkTypeLeft",
                            box(title = "Rides per Weekday Data", solidHeader = TRUE, status = "primary", width = 40,
                                #DT::dataTableOutput("TableWdayLeft", height = 150)
                                div(DT::dataTableOutput("TableWdayLeft"), style = "font-size:107%")
                            ),
                            br()
                          )
                   )
                 )
          ),
          column(3,
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearLeft != ''",
                            box(title = paste("Rides per Day"), solidHeader = TRUE, status = "primary", width = 40,
                                plotOutput("AllDaysLeft", height = 200)
                                ),
                            br()
                            )
                          )
                   ),
                 
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearLeft != ''",
                            box(title = "Rides per Month", solidHeader = TRUE, status = "primary", width = 40,
                                plotOutput("MonthlyDataLeft", height = 200)
                                ),
                            br()
                            )
                   )
                   ),
                 
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearLeft != ''",
                            box(title = "Rides per Weekday", solidHeader = TRUE, status = "primary", width = 40,
                                plotOutput("WeeklyDataLeft", height = 200)
                                ),
                            br()
                            )
                          )
                   )
                 ),
          column(3,
                 fluidRow(
                   conditionalPanel(
                     condition = "input.yearRight != ''",
                     box(title = paste("Rides per Day"), solidHeader = TRUE, status = "primary", width = 40,
                         plotOutput("AllDaysRight", height = 200)
                     ),
                     br()
                   )
                 ),
                 
                 fluidRow(
                   conditionalPanel(
                     condition = "input.yearRight != ''",
                     box(title = "Rides per Month", solidHeader = TRUE, status = "primary", width = 40,
                         plotOutput("MonthlyDataRight", height = 200)
                     ),
                     br()
                   )
                 ),
                 
                 fluidRow(
                   conditionalPanel(
                     condition = "input.yearRight != ''",
                     box(title = "Rides per Weekday", solidHeader = TRUE, status = "primary", width = 40,
                         plotOutput("WeeklyDataRight", height = 200)
                     )
                   ),
                   br()
                   )
                 ),
          column(3,
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearRight != '' && input.checkTypeRight",
                            box(title = "Rides per Day Data", solidHeader = TRUE, status = "primary", width = 40,
                                #DT::dataTableOutput("TableDayLeft", height = 150)
                                div(DT::dataTableOutput("TableDayRight"), style = "font-size:106%")
                            ),
                            br()
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearRight != '' && input.checkTypeRight",
                            box(title = "Rides per Month Data", solidHeader = TRUE, status = "primary", width = 40,
                                #DT::dataTableOutput("TableMonthLeft", height = 150)
                                div(DT::dataTableOutput("TableMonthRight"), style = "font-size:107%")
                            ),
                            br()
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(12,
                          conditionalPanel(
                            condition = "input.yearRight != '' && input.checkTypeRight",
                            box(title = "Rides per Weekday Data", solidHeader = TRUE, status = "primary", width = 40,
                                #DT::dataTableOutput("TableWdayLeft", height = 150)
                                div(DT::dataTableOutput("TableWdayRight"), style = "font-size:107%")
                            ),
                            br()
                            )
                          )
                   )
                 )
          )
        )
    )
  )





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
      theme(plot.title = element_text(hjust = 0.5, size=25), axis.title=element_text(size=15)) 
  })
  
  output$AllDaysLeft <- renderPlot({
    #inputYear = as.numeric(input$year)
    YearSub <- subset(dataLeft(), year == inputYearLeft())
    ggplot(YearSub, aes(x = newDate, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$MonthlyDataLeft <- renderPlot({
    YearSub <- subset(dataLeft(), year == inputYearLeft())
    ggplot(YearSub, aes(x = month, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Month", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$WeeklyDataLeft <- renderPlot({
    YearSub <- subset(dataLeft(), year == inputYearLeft())
    ggplot(YearSub, aes(x = wday, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Weekday", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$AllDaysRight <- renderPlot({
    #inputYear = as.numeric(input$year)
    YearSub <- subset(dataRight(), year == inputYearRight())
    ggplot(YearSub, aes(x = newDate, y = rides/1000, color = (colour = cut(month, c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, Inf))))) + 
      geom_bar(stat = "identity") +
      scale_color_manual(name = "month",
                         values = c("(-Inf,1]" = "red",
                                    "(1,2]" = "yellow",
                                    "(2,3]" = "red",
                                    "(3,4]" = "yellow",
                                    "(4,5]" = "red",
                                    "(5,6]" = "yellow",
                                    "(6,7]" = "red",
                                    "(7,8]" = "yellow",
                                    "(8,9]" = "red",
                                    "(9,10]" = "yellow",
                                    "(10, 11]" = "red",
                                    "(11,12]" = "yellow",
                                    "(12,Inf]" = "red")) +
                         #labels = c("<= 17", "17 < qsec <= 19", "> 19")) + 
      labs(x = "Year", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(legend.position="none") +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$MonthlyDataRight <- renderPlot({
    YearSub <- subset(dataRight(), year == inputYearRight())
    ggplot(YearSub, aes(x = month, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
      labs(x = "Month", y ="Rides (in thousands)") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
  })
  
  output$WeeklyDataRight <- renderPlot({
    YearSub <- subset(dataRight(), year == inputYearRight())
    ggplot(YearSub, aes(x = wday, y = rides/1000)) + geom_bar(stat = "identity", fill = "#6888BE") +
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
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
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
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
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
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableDayRight <- DT::renderDataTable(
    DT::datatable({ 
      YearSub <- subset(dataRight(), year == inputYearRight())
      ReturnData <- YearSub[,c(4, 6)]
    }, 
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
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
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
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
    options = list(searching = FALSE, pageLength = 3, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  }

shinyApp(ui = ui, server = server)

