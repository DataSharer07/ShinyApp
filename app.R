library(dplyr)
library(shiny)
library(plotly)
library(knitr)
library(markdown)

#library(DT)
#setwd("C:/Users/bschleich/Dropbox/R/Lectures/Developing Data Products/Week 4 Proj")
#setwd("~/Dropbox/R/Lectures/Developing Data Products/Week 4 Proj/MyShinyApp/FantasyQB")
#qb<-read.csv("QB.csv")
qb<-read.csv("QB.csv",header=TRUE)
qb["Fantasy_Points"]<-NA
nms <- names(qb)



ui <- navbarPage("Fantasy QB Finder 2016",tabPanel("Explore and Find",
  sidebarPanel(
    selectInput('x', 'X-Axis QB Performance Measure',  names(qb[,3:22]),selected=names(qb)[[3]]),
    selectInput('y', 'Y-Axis QB Performance Measure',  names(qb[,4:22]),selected=names(qb)[[22]]),
    sliderInput("games", "Games Played:",
                min=1, max=16, value=c(5,16)),
    sliderInput("top", "Number of Top QBs displayed:", min=1,max= 10,value=5),
    numericInput("passtd", label = h6("Score for Pass TDs"), value = 6),
    numericInput("rushtd", label = h6("Score for Rush TDs"), value = 6),
    numericInput("pyards", label = h6("Score for 10 Pass Yards"), value = 1),
    numericInput("ryards", label = h6("Score for 10 Rush Yards"), value = 1),
    numericInput("interception", label = h6("Score for an Interception"), value = -1),
    numericInput("fumble", label = h6("Score for Lost Fumble"), value = -1)
  ),
  mainPanel(
    h4("Top QBs based on variable y"),
    tableOutput("summary"),
    #h5("Data retrieved from https://nathanbrixius.wordpress.com/category/fantasy-football/"),
    plotlyOutput('qbPlot')
  )),
  tabPanel("About",uiOutput("page1"),plotlyOutput('trendPlot')
           #mainPanel(includeMarkdown("markdown")#,
                             #includeHTML("README.html")
                             #)
           )
)

server <- function(input, output,session) {
qbnew<-
  reactive({
    mingames <- input$games[1]
    maxgames <- input$games[2]

  if(input$x=="G"){
    d<-qb %>% select(1:3,22,one_of(input$y))
  }
  else if(input$y=="G"){
    d<-qb %>% select(1:3,22,one_of(input$x))
  }
  else{

  d<-d<-qb %>% select(1:3,22,one_of(input$x),one_of(input$y))
  }

    d["Fantasy_Points"]<-qb$PassYds*input$pyards/10+input$passtd*qb$TD+input$rushtd*qb$RushTD+qb$RushYds*input$ryards/10+qb$Int*input$interception+qb$FumL*input$fumble

    d <- d %>%
    filter(
      G >= mingames,
      G <= maxgames
    )

  })

  output$qbPlot <- renderPlotly({

    # build graph with ggplot syntax
    qbn<-qbnew()
    p <- ggplot(qbn, aes_string(x = input$x, y=input$y,text="Name")) +
      geom_point()
    ggplotly(p)

  })
  output$summary <- renderTable({
    qbn<-qbnew()
    head(qbn[order(-qbn[,input$y]),],n=input$top)
  })
  
  output$page1 <- renderUI({
    includeMarkdown("README.Rmd")
    #includeHTML("README.html")
  })
  
  output$trendPlot<-renderPlotly({
    trend<-data.frame(year=c(2005:2011,2014:2016),players=c(12.6,18.0,19.4,29.9,28.4,32.0,35.9,41.5,56.8,57.4))
    p<-ggplot(trend, aes(x = year, y = players)) + geom_line()+geom_point(size=2)+labs(x="Year",y="Estimated Players in Million")+ggtitle("Estimated number of fantasy sports players in USA & Canada by year")+ annotate("text", label = "Data from http://fsta.org/research/industry-demographics/ (Nov, 2016)", x = 2010, y = 10, size = 4, colour = "black")
    gg<-ggplotly(p)
  })

}

shinyApp(ui, server)
