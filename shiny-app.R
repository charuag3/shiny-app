#install.packages("shiny")
#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(httr)
library(randomForest)
library(zoo)
library(xts)

ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Human Activity Recognition"),
                sidebarLayout(
                  sidebarPanel(textOutput("activity_text")),
                  mainPanel(
                    plotOutput("coolplot"),
                    br(), br(),
                    tableOutput("tableOut")
                  )
                ))
print(ui)
Sys.setenv(TZ='GMT-1')
activity_history<-data.frame( "Time" = format(Sys.time(), "%a %b %d %X %Y"), "UserActivity" = "App started")

server <- function(input, output,session) {
  output$tableOut <- renderTable({
    invalidateLater(1000, session)
    activity_history
  },bordered = TRUE,hover = TRUE)
  
  output$activity_text <- renderText({
    invalidateLater(4000, session)
    getActivity()
  })
  
  output$coolplot <- renderPlot({
    invalidateLater(4000, session)
    
    plot_data_response<-GET("http://35.246.49.102:8080/sensorData/getPlotData")
    if(has_content(plot_data_response)){
      plot_data<-read.table(text=content(plot_data_response,"text"), sep =",", header = TRUE, stringsAsFactors = FALSE)
      plot_data
      basket <- cbind(plot_data$xcoor, plot_data$ycoor, plot_data$zcoor,plot_data$xaxis,plot_data$yaxis,plot_data$zaxis)
      zoo.basket <- as.zoo(basket)
      tsRainbow <- rainbow(ncol(zoo.basket))
      plot(x = zoo.basket, ylab = "Sensor Values", main = "Sensor Values",
           col = tsRainbow, screens = 1)
      
      legend(x = "topleft", legend = c("xcoor", "ycoor", "zcoor", "xaxis", "yaxis","zaxis"), 
             lty = 1,col = tsRainbow)
    }
  })
  
  #reading data
  activity<-read.csv("train-mobile.csv")
  activity_train<-droplevels(activity[1:544,])
  summary(activity_train)
  
  
  #creating model
  model <- randomForest(Activity ~ ., data = activity_train, importance = TRUE)
  model
  plot(model)
  
  getActivity <- function() {
    latest_data<-GET("http://35.246.49.102:8080/sensorData/getCsv?activity=test&subject=1&type=rest")
    if(has_content(latest_data)){
      activity_test<-read.table(text=content(latest_data,"text"), sep =",", header = TRUE, stringsAsFactors = FALSE)
      ddd <-predict(model,activity_test)
      if(nrow(activity_history)>=10){
        activity_history<-activity_history[-1,]
      }
      activity_history<<-rbind(activity_history, data.frame( "Time" = format(Sys.time(), "%a %b %d %X %Y"), "UserActivity" = as.character(ddd)))
      print(activity_history)
      return (paste("The user is :",as.character(ddd)))
    }else(
      return ("No real-time data found in DB")
    )
  }
  
  plotRefresh <- function(){
    plot_data_response<-GET("http://35.234.150.106:8080/sensorData/getPlotData")
    plot_data<-read.table(text=content(plot_data_response,"text"), sep =",", header = TRUE, stringsAsFactors = FALSE)
    plot_data
    basket <- cbind(plot_data$xcoor, plot_data$ycoor, plot_data$zcoor,plot_data$xaxis,plot_data$yaxis,plot_data$zaxis)
    zoo.basket <- as.zoo(basket)
    tsRainbow <- rainbow(ncol(zoo.basket))
    plot(x = zoo.basket, ylab = "Sensor Values", main = "Sensor Values",
         col = tsRainbow, screens = 1)
    legend(x = "topleft", legend = c("xcoor", "ycoor", "zcoor", "xaxis", "yaxis","zaxis"), 
           lty = 1,col = tsRainbow)
    
  }
  
}
shinyApp(ui = ui, server = server)