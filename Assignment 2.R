library(ggplot2)
library(shiny)
library(leaflet)

myData <- read.csv("assignment-02-data-formated.csv") 
myData$value<-as.numeric(sub("%","",as.character(myData$value)))
myData$location<-reorder(myData$location,-myData$latitude)

ui<-shinyUI(fluidPage( 
  
  headerPanel("Coral Bleaching"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cor", "1.Select coral type:",
                  c("all", "blue corals","sea fans","sea pens","soft corals","hard corals"),selected = "all"),
      checkboxInput("Smoother", "Show smoother"),
      radioButtons("choice","2.Select smoother choice",choices=c("lm","glm","gam","loess"))
    ),
    
    mainPanel(
              tabsetPanel(
                tabPanel("Plot", plotOutput("myPlot"), leafletOutput("myMap", height = 500))
              )
              )
  )
))

server<-function(input, output){
  
  colorpal<- scale_color_manual(values = c("site03"="purple",
                                          "site01"="blue",
                                          "site05"="green",
                                          "site07"="yellow",
                                          "site08"="orange",
                                          "site06"="red",
                                          "site02"="black",
                                          "site04"="grey"))

  
  
  cortype <- reactive({paste(input$cor)})
  smoother <- reactive({paste(input$choice)})
  output$myPlot <- renderPlot(
    {
      if(cortype()!="all")
      {
        coralData <- myData[myData$coralType==cortype(),]
      }
      else
      {
        coralData <- myData
      }
      p <- ggplot(data=coralData, aes(year, value, color = location))+
        geom_point()+
        colorpal+
        ggtitle("The Total of Coral Bleaching (%) in Great Barrier Reef Between 2010-2017")+
        xlab("year")+ylab("percent bleaching")+
        facet_grid(location~coralType)
      if (input$Smoother==FALSE){print(p)}
      else if(input$Smoother==TRUE)
      {
        p + geom_smooth(method = smoother())
      }
    }
  )
  
  
  output$myMap<-renderLeaflet({
    if(cortype()!="all")
    {
      myData <- myData[myData$coralType==cortype(),]
    }
    myData <- aggregate(value ~ location+ latitude + longitude, FUN = mean, data = myData)
    
    pal<-colorFactor(palette = c("purple","blue","green","yellow","orange","red","black","grey"),myData$location)
    
    leaflet(data=myData) %>%
      addTiles() %>%
      addCircleMarkers(~longitude,~latitude,
                 radius = ~value*0.4,
                 color = ~pal(location),
                 label = ~paste(as.character(round(value,2), nsmall = 2), "%"),
                 labelOptions = labelOptions(noHide = T)) %>%
      addLegend("topright", pal = pal, values = ~location, title = "Location")
  })
}


shinyApp(ui = ui, server = server)
