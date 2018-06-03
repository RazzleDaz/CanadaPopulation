# Packages ----
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(shinythemes)

#Data-------
PopData <- readRDS("PopData.rds")



#Body

ui = fluidPage(theme = shinytheme("cosmo"),
  
  #  Title
  titlePanel("Canada Population Distribution By Age" ),
  
  # Sidebar with slider and controls for animation
  sidebarLayout(
    
    # sidebar with slider
    sidebarPanel(
      #drop down for region
      selectInput(inputId = "selReg", "Region", choices= unique(PopData$Location), selected = 'Canada'),
      
      # Slider with looping
      sliderInput("selYear", "Year", min = min(PopData$Year), max = max(PopData$Year), max(PopData$Year), step = 1, 
                  sep = "", animate=animationOptions(interval=500, loop = FALSE,
                                           playButton = NULL, pauseButton = NULL)),
      textOutput(outputId = "totPop")
    ),
    
  # Show the animated graph
  mainPanel(
    plotOutput(outputId="pop_plot", height=800)
  )
  )
)



server <- function(input, output, session) {
  # Reactive expression to create data frame and graph
  
  aniGraph <- reactive({
    
    # subset the data
    
    Sub_PopData <- filter(PopData, Location==input$selReg)
    
    
    #Determine the axis variables
    iBar <- ceiling(max(abs(Sub_PopData$Population)/40))*10
    tBar <- iBar*4
    
    
    #subset the data for the year for the chart itself
    Sub_PopData <- filter(PopData, Location==input$selReg, Year==input$selYear)
    
    # create the graph
    plot(Sub_PopData %>% 
           ggplot(aes(x = Numeric.Age, y = Population, group = Sex.Type, fill = Sex.Type)) +
           geom_bar(stat = "identity", width = .5) +
           coord_flip() +
           scale_y_continuous(limits = c(-tBar,tBar), breaks = seq(-tBar, tBar, iBar), 
                              labels = comma(abs(seq(-tBar, tBar, iBar)))) +
           scale_x_continuous(limits= c(0,100), breaks = seq(0,100,10),
                              labels = seq(0,100,10)) + 
           geom_hline(color = "black", linetype='dashed', yintercept = seq(-tBar, tBar, iBar)) +
           labs(x = "Age", y = "Total Population") +
           theme(legend.position = "bottom",
                 legend.title = element_blank(),
                 plot.title = element_text(hjust = 0.5),
                 panel.background = element_rect(fill =  "white")) 
         )
  })
  
  totPopCalc <- reactive({
    temp_PopData <- filter(PopData, Location==input$selReg, Year==input$selYear)
    comma(sum(abs(temp_PopData$Population)))
  })
  
  # Show Graph
  output$pop_plot <- renderPlot({
    aniGraph()
  })
  
  #Show total Population
  output$totPop <-
    renderText({paste("Total Population: ", totPopCalc())})
}

shinyApp(ui, server)
