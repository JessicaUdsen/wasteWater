# This is a sample Shiny app, where I look at wastewater/sewage data for a number
# of countries. Data source is here: https://www.kaggle.com/datasets/kkhandekar/oecd-waste-water-treatment/

library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel(h1("OECD Waste Water Treatment App"), 
                           windowTitle = "OECD Waste Water Treatment App"),
                fluidRow(
                  column(5, uiOutput("countrySelect")),
                  column(5, uiOutput("variableSelect"))
                ),
                fluidRow(
                  column(10, h2("Visualizations"))
                ),
                fluidRow(
                  column(10,
                         tabsetPanel(
                           tabPanel("Time Series Plot", plotlyOutput("tsPlot")),
                           tabPanel("Table", tableOutput("dataTable"))
                         )
                  )
                ),
                fluidRow(
                  column(10, h2("Variable Code Book"))
                ),
                fluidRow(
                  column(10, tableOutput("codeBook"))
                )
)


server <- function(input, output) {
  
  waterData <- read_csv('wasteWaterTreatment.csv')
  waterData$Year <- as.integer(waterData$Year)
  
  output$countrySelect <- renderUI({
    countries  <- unique(sort(waterData$Country))
    
    selectInput(inputId = 'Country',
                label = 'Select country',
                choices = countries, 
                multiple = TRUE, 
                selectize = TRUE)
    
  })
  
  output$variableSelect <- renderUI({
    variables <- unique(waterData$Variable)
    
    selectInput(inputId = 'Variable',
                label = 'Select variable',
                choices = variables, 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  dataPlotting <- reactive({
    waterData %>%
      select(Year, Variable, Country, PercentageValue) %>%
      filter(Country %in% input$Country) %>%
      filter(Variable %in% input$Variable) 
  })
  
  minYear <- reactive({
    if(is.null(input$Country) | is.null(input$Variable)){
      return(min(waterData$Year))
    }
    else{
      return(as.character(min(dataPlotting()$Year)))
    }
  })
  maxYear <- reactive({
    if(is.null(input$Country) | is.null(input$Variable)){
      return(max(waterData$Year))
    }
    else{
      return(as.character(max(dataPlotting()$Year)))
    }
  })
  
  output$tsPlot <- renderPlotly({
    plot_ly(dataPlotting(), x=~Year, y=~PercentageValue, 
            type="scatter",color=~Country, mode="lines") %>%
      layout(title = paste(paste(paste(input$Variable,
                                       minYear()), "-"), maxYear()),
             xaxis = list(title = "Year"), 
             yaxis = list(title = "Percentage Value"))
  })
  
  output$dataTable <- renderTable({
    if(nrow(dataPlotting()) == 0){
      return("Select one or more country and a variable")
    }
    else{ 
      dataPlotting()
    }
  })
  
  output$codeBook <- renderTable({
    waterData %>% 
      select(Variable, VariableDescription) %>%
      distinct()
  })
}


shinyApp(ui = ui, server = server)