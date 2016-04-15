
library(googlesheets)
library(shiny)
library(ggplot2)
library(timeDate)
library(zoo)

# read in and pre-process the data
source("helpers.R")
# source("readAndPreProcessData - googlesheets.R")

# server.R

shinyServer(
  function(input, output) {
    financials_combined <- NULL
    
    observeEvent(input$updateData, {
      source("readAndPreProcessData - googlesheets.R")
    })

    output$transactions <- renderPlot({
      ggplot(financials_combined, aes(x = date, y = inflow)) + geom_line()
    })

  }
)