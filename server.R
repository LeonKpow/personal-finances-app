
library(googlesheets)
library(shiny)
library(ggplot2)
library(timeDate)
library(zoo)

# read in and pre-process the data
source("readAndPreProcessData.R")

# server.R

shinyServer(
  function(input, output) {
    
    #Re-read the data if the action button is clicked
    observeEvent(input$updateData, {
      source("readAndPreProcessData.R")
    })
    
    #Code to dynamically set input date ranges
    output$dateControls <- renderUI({
      dateRangeInput("financialsDateRange",
                     "Date range to analyse financials over",
                     start = minDateInData,
                     end = maxDateInData)
    })

    output$transactions <- renderPlot({
      ggplot(financials_combined) +
      geom_bar(aes(date, netInflow, fill = transactionType), stat = "identity", position = "dodge") + 
      geom_line(aes(date, cumulativeNetInflow))
    })

  }
)