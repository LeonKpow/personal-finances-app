# load packages required for this project
library(googledrive) # to more easily access files on my google drive
library(googlesheets4) # to read spreadsheet data from my google drive
library(shiny) # to create/run this app
library(ggplot2) # to produce required plots
library(dplyr) # to conveniently wrangle data
library(lubridate) # for convenient date manipulations
library(DT) # for better presentation of tables in Shiny output

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
    
    #Subset data based on date ranges
    dataToPlot <- reactive({
      plotData <- financials_combined[(financials_combined$date >= input$financialsDateRange[1]) & (financials_combined$date <= input$financialsDateRange[2]), ]
      plotData$cumulativeNetInflow <- cumsum(plotData$netInflow)
      #totals <- data.frame(sum(plotData$inflow), sum(plotData$outflow), sum(plotData$netInflow))
      #colnames(totals) <- c("inflow", "outflow", "netInflow")
      
      #return(list(el1 = plotData, el2 = totals))
      return(plotData)
    })
    
    #calculate data summeries
    dataSummaries <- reactive({
      plotData <- dataToPlot()
      totals <- data.frame(sum(plotData$inflow), sum(plotData$outflow), sum(plotData$netInflow))
      colnames(totals) <- c("inflow", "outflow", "netInflow")
      
      daysInPeriod <- as.numeric(input$financialsDateRange[2] - input$financialsDateRange[1])
      averages <- totals * (as.numeric(input$averagingPeriod) / daysInPeriod)
      return(list(totals = totals, averages = averages))
    })

    #Plot Inflow/Outflows
    output$transactions <- renderPlot({
      ggplot(dataToPlot()) +
      geom_bar(aes(date, netInflow, fill = transactionType), stat = "identity", position = "dodge") + 
      geom_line(aes(date, cumulativeNetInflow))
    })
    
    #Summarise totals and print
    output$summaryTotalsText <- renderText(
      paste(
        "Totals over the period from ",
        input$financialsDateRange[1],
        " to ",
        input$financialsDateRange[2],
        ":",
        sep = ""
      )
    )
    
    output$summaryTotals <- DT::renderDataTable({
      DT::datatable(dataSummaries()$totals,
                    rownames = FALSE,
                    options = list(info = FALSE,
                                   ordering = FALSE,
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE)) %>%
        formatCurrency(c('inflow', 'outflow', 'netInflow'))
    })
    
    output$summaryAveragesText <- renderText(
      paste(
        "Averages over the period from ",
        input$financialsDateRange[1],
        " to ",
        input$financialsDateRange[2],
        ":",
        sep = ""
      )
    )
    
    output$summaryAverages <- DT::renderDataTable({
      DT::datatable(dataSummaries()$averages,
                    rownames = FALSE,
                    options = list(info = FALSE,
                                   ordering = FALSE,
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE)) %>%
        formatCurrency(c('inflow', 'outflow', 'netInflow'))
    })
    
    
    output$textPointer <- renderText("blah blah blah")

  }
)