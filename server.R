# load packages required for this project
library(googledrive) # to more easily access files on my google drive
library(googlesheets4) # to read spreadsheet data from my google drive
library(shiny) # to create/run this app
library(plotly) # to produce required plots
library(scales) # used for clean currency formatting in plot labels
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
    
    #Re-set the date ranges if the reset action button is clicked
    observeEvent(input$resetDateRanges, {
      output$dateControls <- renderUI({
        dateRangeInput("financialsDateRange",
                       "Date range to analyse financials over",
                       start = minDateInData,
                       end = maxDateInData)
      })
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
      plotData <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        group_by(date) %>%
        summarize(inflow = sum(inflow), outflow = sum(outflow), netInflow = sum(netInflow)) %>%
        arrange(date)
      plotData$cumulativeNetInflow <- cumsum(plotData$netInflow)
      return(plotData)
    })
    
    #Produce aggregations of revenues and expenses
    revenueAndExpenseData <- reactive({
      groupingLevel <- as.name(input$groupingLevel)
      aggregatedMonthlyData <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        group_by(monthOfTransaction, !!groupingLevel) %>%
        summarize(netInflow = sum(netInflow))
      
      revenuesBreakdown <- aggregatedMonthlyData %>%
        filter(netInflow > 0) %>%
        group_by(!!groupingLevel) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(category = !!groupingLevel, revenues = netInflow, revenueProportion = netInflowProportion)
      
      revenuesMonthly <- aggregatedMonthlyData %>%
        filter(netInflow > 0) %>%
        group_by(monthOfTransaction) %>%
        summarize(netInflow = sum(netInflow)) %>%
        rename(month = monthOfTransaction, revenues = netInflow)
      
      expensesBreakdown <- aggregatedMonthlyData %>%
        filter(netInflow < 0) %>%
        mutate(netInflow = -netInflow) %>%
        group_by(!!groupingLevel) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(category = !!groupingLevel, expenses = netInflow, expenseProportion = netInflowProportion)
      
      expensesMonthly <- aggregatedMonthlyData %>%
        filter(netInflow < 0) %>%
        mutate(netInflow = -netInflow) %>%
        group_by(monthOfTransaction) %>%
        summarize(netInflow = sum(netInflow)) %>%
        rename(month = monthOfTransaction, expenses = netInflow)
      
      return(list(revenuesBreakdown = revenuesBreakdown,
                  revenuesMonthly = revenuesMonthly,
                  expensesBreakdown = expensesBreakdown,
                  expensesMonthly = expensesMonthly))
    })
    
    #calculate data summeries
    dataSummaries <- reactive({
      plotData <- dataToPlot()
      totals <- dataToPlot() %>%
        summarise(inflow = sum(inflow), outflow = sum(outflow), netInflow = sum(netInflow))
      
      daysInPeriod <- as.numeric(input$financialsDateRange[2] - input$financialsDateRange[1])
      averages <- totals * (as.numeric(input$averagingPeriod) / daysInPeriod)
      return(list(totals = totals, averages = averages))
    })

    #Plot Inflow/Outflows
    output$transactions <- renderPlotly({
      plot_ly(dataToPlot(),
              x = ~date,
              y = ~cumulativeNetInflow,
              type = 'scatter',
              mode = 'lines',
              line = list(color = 'black'),
              hoverinfo = 'text',
              text = ~paste('Date:', date, '<br>',
                            'Cumulative Net Inflow:', dollar(cumulativeNetInflow), '<br>',
                            'Net Inflow:', dollar(netInflow), '<br>',
                            'Inflow:', dollar(inflow), '<br>',
                            'Outflow:', dollar(outflow))) %>%
        layout(xaxis = list(title = "Date"),
               yaxis = list(title = "Cumulative Inflow of Funds",
                            tickformat = "$,"))
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
    
    #Plot Revenues
    output$revenueTimeSeries <- renderPlotly({
      plot_ly(revenueAndExpenseData()$revenuesMonthly,
              x = ~month,
              y = ~revenues,
              type = 'bar',
              hoverinfo = 'text',
              text = ~paste(dollar(revenues), '<br>', month)) %>%
        layout(title = "Monthly Revenues",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Revenues",
                            tickformat = "$,"))
    })
    
    # output$revenuePlot <- renderPlotly({
    #   plot_ly(revenueAndExpenseData()$revenuesBreakdown,
    #           x = ~revenues,
    #           y = ~reorder(category, revenues),
    #           type = 'bar',
    #           orientation = 'h',
    #           hoverinfo = 'text',
    #           text = ~paste(category, '<br>',
    #                         dollar(revenues), '<br>',
    #                         percent(revenueProportion, accuracy = 0.01))) %>%
    #     layout(title = "Revenues",
    #            xaxis = list(title = "Revenues",
    #                         tickformat = "$,"),
    #            yaxis = list(title = "Category"))
    # })
    
    #Print revenue table
    output$revenuesTable <- DT::renderDataTable({
      DT::datatable(revenueAndExpenseData()$revenuesBreakdown,
                    colnames = c("Category", "Revenues", "Proportion of Total"),
                    options = list(searching = FALSE)
                    ) %>%
        formatCurrency(c("revenues")) %>%
        formatPercentage(c("revenueProportion"), digits = 2)
    })
    
    #Plot Expenses
    output$expensesTimeSeries <- renderPlotly({
      plot_ly(revenueAndExpenseData()$expensesMonthly,
              x = ~month,
              y = ~expenses,
              type = 'bar',
              hoverinfo = 'text',
              text = ~paste(dollar(expenses), '<br>', month)) %>%
        layout(title = "Monthly Expenses",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Expenses",
                            tickformat = "$,"))
    })
    
    # output$expensesPlot <- renderPlotly({
    #   plot_ly(revenueAndExpenseData()$expensesBreakdown,
    #           x = ~expenses,
    #           y = ~reorder(category, expenses),
    #           type = 'bar',
    #           orientation = 'h',
    #           hoverinfo = 'text',
    #           text = ~paste(category, '<br>',
    #                         dollar(expenses), '<br>',
    #                         percent(expenseProportion, accuracy = 0.01))) %>%
    #     layout(title = "Expenses",
    #            xaxis = list(title = "Expenses",
    #                         tickformat = "$,"),
    #            yaxis = list(title = "Category"))
    # })
    
    #Print expenses table
    output$expensesTable <- DT::renderDataTable({
      DT::datatable(revenueAndExpenseData()$expensesBreakdown,
                    colnames = c("Category", "Expenses", "Proportion of Total"),
                    options = list(searching = FALSE)
      ) %>%
        formatCurrency(c("expenses")) %>%
        formatPercentage(c("expenseProportion"), digits = 2)
    })

  }
)