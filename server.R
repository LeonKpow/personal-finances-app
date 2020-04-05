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
#source("readAndPreProcessData.R")

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
    
    #Code to dynamically set further revenue filtering selections
    output$revenueFurtherFilteringSelections <- renderUI({
      selectInput("revenueFurtherFiltering",
                  "Select categories to further filter the data on:",
                  choices = revenueAndExpenseData()$revenuesCategoriesList,
                  multiple = TRUE
                  )
    })
    
    #Code to dynamically set further expense filtering selections
    output$expenseFurtherFilteringSelections <- renderUI({
      selectInput("expenseFurtherFiltering",
                  "Select categories to further filter the data on:",
                  choices = revenueAndExpenseData()$expensesCategoriesList,
                  multiple = TRUE
      )
    })
    
    #Subset data based on date ranges and produce data for inflow/outflow plot
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
      
      revenuesCounterpartyBreakdown <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        group_by(counterparty) %>%
        filter(netInflow > 0) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(counterparty = counterparty, revenues = netInflow, revenueProportion = netInflowProportion)
      
      revenuesCategoriesList <- revenuesBreakdown %>%
        distinct(category)
      
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
      
      expensesCounterpartyBreakdown <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        group_by(counterparty) %>%
        filter(netInflow < 0) %>%
        mutate(netInflow = -netInflow) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(counterparty = counterparty, expenses = netInflow, expenseProportion = netInflowProportion)
      
      expensesCategoriesList <- expensesBreakdown %>%
        distinct(category)
      
      expensesMonthly <- aggregatedMonthlyData %>%
        filter(netInflow < 0) %>%
        mutate(netInflow = -netInflow) %>%
        group_by(monthOfTransaction) %>%
        summarize(netInflow = sum(netInflow)) %>%
        rename(month = monthOfTransaction, expenses = netInflow)
      
      return(list(revenuesBreakdown = revenuesBreakdown,
                  revenuesCounterpartyBreakdown = revenuesCounterpartyBreakdown,
                  revenuesCategoriesList = revenuesCategoriesList,
                  revenuesMonthly = revenuesMonthly,
                  expensesBreakdown = expensesBreakdown,
                  expensesCounterpartyBreakdown = expensesCounterpartyBreakdown,
                  expensesCategoriesList = expensesCategoriesList,
                  expensesMonthly = expensesMonthly))
    })
    
    #Produce further filtered aggregations of revenues and expenses
    furtherFilteredRevenueData <- reactive({
      categoryLevel <- as.name(input$groupingLevel)
      
      if (input$groupingLevel == c("category3")) {
        furtherGroupingLevel <- c("category2")
      } else if (input$groupingLevel == c("category2")) {
        furtherGroupingLevel <- c("category1")
      } else if (input$groupingLevel == c("category1")) {
        furtherGroupingLevel <- c("counterparty")
      } else if (input$groupingLevel == c("counterparty")) {
        furtherGroupingLevel <- c("counterparty")
      }
      furtherGroupingLevel <- as.name(furtherGroupingLevel)
      
      aggregatedMonthlyData <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        filter(!!categoryLevel %in% input$revenueFurtherFiltering) %>%
        group_by(monthOfTransaction, !!furtherGroupingLevel) %>%
        summarize(netInflow = sum(netInflow))

      revenuesBreakdown <- aggregatedMonthlyData %>%
        filter(netInflow > 0) %>%
        group_by(!!furtherGroupingLevel) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(category = !!furtherGroupingLevel, revenues = netInflow, revenueProportion = netInflowProportion)

      revenuesCounterpartyBreakdown <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        filter(!!categoryLevel %in% input$revenueFurtherFiltering) %>%
        group_by(counterparty) %>%
        filter(netInflow > 0) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(counterparty = counterparty, revenues = netInflow, revenueProportion = netInflowProportion)

      revenuesMonthly <- aggregatedMonthlyData %>%
        filter(netInflow > 0) %>%
        group_by(monthOfTransaction) %>%
        summarize(netInflow = sum(netInflow)) %>%
        rename(month = monthOfTransaction, revenues = netInflow)
      
      return(list(furtherRevenuesBreakdown = revenuesBreakdown,
                  furtherRevenuesCounterpartyBreakdown = revenuesCounterpartyBreakdown,
                  furtherRevenuesMonthly = revenuesMonthly))
    })
    
    furtherFilteredExpenseData <- reactive({
      categoryLevel <- as.name(input$groupingLevel)
      
      if (input$groupingLevel == c("category3")) {
        furtherGroupingLevel <- c("category2")
      } else if (input$groupingLevel == c("category2")) {
        furtherGroupingLevel <- c("category1")
      } else if (input$groupingLevel == c("category1")) {
        furtherGroupingLevel <- c("counterparty")
      } else if (input$groupingLevel == c("counterparty")) {
        furtherGroupingLevel <- c("counterparty")
      }
      furtherGroupingLevel <- as.name(furtherGroupingLevel)
      
      aggregatedMonthlyData <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        filter(!!categoryLevel %in% input$expenseFurtherFiltering) %>%
        group_by(monthOfTransaction, !!furtherGroupingLevel) %>%
        summarize(netInflow = sum(netInflow))
      
      expensesBreakdown <- aggregatedMonthlyData %>%
        filter(netInflow < 0) %>%
        mutate(netInflow = -netInflow) %>%
        group_by(!!furtherGroupingLevel) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(category = !!furtherGroupingLevel, expenses = netInflow, expenseProportion = netInflowProportion)
      
      expensesCounterpartyBreakdown <- financials_combined %>%
        filter((date >= input$financialsDateRange[1]) & (date <= input$financialsDateRange[2])) %>%
        filter(!!categoryLevel %in% input$expenseFurtherFiltering) %>%
        group_by(counterparty) %>%
        filter(netInflow < 0) %>%
        mutate(netInflow = -netInflow) %>%
        summarize(netInflow = sum(netInflow)) %>%
        arrange(desc(netInflow)) %>%
        mutate(netInflowProportion = netInflow / sum(netInflow)) %>%
        rename(counterparty = counterparty, expenses = netInflow, expenseProportion = netInflowProportion)
      
      expensesMonthly <- aggregatedMonthlyData %>%
        filter(netInflow < 0) %>%
        mutate(netInflow = -netInflow) %>%
        group_by(monthOfTransaction) %>%
        summarize(netInflow = sum(netInflow)) %>%
        rename(month = monthOfTransaction, expenses = netInflow)
      
      return(list(furtherExpensesBreakdown = expensesBreakdown,
                  furtherExpensesCounterpartyBreakdown = expensesCounterpartyBreakdown,
                  furtherExpensesMonthly = expensesMonthly))
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
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
                    ) %>%
        formatCurrency(c("revenues")) %>%
        formatPercentage(c("revenueProportion"), digits = 2)
    })
    
    #Print revenue counterparty breakdown table
    output$revenuesCounterpartyTable <- DT::renderDataTable({
      DT::datatable(revenueAndExpenseData()$revenuesCounterpartyBreakdown,
                    colnames = c("Counterparty", "Revenues", "Proportion of Total"),
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
      ) %>%
        formatCurrency(c("revenues")) %>%
        formatPercentage(c("revenueProportion"), digits = 2)
    })
    
    #Plot further filtered Revenues
    output$furtherFilteredRevenueTimeSeries <- renderPlotly({
      plot_ly(furtherFilteredRevenueData()$furtherRevenuesMonthly,
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
    
    #Print further filtered revenue breakdown
    output$furtherFilteredRevenuesTable <- DT::renderDataTable({
      DT::datatable(furtherFilteredRevenueData()$furtherRevenuesBreakdown,
                    colnames = c("Category", "Revenues", "Proportion of Total"),
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
      ) %>%
        formatCurrency(c("revenues")) %>%
        formatPercentage(c("revenueProportion"), digits = 2)
    })
    
    #Print further filtered revenue breakdown by counterparty
    output$furtherFilteredRevenuesCounterpartyTable <- DT::renderDataTable({
      DT::datatable(furtherFilteredRevenueData()$furtherRevenuesCounterpartyBreakdown,
                    colnames = c("Counterparty", "Revenues", "Proportion of Total"),
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
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
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
      ) %>%
        formatCurrency(c("expenses")) %>%
        formatPercentage(c("expenseProportion"), digits = 2)
    })
    
    #Print expenses counterparty table
    output$expensesCounterpartyTable <- DT::renderDataTable({
      DT::datatable(revenueAndExpenseData()$expensesCounterpartyBreakdown,
                    colnames = c("Counterparty", "Expenses", "Proportion of Total"),
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
      ) %>%
        formatCurrency(c("expenses")) %>%
        formatPercentage(c("expenseProportion"), digits = 2)
    })
    
    #Plot further filtered Expenses
    output$furtherFilteredExpensesTimeSeries <- renderPlotly({
      plot_ly(furtherFilteredExpenseData()$furtherExpensesMonthly,
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
    
    #Print further filtered expense breakdown
    output$furtherFilteredExpensesTable <- DT::renderDataTable({
      DT::datatable(furtherFilteredExpenseData()$furtherExpensesBreakdown,
                    colnames = c("Category", "Expenses", "Proportion of Total"),
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
      ) %>%
        formatCurrency(c("expenses")) %>%
        formatPercentage(c("expenseProportion"), digits = 2)
    })
    
    #Print further filtered expense breakdown by counterparty
    output$furtherFilteredExpensesCounterpartyTable <- DT::renderDataTable({
      DT::datatable(furtherFilteredExpenseData()$furtherExpensesCounterpartyBreakdown,
                    colnames = c("Counterparty", "Expenses", "Proportion of Total"),
                    options = list(searching = FALSE,
                                   lengthMenu = list(c(10, 25, 50, 100, -1), list('10', '25', '50','100', 'All')))
      ) %>%
        formatCurrency(c("expenses")) %>%
        formatPercentage(c("expenseProportion"), digits = 2)
    })

  }
)