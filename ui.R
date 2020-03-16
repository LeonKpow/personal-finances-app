# load packages required for this project
library(plotly)

# ui.R

shinyUI(fluidPage(
  titlePanel("Inflow and Outflow Summary"),

  sidebarLayout(
    sidebarPanel(
      helpText("Shiny app to help visualise my financial spending."),
      
      uiOutput("dateControls"),
      
      actionButton("resetDateRanges", "Reset Date Ranges"),
      
      br(),
      br(),
      
      conditionalPanel(
        condition = "input.financesPanels == 'panelInOut'",
        radioButtons("averagingPeriod",
                     "Select a period to average over:",
                     c("Daily" = 1,
                       "Weekly" = 7,
                       "Fortnightly" = 14,
                       "Monthly" = 30.42,
                       "Yearly" = 365)
                     ),
        br(),
        br()
      ),
      
      conditionalPanel(
        condition = "input.financesPanels == 'panelRevExp'",
        radioButtons("groupingLevel",
                     "Select a grouping level:",
                     c("Level 1" = "category3",
                       "Level 2" = "category2",
                       "Level 3" = "category1",
                       "Counterparty" = "Counterparty")
        ),
        br(),
        br()
      ),
      
      actionButton("updateData", "Refresh Data")
    ),

    mainPanel(
      tabsetPanel(id = "financesPanels",
        tabPanel(title = "Inflow/Outflow", value = "panelInOut",
          plotlyOutput("transactions"),
          br(),
          textOutput("summaryTotalsText"),
          br(),
          DT::dataTableOutput("summaryTotals"),
          br(),
          textOutput("summaryAveragesText"),
          br(),
          DT::dataTableOutput("summaryAverages")
        ),
        tabPanel(title = "Revenue and Expenditure Breakdown", value = "panelRevExp",
          textOutput("textPointer")
        )
      )
    )
  )
))