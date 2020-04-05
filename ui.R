# load packages required for this project
library(plotly)

# ui.R

shinyUI(fluidPage(
  titlePanel("Shiny app to help understand my income and spending patterns."),

  sidebarLayout(
    sidebarPanel(
      helpText("Direct analysis using these controls."),
      
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
                       "Yearly" = 365),
                     selected = 1
                     ),
        br(),
        br()
      ),
      
      conditionalPanel(
        condition = "(input.financesPanels == 'panelRevBreakdown') || (input.financesPanels == 'panelExpBreakdown')",
        radioButtons("groupingLevel",
                     "Select a grouping level:",
                     c("Level 1" = "category3",
                       "Level 2" = "category2",
                       "Level 3" = "category1",
                       "Counterparty" = "counterparty"),
                     selected = "category3"
                     ),
        br(),
        strong("Counterparty breakdown:"),
        checkboxInput("counterpartyDisplayChoice", label = "Display counterparty breakdown", value = FALSE),
        br(),
        br()
      ),
      
      conditionalPanel(
        condition = "(input.financesPanels == 'panelRevBreakdown')",
        uiOutput("revenueFurtherFilteringSelections")
      ),
      
      conditionalPanel(
        condition = "(input.financesPanels == 'panelExpBreakdown')",
        uiOutput("expenseFurtherFilteringSelections")
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
        tabPanel(title = "Revenue Breakdown", value = "panelRevBreakdown",
          plotlyOutput("revenueTimeSeries"),
          br(),
          strong("Breakdown of revenues over requested period:"),
          br(),
          br(),
          DT::dataTableOutput("revenuesTable"),
          br(),
          conditionalPanel(condition = "input.counterpartyDisplayChoice == 1",
                           strong("Breakdown of revenues over requested period by counterparty:"),
                           br(),
                           br(),
                           DT::dataTableOutput("revenuesCounterpartyTable"),
                           br(),
                           br()),
          conditionalPanel(condition = "input.revenueFurtherFiltering.length > 0",
                           strong("Sub-catgeory breakdown of revenues"),
                           plotlyOutput("furtherFilteredRevenueTimeSeries"),
                           br(),
                           br(),
                           DT::dataTableOutput("furtherFilteredRevenuesTable"),
                           br(),
                           br()),
          conditionalPanel(condition = "(input.revenueFurtherFiltering.length > 0) & (input.counterpartyDisplayChoice == 1)",
                           strong("Sub-catgeory breakdown of revenues by counterparty"),
                           br(),
                           br(),
                           DT::dataTableOutput("furtherFilteredRevenuesCounterpartyTable"),
                           br(),
                           br())
        ),
        tabPanel(title = "Expenditure Breakdown", value = "panelExpBreakdown",
          plotlyOutput("expensesTimeSeries"),
          br(),
          strong("Breakdown of expenses over requested period:"),
          br(),
          br(),
          DT::dataTableOutput("expensesTable"),
          br(),
          conditionalPanel(condition = "input.counterpartyDisplayChoice == 1",
                           strong("Breakdown of expenses over requested period by counterparty:"),
                           br(),
                           br(),
                           DT::dataTableOutput("expensesCounterpartyTable"),
                           br(),
                           br()),
          conditionalPanel(condition = "input.expenseFurtherFiltering.length > 0",
                           strong("Sub-catgeory breakdown of expenses"),
                           plotlyOutput("furtherFilteredExpensesTimeSeries"),
                           br(),
                           br(),
                           DT::dataTableOutput("furtherFilteredExpensesTable"),
                           br(),
                           br()),
          conditionalPanel(condition = "(input.expenseFurtherFiltering.length > 0) & (input.counterpartyDisplayChoice == 1)",
                           strong("Sub-catgeory breakdown of expenses by counterparty"),
                           br(),
                           br(),
                           DT::dataTableOutput("furtherFilteredExpensesCounterpartyTable"),
                           br(),
                           br())
        )
      )
    )
  )
))