# ui.R

shinyUI(fluidPage(
  titlePanel("Inflow and Outflow Summary"),

  sidebarLayout(
    sidebarPanel(
      helpText("Shiny app to help visualise my financial spending."),
      
      uiOutput("dateControls"),
      
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
        condition = "input.financesPanels == 'panelExpenditure'",
        actionButton("bar","Bar"),
        br(),
        br()
      ),
      
      actionButton("updateData", "Refresh Data")
    ),

    mainPanel(
      tabsetPanel(id = "financesPanels",
        tabPanel(title = "Inflow/Outflow", value = "panelInOut",
          plotOutput("transactions"),
          br(),
          textOutput("summaryTotalsText"),
          br(),
          DT::dataTableOutput("summaryTotals"),
          br(),
          textOutput("summaryAveragesText"),
          br(),
          DT::dataTableOutput("summaryAverages")
        ),
        tabPanel(title = "Expenditure Breakdown", value = "panelExpenditure",
          textOutput("textPointer")
        )
      )
    )
  )
))