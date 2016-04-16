# ui.R

shinyUI(fluidPage(
  titlePanel("Inflow and Outflow Summary"),

  sidebarLayout(
    sidebarPanel(
      helpText("Shiny app to help visualise my financial spending."),
      
      uiOutput("dateControls"),
      
      br(),
      
      conditionalPanel(
        "$('li.active a').first().html()==='Expenditure Breakdown'",
        actionButton("bar","Bar"),
        
        br()
      ),
      
      actionButton("updateData", "Refresh Data")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Inflow/Outflow",
          plotOutput("transactions"),
          br(),
          textOutput("summaryTotalsText"),
          br(),
          tableOutput("summaryTotals")
        ),
        tabPanel("Expenditure Breakdown", textOutput("textPointer"))
      )
    )
  )
))