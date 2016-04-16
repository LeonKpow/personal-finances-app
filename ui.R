# ui.R

shinyUI(fluidPage(
  titlePanel("Inflow and Outflow Summary"),

  sidebarLayout(
    sidebarPanel(
      helpText("Shiny app to help visualise my financial spending."),
      
      uiOutput("dateControls"),
      
      actionButton("updateData", "Refresh Data")
    ),

    mainPanel(plotOutput("transactions"))
  )
))