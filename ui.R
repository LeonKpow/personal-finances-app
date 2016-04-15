# ui.R

shinyUI(fluidPage(
  titlePanel("Summary of Finances"),

  sidebarLayout(
    sidebarPanel(
      helpText("Shiny app to help visualise my financial spending."),
      
      actionButton("updateData", "Refresh Data")
    ),

    mainPanel(plotOutput("transactions"))
  )
))