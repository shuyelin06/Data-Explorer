source("./Scripts/Frontend/Data Visualization Tab/Plot Tab.R")

dataVisualizeTab <- tabPanel(
  title = "Visualize Data",
  
  # UI Elements
  titlePanel("Visualize Data"),
  
  navlistPanel(
    plotDataNav
  )
)