source("./Scripts/Frontend/Data Visualization Tab/Plot Tab.R", local = TRUE)

dataVisualizeTab <- tabPanel(
  title = "Visualize Data",
  
  # UI Elements
  titlePanel("Visualize Data"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(
        outputId = "visualInput" 
      )
    ),
    
    mainPanel(
      plotOutput(
        outputId = "visualView"
      )
    )
    
  )
)