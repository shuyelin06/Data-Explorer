layerDefineNav <- tabPanel(
  # Title of Tab 
  title = "Continuous vs Discrete",
  
  # UI Elements
  titlePanel("Define Continuous and Discrete Data"),
  
  uiOutput(
    outputId = "layerDefineOutput"
  ),
  
  actionButton(
    inputId = "layerDefineSave",
    label = "Load Data"
  )
)