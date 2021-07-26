dataSelectNav <- tabPanel(
  # Title of Tab 
  title = "Retrieve Data",
  
  # UI Elements
  titlePanel("Select Data to be Displayed"),
  
  uiOutput(
    outputId = "selectAddData"
  ),
  
  actionButton(
    inputId = "selectDataRetrieve",
    label = "Load Data"
  )
)