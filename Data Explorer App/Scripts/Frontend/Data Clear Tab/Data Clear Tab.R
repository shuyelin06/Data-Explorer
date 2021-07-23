dataClearTab <- tabPanel(
  # Title of Tab 
  title = "Clear Data",
  
  # UI Elements
  titlePanel("Clear Data"),
  
  actionButton(
    inputId = "clearDataButton", 
    label = "Clear Existing Data"
    ),
  
  actionButton(
    inputId = "clearSeasonDefButton",
    label = "Clear Season Definitions"
  )
)
