# User Interface (Frontend) Main Code File

# Importing each of the main tabs - upload, definition, and visualization
source("./Scripts/Frontend/Data Upload Tab/Data Upload Tab.R")
source("./Scripts/Frontend/Data Definition Tab/Data Definition Tab.R")
source("./Scripts/Frontend/Data Visualization Tab/Data Visualization Tab.R")

ui <- fluidPage(
  # Set the App Title
  titlePanel("Merkle Research Group - Data Explorer"),
  
  tabsetPanel(
    type = "tabs",
    dataUploadTab,
    dataDefineTab,
    dataVisualizeTab
  )
  
)