# User Interface (Frontend) Main Code File

# Importing each of the main tabs - upload, definition, and visualization
source("./Scripts/Frontend/Data Upload Tab/Data Upload Tab.R", local = TRUE)
source("./Scripts/Frontend/Data Definition Tab/Data Definition Tab.R", local = TRUE)
source("./Scripts/Frontend/Data Visualization Tab/Data Visualization Tab.R", local = TRUE)
source("./Scripts/Frontend/Data Clear Tab/Data Clear Tab.R", local = TRUE)

ui <- fluidPage(
  # Set the App Title
  titlePanel("Merkle Research Group - Data Explorer"),
  
  tabsetPanel(
    type = "tabs",
    
    dataUploadTab,
    dataDefineTab,
    dataVisualizeTab,
    dataClearTab,
    
    id = "dataTabs"
  )
  
)