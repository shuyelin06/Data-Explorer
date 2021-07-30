# Serverside Code Main File
server <- function(input, output, session) {
  # Reactive File Reader for the RasterLayer data
  layerData <- reactiveFileReader(5000, session, paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"), read.csv)
  
  # Reactive value for the migration data (if the data is updated, the data visualization tab will automatically update)
  migrationData <- reactiveVal()
  
  # Server Code for the Data Upload Tab
  source("./Scripts/Backend/Data Upload.R", local = TRUE)
  
  # Server Code for the Data Definition Tab
  source("./Scripts/Backend/Data Definition.R", local = TRUE)
  
  # Server code for the Data Visualization Tab
  source("./Scripts/Backend/Data Visualization.R", local = TRUE)
  
  # Server code for the Data Clear Tab
  source("./Scripts/Backend/Data Clear.R", local = TRUE)
}