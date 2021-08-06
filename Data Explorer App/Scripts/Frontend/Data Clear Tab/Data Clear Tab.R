source("./Scripts/Frontend/Data Clear Tab/Clear Migration Data.R", local = TRUE)
source("./Scripts/Frontend/Data Clear Tab/Clear Biological Year.R", local = TRUE)
source("./Scripts/Frontend/Data Clear Tab/Clear Raster Layers.R", local = TRUE)

dataClearTab <- tabPanel(
  # Title of Tab 
  title = "Clear Data",
  
  # UI Elements
  titlePanel("Clear Data"),
  
  navlistPanel(
    clearMigrationPanel,
    clearBioYearPanel,
    clearRasterPanel
  )
)
