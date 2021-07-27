# Import the sub-panels for the tab

source("./Scripts/Frontend/Data Upload Tab/Upload ShapeFile.R", local = TRUE)
source("./Scripts/Frontend/Data Upload Tab/Upload TIF File.R", local = TRUE)

dataUploadTab <- tabPanel(
  # Title of Tab 
  title = "Upload Data",
  
  # UI Elements
  titlePanel("Upload Data"),
  
  navlistPanel(
    sfUploadNav,
    tifUploadNav
  )
)
