# Import the sub-panels for the tab

source("./Scripts/Frontend/Data Upload Tab/Upload ShapeFile.R")
source("./Scripts/Frontend/Data Upload Tab/Upload TIF File.R")

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
