sfUploadNav <- tabPanel(
  title = "Upload a Shape File", 
  
  # Select a .shp File
  shinyFilesButton(
    id = "sfFileUpload",
    label = "Find .shp File",
    title = "Select the .shp File You Want to Import",
    multiple = FALSE,
  ),
  textOutput(
    outputId = "sfFileDisplay"
  ),
  
  uiOutput(
    outputId = "sfColSelect"
  ),
  
  br(),
  
  # Select Tmax
  sliderInput(
    inputId = "sfTmax",
    label = "Select a Tmax for Bursts (in hours)",
    min = 1,
    max = 24,
    value = 7
  ),
  
  br(),
  
  # Load Data
  actionButton(
    inputId = "sfSubmit",
    label = "Upload Data"
  )
)