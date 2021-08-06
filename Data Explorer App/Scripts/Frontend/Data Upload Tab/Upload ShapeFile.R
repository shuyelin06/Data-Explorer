sfUploadNav <- tabPanel(
  title = "Upload a Shape File", 
  
  sidebarLayout(
    sidebarPanel(
      shinyFilesButton(
        id = "sfFileUpload",
        label = "Find .shp File",
        title = "Select the .shp File You Want to Import",
        multiple = FALSE,
      ),
      
      br(),
      
      textOutput(
        outputId = "sfFileDisplay"
      )
    ),
    mainPanel(
      uiOutput(
        outputId = "sfColSelect"
      ),
      
      # Select Tmax
      sliderInput(
        inputId = "sfTmax",
        label = "Select a Tmax for Bursts (in hours)",
        min = 1,
        max = 24,
        value = 7
      ),
      
      # Load Data
      actionButton(
        inputId = "sfSubmit",
        label = "Upload Data"
      )
    )
  )
  
)