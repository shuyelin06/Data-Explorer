tifUploadNav <- tabPanel(
  title = "Upload a TIF or IMG File",
  
  sidebarLayout(
    sidebarPanel(
      shinyFilesButton(
        id = "layerUpload",
        label = "Find .tif or .img Files",
        title = "Select Files You Want to Import",
        multiple = TRUE,
      ),
      
      br(),
      
      textOutput(
        outputId <- "layerConsole"
      )
    ),
    mainPanel(
      uiOutput(
        outputId = "defineLayerInfo"
      ),
      
      br(),
      
      actionButton(
        inputId = "layerSubmit",
        label = "Import File"
      )
    )
  )
 
)
