tifUploadNav <- tabPanel(
  title = "Upload a TIF or IMG File",
  
  shinyFilesButton(
    id = "layerUpload",
    label = "Find .tif or .img File",
    title = "Select the .tif or .img File You Want to Import",
    multiple = FALSE,
  ),
  textOutput(
    outputId = "layerFileDisplay"
  ),
  
  br(),
  
  radioButtons(
    inputId = "layerPurpose",
    label = "What Data Does this File Display?",
    choices = {
      append(
        unique(
          read.csv(
            paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/")
          )$Data.Type
        ), 
        c("Other")
      )
    },
    selected = character(0)
  ),
  
  conditionalPanel(
    condition = "input.layerPurpose == 'Other'",
    textInput(
      inputId = "layerPurposeText",
      label = "Please enter the purpose of the data here"
    )
  ),
  
  br(),
  
  checkboxInput(
    inputId = "layerDateRangeCheck",
    label = "Valid Under a Date Range?",
    value = FALSE
  ),
  conditionalPanel(
    condition = "input.layerDateRangeCheck",
    dateRangeInput(
      inputId = "layerDateRange",
      label = "Enter the Date Range in which this TIF file is valid."
    )
  ),
  
  br(),
  
  actionButton(
    inputId = "layerSubmit",
    label = "Import File"
  ),
  
  br(),
  br(),
  
  # Where console output messages are sent
  textOutput(
    outputId = "layerConsole"
  )
  
  
)
