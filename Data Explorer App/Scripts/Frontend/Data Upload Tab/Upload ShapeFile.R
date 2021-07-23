sfUploadNav <- tabPanel(
  title = "Upload a Shape File", 
  
  fileInput(
    inputId = "sfFileUpload", 
    label = "Upload Files",
    multiple = TRUE,
    accept = c('.dbf', '.shp', '.sbn', '.sbx', '.shx', '.prj')
  ),
  
  textInput(
    inputId = "sfIdInput",
    label = "Name of ID Column"
  ),
  
  textInput(
    inputId = "sfDateInput",
    label = "Name of Date Column"
  ),
  
  checkboxInput(
    inputId = "sfUniqueCheck",
    label = "Unique?",
  ),
  
  actionButton(
    inputId = "sfSubmitButton",
    label = "Upload Data"
  )
)