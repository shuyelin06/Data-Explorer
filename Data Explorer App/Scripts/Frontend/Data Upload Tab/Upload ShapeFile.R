sfUploadNav <- tabPanel(
  title = "Upload a Shape File", 
  
  fileInput(
    inputId = "sfFileUpload", 
    label = "Upload A DBF File",
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
  
  radioButtons(
    inputId = "sfRadioButton",
    label = "Unique Animals?",
    choices = c(
      "Yes" = "yes",
      "No" = "no"
    )
  ),
  
  actionButton(
    inputId = "sfSubmitButton",
    label = "Upload Data"
  )
)