dbfUploadNav <- tabPanel(
  title = "Upload a DBF File", 
  
  fileInput(
    inputId = "dbfFileUpload", 
    label = "Upload A DBF File",
    accept = '.dbf'
  ),
  
  textInput(
    inputId = "dbfIdInput",
    label = "Name of ID Column"
  ),
  
  textInput(
    inputId = "dbfDateInput",
    label = "Name of Date Column"
  ),
  
  actionButton(
    inputId = "dbfSubmitButton",
    label = "Upload Data"
  )
)