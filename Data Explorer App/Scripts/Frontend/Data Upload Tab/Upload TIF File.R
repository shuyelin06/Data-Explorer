tifUploadNav <- tabPanel(
  title = "Upload a TIF File",
  
  fileInput(
    inputId = "tifFileUpload",
    label = "Upload a TIF File",
    multiple = FALSE,
    accept = c('.tif')
  ),
  
  radioButtons(
    inputId = "tifFilePurpose",
    label = "What Data Does this File Display?",
    choices = c(
      "Elevation",
      "Foraging",
      "Other"
    )
  ),
  conditionalPanel(
    condition = "input.tifFilePurpose == 'Other'",
    textInput(
      inputId = "tifFilePurposeTextInput",
      label = "Please enter the purpose of the data here"
    )
  ),
  
  checkboxInput(
    inputId = "tifDateRangeCheck",
    label = "Valid Under a Date Range?",
    value = FALSE
  ),
  conditionalPanel(
    condition = "input.tifDateRangeCheck",
    dateRangeInput(
      inputId = "tifDateRange",
      label = "Enter the Date Range in which this TIF file is valid."
    )
  ),
  
  actionButton(
    inputId = "tifSubmitButton",
    label = "Submit TIF File"
  )
  
  
)