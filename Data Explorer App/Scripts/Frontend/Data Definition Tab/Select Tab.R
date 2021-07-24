dataSelectNav <- tabPanel(
  # Title of Tab 
  title = "Select Data",
  
  # UI Elements
  titlePanel("Select Data to be Displayed"),
  
  selectInput(
    inputId = "groupBySelect",
    label = "Group Data By...",
    choices = c(
      "Click Here to Choose...",
      "Year",
      "Seasons",
      "Months",
      "Animal ID"
    ),
    selected = "Click Here to Choose..."
  ),
  
  checkboxGroupInput(
    inputId = "displayCheck",
    label = "Data to Display",
    choices = c(
      "Distance Traveled",
      "Speed",
      "Absolute Angle",
      "Relative Angle"
    )
  )
)