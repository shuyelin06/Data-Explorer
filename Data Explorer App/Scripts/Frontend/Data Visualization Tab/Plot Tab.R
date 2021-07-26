plotDataNav <- tabPanel(
  # Title of Tab 
  title = "Data Plotting",
  
  # UI Elements
  titlePanel("Plot Data"),
  
  radioButtons(
    inputId = "dataPlotGrouping",
    label = "Group Data By..",
    choices = c(
      "Biological Year",
      "Seasons",
      "Month"
    )
  ),
  
  checkboxInput(
    inputId = "dataPlotDifferentiateIDs",
    label = "Display Animal IDs Separately?"
  ),
  
  conditionalPanel(
    condition = "input.dataPlotDifferentiateIDs",
    titlePanel("Choose Animal IDs to Display"),
    uiOutput(
      outputId = "dataPlotChooseIDs"
    )
  ),
  
  checkboxGroupInput(
    inputId = "dataPlotDisplay",
    label = "Data to Display",
    choices = c(
      "Distance",
      "Speed",
      "Absolute Angle",
      "Relative Angle"
    )
  )
)