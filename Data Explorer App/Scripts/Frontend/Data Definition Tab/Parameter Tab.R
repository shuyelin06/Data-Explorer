paramNav <- tabPanel(
  # Title of Tab 
  title = "Biological Year",
  
  # UI Elements
  titlePanel("Define The Biological Year"),
  
  # Choosing the start of the biological year
  selectInput(
    inputId = "biologicalYearSelect",
    label = "Choose Start Month",
    choices = append(append(c("Click Here to Choose..."), monthNames$name[10:12]), monthNames$name[1:9]),
    selected = {
      inputSelected <- "Click Here to Choose..."
      
      # Checking if there are already settings saved for the biological year
      bioYear <- read.csv(bioYearPath)
      if(nrow(bioYear) == 12){
        inputSelected <- bioYear$month[1]
      }
      
      inputSelected
    }
  ),
  
  # Defining seasons in the biological year
  # https://shiny.rstudio.com/articles/dynamic-ui.html
  conditionalPanel(
    condition = "input.biologicalYearSelect != 'Click Here to Choose...'",
    
    # Output where the season defining where be displayed
    uiOutput(
      outputId = "seasonDefining"
    ),
    
    # Button to save changes
    actionButton(
      inputId = "seasonSave",
      label = "Save Changes"
    )
  ),
  
  # Console Messages
  uiOutput(
    outputId = "seasonDefiningConsole"
  )
)

rm(inputSelected)