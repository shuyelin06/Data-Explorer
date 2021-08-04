paramNav <- tabPanel(
  # Title of Tab 
  title = "Biological Year",
  
  # UI Elements
  titlePanel("Define The Biological Year"),
  
  # Choosing the start of the biological year
  selectInput(
    inputId = "biologicalYearSelect",
    label = "Choose Start Month",
    
    # Begins from Oct -> Dec, then Jan -> Sept
    choices = append(monthNames$name[10:12], monthNames$name[1:9]),
    selected = {
      # Default Selected Value: October
      inputSelected <- "October"
      
      # Checking if there are already settings saved for the biological year
      bioYear <- read.csv(paste(files$bioYear[3], files$bioYear[2], sep = "/"))
      if(nrow(bioYear) == 12){
        inputSelected <- bioYear$month[1]
      }
      
      inputSelected
    }
  ),
  
  # Output where the season defining where be displayed
  uiOutput(
    outputId = "seasonDefining"
  ),
  
  # Button to save changes
  actionButton(
    inputId = "seasonSave",
    label = "Save Changes"
  )
)

rm(inputSelected)