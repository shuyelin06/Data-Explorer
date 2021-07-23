
# On dropdown change, render UI to allow the user to define their seasons

observeEvent(input$biologicalYearSelect, {
  output$seasonDefining <- renderUI({
    # Checking if an actual month was selected
    if(input$biologicalYearSelect != "Click Here to Choose..."){
      
      # Adding Title Panel
      uiOutput <- tagList(titlePanel("Define Seasons for the Biological Year"))
      
      # --- 
      # Adding inputs for defining seasons (starting with the month selected)
      # ---
      # Extracting the start month which was selected
      startMonth <- input$biologicalYearSelect
      
      # Creating the biological year from this start month 
      biologicalYear <- append(monthNames$name[match(startMonth, monthNames$name): 12], monthNames$name[1: match(startMonth, monthNames$name)-1])
      
      # Extracting preexisting biological year data
      savedBioYearData <- read.csv(bioYearPath)
      
      # Creating inputs
      for(month in biologicalYear){
        inputValue = ""
        if(nrow(savedBioYearData) == 12){
          inputValue = savedBioYearData$season[match(month, savedBioYearData$month)]
        }
        
        uiOutput <- tagList(uiOutput, isolate(textInput(
          inputId = paste("seasonSelect", month, sep = ""),
          label = month,
          value = inputValue
        )))
      }
      
      # Return the ui elements created
      uiOutput
    } 
    
  })
  
  
})

# On click of the save button, validate that all the data necessary is provided, and save it to a .csv file for later use.
observeEvent(input$seasonSave, {
  # ---
  # Validating that all months have a defined season to them
  # ---
  
  tests <- c()
  
  for(month in monthNames$name){
    tests <- need(
      input[[paste("seasonSelect", month, sep = "")]],
      label = month
    )
    
  }
  validate(tests)
  
  print("Data Validation Passed")
  
  
  # ---
  # If validation is successful, save the season definitions for later use
  # ---
  
  seasonDefinitions <- data.frame(matrix(ncol = 3, nrow = 12))
  
  # Adding Months to Dataframe in the Order of the Biological Year
  seasonDefinitions[,1] <- as.numeric(append(
    monthNames$month[match(input$biologicalYearSelect, monthNames$name): 12], 
    monthNames$month[1: match(input$biologicalYearSelect, monthNames$name)-1]
  ))
  
  names <- c()
  seasons <- c()
  
  for(number in seasonDefinitions[,1]){
    month <- monthNames$name[match(number, monthNames$month)]
    
    names <- append(names, month)
    seasons <- append(seasons, input[[paste("seasonSelect", month, sep = "")]])
  }
  
  seasonDefinitions[,2] <- names
  seasonDefinitions[,3] <- seasons
  
  colnames(seasonDefinitions) <- c("number", "month", "season")
  
  write.csv(seasonDefinitions, file = bioYearPath)

  print("Biological Year Saved")
})
