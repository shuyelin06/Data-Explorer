# --- 
# Event Observer for the Tabs (Fires When a Tab is Selected)

# This Observer May Contain Code for Both Tabs
# ---
observeEvent(input$dataDefineTab, {
  print(paste("Data Definition Tab Changed to: ", input$dataDefineTab))
  
  # When selecting the "Retrieve Data" tab, update the checkboxInput with all unique data types from the .csv file.
  if(input$dataDefineTab == "Retrieve Data"){
    output$selectAddData <- renderUI(
      {
        # Retrieve all of the different data types contained in the raster layers
        dataFrame <- read.csv(paste(rasterLayerPath, "Layers.csv", sep = "/"))
        
        dataTypes <- unique(dataFrame$Data.Type)
        
        checkboxGroupInput(
          inputId = "selectTIFinfo",
          label = "Include TIF Information",
          choices = dataTypes
        )
      }
    )
  }
})


# ---
# Event Observers for the Biological Year Select
# ---

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
  
  print("Defining Biological Year: Data Validation Passed")
  
  
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
  
  write.csv(seasonDefinitions, file = bioYearPath, row.names = FALSE)
  
  print("Defining Biological Year: Biological Year Saved")
})


# ---
# Server Code for the Data Select Sub-Tab

# This tab will be where we load the data with additional raster layer information of what we might want.
# Since there may be multiple datatypes uploaded, selecting a few can reduce the amount of time it takes to load the data for plotting
# ---

library(sf)

library(raster)
library(dplyr)

observeEvent(input$selectDataRetrieve, {
  # Not Tested Yet
  
  # Retrieving the Selected Data that the User Wants
  requestedData <- input$selectTIFinfo
  
  # --- 
  # Importing Data
  # ---
  
  # Import Migration Data
  migrationData <- st_read(dataFilePath)
  
  # Import Additional Information (TIF Files)
  layerData <- read.csv(paste(rasterLayerPath, "Layers.csv", sep = "/"))
  
  
  # ---
  # Formatting Migration Data
  # ---
  
  # Converting ID Column to Numeric
  migrationData$id <- as.numeric(migrationData$id)
  
  # Converting Date Column to POSIXct
  migrationData$date <- as.POSIXct(migrationData$date, format = "%Y-%m-%d %H:%M:%S")
  
  # Removing Burst, DT and StepFlag columns (don't think they'll be used in plotting)
  migrationData <- dplyr::select(migrationData, id, date, dist, speed, abs_angle,rel_angle, geometry)
  
  
  # ---
  # Adding Information from TIF Files
  # --- 
  
  for(dataType in dplyr::distinct(layerData$`Data Type`)){
    print(paste("Extracting Values for", dataType))
    
    # Adding a new blank column for the data type
    migrationData[[dataType]] <- vector(mode = "numeric", length = nrow(migrationData))
    
    # Extract the data for all files with this data type
    for(dataRow in layerData$`Data Type` == dataType){
      rasterLayer <- raster(paste("./Data/Raster Layers", dataRow$File.Name, sep = "/"))
      
      # If there are no date ranges, extract all values associated with the geometries
      if(is.na(dataRow$Start.Date) | is.na(dataRow$End.Date)){
        migrationData[[dataType]] <- raster::extract(rasterLayer, as_Spatial(migrationData$geometry))
      } 
      # If there are date ranges, extract values associated with the geometries which are within the date range
      else {
        startDate <- as.POSIXct(dataRow$Start.Date)
        endDate <- as.POSIXct(dataRow$End.Date)
        
        migrationData[[dataType]][migrationData$date > startDate & migrationData$date < endDate] <- raster::extract(rasterLayer, as_Spatial(migrationData$geometry[migrationData$date > startDate & migrationData$date < endDate]))
      }
    }
  }
  
  # -- 
  # Exporting Data
  # --
  View(migrationData)
  
  
  
})