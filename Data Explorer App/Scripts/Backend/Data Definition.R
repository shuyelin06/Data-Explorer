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
        dataFrame <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))
        
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
      savedBioYearData <- read.csv(paste(files$bioYear[3], files$bioYear[2], sep = "/"))
      
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
  
  write.csv(seasonDefinitions, file = paste(files$bioYear[3], files$bioYear[2], sep = "/"), row.names = FALSE)
  
  print("Defining Biological Year: Biological Year Saved")
})


# ---
# Server Code for the Data Select Sub-Tab

# This tab will be where we load the data with additional raster layer information of what we might want.
# Since there may be multiple datatypes uploaded, selecting a few can reduce the amount of time it takes to load the data for plotting
# ---
observeEvent(input$selectDataRetrieve, {
  totalTime <- Sys.time() # Record time elapsed
  
  # ---
  # Importing and Formatting Migration Data
  # ---
  time <- Sys.time()
  print("Data Select: Importing and Formatting Migration Data")
  
  # Importing Migration Data
  migrationData <<- st_read(paste(files$migrationData[3], files$migrationData[2], sep = "/"))
  
  # Converting ID Column to Numeric
  migrationData$id <<- as.numeric(migrationData$id)
  
  # Converting Date Column to POSIXct
  migrationData$date <<- as.POSIXct(migrationData$date, format = "%Y-%m-%d %H:%M:%S")
  
  # Removing Burst, DT and StepFlag columns (I don't think they'll be important in plotting)
  migrationData <<- dplyr::select(migrationData, id, date, dist, speed, abs_angle, rel_angle, geometry)
  
  print(paste("Data Select: Finished Formatting Migration Data", "-", Sys.time() - time, "seconds"))
  
  
  # ---
  # Adding Biological Year and Seasons
  # ---
  time <- Sys.time()
  print("Data Select: Adding Bio Year and Seasons")
  
  # Extracting information from the Biological Year.csv file
  bioYearData <- read.csv("./Data/Settings/Biological Year.csv") # Importing information from the file
  
  
  # Adding Seasons to the Migration Data
  for(i in 1:nrow(bioYearData)){
    # Obtaining the month
    month <- base::as.character(bioYearData$number[i])
    if(length(month) == 1){
      month <- paste("0", month, sep = "") # Done to meet the format of the month when extracting it from POSIXct
    }
    
    # Obtaining the associated season
    season <- bioYearData$season[i]
    
    # Adding the seasons to the migration data 
    migrationData$season[format(migrationData$date, "%m") == month] <<- season
  }
  rm(i, month, season) # Clearing unneeded variables from memory
  
  
  # Adding Biological Year to the Migration Data
  startMonth <- bioYearData$number[1] # Finding the start month to the biological year
  rm(bioYearData) # Removing bioYearData (no longer needed)
  
  biologicalYear <- data.frame(matrix(ncol = 3, nrow = 0)) # Dataframe to be used in adding the bioYear column
  
  min <- min(migrationData$date) # Smallest POSIXct date in the data
  max <- max(migrationData$date) # Largest POSIXct date in the data
  
  for(bioYear in seq(from = as.numeric(format(min, "%Y")) - 1, to = as.numeric(format(max, "%Y")) + 1)){
    startDate <- NA
    endDate <- NA
    
    # If start month is from Oct - Dec, the bioyear STARTS from the PREVIOUS year's Oct-Dec
    if(10 <= startMonth & startMonth <= 12){
      startDate <- as.POSIXct(paste(bioYear - 1, startMonth, 1, sep = "/"), format = "%Y/%m/%d")
      endDate <- as.POSIXct(paste(bioYear, startMonth, 1, sep = "/"), format = "%Y/%m/%d") - 1
    } 
    # If the start month is from Jan - Sept, the bioyear ENDS on the NEXT year's Jan - Sept
    else {
      startDate <- as.POSIXct(paste(bioYear, startMonth, 1, sep = "/"), format = "%Y/%m/%d")
      endDate <- as.POSIXct(paste(bioYear + 1, startMonth, 1, sep = "/"), format = "%Y/%m/%d") - 1
    }
    
    # Adding to the biological year dataframe
    biologicalYear <- rbind(biologicalYear, c(bioYear, startDate, endDate))
  }
  rm(min, max, bioYear, startMonth, startDate, endDate) # Removing unneeded variables from memory
  
  colnames(biologicalYear) <- c("bioYear", "start", "end") # Formatting the BioYear Data Frame
  
  for(i in 1:nrow(biologicalYear)){
    withinYear <- biologicalYear$start[i] <= migrationData$date & migrationData$date <= biologicalYear$end[i]
    
    migrationData$bioYear[withinYear] <<- biologicalYear$bioYear[i]
  }
  rm(biologicalYear, i, withinYear) # Removing unneeded variables from memory
  
  # Moving bioYear and seasons to the front of the dataframe (as they're used for grouping)
  migrationData <<- dplyr::select(migrationData, id, date, bioYear, season, dist, speed, abs_angle, rel_angle, geometry)
  
  print(paste("Data Select: Finished Adding Bio Year and Seasons", "-", Sys.time() - time, "seconds"))
  
  # ---
  # Adding Information from Selected TIF Files
  # --- 
  time <- Sys.time()
  print("Data Select: Adding TIF File Information")
  
  # Retrieving the TIF Data that the User Wants
  requestedData <- input$selectTIFinfo
  
  # Import TIF File Information
  layerData <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))
  
  for(dataType in requestedData){
    print(paste("Extracting Values for", dataType))
    
    # Adding a new blank column for the data type
    migrationData[[dataType]] <<- vector(mode = "numeric", length = nrow(migrationData))
    
    # Iterating through every row with the given data type
    rows <- layerData[layerData$Data.Type == dataType,]
    for(row in 1:nrow(rows)){
      
      print(rows[row, "File.Name"])
      # Creating Raster Layer
      rasterLayer <- raster(paste(files$rasterLayers[3], rows[row, "File.Name"], sep = "/"))
      
      # Taking the Migration Data's geometry and converting it to Spatial Points (with the crs of the raster layer)
      geometry <- spTransform(as_Spatial(migrationData$geometry), crs(rasterLayer))
      
      # Tells us which rows to extract from geometry and edit on the migration data frame (so if there are multiple raster layers with different extents, we don't overwrite data from other layers)
      noValue <- is.na(migrationData[[dataType]]) | (migrationData[[dataType]] == 0) 
      
      if(is.na(rows[row,"Start.Date"]) | is.na(rows[row,"End.Date"])){
        migrationData[[dataType]][noValue] <<- raster::extract(rasterLayer, geometry[noValue])
      } else {
        startDate <- as.POSIXct(rows[row, "Start.Date"], format = "%Y-%m-%d")
        endDate <- as.POSIXct(rows[row, "End.Date"], format = "%Y-%m-%d")
        
        withinDates <- migrationData$date > startDate & migrationData$date < endDate
        rm(startDate)
        rm(endDate)
        
        migrationData[[dataType]][noValue & withinDates] <<- raster::extract(rasterLayer, geometry[noValue & withinDates])
      }
      
      rm(rasterLayer)
      rm(geometry)
      rm(selectRows)
    }
    rm(rows)
    
    print(paste("Finished Extracting Values for", dataType))
  }
  rm(dataType, requestedData, layerData)
  
  print(paste("Data Select: Finished Adding TIF File Information", "-", Sys.time() - time, "seconds"))
  rm(time)
  
  # -- 
  # Exporting Data
  # --
  print("Data Select: Finished")
  print(paste("Data Select: Took", Sys.time() - totalTime, "seconds"))
})