# ---
# Biological Year Select
# ---

# Render UI (for season definitions) to match the dropdown change
output$seasonDefining <- renderUI({
  dropDownValue <- input$biologicalYearSelect 
  # Checking if an actual month was selected
  if(dropDownValue != "Click Here to Choose..."){
    
    # Adding Title Panel
    uiOutput <- tagList(titlePanel("Define Seasons for the Biological Year"))
    
    # --- 
    # Adding inputs for defining seasons (starting with the month selected)
    # ---
    
    # Creating the biological year from the
    biologicalYear <- append(monthNames$name[match(dropDownValue, monthNames$name): 12], monthNames$name[1: match(dropDownValue, monthNames$name)-1])
    
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
# Data Select
# ---

# Check Box select for selecting layer information
output$selectAddData <- renderUI(
  {
    dataTypes <- unique(layerData()$Data.Type)
    
    if(length(dataTypes) == 0){
      h3("No TIF Data Found")
    } else {
      checkboxGroupInput(
        inputId = "selectTIFinfo",
        label = "Include TIF Information",
        choices = dataTypes
      )
    }
  }
)

# Load Data on a Button Press
observeEvent(input$selectDataRetrieve, {
  totalTime <- Sys.time() # Record time elapsed
  
  # ---
  # Importing and Formatting Migration Data
  # ---
  time <- Sys.time()
  print("Data Select: Importing and Formatting Migration Data")
  
  # Importing Migration Data
  data <- st_read(paste(files$migrationData[3], files$migrationData[2], sep = "/"))
  
  # Converting ID Column to Numeric
  data$id <- as.numeric(data$id)
  
  # Converting Date Column to POSIXct
  data$date <- lubridate::ymd_hms(data$date, 
                                  quiet = TRUE,
                                  truncated = 3
  )
  
  # Removing Burst, DT and StepFlag columns (I don't think they'll be important in plotting)
  data <- dplyr::select(data, id, date, dist, speed, abs_angle, rel_angle, geometry)
  
  print(paste("Data Select: Finished Formatting Migration Data", "-", Sys.time() - time, "seconds"))
  
  
  # ---
  # Adding Biological Year and Seasons
  # ---
  time <- Sys.time()
  print("Data Select: Adding Bio Year and Seasons")
  
  # Extracting information from the Biological Year.csv file
  bioYearData <- read.csv(paste(files$bioYear[3], files$bioYear[2], sep = "/")) # Importing information from the file
  
  
  # Adding Seasons to the Migration Data
  for(i in 1:nrow(bioYearData)){
    month <- bioYearData$number[i] # Obtaining the month
    season <- bioYearData$season[i] # Obtaining the associated season
    
    # Adding the season to the migration data 
    data$season[month(data$date) == month] <- season
  }
  rm(i, month, season) # Clearing unneeded variables from memory
  
  
  # Adding Biological Year to the Migration Data
  startMonth <- bioYearData$number[1] # Finding the start month to the biological year
  rm(bioYearData) # Removing bioYearData (no longer needed)
  
  biologicalYear <- data.frame(matrix(ncol = 3, nrow = 0)) # Dataframe to be used in adding the bioYear column
  
  min <- min(data$date) # Smallest POSIXct date in the data
  max <- max(data$date) # Largest POSIXct date in the data
  
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
    withinYear <- biologicalYear$start[i] <= data$date & data$date <= biologicalYear$end[i]
    
    data$bioYear[withinYear] <- biologicalYear$bioYear[i]
  }
  rm(biologicalYear, i, withinYear) # Removing unneeded variables from memory
  
  # Moving bioYear and seasons to the front of the dataframe (as they're used for grouping)
  data <- dplyr::select(data, id, date, bioYear, season, dist, speed, abs_angle, rel_angle, geometry)
  
  print(paste("Data Select: Finished Adding Bio Year and Seasons", "-", Sys.time() - time, "seconds"))
  
  # ---
  # Adding Information from Selected TIF Files
  # --- 
  time <- Sys.time()
  print("Data Select: Adding TIF File Information")
  
  # Retrieving the TIF Data that the User Wants
  requestedData <- input$selectTIFinfo
  
  # Import layer Information
  layerData <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))
  
  for(dataType in requestedData){
    print(paste("Extracting Values for", dataType))
    
    # Adding a new blank column for the data type
    data[[dataType]] <- NA
    
    # Iterating through every row with the given data type
    rows <- layerData[layerData$Data.Type == dataType,]
    for(row in 1:nrow(rows)){
      path <- rows[row,"Path"]

      # Check if raster layer file exists
      if(!file.exists(path)){
        warning(paste("File", path, "could not be found"))
        next
      }
      
      # Creating Raster Layer
      rasterLayer <- raster(path)
      rm(path)
      
      # Taking the Migration Data's geometry and converting it to Spatial Points (with the crs of the raster layer)
      geometry <- spTransform(as_Spatial(data$geometry), crs(rasterLayer))
      print("Geometry created")
      
      # Tells us which rows to extract from geometry and edit on the migration data frame (so if there are multiple raster layers with different extents, we don't overwrite data from other layers)
      noValue <- is.na(data[[dataType]]) 
      print("NO value")
      
      if(is.na(rows[row,"Start.Date"]) | is.na(rows[row,"End.Date"])){
        data[[dataType]][noValue] <- raster::extract(rasterLayer, geometry[noValue])
      } else {
        startDate <- as.POSIXct(rows[row, "Start.Date"], format = "%Y-%m-%d")
        endDate <- as.POSIXct(rows[row, "End.Date"], format = "%Y-%m-%d")
        
        withinDates <- startDate <= data$date  & data$date <= endDate
        rm(startDate)
        rm(endDate)
        
        data[[dataType]][noValue & withinDates] <- raster::extract(rasterLayer, geometry[noValue & withinDates])
      }
      print("Extraction")
      
      rm(rasterLayer, geometry)
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
  migrationData(data)
  
  print("Data Select: Finished")
  print(paste("Data Select: Took", Sys.time() - totalTime, "seconds"))
})