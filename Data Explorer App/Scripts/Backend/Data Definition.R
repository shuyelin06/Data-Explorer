# ---
# Code for Biological Year Select
# ---

# Render the season inputs (and their appropriate order) based on the dropdown change
output$seasonDefining <- renderUI({
  # This season defining is reactive; it will automatically change based on the dropdown
  dropDownValue <- input$biologicalYearSelect 
  
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
  
})

# On click of the save button, validate that all the data necessary is provided, and save it to a .csv file for later use.
observeEvent(input$seasonSave, {
  # ---
  # Validating that all months have a defined season to them
  # ---
  cat("\n\n --- Defining BioYear: Validating... --- ")
  
  tests <- c()
  
  for(month in monthNames$name){
    tests <- append(tests, need(
      input[[paste("seasonSelect", month, sep = "")]],
      label = month
    ))
    
  }
  validate(tests)
  
  cat("\n --- Defining BioYear: Valiation Check Passed --- ")
  
  
  # ---
  # If validation is successful, save the season definitions for later use
  # ---
  cat("\n\n --- Defining BioYear: Saving BioYear --- ")
  
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
  
  cat("\n --- Defining BioYear: BioYear Saved --- ")
})

# ---
# Layer Define
# ---
output$layerDefineOutput <- renderUI(
  {
    dataTypes <- unique(layerData()$Data.Type)
    dataDefine <- layerDefinitions()
    
    uiOutput <- tagList()
    for(type in dataTypes){
      # If the data had already been defined before, display it.
      existingType <- character(0)
      
      if(type %in% dataDefine$DataType){
        existingType <- dataDefine$Definition[match(type, dataDefine$DataType)]
      }
      
      
      uiOutput <- tagList(
        uiOutput,
        
        isolate(
          radioButtons(
            inputId <- paste("define", type, sep = ""),
            label = type,
            choices = c(
              "Continuous",
              "Discrete"
            ),
            selected = existingType,
            inline = TRUE
          )
        )
        
      )
    }
    
    uiOutput
  }
)

observeEvent(input$layerDefineSave, {
  cat("\n\n --- Defining Layers: Saving Continuous/Discrete Definitions --- ")
  
  dataTypes <- unique(layerData()$Data.Type)
  
  dataframe <- data.frame(matrix(ncol = 2, nrow = 0))
  
  # Defining Movement Parameters as Continuous 
  dataframe <- rbind(dataframe, 
                     c("Distance", "Continuous"), 
                     c("Speed", "Continuous"), 
                     c("Relative.Angle", "Continuous")
                     )
  
  for(type in dataTypes){
    text <- input[[paste("define", type, sep = "")]]
    
    if(text != ""){
      dataframe <- rbind(dataframe, c(type, text))
      cat(paste("\n", type, "-", text))
    }
  }
  
  colnames(dataframe) <- c("DataType", "Definition")
  
  write.csv(dataframe, paste(files$layerDefine[3], files$layerDefine[2], sep = "/"))
  
  cat("\n --- Defining Layers: Finished Saving Continuous/Discrete Definitions --- ")
})

# ---
# Data Select
# ---

# Check Box select for selecting layer information
output$selectAddData <- renderUI(
  {
    dataTypes <- unique(layerData()$Data.Type)
    dataDefine <- layerDefinitions()
    
    if(length(dataTypes) != 0){
      # Data that does not have continuous/discrete assigned to them cannot be loaded 
      dataTypes <- dataTypes[dataTypes %in% dataDefine$DataType]
      
      # Creating a CheckboxGroupInput so the user can choose what layer data to load
      checkboxGroupInput(
        inputId = "selectTIFinfo",
        label = "Choose What Raster Layer Data to Load",
        choices = dataTypes
      )
    } else {
      h3("No Raster Layers Found")
    }
  }
)

# Load Data on a Button Press
observeEvent(input$selectDataRetrieve, {
  time <- Sys.time() # Record time elapsed
  
  # ---
  # Importing and Formatting Migration Data
  # ---
  cat("\n\n --- Data Select: Importing and Formatting Migration Data --- ")

  # Importing Migration Data
  cat("\n\n - Importing Migration Data -")
  data <- st_read(paste(files$migrationData[3], files$migrationData[2], sep = "/"))
  cat("\nFinished Importing Migration Data")
  
  if(nrow(data) == 0){
    stop("Migration Data File is Empty")
  }
  
  # Renaming Columns
  cat("\n\n - Renaming Columns - \n")
  data <- dplyr::rename(data, Distance = dist, Speed = speed, Relative.Angle = rel_angle)
  cat("\nFinished Renaming Columns")
  
  # Converting ID Column to Numeric
  cat("\n\n - Converting ID Column to Numeric -")
  data$id <- as.numeric(data$id)
  cat("\nFinished Converting ID Column to Numeric")
  
  # Converting Date Column to POSIXct
  cat("\n\n - Converting Date Column to POSIXct - ")
  data$date <- lubridate::ymd_hms(data$date, 
                                  quiet = TRUE,
                                  truncated = 3
  )
  cat("\nFinished Converting Date Column to POSIXct")
  
  
  cat("\n --- Data Upload: Finished Formatting Migration Data --- ")
  
  
  
  # ---
  # Adding Biological Year, Seasons, and Months
  # ---
  cat("\n\n --- Data Select: Adding Biological Year, Seasons and Month --- ")

  # Extracting information from the Biological Year.csv file
  bioYearData <- read.csv(paste(files$bioYear[3], files$bioYear[2], sep = "/")) # Importing information from the file
  
  
  # Adding Month to the Migration Data
  cat("\n\n - Adding Month Column to Data -")
  data$month <- month(data$date)
  cat("\nFinished Adding Month Column")
  
  
  # Adding Seasons to the Migration Data
  cat("\n\n - Adding Seasons Column to Data -")
  for(i in 1:nrow(bioYearData)){
    month <- bioYearData$number[i] # Obtaining the month
    season <- bioYearData$season[i] # Obtaining the associated season
    
    # Adding the season to the migration data 
    data$season[month(data$date) == month] <- season
  }
  rm(i, month, season) # Clearing unneeded variables from memory
  cat("\nFinished Adding Seasons Column")
  
  
  # Adding Biological Year to the Migration Data
  cat("\n\n - Adding Biological Year Column to Data -")
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
  cat("\nFinished Adding Biological Year Column")
  
  # Moving bioYear, seasons, and month to the front of the dataframe (as they're used for grouping)
  data <- dplyr::select(data, id, date, bioYear, season, month, Distance, Speed, Relative.Angle, geometry)
  
  cat("\n --- Data Upload: Finished Adding Biological Year, Season and Months --- ")
  
  # ---
  # Adding Information from Selected TIF Files
  # --- 
  cat("\n\n --- Data Select: Adding Selected Raster Layer Information --- ")
  
  # Retrieving the TIF Data that the User Wants
  requestedData <- input$selectTIFinfo
  
  # Import layer Information
  layerData <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))
  
  for(dataType in requestedData){
    cat(paste("\n\n - Extracting Values for", dataType,"-"))
    
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
      
      # Tells us which rows to extract from geometry and edit on the migration data frame (so if there are multiple raster layers with different extents, we don't overwrite data from other layers)
      noValue <- is.na(data[[dataType]]) 
      
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
      
      rm(rasterLayer, geometry)
    }
    rm(rows)
    
    cat(paste("\nFinished Extracting Values for", dataType))
  }
  rm(dataType, requestedData, layerData)
  
  cat("\n --- Data Upload: Finished Adding Raster Layer Information --- ")
  
  # -- 
  # Exporting Data
  # --
  cat("\n\n --- Data Select: Exporting Data --- ")
  migrationData(data)
  
  cat("\n --- Data Upload: Finished Exporting Data --- ")
  cat(paste("\nTook", Sys.time() - time, "Seconds"))
})