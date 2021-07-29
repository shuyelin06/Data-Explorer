# Root directory for the file selects will begin from
root <- c(Documents = path.expand("~"))

# ---
# Server Code for the Shape File Upload
# ---

shapeFileData <- NA

# Select the .shp File and ID/Date Columns
observe({
  # Code for the ShapeFile Upload Button
  shinyFileChoose(
    input = input, 
    id = 'sfFileUpload',
    roots = root,
    filetypes = c('dbf', 'prj', 'shp', 'shx'),
  )
  

  sfFilePath <- parseFilePaths(root, input$sfFileUpload)$datapath
  
  # If there is a path, load the data
  if(length(sfFilePath) != 0){
    output$sfFileDisplay <- renderText(sfFilePath)
    
    shapeFileData <<- st_read(sfFilePath)
    
    output$sfColSelect <- renderUI({
      columnNames <- colnames(shapeFileData)
      
      uiOutput <- tagList(titlePanel("Select ID and Date Column"))
      
      # Adding Radio Buttons for ID Column Select
      uiOutput <- tagList(uiOutput, isolate(radioButtons(
        inputId <- "sfIdCol",
        label = "Select ID Column",
        choices = columnNames,
        selected = character(0)
      )))
      
      # Adding Radio Buttons for Date Column Select
      uiOutput <- tagList(uiOutput, isolate(radioButtons(
        inputId <- "sfDateCol",
        label = "Select Date Column",
        choices = columnNames,
        selected = character(0)
      )))
      
    })
    
    output$sfConsole <- renderText("Data Successfully Uploaded!")
  }


})

# Retrieve Other Information, Calculate Movement Parameters, and Save File
observeEvent(input$sfSubmit, {
  # ---
  # Validation: Checking that all necessary fields are filled
  # ---
  output$sfConsole <- renderText("Validating...")

  # Commented out for now as validation is not working 
  validate(
    # Check that there is data loaded
    need(shapeFileData, message = 'There is no ShapeFile Loaded'),
    
    # Check that the id/date columns are selected
    need(input$sfIdCol, message = 'ID Column is empty'),
    
    need(input$sfDateCol, message = 'Date Column is empty')
  )
  
  output$sfConsole <- renderText("Validation Tests Passed")
  
  # ---- End of Validation ----
  
  
  # ---
  # Calculating Movement Parameters
  # ---
  output$sfConsole <- renderText("Calculating Movement Parameters...")
  
  # Changing the id and date column names to "id" and "date", respectively (so I can use the dplyr::distinct function for id and date)
  columnNames <- colnames(shapeFileData)
  
  indices <- match(c(input$sfIdCol, input$sfDateCol), columnNames)
  columnNames[indices[1]] <- "id" # Changing ID column name to "id"
  columnNames[indices[2]] <- "date" # Changing date column name to "date"
  
  colnames(shapeFileData) <<- columnNames
  
  rm(columnNames, indices)
  
  
  # Calculating Movement Parameters
  shapeFileData$date <<- lubridate::ymd_hms(shapeFileData$date, 
                                            quiet = TRUE,
                                            truncated = 3
                                            ) # Converting Date Column to POSIXct format
  # ^ Currently, the timezone becomes UTC. I'm not sure if this will be an issue later?
  # For some reason, trying to change the time zone causes the RStudio application to freeze. I'm not sure why.
  
  if(sum(is.na(shapeFileData)) > 0){
    warning(paste(sum(is.na(shapeFileData)), "rows will be deleted because the date was not read in properly"))
  }
  
  shapeFileData <<- na.omit(shapeFileData) # Removing all rows with NA values
 
  shapeFileData <<- dplyr::distinct(shapeFileData, id, date, .keep_all = TRUE) # Removing duplicate values
  
  
  shapeFileData <<- shapeFileData[order(shapeFileData$id, shapeFileData$date),] # Ordering by ID and Date

  shapeFileData$burst <<- CalcBurst(data = shapeFileData, Tmax = 3600 * as.numeric(input$sfTmax)) # Adding Bursts
  
  shapeFileData <<- CalcMovParams(data = shapeFileData) # Calculating Parameters
  
  
  output$sfConsole <- renderText("Movement Parameters Calculated")

  # ---- End of Data Manipulation ----
  
  
  # ---
  # Data Saving
  # ---
  output$sfConsole <- renderText("Saving Migration Data...")
  
  # Converting date column back to characters
  shapeFileData$date <<- as.character(shapeFileData$date)
  
  # Overwriting the data file
  st_write(shapeFileData, 
           dsn = paste(files$migrationData[3], files$migrationData[2], sep = "/"), 
           driver = "ESRI Shapefile", 
           append = FALSE, 
           delete_layer = TRUE
  )
  
  output$sfConsole <- renderText("Migration Data Saved")
  
  # ---- End of Data Saving ----
})


# ---
# Server Code for TIF File Upload
# ---

# File Select for the .tif and .img files
observe({
  shinyFileChoose(
    input = input, 
    id = 'layerUpload',
    roots = root,
    filetypes = c('tif', 'img'),
  )
  
  layerPath <- parseFilePaths(root, input$layerUpload)$datapath
  
  if(length(layerPath) != 0){
    # Show the layer path
    output$layerFileDisplay <- renderText(layerPath)
    
    output$layerConsole <- renderText("Layer Path Imported")
  }
})


# Information Recording
observeEvent(input$layerSubmit, {
  # ---
  # Validation
  # ---
  output$layerConsole <- renderText("Validating...")
  
  layerPath <- parseFilePaths(root, input$layerUpload)$datapath
  
  validate(
    # Checking if layer exists
    need(length(layerPath) != 0 & file.exists(layerPath), message = "Layer Does Not Exist"),
    
    # Checking if a data type is provided
    need(length(input$layerPurpose) != 0 | length(input$layerPurposeText) != 0, message = "Data Type Not Provided")
  )
  
  output$layerConsole <- renderText("Validation Tests Passed")
  
  # -----
  
  # ---
  # Extracting Information
  # ---
  output$layerConsole <- renderText("Extracting Information...")
  
  # Extracting the Type of Data the .tif File Contains
  dataType <- input$layerPurpose
  if(dataType == "Other"){
    dataType <- input$layerPurposeText
  }
  
  # Extracting the Date Range of the .tif File.
  startDate <- NA
  endDate <- NA
  if(input$layerDateRangeCheck){
    startDate <- input$layerDateRange[1]
    endDate <- input$layerDateRange[2]
  }
  
  
  output$layerConsole <- renderText("Information Extracted")
  
  # --- 
  # Saving Information
  # --- 
  output$layerConsole <- renderText("Saving Information")
  
  # The information will be saved to a .csv file for convenience 
  layerInformation <- data.frame(matrix(ncol = 4, nrow = 1))
  
  # Setting the file name
  layerInformation[1,1] <- layerPath
  rm(layerPath)
  
  # Setting the information type
  layerInformation[1,2] <- dataType
  rm(dataType)
  
  # Setting the start date
  layerInformation[1,3] <- as.character(startDate)
  rm(startDate)
  
  # Setting the end date
  layerInformation[1,4] <- as.character(endDate)
  rm(endDate)
  
  # Setting column names
  colnames(layerInformation) <- c("Path", "Data.Type", "Start.Date", "End.Date")
  
  # Saving the information to a .csv file
  existingData <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))
  
  write.csv(rbind(existingData,layerInformation), 
            file = paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"),
            row.names = FALSE)
  rm(layerInformation, existingData)
  
  output$layerConsole <- renderText("Information Saved")
})
