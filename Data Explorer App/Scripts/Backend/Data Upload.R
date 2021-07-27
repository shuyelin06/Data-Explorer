# ---
# Server Code for the Data Upload Tab Changes
# ---


# ---
# Server Code for the Shape File Upload

# Code runs when the submit button in the ShapeFile tab is pressed
# ---
observeEvent(input$sfSubmitButton, {
  # ---
  # Validation: Checking that all necessary fields are filled
  # ---
  
  
  # ---- End of Validation ----
  
  
  # ---
  # Extracting Information: Pulling the Information Uploaded
  # ---
  
  # Extracting the data
  filemap <- input$sfFileUpload
  previousDirec <- getwd()
  
  uploadDirec <- dirname(filemap$datapath[1])
  setwd(uploadDirec)
  
  for(i in 1:nrow(filemap)){
    file.rename(filemap$datapath[i], filemap$name[i])
  }
  
  setwd(previousDirec)
  
  uploadData <- st_read(paste(uploadDirec, filemap$name[grep(pattern = "*.shp$", filemap$name)], sep = "/"))
  
  rm(previousDirec)
  rm(uploadDirec)
  rm(filemap)
  
  
  print("ShapeFile Upload: Data Extraction Complete")
  # ---- End of Data Extraction ----
  
  
  # ---
  # Manipulating Data: Altering the Data so it Meets Criteria for Saving 
  # ---
  
  # Reading the csv file where the previous information is stored
  existingData <- st_read(paste(files$migrationData[3], files$migrationData[2], sep = "/"))
  
  
  # Changing column names to match that of existing data
  columnNames <- colnames(uploadData)
  
  indices <- match(c(input$sfIdInput, input$sfDateInput), columnNames)
  columnNames[indices[1]] <- "id" # Changing ID column name to "id"
  columnNames[indices[2]] <- "date" # Changing date column name to "date"
  
  colnames(uploadData) <- columnNames
  
  rm(columnNames)
  rm(indices)
  
  
  # Checking if the animals are unique or not; if they are, change conflicting IDs with preexisting animals
  if(nrow(existingData) != 0 & input$sfUniqueCheck == TRUE){
    print("ShapeFile Upload: Upload Data is Unique")
    
    # Get the IDs in both data sets
    existingIDs <- unique(existingData$id)
    
    uploadedIDs <- unique(uploadData$id)
    
    # Find the IDs in the uploaded data that conflict
    conflictIndices <- match(existingIDs, uploadedIDs)
    
    # Changing the ids of the animals to a unique one
    newID <- max(as.numeric(existingIDs)) + 1
    for(i in conflictIndices){
      ID <- uploadedIDs[i]
      
      uploadData$id[uploadData$id == ID] <- as.character(newID)
      
      newID <- newID + 1
    }
    
    rm(existingIDs)
    rm(uploadedIDs)
    rm(conflictIndices)
    rm(ID)
    rm(newID)
  }
  
  
  # Calculating Movement Parameters
  uploadData$date <- as.POSIXct(uploadData$date, format = "%Y-%m-%d %H:%M:%S") # Converting Date Column to POSIXct format
  
  uploadData <- na.omit(uploadData) # Removing all rows with NA values
 
  uploadData <- dplyr::distinct(uploadData, id, date, .keep_all = TRUE) # Removing duplicate values
  
  
  uploadData <- uploadData[order(uploadData$id, uploadData$date),] # Ordering by ID and Date

  uploadData$burst <- CalcBurst(data = uploadData) # Adding Bursts
  
  uploadData <- CalcMovParams(data = uploadData) # Calculating Parameters
  
  
  print("ShapeFile Upload: Data Manipulation (Movement Parameters) Complete")
  # ---- End of Data Manipulation ----
  
  
  # ---
  # Data Saving
  # ---
  
  # Converting date column back to characters
  uploadData$date <- as.character(uploadData$date)
  
  # Matching column names with that of existing data
  columnNames <- colnames(uploadData)
  
  indices <- match(c("abs.angle", "rel.angle"), columnNames)
  columnNames[indices[1]] <- "abs_angle" # Changing name of abs.angle column
  columnNames[indices[2]] <- "rel_angle" # Changing name of rel.angle column
  
  colnames(uploadData) <- columnNames
  
  rm(columnNames)
  rm(indices)
  
  # Reordering columns to match that of existing data
  uploadData <- dplyr::select(uploadData, colnames(existingData))
  
  # Combining with preexisting data
  data <- rbind(existingData, uploadData)
  
  rm(uploadData)
  rm(existingData)
  
  # Sorting ID and Date
  data <- data[order(data$id, data$date),]
  
  # Overwriting the data file
  st_write(data, dsn = paste(files$migrationData[3], files$migrationData[2], sep = "/"), driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
  
  print("ShapeFile Upload: Data Saving Successful")
  # ---- End of Data Saving ----
})

# ---
# Server Code for the TIF File Upload
# ---

observeEvent(input$tifSubmitButton, {
  # ---
  # Validation
  # ---
  
  
  print("TIF Upload: Validation Complete")
  
  # ---
  # Extracting Information About the .tif File
  # ---
  
  # Create a Raster Layer from the Data
  layer <- raster(input$tifFileUpload$datapath)
  
  # The rasterlayer information will later be saved to a .csv file later for convenience 
  # The dataframe will contain information: File Name, Information, Start Date, End Date
  layerInformation <- data.frame(matrix(ncol = 4, nrow = 1))
  
  # Extracting the Type of Data the .tif File Contains
  dataType <- input$tifFilePurpose
  if(dataType == "Other"){
    dataType <- input$tifFilePurposeTextInput
  }
  
  # Extracting the Date Range of the .tif File.
  startDate <- NA
  endDate <- NA
  if(input$tifDateRangeCheck){
    startDate <- input$tifDateRange[1]
    endDate <- input$tifDateRange[2]
  }
  
  # Generating the Name of the File
  fileName <- paste(paste(dataType, as.numeric(startDate), as.numeric(endDate), sep = "-"), ".tif", sep = "")
  
  print("TIF Upload: Information Extraction and Generation Complete")
  
  # ---
  # Setting and Saving Information About the .tif File
  # ---
  
  # Setting the file name
  layerInformation[1,1] <- fileName
  
  # Setting the information type
  layerInformation[1,2] <- dataType
  
  # Setting the start date
  layerInformation[1,3] <- as.character(startDate)
  
  # Setting the end date
  layerInformation[1,4] <- as.character(endDate)
  
  # Setting column names
  colnames(layerInformation) <- c("File.Name", "Data.Type", "Start.Date", "End.Date")
  
  # Saving the information to a .csv file
  existingData <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))
  
  write.csv(rbind(existingData,layerInformation), 
            file = paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"), 
            append = TRUE,
            row.names = FALSE)
  
  print("TIF Upload: File Information Recorded")
  
  # ---
  # Saving the .tif File
  # ---
  
  raster::writeRaster(layer, paste(files$rasterLayers[3], fileName, sep = "/"))
  
  print("TIF Upload: Raster Layer Saved")
})