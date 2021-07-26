# ---
# Server Code for the Shape File Upload
# ---
library(sf)
library(dplyr)
library(circular)

source("./Scripts/Functions/CalcBurst.R")
source("./Scripts/Functions/CalcMovParams.R")

# Event occurs when the submit button in the ShapeFile tab is pressed
observeEvent(input$sfSubmitButton, {
  # ---
  # Validation: Checking that all necessary fields are filled
  # ---
  
  
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
  
  
  # Extracting the ID column name
  idCol <- input$sfIdInput
  
  # Extracting the Date column name
  dateCol <- input$sfDateInput
  
  
  print("ShapeFile Upload: Data Extraction Complete")
  
  # ---
  # Manipulating Data: Altering the Data so it Meets Criteria for Saving 
  # ---
  
  # Reading the csv file where the previous information is stored
  existingData <- st_read(dataFilePath)
  
  # Checking if the animals are unique or not. 
  # If the animals are unique, we change the IDs of the animals whose IDs conflict with preexisting animals
  if(input$sfUniqueCheck == TRUE & nrow(existingData) != 0){
    print("ShapeFile Upload: Upload Data is Unique")
    
    # Get the IDs in both data sets
    existingIDs <- unique(existingData$id)
    
    uploadedIDs <- unique(uploadData[[idCol]])
    
    # Find the IDs in the uploaded data that conflict
    conflictIndices <- match(existingIDs, uploadedIDs)
    
    # Changing the ids of the animals to a unique one
    newID <- max(as.numeric(existingIDs)) + 1
    for(i in conflictIndices){
      ID <- uploadedIDs[i]
      
      uploadData[uploadData[[idCol]] == ID, idCol] <- as.character(newID)
      
      newID <- newID + 1
    }
  }
  
  # Converting Date Column to POSIXct format
  uploadData[[dateCol]] <- as.POSIXct(uploadData[[dateCol]], format = "%Y-%m-%d %H:%M:%S")
  
  
  # Removing all rows with NA values
  uploadData <- na.omit(uploadData)
  
  # Ordering by ID and Date
  uploadData <- uploadData[order(uploadData[[idCol]], uploadData[[dateCol]]),]
  
  # Removing duplicate values using the Dplyr distinct function
  uploadData <- dplyr::distinct(uploadData)
  
  
  # Calculating Movement Parameters - Adding Bursts
  uploadData$burst <- CalcBurst(data = uploadData, id_name = idCol, date_name = dateCol, Tmax = 3600 * 7)
  
  # Calculating Movement Parameters
  uploadData <- CalcMovParams(data = uploadData, id_name = idCol, date_name = dateCol)
  
  
  # Converting date column back to characters
  uploadData[[dateCol]] <- as.character(uploadData[[dateCol]])
  
  print("ShapeFile Upload: Data Manipulation (Movement Parameters) Complete")
  
  # ---
  # Combining with the Preexisting data
  # ---
  
  # Editing column names to match that of existing data
  columnNames <- colnames(uploadData)
  
  indices <- match(c(idCol, dateCol, "abs.angle", "rel.angle"), columnNames)
  columnNames[indices[1]] <- "id" # Changing ID column name to "id"
  columnNames[indices[2]] <- "date" # Changing date column name to "date"
  columnNames[indices[3]] <- "abs_angle"
  columnNames[indices[4]] <- "rel_angle"
  
  colnames(uploadData) <- columnNames
  
  # Reordering Columns to match that of existing data
  uploadData <- dplyr::select(uploadData, colnames(existingData))
  
  # Combining with preexisting data
  data <- rbind(existingData, uploadData)
  
  # Sorting the data by ID and Date
  data <- data[order(data$id, data$date),]
  
  # Overwriting the data file
  st_write(data, dsn = dataFilePath, driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
  
  print("ShapeFile Upload: Data Saving Successful")
})

# ---
# Server Code for the TIF File Upload
# ---
library(dplyr)
library(raster)

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
  existingData <- read.csv(paste(rasterLayerPath, "Layers.csv", sep = "/"))
  
  write.csv(rbind(existingData,layerInformation), 
            file = paste(rasterLayerPath, "Layers.csv", sep = "/"), 
            append = TRUE,
            row.names = FALSE)
  
  print("TIF Upload: File Information Recorded")
  
  # ---
  # Saving the .tif File
  # ---
  
  raster::writeRaster(layer, paste(rasterLayerPath, fileName, sep = "/"))
  
  print("TIF Upload: Raster Layer Saved")
})