library(sf)
library(dplyr)
library(circular)

source("./Scripts/Functions/CalcBurst.R")
source("./Scripts/Functions/CalcMovParams.R")

# Event occurs when the submit button in the ShapeFile tab is pressed
observeEvent(input$sfSubmitButton, {
  # ---
  # Extracting Information
  # ---
  
  # Extracting the data
  filemap <- input$sfFileUpload
  previouswd <- getwd()
  
  uploadDirec <- dirname(filemap$datapath[1])
  setwd(uploadDirec)
  
  for(i in 1:nrow(filemap)){
    file.rename(filemap$datapath[i], filemap$name[i])
  }
  
  setwd(previouswd)
  rm(previouswd)
  
  uploadData <- st_read(paste(uploadDirec, filemap$name[grep(pattern = "*.shp$", filemap$name)], sep = "/"))
  rm(uploadDirec)
  rm(filemap)
  
  
  # Extracting the ID column name
  idCol <- input$sfIdInput
  
  # Extracting the Date column name
  dateCol <- input$sfDateInput
  
  
  print("Data Extraction Complete")
  
  # ---
  # Manipulating the Received Data 
  # ---
  
  # Reading the csv file where the previous information is stored
  existingData <- st_read(dataFilePath)
  
  # Checking if the animals are unique or not. 
  # If the animals are unique, we change the IDs of the animals whose IDs conflict with preexisting animals
  if(input$sfRadioButton == "yes" & nrow(existingData) != 0){
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
  
  # Ordering
  uploadData <- uploadData[order(uploadData[[idCol]], uploadData[[dateCol]]),]
  
  # Removing duplicate values using the Dplyr distinct function
  uploadData <- dplyr::distinct(uploadData)
  
  
  # Calculating Movement Parameters - Adding Bursts
  uploadData$burst <- CalcBurst(data = uploadData, id_name = idCol, date_name = dateCol, Tmax = 3600 * 7)
  
  # Calculating Movement Parameters
  uploadData <- CalcMovParams(data = uploadData, id_name = idCol, date_name = dateCol)
  
  
  # Converting date column back to characters
  uploadData[[dateCol]] <- as.character(uploadData[[dateCol]])
  
  print("Data Manipulation (Movement Parameters) Complete")
  
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
  migrationData <- rbind(existingData, uploadData)
  
  # Sorting the data by ID and Date
  migrationData <- migrationData[order(migrationData$id, migrationData$date),]
  
  # Overwriting the data file
  st_write(migrationData, dsn = dataFilePath, driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
  
  print("Data Saving Successful")
})