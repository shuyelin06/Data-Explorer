library(dplyr)
library(raster)

observeEvent(input$tifSubmitButton, {
  # ---
  # Validation
  # ---

  
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
  colnames(layerInformation) <- c("File Name", "Data Type", "Start Date", "End Date")
  
  # Saving the information to a .csv file
  write.csv(layerInformation, file = paste(rasterLayerPath, "Layers.csv", sep = "/"))
  
  # ---
  # Saving the .tif File
  # ---
  raster::writeRaster(layer, paste(rasterLayerPath, fileName, sep = "/"))
})