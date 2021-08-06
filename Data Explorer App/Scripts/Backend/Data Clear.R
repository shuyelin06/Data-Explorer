# ---
# Clear Migration Data
# ---
observeEvent(input$clearDataButton, {
  cat("\n\n --- Data Clear: Clearing Migration Data --- ")
  
  path <- paste(files$migrationData[3], files$migrationData[2], sep = "/")
  
  data <- st_read(path)
  
  data <- data[0,]
  
  st_write(data, dsn = path, driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
  
  cat("\n --- Data Select: Migration Data Cleared --- ")
})


# ---
# Clear Season Definitions
# ---
observeEvent(input$clearBioYearButton, {
  cat("\n\n --- Data Clear: Clearing the BioYear --- ")
  
  path <- paste(files$bioYear[3], files$bioYear[2], sep = "/")
  
  dataframe <- data.frame(matrix(ncol = 3, nrow = 0))
  
  colnames(dataframe) <- c("number", "month", "season")
  
  write.csv(dataframe, path, row.names = FALSE)
  
  cat("\n --- Data Clear: BioYear Cleared --- ")
})


# ---
# Clear Raster Layers
# ---
observeEvent(input$clearRasterButton, {
  cat("\n\n --- Data Clear: Clearing the Raster Layers --- ")
  
  layerPath <- paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/")
  defPath <- paste(files$layerDefine[3], files$layerDefine[2], sep = "/")
  
  # Clearing the Layer .csv File
  dataframe <- data.frame(matrix(ncol = 4, nrow = 0))
  
  colnames(dataframe) <- c("Path", "Data.Type", "Start.Date", "End.Date")
  
  write.csv(dataframe, layerPath)
  
  # Clearing the Definition File
  dataframe <- data.frame(matrix(ncol = 2, nrow = 0))
  
  dataframe <- rbind(dataframe, 
                     c("Distance", "Continuous"), 
                     c("Speed", "Continuous"), 
                     c("Relative.Angle", "Continuous")
  )
  
  colnames(dataframe) <- c("DataType", "Definition")
  
  write.csv(dataframe, defPath)
  
  cat("\n --- Data Clear: Raster Layers Cleared --- ")
})
