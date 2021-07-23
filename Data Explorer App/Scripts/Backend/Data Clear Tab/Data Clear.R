require(sf)

observeEvent(input$clearDataButton, {
  dataFilePath <- "./Data/Migration Data/Migration Data.shp"
  
  migrationData <- st_read(dataFilePath)
  
  migrationData <- migrationData[0,]
  
  st_write(migrationData, dsn = dataFilePath, driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
})

