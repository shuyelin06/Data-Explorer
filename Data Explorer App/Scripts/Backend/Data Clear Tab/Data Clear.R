require(sf)

# Clear All Migration Data
observeEvent(input$clearDataButton, {
  migrationData <- st_read(dataFilePath)
  
  migrationData <- migrationData[0,]
  
  st_write(migrationData, dsn = dataFilePath, driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
})


# Clear the Season Definitions
observeEvent(input$clearSeasonDefButton, {
  dataframe <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(dataframe) <- c("number", "month", "season")
  
  write.csv(dataframe, bioYearPath)
  
  print("Season Definitions Cleared")
})

