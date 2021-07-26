# Clear Preexisting Migration Data

library(sf)

observeEvent(input$clearDataButton, {
  data <- st_read(dataFilePath)
  
  data <- data[0,]
  
  st_write(data, dsn = dataFilePath, driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
  
  print("Data Clear: Migration Data Cleared")
})


# Clear the Season Definitions
observeEvent(input$clearSeasonDefButton, {
  dataframe <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(dataframe) <- c("number", "month", "season")
  
  write.csv(dataframe, bioYearPath, row.names = FALSE)
  
  print("Data Clear: Season Definitions Cleared")
})


# Clear the TIF Files
