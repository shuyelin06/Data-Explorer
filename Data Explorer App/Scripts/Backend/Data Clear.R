# Clear Preexisting Migration Data
observeEvent(input$clearDataButton, {
  path <- paste(files$migrationData[3], files$migrationData[2], sep = "/")
  
  data <- st_read(path)
  
  data <- data[0,]
  
  st_write(data, dsn = path, driver = "ESRI Shapefile", append = FALSE, delete_layer = TRUE)
  
  print("Data Clear: Migration Data Cleared")
})


# Clear the Season Definitions
observeEvent(input$clearSeasonDefButton, {
  path <- paste(files$bioYear[3], files$bioYear[2], sep = "/")
  
  dataframe <- data.frame(matrix(ncol = 3, nrow = 0))
  
  colnames(dataframe) <- c("number", "month", "season")
  
  write.csv(dataframe, path, row.names = FALSE)
  
  print("Data Clear: Season Definitions Cleared")
})


# Clear the TIF Files
