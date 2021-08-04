# This is where I'm copy and pasting scripts that may be used later. For example, I could use code here to create helper scripts to help format data for the app or whatnot.

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
  
  rm(existingIDs, uploadedIDs, conflictIndices, ID, newID)
}



# ---
# If no groupby is specified, display data in one variable plots
if(is.null(groupBy)){
  # Iterate through every data type
  for(dataType in displayData){
    plot <- NA
    
    # Create plot
    plot <- ggplot(data, aes_string(dataType))
    
    
    # If data is continuous, use geom_density
    plot <- plot + geom_density(kernel = "gaussian")
    
    # If data is discrete, use geom_bar (don't know how to know if its discrete?)
    
    
    
    plots <- append(plots, list(plot))
  }
} 

# Else, display data in x/y variable plots
else {
  # Have a group of plots for each data type
  for(dataType in displayData){
    # Have a plot for each grouping
    for(grouping in groupBy){
      plot <- NA
      
      # Create plot
      plot <- ggplot(data, aes_string("date", dataType))
      
      plot <- plot + geom_line()
      
      
      plots <- append(plots, list(plot))
    }
    
  }
}
