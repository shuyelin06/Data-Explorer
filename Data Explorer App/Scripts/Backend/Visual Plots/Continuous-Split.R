ChronContSplit <- function(migrationData, dataType, groupBy, groupings, chronological, selectedIDs){
  # Step 1: Create a dataframe with an ID and Grouping column
  data <- data.frame(matrix(ncol = 0, nrow = length(groupings) * length(selectedIDs)))
  
  data$grouping <- rep(groupings, times = length(selectedIDs))
  data$id <- rep(selectedIDs, each = length(groupings))
  
  
  # Step 2: Calcuate the Averages (for each individual ID)
  averages <- numeric(nrow(data))
  i <- 1
  if(chronological){
    for(id in unique(data$id)){
      temp <- migrationData[migrationData$id == id,]
      
      for(group in groupings){
        # Extracting Biological Year and Group
        split <- strsplit(data$grouping[i], c("-"))[[1]] # Using Regex to extract bioYear and group 
        
        year <- as.numeric(split[1]) # Getting Biological Year
        group <- split[2] # Getting Grouping (season, month, biological year)
        rm(split)
        
        values <- dplyr::filter(temp, bioYear == year, (!!!rlang::syms(groupBy)) == group)[[dataType]] 
        
        averages[i] <- mean(na.omit(values))
        i <- i + 1
      }
      rm(temp)
    }
  } else {
    for(id in unique(data$id)){
      temp <- migrationData[migrationData$id == id,]
      
      for(group in groupings){
        values <- dplyr::filter(temp, (!!!rlang::syms(groupBy)) == group)[[dataType]] 
        
        averages[i] <- mean(na.omit(values))
        i <- i + 1
      }
      rm(temp)
      
    }
  }
  
  # Adding the averages column
  data$average <- averages
  rm(averages, i)
  
  # Formatting the Data
  data$id <- as.character(data$id) # Setting IDs to characters (so the legend doesn't show a gradient)
  data <- na.omit(data) # Removing NA values
  
  # Creating the plot
  plot <- ggplot(data, mapping = aes(x = factor(grouping, levels = unique(grouping)), y = average, fill = id))
  
  
  plot <- plot + geom_col(position = "dodge") # Creating the Visuals
  
  plot <- plot + xlab("Time Group") + ylab(paste("Average", dataType)) + ggtitle(paste("Average", dataType, "per", groupBy)) # Adding labels
  
  if(chronological){
    plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = ""))) # Changing the x-axis tick labels
  } else {
    plot <- plot + scale_x_discrete(labels = data$grouping)
  }
  
  
  plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
  
  plot
}