ChronContNoSplit <- function(migrationData, dataType, groupBy, groupings, chronological){
  data <- data.frame(grouping = groupings)
  
  # Calcuating the Averages (for Later Plotting)
  averages <- numeric(nrow(data))
  if(chronological){ # Chronological Code
    
    for(i in 1:nrow(data)){
      # Extracting Biological Year and Group
      split <- strsplit(data$grouping[i], c("-"))[[1]] # Using Regex to extract bioYear and group 
      
      year <- as.numeric(split[1]) # Getting Biological Year
      group <- split[2] # Getting Grouping (season, month, biological year)
      
      rm(split)
      
      # Extracting Values in this Group
      values <- dplyr::filter(migrationData[migrationData$bioYear == year,], (!!!rlang::syms(groupBy)) == group)[[dataType]]
      
      averages[i] <- mean(na.omit(values))
    }
    
  } else { # Non-Chronological Code
    for(i in 1:nrow(data)){
      group <- data$grouping[i]
      
      # Extracting Values in this Group
      values <- dplyr::filter(migrationData, (!!!rlang::syms(groupBy)) == group)[[dataType]]
      
      averages[i] <- mean(na.omit(values))
    }
  }
  
  # Adding the averages column
  data$average <- averages
  rm(averages)
  data <- na.omit(data) # Removing NA values
  
  
  # Adding Data
  plot <- ggplot(data, mapping = aes(x = factor(grouping, levels = unique(grouping)), y = average)) # Creating the plot
  
  # Creating the Visuals
  plot <- plot + geom_col() # Adding columns
  
  # Adding Labels
  plot <- plot + xlab("Time Group") + ylab(paste("Average", dataType)) + ggtitle(paste("Average", dataType, "per", groupBy)) # Adding labels
  
  if(chronological){
    plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
  } else {
    plot <- plot + scale_x_discrete(labels = data$grouping)
  }
  
  plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
  
  plot
}
