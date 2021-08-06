ChronDiscNoSplit <- function(migrationData, dataType, groupBy, groupings, chronological){
  discreteData <- unique(migrationData[[dataType]])
  
  # Creating Dataframe
  data <- data.frame(matrix(ncol = 0, nrow = length(groupings) * length(discreteData)))
  
  data$grouping <- rep(groupings, times = length(discreteData))
  data$type <- rep(discreteData, each = length(groupings))
  
  # Calculating Averages
  totals <- numeric(nrow(data))
  i <- 1
  if(chronological){
    for(d in discreteData){
      temp <- migrationData[migrationData[[dataType]] == d,]
      
      for(grouping in groupings){
        # Extracting Biological Year and Group
        split <- strsplit(grouping, c("-"))[[1]] # Using Regex to extract bioYear and group 
        
        year <- as.numeric(split[1]) # Getting Biological Year
        group <- split[2] # Getting Grouping (season, month, biological year)
        
        rm(split)
        
        
        totals[i] <- nrow(dplyr::filter(temp, bioYear == year, (!!!rlang::syms(groupBy)) == group))
        i <- i + 1
      }
      rm(temp)
    }
  } else {
    for(d in discreteData){
      temp <- migrationData[migrationData[[dataType]] == d,]
      
      for(group in groupings){
        totals[i] <- nrow(dplyr::filter(temp, (!!!rlang::syms(groupBy)) == group))
        i <- i + 1
      }
      rm(temp)
    }
  }
  
  
  data$count <- totals
  rm(totals, i, discreteData)
  
  # Formatting Data Before Plotting
  data$type <- as.character(data$type)
  data <- na.omit(data)
  
  # Plotting Data
  plot <- ggplot(data, mapping = aes(x = factor(grouping, levels = unique(grouping)), y = count, fill = type)) # Creating the plot
  
  plot <- plot + geom_col(position = "stack") # Adding columns
  
  # Adding Labels
  plot <- plot + xlab("Time Group") + ylab(paste("Number of Occurrences")) + ggtitle(paste("Number of Occurrences of", dataType, "per", groupBy)) # Adding labels
  
  if(chronological){
    plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = ""))) # Changing the x-axis tick labels
  } else {
    plot <- plot + scale_x_discrete(labels = data$grouping)
  }
  
  plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
  
  plot
}