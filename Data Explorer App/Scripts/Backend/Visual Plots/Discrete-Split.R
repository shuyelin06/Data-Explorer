ChronDiscSplit <- function(migrationData, dataType, groupBy, groupings, chronological, selectedIDs){
  discreteData <- unique(migrationData[[dataType]])
  
  # Creating Dataframe
  data <- data.frame(matrix(ncol = 0, nrow = length(groupings) * length(discreteData) * length(selectedIDs)))
  
  data$id <- rep(selectedIDs, each = length(discreteData) * length(groupings)) # Id column
  data$grouping <- rep(groupings, times = length(discreteData) * length(selectedIDs)) # Grouping column
  data$type <- rep(rep(discreteData, each = length(groupings)), times = length(selectedIDs)) # Type column
  
  # Calculating Averages
  totals <- numeric(nrow(data))
  i <- 1
  if(chronological){
    for(id in unique(data$id)){
      temp <- migrationData[migrationData$id == id,]
      
      for(d in discreteData){
        temp2 <- temp[temp[[dataType]] == d,]
        
        for(grouping in groupings){
          # Extracting Biological Year and Group
          split <- strsplit(grouping, c("-"))[[1]] # Using Regex to extract bioYear and group 
          
          year <- as.numeric(split[1]) # Getting Biological Year
          group <- split[2] # Getting Grouping (season, month, biological year)
          
          rm(split)
          
          
          totals[i] <- nrow(dplyr::filter(temp2, bioYear == year, (!!!rlang::syms(groupBy)) == group))
          i <- i + 1
        }
        
        rm(temp2)
      }
      rm(temp)
    }
  } else {
    for(id in unique(data$id)){
      temp <- migrationData[migrationData$id == id,]
      
      for(d in discreteData){
        temp2 <- temp[temp[[dataType]] == d,]
        
        for(group in groupings){
          totals[i] <- nrow(dplyr::filter(temp2, (!!!rlang::syms(groupBy)) == group))
          i <- i + 1
        }
        
        rm(temp2)
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
  plot <- plot + xlab("Time Group") + ylab(paste("Number of Occurrences")) + ggtitle(paste("Number of Occurrences of", dataType, "Per", groupBy, "For Different IDs")) # Adding labels
  
  if(chronological){
    plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = ""))) # Changing the x-axis tick labels
  } else {
    plot <- plot + scale_x_discrete(labels = data$grouping)
  }
  
  plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
  
  # Facet Wrapping (Each ID will get its own plot)
  plot <- plot + facet_wrap(~id)
  
  plot
}