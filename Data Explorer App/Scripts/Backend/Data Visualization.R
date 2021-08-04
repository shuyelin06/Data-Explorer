# Data Visualization: Reactive Input Area
output$visualInput <- renderUI({
  # Migration Data
  data <- migrationData()
  
  dataDefine <- layerDefinitions()
  
  # UI Output 
  uiOutput <- NA
  
  if(is.null(data)){
    uiOutput <- h3("Please Load Migration Data First")
  } else {
    uiOutput <- tagList(
    
    isolate(
      checkboxGroupInput(
        inputId = "visualChooseData",
        label = "Select the Data You Would Like to Display",
        choices = {
          # Removing GroupBy columns
          list <- colnames(data)
          
          # Removing all data types who don't have discrete or continuous defined for them
          list[list %in% dataDefine$DataType]
        }
      )
    ),
    
    isolate(
      radioButtons(
        inputId = "visualGroupBy",
        label = "Group Data By...",
        choices = c(
          "None",
          "bioYear",
          "season",
          "month"
        )
      )
    ),
    
    isolate(
      checkboxInput(
        inputId = "visualSplitIds",
        label = "Display Animal IDs Separately?"
      )
    ),
    conditionalPanel(
      condition = "input.visualSplitIds",
      checkboxGroupInput(
        inputId = "visualChooseIds",
        label = "Select the IDs You Would Like to Include",
        choices = unique(data$id)
      )
    )
    
    )
    
  }
  
  uiOutput
})

# Data Visualization: Reactive Output Area
output$visualView <- renderPlot({
  # Migration Data
  migrationData <- st_drop_geometry(migrationData())
  
  # Getting if the Layer Data is Discrete or Continuous
  layerDefine <- layerDefinitions()
  
  # Importing the Data the user wants to display
  displayData <- input$visualChooseData
  
  # Importing how the user wants to group the data
  groupBy <- input$visualGroupBy 
  
  # Importing if the user wants to split animal IDs
  splitIDs <- input$visualSplitIds
  
  # For now, if split IDs is selected, all ids will be displayed
  selectedIDs <- input$visualChooseIds
  
  uiOutput <- NA
  
  # Checking if a groupBy and dataType is selected
  if(is.null(displayData)){
    uiOutput <- h3("Please Select Data and a Grouping to Display")
  } 
  
  # Checking if SplitIDs is selected and, if so, if any IDs are selected
  else if(splitIDs & is.null(selectedIDs)){
    uiOutput <- h3("Please Select IDs")
  } 
  
  # Density Plots if no group by is selected
  else if(groupBy == "None"){
    
  }
  
  # Plot continiuous and discrete data
  else {
    # List of plots to be displayed
    plots <- list()
    
    # Create the groupings
    temp <- dplyr::distinct(dplyr::select(migrationData, c('bioYear', groupBy)))
    groupings <- paste(temp$bioYear, temp[[groupBy]], sep = "-")
    rm(temp)
    
    # Plots for Continuous Data
    for(dataType in displayData[displayData %in% layerDefine$DataType[layerDefine$Definition == "Continuous"]]){
      # Plotting for Split IDs
      if(splitIDs){
        
        # Step 1: Create a dataframe with an ID and Grouping column
        data <- data.frame(matrix(ncol = 0, nrow = length(groupings) * length(selectedIDs)))
        
        data$grouping <- rep(groupings, times = length(selectedIDs))
        data$id <- rep(selectedIDs, each = length(groupings))
        
        
        # Step 2: Calcuate the Averages (for each individual ID)
        time <- Sys.time() # Keeping track of time (to optimize time later)
        
        averages <- numeric(nrow(data))
        i <- 1
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
        print(paste("Data Visual: Calculating Averages", "-", Sys.time() - time, "seconds"))
        
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
        plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
        
        plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
        
        plots <- append(plots, list(plot))
      } 
      
      # Plotting Code for Non-Split IDs
      else { 
        data <- data.frame(grouping = groupings)
        
        # Calcuating the Averages (for Later Plotting)
        time <- Sys.time()
        
        averages <- numeric(nrow(data))
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
        print(paste("Data Visual: Calculating Averages", "-", Sys.time() - time, "seconds"))
        
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
        plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
        plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
        
        plots <- append(plots, list(plot))
      }
      
    }
      
    print(displayData[displayData %in% layerDefine$DataType[layerDefine$Definition == "Discrete"]])
    # Plots for Discrete Data
    for(dataType in displayData[displayData %in% layerDefine$DataType[layerDefine$Definition == "Discrete"]]){
      print(paste("Discrete Data", dataType))
      
      if(splitIDs){
        discreteData <- unique(migrationData[[dataType]])
        
        # Creating Dataframe
        data <- data.frame(matrix(ncol = 0, nrow = length(groupings) * length(discreteData) * length(selectedIDs)))
        
        data$id <- rep(selectedIDs, each = length(discreteData) * length(groupings)) # Id column
        data$grouping <- rep(groupings, times = length(discreteData) * length(selectedIDs)) # Grouping column
        data$type <- rep(rep(discreteData, each = length(groupings)), times = length(selectedIDs)) # Type column
        
        # Calculating Averages
        totals <- numeric(nrow(data))
        i <- 1
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
        plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
        plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
        
        # Facet Wrapping (Each ID will get its own plot)
        plot <- plot + facet_wrap(~id)
        
        plots <- append(plots, list(plot))
      } else {
        discreteData <- unique(migrationData[[dataType]])
        
        # Creating Dataframe
        data <- data.frame(matrix(ncol = 0, nrow = length(groupings) * length(discreteData)))
        
        data$grouping <- rep(groupings, times = length(discreteData))
        data$type <- rep(discreteData, each = length(groupings))
        
        # Calculating Averages
        totals <- numeric(nrow(data))
        i <- 1
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
        plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
        plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot

        plots <- append(plots, list(plot))
      }
    }
    
    uiOutput <- plot_grid(plotlist = plots)
  }
    
  uiOutput
})
