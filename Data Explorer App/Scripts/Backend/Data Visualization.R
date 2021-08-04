# Data Visualization: Reactive Input Area
output$visualInput <- renderUI({
  # Migration Data
  data <- migrationData()
  
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
          list <- colnames(dplyr::select(data, c(-id, -date, -bioYear, -season, -month)))
          
          # Removing the geometry column (we can have a specific area to plot this later)
          list[!(list %in% c("geometry"))]
        }
      )
    ),
    
    isolate(
      radioButtons(
        inputId = "visualGroupBy",
        label = "Group Data By...",
        choices = c(
          "bioYear",
          "season",
          "month"
        ),
        selected = character(0)
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
  
  # Importing the Data the user wants to display
  displayData <- input$visualChooseData
  
  # Importing how the user wants to group the data
  groupBy <- input$visualGroupBy 
  
  # Importing if the user wants to split animal IDs
  splitIDs <- input$visualSplitIds
  
  # For now, if split IDs is selected, all ids will be displayed
  selectedIDs <- input$visualChooseIds
  
  uiOutput <- NA
  if(is.null(displayData) | is.null(groupBy)){
    uiOutput <- h3("Please Select Data and a Grouping to Display")
  } 
  else if(splitIDs & is.null(selectedIDs)){
    uiOutput <- h3("Please Select IDs")
  } 
  else {
    # https://stackoverflow.com/questions/2908822/speed-up-the-loop-operation-in-r
    # http://r-statistics.co/Strategies-To-Improve-And-Speedup-R-Code.html#:~:text=Strategies%20to%20Speed-up%20R%20Code%201%20Vectorize%20and,ifelse%20%28%29%20whenever%20possible.%20...%20More%20items...%20
    
    # List of plots to be displayed
    plots <- list()
    
    # Create the groupings
    temp <- dplyr::distinct(dplyr::select(migrationData, c('bioYear', groupBy)))
    groupings <- paste(temp$bioYear, temp[[groupBy]], sep = "-")
    rm(temp)
    
    # Plots for Continuous Data
    for(dataType in displayData){
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
      
      
      uiOutput <- plot_grid(plotlist = plots)
    }
      
  }
    
  uiOutput
})
