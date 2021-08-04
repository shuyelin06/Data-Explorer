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
        choices = order(unique(data$id))
      )
    )
    
    )
    
  }
  
  uiOutput
})

# Data Visualization: Reactive Output Area
output$visualView <- renderPlot({
  # Migration Data
  migrationData <- migrationData()

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
    plots <- list()
    
    # Get values for unique biological year - season pairs
    temp <- dplyr::distinct(dplyr::select(st_drop_geometry(migrationData), c('bioYear', groupBy)))
    groupings <- paste(temp[["bioYear"]], temp[[groupBy]], sep = "-")
    rm(temp)
    
    # Continuous Data Plotting
    for(dataType in displayData){
      # Initializing dataframe to hold our extracted data
      data <- data.frame(grouping = groupings)
      
      if(splitIDs){ # If split IDs is specified, create an id column (and have the same groupings for every ID)
        data$grouping <- c() # Clearing the grouping column
        data$id <- c() # Creating an ID column
        
        # Adding groupings for each ID 
        for(id in selectedIDs){
          idFrame <- data.frame(grouping = groupings, id = id)
          data <- rbind(data, idFrame)
        }
        
        rm(id) # Removing unnecessary variables
      }
      
      # For every grouping, average the values for that time frame (and ID)
      averages <- vector(mode = "numeric", length = nrow(data))
      
      for(i in 1:nrow(data)){
        # Extracting Biological Year and Group
        split <- strsplit(data$grouping[i], c("-"))[[1]] # Using Regex to extract bioYear and group 
        
        bioYear <- split[1] # Getting Biological Year
        group <- split[2] # Getting Grouping (season, month, biological year)
        
        rm(split)
        
        # Extracting the data values for that time frame
        temp <- NA
        if(splitIDs){ # If an ID column is specified, filter the data frame with ID as well
          yearMatch <- migrationData[['bioYear']] == as.numeric(bioYear)
          groupMatch <- migrationData[[groupBy]] == group
          idMatch <- migrationData$id == data$id[i]
          
          temp <- migrationData[[dataType]][yearMatch & groupMatch & idMatch]
          rm(idMatch, yearMatch, groupMatch)
        } 
        
        else {
          yearMatch <- migrationData[['bioYear']] == as.numeric(bioYear)
          groupMatch <- migrationData[[groupBy]] == group
          
          temp <- migrationData[[dataType]][yearMatch & groupMatch]
          rm(yearMatch, groupMatch)
        }
        
        temp <- na.omit(temp)
        averages[i] <- mean(temp)
        
        rm(bioYear, group, temp)
      }
      
      # Adding the averages column
      data$average <- averages
      rm(averages)
      
      # Formatting the groupings before plotting
      if(splitIDs){
        data$id <- as.character(data$id) # Setting IDs to characters (so the legend doesn't show a gradient)
      }
      data <- na.omit(data) # Removing NA values
      
      # Creating the plot
      plot <- NA
      
      if(splitIDs){ # Plot for split ids
        # Adding Data
        plot <- ggplot(data, mapping = aes(x = factor(grouping, levels = unique(grouping)), y = average, fill = id))
        
        # Creating the Visuals
        plot <- plot + geom_col(position = "dodge")
        
        # Adding Labels
        # Adding Labels
        plot <- plot + xlab("Time Group") + ylab(paste("Average", dataType)) + ggtitle(paste("Average", dataType, "per", groupBy)) # Adding labels
        plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
        plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
      } else { # Plot for non-split ids
        # Adding Data
        plot <- ggplot(data, mapping = aes(x = factor(grouping, levels = unique(grouping)), y = average)) # Creating the plot
        
        # Creating the Visuals
        plot <- plot + geom_col() # Adding columns
        
        # Adding Labels
        plot <- plot + xlab("Time Group") + ylab(paste("Average", dataType)) + ggtitle(paste("Average", dataType, "per", groupBy)) # Adding labels
        plot <- plot + scale_x_discrete(labels = lapply(strsplit(data$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
        plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
      }
      
      plots <- append(plots, list(plot))
    }
    
    
    uiOutput <- plot_grid(plotlist = plots, ncol = 1)
    
  }
  
  uiOutput
})

# ---
# Plotting Test Code
# ---
# https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf

# Continuous Data: Create a Grouped Barchart (grouped in case IDs are split)





