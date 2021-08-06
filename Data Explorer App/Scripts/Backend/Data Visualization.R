# Importing Plotting Functions (code was moved to reduce clutter)
source("./Scripts/Backend/Visual Plots/Continuous-Split.R", local = TRUE)
source("./Scripts/Backend/Visual Plots/Continuous-NoSplit.R", local = TRUE)

source("./Scripts/Backend/Visual Plots/Discrete-Split.R", local = TRUE)
source("./Scripts/Backend/Visual Plots/Discrete-NoSplit.R", local = TRUE)

# Data Visualization: Reactive Input Area
output$visualInput <- renderUI({
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
          "Biological Year",
          "Season",
          "Month"
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.visualGroupBy != 'None'",
      checkboxInput(
        inputId = "visualChronological",
        label = "Display Groups Chronologically?"
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
  # Importing Data
  migrationData <- migrationData() # Migration Data
  layerDefine <- layerDefinitions() # Discrete/Continuous Layer Data
  
  # Booleans to determine what plots to create
  chronological <- input$visualChronological # If the user wants to display the groups chronologically or not
  splitIDs <- input$visualSplitIds # If the user wants to split animal IDs or not
  
  
  # Importing the Data the user wants to display
  displayData <- input$visualChooseData # The data the user wants to display
  groupBy <- input$visualGroupBy # The way the user wants to group the data
  selectedIDs <- input$visualChooseIds # The IDs the user wants to display (if they want to split IDs)
  
  uiOutput <- NA
  
  # Checking if a groupBy and dataType is selected
  if(is.null(displayData)){
    cat("\n\n Unable to Plot: No DataTypes Selected")
  } 
  
  # Checking if SplitIDs is selected and, if so, if any IDs are selected
  else if(splitIDs & is.null(selectedIDs)){
    cat("\n\n Unable to Plot: No IDs Selected ")
  } 
  
  # If all conditions pass, plot the data 
  else {
    cat("\n\n --- Data Visualization: Plotting --- ")
    # ---
    # Preparing for Plotting
    # ---
    
    cat("\n\n - Removing Geometry Column from Data - ")
    # Removing Geometry Column from Data
    migrationData <- st_drop_geometry(migrationData)
    
    
    cat("\n\n - Extracting the GroupBy Selection - ")
    # Converting groupBy to the appropriate column name
    if(groupBy == "Biological Year"){
      groupBy <- "bioYear"
    } else if (groupBy == "Season"){
      groupBy <- "season"
    } else if (groupBy == "Month"){
      groupBy <- "month"
    }

    # ---
    # Plotting
    # ---
    
    # List of plots to be displayed
    plots <- list()
    
    # Plots for if GroupBy is "None"
    if(groupBy == "None"){
      cat("\n\n - Group By is None - Creating Density/Bar Plots - ")
      for(dataType in displayData){
        cat(paste("\n- Plotting for", dataType))
            
        # Pulling Data
        data <- migrationData[!is.na(migrationData[[dataType]]), c("id", dataType)]
        if(splitIDs) data <- data[data$id %in% selectedIDs,] # If splitting IDs, only keep the IDs selected
        
        # Initializing plot
        plot <- ggplot(data, aes((!!!rlang::syms(dataType))))
        
        # Continuous Plot (Density Plot)
        if(dataType %in% layerDefine$DataType[layerDefine$Definition == "Continuous"]) {
          plot <- plot + geom_density()
          plot <- plot + ylab("Density")
          plot <- plot + ggtitle(paste("Density of", dataType))
        }
        # Discrete Plot (Bar Plot)
        else {
          plot <- plot + geom_bar()
          plot <- plot + ylab("Frequency")
          plot <- plot + ggtitle(paste("Frequency of", dataType))
        }

        
        # If splitting by IDs, use facet_wrap        
        if(splitIDs) plot <- plot + facet_wrap(~id)
        
        # Adding Labels
        plot <- plot + xlab(dataType)
        
        plots <- append(plots, list(plot))
        cat(paste("\nFinished plotting for", dataType))
      }
    }
    
    # Plotting for Different GroupBys
    else {
      cat("\n\n - Group By is Not None - ")
      groupings <- NA
      if(chronological){
        cat("\n - Chronological is True - ")
        temp <- dplyr::distinct(dplyr::select(migrationData, c('bioYear', groupBy)))
        groupings <- paste(temp$bioYear, temp[[groupBy]], sep = "-")
        rm(temp)
      } else {
        cat("\n - Chronological is False - ")
        groupings <- unique(migrationData[[groupBy]])
      }
      
      
      # Plots for Continuous Data
      for(dataType in displayData[displayData %in% layerDefine$DataType[layerDefine$Definition == "Continuous"]]){
        cat(paste("\n- Plotting for", dataType))
        # Plotting for Split IDs
        if(splitIDs){
          plots <- append(plots, list(ChronContSplit(migrationData, dataType, groupBy, groupings, chronological, selectedIDs)))
        } 
        # Plotting Code for Non-Split IDs
        else { 
          plots <- append(plots, list(ChronContNoSplit(migrationData, dataType, groupBy, groupings, chronological)))
        }
        cat(paste("\nFinished plotting for", dataType))
      }
      
      # Plots for Discrete Data
      for(dataType in displayData[displayData %in% layerDefine$DataType[layerDefine$Definition == "Discrete"]]){
        cat(paste("\n- Plotting for", dataType))
        if(splitIDs){
          plots <- append(plots, list(ChronDiscSplit(migrationData, dataType, groupBy, groupings, chronological, selectedIDs)))
        } 
        else {
          plots <- append(plots, list(ChronDiscNoSplit(migrationData, dataType, groupBy, groupings, chronological)))
        }
        cat(paste("\nFinished plotting for", dataType))
      }
    }
  
    uiOutput <- plot_grid(plotlist = plots)
    cat("\n --- Data Visualization: Finished Plotting --- ")
  }
    
  uiOutput
})
