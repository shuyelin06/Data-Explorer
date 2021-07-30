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
          list <- colnames(dplyr::select(data, c(-id, -date, -bioYear, -season)))
          
          # Removing the geometry column (we can have a specific area to plot this later)
          list[!(list %in% c("geometry"))]
        }
      )
    ),
    
    isolate(
      checkboxGroupInput(
        inputId = "visualGroupBy",
        label = "Group Data By...",
        choices = c(
          "bioYear",
          "season",
          "Month"
        )
      )
    ),
    
    isolate(
      checkboxInput(
        inputId = "visualSplitIds",
        label = "Display Animal IDs Separately?"
      )
    )
    
    )
    
  }
  
  uiOutput
})

# Data Visualization: Reactive Output Area
output$visualView <- renderPlot({
  # Migration Data
  data <- migrationData()

  # Importing the Data the user wants to display
  displayData <- input$visualChooseData
  
  # Importing how the user wants to group the data
  groupBy <- input$visualGroupBy 
  
  # Importing if the user wants to split animal IDs
  splitIds <- input$visualSplitIds
  
  uiOutput <- NA
  if(is.null(displayData)){
    uiOutput <- h3("Please Select Data to Display")
  } else {
    # https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
    
    plots <- list()
    
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
    
    uiOutput <- plot_grid(plotlist = plots, ncol = 2)
    
  }
  
  uiOutput
})