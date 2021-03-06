# Root directory for the file selects will begin from
root <- c(Home = path.expand("~"))

# ---
# Server Code for the Shape File Upload
# ---

# Will store the shape file data
shapeFileData <- reactiveVal(value = NULL)

# Instantiate the .shp file upload button
shinyFileChoose(
  input = input, 
  id = 'sfFileUpload',
  roots = root,
  filetypes = c('shp'),
)

# Reactive text which displays the path of the file selected
output$sfFileDisplay <- renderText({
  parseFilePaths(root, input$sfFileUpload)$datapath
})

# Load the Data in After Selecting an Appropriate .shp File
observe({
  sfFilePath <- parseFilePaths(root, input$sfFileUpload)$datapath
  
  # If there is a path, load the data
  if(length(sfFilePath) != 0){
    cat("Data Upload: Path obtained")
    
    data <- st_read(sfFilePath)
    
    # Checking if the file is a point file
    cat("\n\n --- Data Upload: Checking if the file is a point file --- ")
    if(!all(st_geometry_type(data) == "POINT")){
      cat("\nFile is not a point file")
      stop("File is not a point file!") # If the file is not, throw an error
    } 
    # If the file is a point file, transform the points to a particular projection
    else {
      cat("\nFile is a point file; reprojecting to +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
      st_transform(data, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    }
    
    shapeFileData(data)
  }
})

# Load the Column Names (When Data is Loaded)
output$sfColSelect <- renderUI({
  data <- shapeFileData()
  
  if(is.data.frame(data)){
    cat("\n\n --- Data Upload: Rendering the column select for ID/Date --- ")
    
    
    columnNames <- colnames(data)
    
    uiOutput <- tagList(titlePanel("Select ID and Date Column"))
    
    # Adding Radio Buttons for ID Column Select
    uiOutput <- tagList(uiOutput, isolate(radioButtons(
      inputId <- "sfIdCol",
      label = "Select ID Column",
      choices = columnNames,
      selected = character(0)
    )))
    
    # Adding Radio Buttons for Date Column Select
    uiOutput <- tagList(uiOutput, isolate(radioButtons(
      inputId <- "sfDateCol",
      label = "Select Date Column",
      choices = columnNames,
      selected = character(0)
    )))
    
    cat("\n --- Data Upload: Finished rendering the column select for ID/Date --- ")
    
    uiOutput
  }
})

# Retrieve Other Information, Calculate Movement Parameters, and Save File
observeEvent(input$sfSubmit, {
  data <- shapeFileData()
  
  # ---
  # Validation: Checking that all necessary fields are filled
  # ---
  cat("\n\n --- Data Upload: Validating --- ")

  validate(
    # Check that there is data loaded
    need(data, message = 'There is no ShapeFile Loaded'),
    
    # Check that the id/date columns are selected
    need(input$sfIdCol, message = 'ID Column is empty'),
    
    need(input$sfDateCol, message = 'Date Column is empty')
  )
  
  cat("\n --- Data Upload: Validation Tests Passed --- ")
  
  # ---- End of Validation ----
  
  
  # ---
  # Calculating Movement Parameters
  # ---
  cat("\n\n --- Data Upload: Calculating Movement Parameters --- ")
  
  # Changing the id and date column names to "id" and "date", respectively (to later use dplyr::distinct)
  cat("\n\n - Changing name of ID and Date Columns - ")
  
  columnNames <- colnames(data)
  
  indices <- match(c(input$sfIdCol, input$sfDateCol), columnNames)
  columnNames[indices[1]] <- "id" # Changing ID column name to "id"
  columnNames[indices[2]] <- "date" # Changing date column name to "date"
  
  colnames(data) <- columnNames
  
  rm(columnNames, indices)
  cat("\nFinished changing name of ID and Date Columns")
  
  # Calculating Movement Parameters
  cat("\n\n - Converting date column to POSIXct - ")
  data$date <- lubridate::ymd_hms(data$date, 
                                            quiet = TRUE,
                                            truncated = 3
                                            ) # Converting Date Column to POSIXct format
  # ^ Currently, the timezone becomes UTC. I'm not sure if this will be an issue later?
  # For some reason, trying to change the time zone causes the RStudio application to freeze. I'm not sure why.
  cat("\nFinished converting date to POSIXct")
  
  
  # Warn the user if there were dates that were round NA 
  if(sum(is.na(data)) > 0){
    warning(paste(sum(is.na(data)), "rows will be deleted because the date was not read in properly"))
  }
  
  
  cat("\n\n - Removing NA values - ")
  data <- na.omit(data) # Removing all rows with NA values
  cat("\nFinished removing NA values")
  
  
  cat("\n\n - Removing duplicate values - ")
  data <- dplyr::distinct(data, id, date, .keep_all = TRUE) # Removing duplicate values
  cat("\nFinished removing duplicate values")
  
  
  cat("\n\n - Ordering by ID and Date - ")
  data <- data[order(data$id, data$date),] # Ordering by ID and Date
  cat("\nFinished ordering by ID and Date")
  
  
  cat("\n\n - Calculating Bursts - ")
  data$burst <- CalcBurst(data, Tmax = 3600 * as.numeric(input$sfTmax)) # Adding Bursts
  cat("\nFinished calculating Bursts")
  
  
  cat("\n\n - Calculating Movement Parameters - \n")
  data <- CalcMovParams(data) # Calculating Parameters
  cat("\nFinished Calculating Movement Parameters")
  
  
  cat("\n\n - Removing Unnecessary Columns - \n")
  data <- dplyr::select(data, !c(burst, dt, abs.angle, StepFlag))
  cat("\nFinished Removing Columns")
  
  cat("\n --- Data Upload: Movement Parameters Calculated --- ")

  # ---- End of Data Manipulation ----
  
  
  # ---
  # Data Saving
  # ---
  cat("\n\n --- Data Upload: Saving Migration Data --- ")
  
  # Converting date column back to characters
  cat("\n\n - Converting date column back to characters - ")
  data$date <- as.character(data$date)
  cat("\nFinished converting date column back to characters")
  
  # Overwriting the data file
  cat("\n\n - Overwriting the migration data file - ")
  st_write(data, 
           dsn = paste(files$migrationData[3], files$migrationData[2], sep = "/"), 
           driver = "ESRI Shapefile", 
           append = FALSE, 
           delete_layer = TRUE
  )
  cat("\nFinished overwriting the migration data file")
  
  shapeFileData(NULL) # Clearing the shapeFileData to save memory
  output$sfFileDisplay <- renderText({
    "Migration Data Saved"
  })
  
  cat("\n --- Data Upload: Migration Data Saved --- ")
  
  # ---- End of Data Saving ----
})


# ---
# Server Code for TIF File Upload
# ---

layerPath <- reactiveVal(value = NULL)

# File Chooser
shinyFileChoose(
  input = input, 
  id = 'layerUpload',
  roots = root,
  filetypes = c('tif', 'img'),
)


# Update the Layer Paths after files are selected
observe({
  layerPath(parseFilePaths(root, input$layerUpload)$datapath)
})

output$defineLayerInfo <- renderUI({
  layers <- layerPath()
  
  if(length(layers) != 0){
    cat("\nData Upload: Layer Path/Paths Found")
    
    uiOutput <- tagList(titlePanel("Provide Information on Each Layer"))
    
    cat("\n\n --- Data Upload: Rendering UI for each path --- ")
    for(i in 1:ncell(layers)){
      path <- layers[i]
      
      # Path of the file 
      uiOutput <- tagList(uiOutput, 
                          
        titlePanel(path),
        
        isolate(radioButtons(
          inputId = paste("layerData", as.character(i), sep = ""),
          label = "What Data Does this File Display?",
          choices = {
            append(
              unique(
                read.csv(
                  paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/")
                )$Data.Type
              ), 
              c("Other")
            )
          },
          selected = character(0)
        )),
        
        isolate(conditionalPanel(
          condition = paste("input.", paste("layerData", as.character(i), sep = ""), " == 'Other'", sep = ""),
          textInput(
            inputId = paste("layerData", as.character(i), "Text", sep = ""),
            label = "Please enter the purpose of the data here"
          )
        )),
        
        isolate(checkboxInput(
          inputId = paste("layerDateCheck", as.character(i), sep = ""),
          label = "Valid Under a Date Range?",
          value = FALSE
        )),
        
        isolate(conditionalPanel(
          condition = paste("input.", paste("layerDateCheck", as.character(i), sep = ""), sep = ""),
          dateRangeInput(
            inputId = paste("layerDateRange", as.character(i), sep = ""),
            label = "Enter the Date Range in which this file is valid."
          )
        )))
    }
    cat("\n --- Data Upload: Finished rendering UI for each path --- ")
    uiOutput
  }
})

# Information Recording
observeEvent(input$layerSubmit, {
  layers <- layerPath()
  
  # ---
  # Validation
  # ---
  cat("\n\n --- Layer Upload: Validating... --- ")
  
  validate(
    need(layers, label = "Layers")
  )
  
  tests <- c()
  for(i in 1:ncell(layers)){
    layer <- layers[i]
    tests <- append(tests, 
                    need(input[[paste("layerData", as.character(i), sep = "")]], label = "No DataType Selected")
                    )
  }

  validate(tests)
  
  cat("\n --- Layer Upload: Validation Check Passed --- ")
  
  # ---
  # Extracting Information
  # ---
  cat("\n\n --- Data Upload: Extracting layer information --- ")
  
  # The information will be saved to a .csv file for convenience 
  layerInformation <- data.frame(matrix(ncol = 4, nrow = 0))
  
  for(i in 1:ncell(layers)){
    # Extracting the path
    path <- layers[i]
    cat(paste("\nExtracting information for", path))
    
    # Extracting the Type of Data the .tif File Contains
    cat("\n\n - Extracting data type of layer - ")
    dataType <- input[[paste("layerData", as.character(i), sep = "")]]
    if(dataType == "Other"){
      dataType <- input[[paste("layerData", as.character(i), "Text", sep = "")]]
    }
    cat("\nFinished extracting data type of layer")
    
    # Extracting the Date Range of the .tif File.
    cat("\n\n - Extracting date range of layer - ")
    startDate <- NA
    endDate <- NA
    if(input[[paste("layerDateCheck", as.character(i), sep = "")]]){
      startDate <- input[[paste("layerDateRange", as.character(i), sep = "")]][1]
      endDate <- input[[paste("layerDateRange", as.character(i), sep = "")]][2]
    }
    cat("\nFinished extracting date range of layer")
    
    
    layerInformation <- rbind(layerInformation, c(path, dataType, as.character(startDate), as.character(endDate)))
  }
  # Setting column names
  colnames(layerInformation) <- c("Path", "Data.Type", "Start.Date", "End.Date")
  
  cat("\n --- Data Upload: Finished extracting layer information --- ")
  
  
  # Saving the information to a .csv file
  cat("\n\n --- Data Upload: Saving layer information --- ")
  
  cat("\n\n - Pulling existing layer information - ")
  existingData <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))
  
  cat("\n\n - Writing information to file - ")
  write.csv(rbind(existingData,layerInformation), 
            file = paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"),
            row.names = FALSE)
  rm(layerInformation, existingData)
  
  cat("\n --- Data Upload: Finished saving layer information --- ")
  
  layerPath(NULL)
  output$layerConsole <- renderText({
    "Raster Layers Uploaded"
  })
})
