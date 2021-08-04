# This script is where I create large chunks of code for testing (so I don't need to run the app repetitively to test code)

files <- read.csv("./Data/Settings/Files.csv")

time <- Sys.time()
print("Data Select: Importing and Formatting Migration Data")

# Importing Migration Data
data <- st_read(paste(files$migrationData[3], files$migrationData[2], sep = "/"))

# Converting ID Column to Numeric
data$id <- as.numeric(data$id)

# Converting Date Column to POSIXct
data$date <- lubridate::ymd_hms(data$date, 
                                quiet = TRUE,
                                truncated = 3
)

# Removing Burst, DT and StepFlag columns (I don't think they'll be important in plotting)
data <- dplyr::select(data, id, date, dist, speed, abs_angle, rel_angle, geometry)

print(paste("Data Select: Finished Formatting Migration Data", "-", Sys.time() - time, "seconds"))


# ---
# Adding Biological Year, Seasons, and Months
# ---
time <- Sys.time()
print("Data Select: Adding Bio Year and Seasons")

# Adding Month to the Migration Data
data$month <- month(data$date)


# Extracting information from the Biological Year.csv file
bioYearData <- read.csv(paste(files$bioYear[3], files$bioYear[2], sep = "/")) # Importing information from the file

# Adding Seasons to the Migration Data
for(i in 1:nrow(bioYearData)){
  month <- bioYearData$number[i] # Obtaining the month
  season <- bioYearData$season[i] # Obtaining the associated season
  
  # Adding the season to the migration data 
  data$season[month(data$date) == month] <- season
}
rm(i, month, season) # Clearing unneeded variables from memory


# Adding Biological Year to the Migration Data
startMonth <- bioYearData$number[1] # Finding the start month to the biological year
rm(bioYearData) # Removing bioYearData (no longer needed)

biologicalYear <- data.frame(matrix(ncol = 3, nrow = 0)) # Dataframe to be used in adding the bioYear column

min <- min(data$date) # Smallest POSIXct date in the data
max <- max(data$date) # Largest POSIXct date in the data

for(bioYear in seq(from = as.numeric(format(min, "%Y")) - 1, to = as.numeric(format(max, "%Y")) + 1)){
  startDate <- NA
  endDate <- NA
  
  # If start month is from Oct - Dec, the bioyear STARTS from the PREVIOUS year's Oct-Dec
  if(10 <= startMonth & startMonth <= 12){
    startDate <- as.POSIXct(paste(bioYear - 1, startMonth, 1, sep = "/"), format = "%Y/%m/%d")
    endDate <- as.POSIXct(paste(bioYear, startMonth, 1, sep = "/"), format = "%Y/%m/%d") - 1
  } 
  # If the start month is from Jan - Sept, the bioyear ENDS on the NEXT year's Jan - Sept
  else {
    startDate <- as.POSIXct(paste(bioYear, startMonth, 1, sep = "/"), format = "%Y/%m/%d")
    endDate <- as.POSIXct(paste(bioYear + 1, startMonth, 1, sep = "/"), format = "%Y/%m/%d") - 1
  }
  
  # Adding to the biological year dataframe
  biologicalYear <- rbind(biologicalYear, c(bioYear, startDate, endDate))
}
rm(min, max, bioYear, startMonth, startDate, endDate) # Removing unneeded variables from memory

colnames(biologicalYear) <- c("bioYear", "start", "end") # Formatting the BioYear Data Frame

for(i in 1:nrow(biologicalYear)){
  withinYear <- biologicalYear$start[i] <= data$date & data$date <= biologicalYear$end[i]
  
  data$bioYear[withinYear] <- biologicalYear$bioYear[i]
}
rm(biologicalYear, i, withinYear) # Removing unneeded variables from memory

# Moving bioYear, seasons, and month to the front of the dataframe (as they're used for grouping)
data <- dplyr::select(data, id, date, bioYear, season, month, dist, speed, abs_angle, rel_angle, geometry)

print(paste("Data Select: Finished Adding Bio Year and Seasons", "-", Sys.time() - time, "seconds"))

# ---
# Adding Information from Selected TIF Files
# --- 
time <- Sys.time()
print("Data Select: Adding TIF File Information")

# Retrieving the TIF Data that the User Wants
requestedData <- input$selectTIFinfo

# Import layer Information
layerData <- read.csv(paste(files$rasterLayers[3], files$rasterLayers[2], sep = "/"))

for(dataType in layerData$Data.Type){
  print(paste("Extracting Values for", dataType))
  
  # Adding a new blank column for the data type
  data[[dataType]] <- NA
  
  # Iterating through every row with the given data type
  rows <- layerData[layerData$Data.Type == dataType,]
  for(row in 1:nrow(rows)){
    path <- rows[row,"Path"]
    
    # Check if raster layer file exists
    if(!file.exists(path)){
      warning(paste("File", path, "could not be found"))
      next
    }
    
    # Creating Raster Layer
    rasterLayer <- raster(path)
    rm(path)
    
    # Taking the Migration Data's geometry and converting it to Spatial Points (with the crs of the raster layer)
    geometry <- spTransform(as_Spatial(data$geometry), crs(rasterLayer))
    print("Geometry created")
    
    # Tells us which rows to extract from geometry and edit on the migration data frame (so if there are multiple raster layers with different extents, we don't overwrite data from other layers)
    noValue <- is.na(data[[dataType]]) 
    print("NO value")
    
    if(is.na(rows[row,"Start.Date"]) | is.na(rows[row,"End.Date"])){
      data[[dataType]][noValue] <- raster::extract(rasterLayer, geometry[noValue])
    } else {
      startDate <- as.POSIXct(rows[row, "Start.Date"], format = "%Y-%m-%d")
      endDate <- as.POSIXct(rows[row, "End.Date"], format = "%Y-%m-%d")
      
      withinDates <- startDate <= data$date  & data$date <= endDate
      rm(startDate)
      rm(endDate)
      
      data[[dataType]][noValue & withinDates] <- raster::extract(rasterLayer, geometry[noValue & withinDates])
    }
    print("Extraction")
    
    rm(rasterLayer, geometry)
  }
  rm(rows)
  
  print(paste("Finished Extracting Values for", dataType))
}
rm(dataType, requestedData, layerData)

print(paste("Data Select: Finished Adding TIF File Information", "-", Sys.time() - time, "seconds"))
rm(time)






# ----
# Plotting Code
# ----


# --

# Importing the Data the user wants to display
displayData <- c("Tree Cover")

dataType <- "Test"
# Importing how the user wants to group the data
groupBy <- "month"

# Importing if the user wants to split animal IDs
splitIDs <- FALSE

# For now, if split IDs is selected, all ids will be displayed
selectedIDs <- c(1, 11)



# Create the groupings
temp <- unique(dplyr::select(migrationData, c('bioYear', groupBy)))
groupings <- paste(temp$bioYear, temp[[groupBy]], sep = "-")
rm(temp)


# --- Plotting Discrete (Non-Split)

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
rm(totals, i)


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

plot

plots <- append(plots, list(plot))




# --- Plotting Discrete (Split)

dataType <- "Test"
# Importing how the user wants to group the data
groupBy <- "month"

# Importing if the user wants to split animal IDs
splitIDs <- FALSE

# For now, if split IDs is selected, all ids will be displayed
selectedIDs <- c(1, 11)


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
rm(totals, i)


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

plot

plots <- append(plots, list(plot))
