# This script is where I create large chunks of code for testing (so I don't need to run the app repetitively to test code)

files <- read.csv("./Data/Settings/Files.csv")

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


# ---
# Adding Biological Year, Seasons, and Months
# ---

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

# Moving bioYear and seasons to the front of the dataframe (as they're used for grouping)
data <- dplyr::select(data, id, date, bioYear, season, dist, speed, abs_angle, rel_angle, geometry)

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


bioYear <- 2012
dplyr::filter(data, (!!!rlang::syms("bioYear")) == bioYear, season == "Middle")$season


# ----
# Plotting Code
# ----

# Importing the Data the user wants to display
displayData <- c("dist", "speed")

# Importing how the user wants to group the data
groupBy <- "bioYear"

# Importing if the user wants to split animal IDs
splitIds <- FALSE

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
  plot(plot_grid(plotlist = plots, ncol = 2))
}


# ---
# Plotting Test Code
# ---
# https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf

# Continuous Data: Create a Grouped Barchart (grouped in case IDs are split)

# When group by is selected (CONTINUOUS DATA), average the values for a given time period and plot those values 
splitIDs <- TRUE
selectedIDs <- unique(data$id)
groupBy <- "month"
dataType <- "Tree Cover"

# Get unique values for unique biological year - season pairs
temp <- dplyr::distinct(dplyr::select(st_drop_geometry(data), c('bioYear', groupBy)))

groupings <- data.frame(grouping = paste(temp[["bioYear"]], temp[[groupBy]], sep = "-"))
rm(temp)


if(splitIDs){ # If split IDs is specified, create the same groupings for each id
  groups <- groupings$grouping # Saving the groupings 
  
  groupings$grouping <- c() # Clearing the grouping column
  groupings$id <- c() # Creating an ID column
  
  # Adding groupings for each ID 
  for(id in selectedIDs){
    idFrame <- data.frame(grouping = groups, id = id)
    
    groupings <- rbind(groupings, idFrame)
  }
  
  rm(id, groups, idFrame, splitIDs, selectedIDs) # Removing unnecessary variables
  
}

# For every grouping, average the values for that time frame (and ID)
averages <- c() 

for(i in 1:nrow(groupings)){
  # Extracting Biological Year and Group
  grouping <- groupings$grouping[i]
  
  split <- strsplit(grouping, c("-"))[[1]]
  
  bioYear <- split[1] # Getting Biological Year
  group <- split[2] # Getting Grouping (season, month, biological year)
  
  rm(split, grouping)
  
  # Extracting the data values for that time frame
  temp <- NA
  if('id' %in% colnames(groupings)){ # If an ID column is specified, filter the data frame with ID as well
    yearMatch <- data[['bioYear']] == as.numeric(bioYear)
    groupMatch <- data[[groupBy]] == group
    idMatch <- data$id == groupings$id[i]
      
    temp <- data[[dataType]][yearMatch & groupMatch & idMatch]
    rm(idMatch, yearMatch, groupMatch)
  } else {
    yearMatch <- data[['bioYear']] == as.numeric(bioYear)
    groupMatch <- data[[groupBy]] == group
    
    temp <- data[[dataType]][yearMatch & groupMatch]
    rm(yearMatch, groupMatch)
  }
  
  temp <- na.omit(temp)
  
  averages <- append(averages, mean(temp))
  
  rm(bioYear, group, temp)
}

# Adding the averages column
groupings$average <- averages
rm(averages)

# Formatting the groupings before plotting
groupings$id <- as.character(groupings$id) # Setting IDs to characters (so the legend doesn't show a gradient)
groupings <- na.omit(groupings) # Removing NA values


# Creating the plot
plot <- NA

if('id' %in% colnames(groupings)){ # Plot for split ids
  # Adding Data
  plot <- ggplot(data = groupings, mapping = aes(x = factor(grouping, levels = unique(grouping)), y = average, fill = id))
  
  # Creating the Visuals
  plot <- plot + geom_col(position = "dodge")
  
  # Adding Labels
  # Adding Labels
  plot <- plot + xlab("Time Group") + ylab(paste("Average", dataType)) + ggtitle(paste("Average", dataType, "per", groupBy)) # Adding labels
  plot <- plot + scale_x_discrete(labels = lapply(strsplit(groupings$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
  plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
} else { # Plot for non-split ids
  # Adding Data
  plot <- ggplot(data = groupings, mapping = aes(x = factor(grouping, levels = unique(grouping)), y = average)) # Creating the plot
  
  # Creating the Visuals
  plot <- plot + geom_col() # Adding columns
  
  # Adding Labels
  plot <- plot + xlab("Time Group") + ylab(paste("Average", dataType)) + ggtitle(paste("Average", dataType, "per", groupBy)) # Adding labels
  plot <- plot + scale_x_discrete(labels = lapply(strsplit(groupings$grouping, c("-")), function(vector) paste(vector[2], " (", vector[1], ")", sep = "")))# Changing the x-axis tick labels
  plot <- plot + theme(axis.text.x = element_text(face = "bold", angle = 45))# Customizing the theme of the plot
}


plot




# Time to beat: 3.52612996101379 seconds
# Extracting Biological Year and Group
split <- strsplit(data$grouping[i], c("-"))[[1]] # Using Regex to extract bioYear and group 

year <- as.numeric(split[1]) # Getting Biological Year
group <- split[2] # Getting Grouping (season, month, biological year)

lapply(list(data$grouping, data$id), print)





time <- Sys.time()
vector <- sapply(paste(data$id, data$grouping, sep = "-"), extractWithSplit)

extractWithSplit <- function(d){
  split <- strsplit(d, c("-"))[[1]]
  
  values <- dplyr::filter(migrationData[migrationData$id == as.numeric(split[1])], bioYear == as.numeric(split[2]), (!!!rlang::syms(groupBy)) == split[3])
  
  mean(values[[dataType]])
}
print(vector)
print(Sys.time() - time)

groupBy <- "month"
dataType <- "Tree Cover"
system.time({
  d <- dplyr::filter(migrationData[migrationData$id == 1,], bioYear == 2011, (!!!rlang::syms(groupBy)) == 2)
  
  print(d)
  })
averages <- dplyr::filter(migrationData, bioYear == year, (!!!rlang::syms(groupBy)) == group)
averages <- numeric(nrow(d))