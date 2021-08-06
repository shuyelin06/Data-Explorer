# Shiny Documentation: https://shiny.rstudio.com/reference/shiny/1.6.0/
# Remove All Variables: rm(list = ls(all = TRUE))

# Checking that the user has the required packages installed
if(any(c("shiny", 
         "shinyFiles", 
         "ggplot2",
         "cowplot",
         "lubridate",
         "dplyr",
         "raster",
         "sf",
         "circular") 
       %in% installed.packages()[,1] == FALSE)){
  
  stop("You must install the following packages: shiny, shinyFiles, ggplot2, cowplot, lubridate, dplyr, raster, sf, and circular")
}


# Loading Packages
require(shiny) # Loading the Shiny Package used to create the app
require(shinyFiles) # Loading the shinyFiles package needed to access client-side files

require(ggplot2) # Loading the ggplot2 package used for data plotting
require(cowplot) # Loading the cowplot package used to have multiple plots in a grid - like layout.

require(lubridate) # Loading the lubridate package used for dates
require(dplyr) # Loading the dplyr package used for data manipulation
require(raster) # Loading the raster package used for tif and img files
require(sf) # Loading the sf package used for shapefiles

require(circular) # Loading the circular package needed for the custom functions
source("./Scripts/Functions/CalcBurst.R", local = TRUE) # Loading the CalcBurst custom function created by Dr. Merkle
source("./Scripts/Functions/CalcMovParams.R", local = TRUE) # Loading the CalcMovParams custom function created by Dr. Merkle


# Defining global variables (will be used throughout the app)
files <- read.csv("./Data/Settings/Files.csv") # Contains the various different file paths

monthNames <- data.frame(month = 1:12, name = c( # Contains the months in the year and their names
  "January", 
  "February", 
  "March", 
  "April", 
  "May", 
  "June", 
  "July", 
  "August", 
  "September", 
  "October", 
  "November", 
  "December"
))


# Application Set-Up 
options(shiny.maxRequestSize = 500 * 1024^2) # Changing File Upload Size to 500 MB max

source("./Scripts/Frontend/User Interface.R", local = TRUE) # Importing the User Interface Main File (Frontend)

source("./Scripts/Backend/Server.R", local = TRUE) # Importing the Server Code Main File (Backend)


# Running the Shiny App
shinyApp(ui = ui, server = server)
