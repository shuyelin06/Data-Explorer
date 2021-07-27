# Shiny Documentation: https://shiny.rstudio.com/reference/shiny/1.6.0/
# Remove All Variables: rm(list = ls(all = TRUE))

# Checking that the user has the required packages installed
if(any(c("circular", "sf", "raster", "dplyr", "shiny") %in% installed.packages()[,1] == FALSE)){
  stop("You must install the following packages: circular, sf, raster, dplyr, shiny")
}


# Loading Packages
library(shiny) # Loading the Shiny Package

library(dplyr) # Loading the dplyr package
library(raster) # Loading the raster package
library(sf) # Loading the sf package

source("./Scripts/Functions/CalcBurst.R") # Loading the CalcBurst custom function created by Dr. Merkle
source("./Scripts/Functions/CalcMovParams.R") # Loading the CalcMovParams custom function created by Dr. Merkle
library(circular) # Loading the circular package needed for the custom functions


# Defining global variables (will be used throughout the app)
files <- read.csv("./Data/Settings/Files.csv") # Contains the various different file paths

migrationData <- NA # Will contain the saved migration data

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
