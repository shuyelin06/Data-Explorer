# Loading the Shiny Package
# Functions: https://shiny.rstudio.com/reference/shiny/1.6.0/
# rm(list = setdiff(ls(), lsf.str()))
library(shiny)

library(dplyr)
library(raster)
library(sf)

source("./Scripts/Functions/CalcBurst.R")
source("./Scripts/Functions/CalcMovParams.R")
library(circular)

# ---
# Variable Definitions

# Defining global variables that many scripts use, including file paths. 
# This could later be all held in some settings file for accessibility and viewability
# ---

# Loading the File Paths in
files <- read.csv("./Data/Settings/Files.csv")

# Migration Data Global Variable
migrationData <- NA

# Month Names Global Variable
monthNames <- data.frame(month = 1:12, name = c(
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

# --- 
# Application Set-Up 

# Setting up the application and its settings
# ---

# Changing File Upload Size to a100 MB max
options(shiny.maxRequestSize = 100 * 1024^2)

# Importing the User Interface Main File (Frontend)
source("./Scripts/Frontend/User Interface.R", local = TRUE)

# Importing the Server Code Main File (Backend)
source("./Scripts/Backend/Server.R", local = TRUE)


# Running the Shiny App
shinyApp(ui = ui, server = server)