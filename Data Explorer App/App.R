# Loading the Shiny Package
# Functions: https://shiny.rstudio.com/reference/shiny/1.6.0/
library(shiny)

# Changing File Upload Size to a100 MB max
options(shiny.maxRequestSize = 100 * 1024^2)

# Importing the User Interface Main File (Frontend)
source("./Scripts/Frontend/User Interface.R")

# Importing the Server Code Main File (Backend)
source("./Scripts/Backend/Server.R")


# Running the Shiny App
shinyApp(ui = ui, server = server)