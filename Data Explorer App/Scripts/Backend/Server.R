# Serverside Code Main File
server <- function(input, output, session) {
  # Server Code for the Data Upload Tab
  source("./Scripts/Backend/Data Upload Tab/Data Upload.R", local = TRUE)
  
  # Server Code for the Data Definition Tab
  source("./Scripts/Backend/Data Definition Tab/Data Definition.R", local = TRUE)
  
  # Server code for the Data Visualization Tab
  source("./Scripts/Backend/Data Clear Tab/Data Clear.R", local = TRUE)
}