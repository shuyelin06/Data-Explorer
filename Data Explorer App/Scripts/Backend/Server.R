# Serverside Code Main File
server <- function(input, output, session) {
  # Event Observer for When the Tabs Change
  observeEvent(input$dataTabs, {
    print(paste("Main Tab Changed to:", input$dataTabs))
  })
  
  # Server Code for the Data Upload Tab
  source("./Scripts/Backend/Data Upload.R", local = TRUE)
  
  # Server Code for the Data Definition Tab
  source("./Scripts/Backend/Data Definition.R", local = TRUE)
  
  # Server code for the Data Visualization Tab
  source("./Scripts/Backend/Data Visualization.R", local = TRUE)
  
  # Server code for the Data Clear Tab
  source("./Scripts/Backend/Data Clear.R", local = TRUE)
}