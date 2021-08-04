source("./Scripts/Frontend/Data Definition Tab/Parameter Tab.R", local = TRUE)
source("./Scripts/Frontend/Data Definition Tab/Select Tab.R", local = TRUE)
source("./Scripts/Frontend/Data Definition tab/LayerDefine Tab.R", local = TRUE)

dataDefineTab <- tabPanel(
  title = "Define Data",
  
  # UI Elements
  titlePanel("Define Parameters in Your Data"),
  
  navlistPanel(
    paramNav,
    layerDefineNav,
    dataSelectNav,
    
    id = "dataDefineTab"
  )
)