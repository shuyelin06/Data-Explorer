source("./Scripts/Frontend/Data Definition Tab/Parameter Tab.R")
source("./Scripts/Frontend/Data Definition Tab/Grouping Tab.R")

dataDefineTab <- tabPanel(
  title = "Define Data",
  
  # UI Elements
  titlePanel("Define Parameters in Your Data"),
  
  navlistPanel(
    paramNav,
    groupingNav
  )
)