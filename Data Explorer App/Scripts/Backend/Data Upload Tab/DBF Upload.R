library(sf)

# Event occurs when the submit button in the DBF tab is pressed
observeEvent(input$dbfSubmitButton, {
  # Notification that the button has been pressed
  print("Button Pressed") 
  
  # DBF file patha
  # The data can be read, but the geometries column disappears?
  print(st_read(input$dbfFileUpload$datapath))
  
  # ID column name
  print(input$dbfIdInput)
  
  # Date column name
  print(input$dbfDateInput)
})