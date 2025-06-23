library(shiny)
library(FIRSTkit)

shinyApp(ui = FIRSTkit::app_ui(), server = FIRSTkit::app_server)
