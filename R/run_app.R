##*********************************************
##
## @file: run_app.R
##
## Run FIRSTkit software
##
## Author:
## Israel Almodovar-Rivera PhD
## Department of Mathematical Sciences
## University of Puerto Rico at Mayaguez
## israel.almodovar@upr.edu
## Copyright June 2025
##*********************************************

FIRSTkit <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
