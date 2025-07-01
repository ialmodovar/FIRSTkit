##*********************************************
##*
##* @file: app_ui.R
##*
##* UI for FIRSTkit software
##*
##* Author:
##* Israel Almodovar-Rivera PhD
##* Department of Mathematical Sciences
##* University of Puerto Rico at Mayaguez
##* israel.almodovar@upr.edu
##* Copyright June 2025
##*********************************************

app_ui <- function() {
  navbarPage("FIRSTkit",
             theme = shinythemes::shinytheme("flatly"),
             collapsible = TRUE,
             tags$head(tags$style(HTML(paste(
               '.navbar-static-top {background-color: green;}',
               '.navbar-default .navbar-nav>.active>a {background-color: green;}',
               sep = "\n"
             )))),
             intro_ui,
             data_ui,
             descriptive_stats_ui,
             probability_ui,
             stats_inference_ui,
             linear_regression_ui,
  )
}
