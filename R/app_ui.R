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


logo <- dataURI(file = "inst/app/www/firstkit-logo-final-header.png", mime = "image/png")

app_ui <- function() {
  navbarPage(
    title = HTML(paste0(
      '<div style="display: flex; align-items: center;">',
      '<img src="', logo, '" height="40" style="margin-top: -13px;">',
      '</div>'
    )),
windowTitle="FIRSTkit",
theme = shinytheme("flatly"),
             collapsible = TRUE,
             tags$head(
               tags$style(HTML(paste(
               ".navbar-static-top {background-color: green;}",
               ".navbar-default .navbar-nav>.active>a {background-color: green;}",
               "body {font-size: 16px;}",
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
