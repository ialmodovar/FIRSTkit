library("shiny")
library("readxl")
library("readODS")
library("googlesheets4")
library("plotly")
library("dplyr")
library("venn")
library("shinythemes")
library("MASS")
library("DiagrammeR") 
library("car")
library("ggplot2")

options(shiny.maxRequestSize = 30 * 1024^2)

source("intro_module.R")
source("firstkit_data_module.R")
source("probability_module.R")
source("descriptive_statistics_module.R")
source("statistical_inference.R")
source("linear_regression_module.R")

ui <- navbarPage("FIRSTkit",
                 theme = shinytheme("flatly"), 
                 collapsible = TRUE,
                 tags$head(tags$style(HTML(paste(
                   '.navbar-static-top {background-color: green;}',
                   '.navbar-default .navbar-nav>.active>a {background-color: green;}',
                   sep = "\n"
                 )))),
                 intro_ui,
                 data_ui,
                 probability_ui,
                 descriptive_stats_ui,
                 stats_inference_ui,
                 linear_regression_ui,
)


server <- function(input, output, session) {
  
  intro_server(input,output,session)
  firstkit.data <- data_server(input, output, session)
  probability_server(input,output,session)
  descriptive_stats_server(input,output,session,firstkit.data)
  stats_inference_server(input,output,session,firstkit.data)
  linear_regression_server(input,output,session,firstkit.data)
}

shinyApp(ui = ui, server = server)
