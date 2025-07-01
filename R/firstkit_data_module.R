##*********************************************
##*
##* @file: firstkit_data_module.R
##*
##* Server function for FIRSTkit software
##*
##* Author:
##* Israel Almodovar-Rivera PhD
##* Department of Mathematical Sciences
##* University of Puerto Rico at Mayaguez
##* israel.almodovar@upr.edu
##* Copyright June 2025
##*********************************************


data_ui <- tabPanel("Data",
         sidebarLayout(
           sidebarPanel(
             radioButtons("upload_method", "Select Data Input Method:",
                          choices = c("Upload File" = "file", "Google Sheets" = "gsheet", "Manual Entry" = "manual")),
             conditionalPanel(
               condition = "input.upload_method == 'file'",
               fileInput("file1", "Choose file", accept = c(".csv", ".xlsx", ".xls", ".ods"))
             ),
             conditionalPanel(
               condition = "input.upload_method == 'gsheet'",
               textInput("gsheet_url", "Enter Google Sheets URL"),
               actionButton("load_gsheet", "Load Data")
             ),
             conditionalPanel(
               condition = "input.upload_method == 'manual'",
               textAreaInput("manual_data", "Input data (including header)", "", rows = 8)
             )
           ),
           mainPanel(tableOutput("contents"))
         )
)

data_server <- function(input, output, session) {
  firstkit.data <- reactiveVal(NULL)
  
  observeEvent({
    input$file1
    input$load_gsheet
    input$manual_data
    input$upload_method
  }, {
    req(input$upload_method)
    
    if (input$upload_method == "file" && !is.null(input$file1)) {
      ext <- tools::file_ext(input$file1$name)
      df <- switch(ext,
                   csv = read.csv(input$file1$datapath),
                   xlsx = read_excel(input$file1$datapath),
                   xls  = read_excel(input$file1$datapath),
                   ods  = read_ods(input$file1$datapath),
                   NULL)
      firstkit.data(df)
      
    } else if (input$upload_method == "gsheet") {
      observeEvent(input$load_gsheet, {
        tryCatch({
          df <- read_sheet(input$gsheet_url)
          firstkit.data(df)
        }, error = function(e) {
          showNotification("Error loading Google Sheet.", type = "error")
        })
      }, ignoreInit = TRUE)
      
    } else if (input$upload_method == "manual") {
      tryCatch({
        df <- read.csv(text = input$manual_data)
        firstkit.data(df)
      }, error = function(e) {
        showNotification("Error reading manual input.", type = "error")
      })
    }
  })
  
  output$contents <- renderTable({
    head(firstkit.data(), 15)
  })
  
  return(firstkit.data)  
}
