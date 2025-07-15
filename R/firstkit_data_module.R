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
               helpText("FIRSTkit uses the R package googlesheets4. Make sure you have OAuth access credentials. For any question about googlesheets4 package, see https://github.com/tidyverse/googlesheets4."),
               actionButton("load_gsheet", "Load Data")
             ),
             conditionalPanel(
               condition = "input.upload_method == 'manual'",
               textAreaInput("manual_data", "Input data (including header)", "", rows = 8),
               downloadButton("download", "Download .csv"),
               br(),
               downloadButton("download2", "Download .xlsx/.xlsx"),
               br(),
               downloadButton("download3", "Download .ods")
             ),
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
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$manual_data, ".csv")
    },
    content = function(file) {
      write.csv(firstkit.data(), file,row.names = FALSE)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      paste0(input$manual_data, ".xlsx")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet 1")
      writeData(wb,sheet = "Sheet 1", firstkit.data(),rowNames = FALSE)
      saveWorkbook(wb,file, overwrite = TRUE)
    }
  )
  
  output$download3 <- downloadHandler(
    filename = function() {
      paste0(input$manual_data, ".ods")
    },
    content = function(file) {
      write_ods(firstkit.data(), file,row_names = FALSE)
    }
  )
  
  return(firstkit.data)  
}
