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

mcode <- function(tinput) {
  df <- tryCatch(read.csv(text = tinput, header = TRUE), error = function(e) NULL)
  if (is.null(df)) return("Invalid manual input.")
  
  code.lines <- lapply(names(df), function(col) {
    values <- paste(df[[col]], collapse = ", ")
    paste0(col, " = c(", values, ")")
  })
  
  paste0("mydata <- data.frame(", paste(code.lines, collapse = ", "), ")")
}

dd <- data(package = "datasets")
dnames <- dd$results[,"Item"]
dnames <- sub(" \\(.*", "", dnames)

data_ui <- tabPanel("Data",
         sidebarLayout(
           sidebarPanel(
              radioButtons("upload_method", "Select Data Input Method:",
                           choices = c("R datasets" = "rdata","Upload File" = "file", "Google Sheets" = "gsheet", "Manual Entry" = "manual")),
              conditionalPanel(
              condition="input.upload_method =='rdata'",
               selectInput("rdatasets", "Choose Dataset:", choices = dnames),
             ),
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
           mainPanel(
             tabsetPanel(
               tabPanel("Data",
                        br(),
                         dataTableOutput("contents")), 
               tabPanel("R code",
                        br(),
                        verbatimTextOutput("code"))
             )
         )
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
  
  observeEvent(input$rdatasets, {
    req(input$upload_method == "rdata", input$rdatasets)
    tryCatch({
      df <- get(input$rdatasets, "package:datasets")
      
      if (is.vector(df) || is.ts(df)) {
        df <- data.frame(x = as.numeric(df))
      } else if (is.matrix(df) || is.table(df)) {
        df <- as.data.frame(df)
      }
      
      firstkit.data(df)
    }, error = function(e) {
      showNotification("Error loading R dataset.", type = "error")
    })
  })
  
  
  output$contents <- renderDT({
    req(firstkit.data())
    datatable(firstkit.data(), options = list(pageLength = 10))
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
  
  output$code <- renderText({
    req(input$upload_method)
    
    code <- switch(input$upload_method,
                   file = {
                     if (!is.null(input$file1)) {
                       ext <- tools::file_ext(input$file1$name)

                       switch(ext,
                              csv  = paste0('mydata <- read.csv(file = "', input$file1$name, '")'),
                              xlsx = paste0('library("readxl")\nmydata <- read_excel(path = "', input$file1$name, '")'),
                              xls  = paste0('library("readxl")\nmydata <- read_excel(path = "', input$file1$name, '")'),
                              ods  = paste0('library("readODS")\nmydata <- read_ods(path = "', input$file1$name, '")'),
                              "Unknown file type")
                     } else {
                       "No file selected."
                     }
                   },
                   gsheet = {
                     if (nzchar(input$gsheet_url)) {
                       paste0('library("googlesheet4")\nmydata <- read_sheet("', input$gsheet_url, '")')
                     } else {
                       "No Google Sheets URL provided."
                     }
                   },
                   manual = {
                     if (nzchar(input$manual_data)) {
                       mcode(input$manual_data)
                     } else {
                       "No manual text provided."
                     }
                   },
                   rdata = {
                     paste0("mydata <- ",input$rdatasets)
                   }
    )
    
    code
  })
  
  return(firstkit.data)  
}
