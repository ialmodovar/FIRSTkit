library(shiny)
library(readxl)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "datachoice",
        label = "Data",
        choices = c("Upload file","Input data"),
        multiple = FALSE,
        selected = NULL##"Input data"
      ),
      hr(),
      conditionalPanel(
      condition = "input.datachoice == 'Upload file'",
      fileInput("datachoice",
                "Data",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                  ".csv",
                  ".xlsx",
                  ".xls",
                  ".ods"
                ),
                placeholder = "...",
                buttonLabel = "Upload File"
      ),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      conditionalPanel(
        condition = "output.dateiformat == 'csvtxt'",
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t",
                                 WhiteSpace=""),
                     selected = ","),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        

      ),
      conditionalPanel(
        condition = "output.dateiformat == 'xlsx'",
        
        p("Import options for xlsx go here")
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Diagramm",
                 fluidPage(plotOutput("userBoxplot"))),
        
        tabPanel("Tabelle",
                 fluidPage(tableOutput("userTable")))
      )
    )
  )
)

server <- function(input, output) {
  
  userDaten <- reactive({
    
    validate(need(input$userDatei, "No file is selected"))
    infile <- input$userDatei
    
    if (tolower(tools::file_ext(infile$datapath)) == "xlsx") { 
      
      output$dateiformat <- renderText("xlsx")
      outputOptions(output, "dateiformat", suspendWhenHidden = FALSE)
      
      daten <- read_xlsx(infile$datapath,
                         col_names = input$header)
      
    } else {
      
      output$dateiformat <- renderText("csvtxt")
      outputOptions(output, "dateiformat", suspendWhenHidden = FALSE)
      
      daten <- read.csv(
        infile$datapath,
        header = input$header,
        dec = input$dec,
        sep = input$sep,
        quote = input$quote
      )
    }
  })
  
  output$userBoxplot <- renderPlot({
    validate(
      need(any(unlist(lapply(userDaten(), is.numeric))),
           "Dezimalzeichen und Seperator müssen angepasst werden.")
    )
    boxplot(userDaten())
  })
  
  output$userTabelle <- renderTable({
    userDaten()
  })
}

shinyApp(ui = ui, server = server)
