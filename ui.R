shinyUI(
  fluidPage(  
  withMathJax(
    navbarPage(
      theme = shinytheme("flatly"), collapsible = TRUE,
      tags$head(tags$style(HTML('.navbar-static-top {background-color: green;}',
                                '.navbar-default .navbar-nav>.active>a {background-color: green;}'))),
      title = "FIRSTkit",
      tabPanel(title="Data",
               uiOutput("FIRSTkitData")),
      tabPanel(title = "Descriptive Statistics",
               uiOutput("descriptive")),
      navbarMenu("Probability Theory",
                 tabPanel("Venn Diagram",
                          uiOutput("VennDiagram")),
                 tabPanel("Conditional Probability",
                          uiOutput("BayesTreeDiagram")),
                 tabPanel("Probability Distribution Functions",
                          uiOutput("ProbDistFnt"))
      ),
      navbarMenu("Inference",
                 tabPanel("One-Sample",
                          uiOutput("OneSample")),
                 tabPanel("Two-Samples",
                          uiOutput("TwoSample")),
                 tabPanel("K-Samples",
                          uiOutput("kSamples"))
      ),
      tabPanel("Linear Regression", 
               uiOutput("slr")),
      tabPanel(title = "Help",
               uiOutput("intro")),
    )
  )
)
)
