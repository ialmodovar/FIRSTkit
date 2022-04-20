
shinyUI(
  fluidPage(  
  withMathJax(
    navbarPage(
      title = "FIRSTkit",
      tabPanel(title = "Home",
               uiOutput("intro")),
      tabPanel(title = "Descriptive Statistics",
               uiOutput("descriptive")),
      navbarMenu("Probability Theory",
                 tabPanel("Events"),
                 tabPanel("Bayes Tree Diagram",
                          uiOutput("BayesTreeDiagram")),
                 tabPanel("Distribution Functions")
      ),
      navbarMenu("Inference",
                 tabPanel("One-Sample",
                          uiOutput("OneSample")),
                 tabPanel("Two-Sample",
                          uiOutput("TwoSample")),
                 tabPanel("Three-Sample",
                          uiOutput("kSample"))
      ),
      tabPanel("Linear Regression", 
               uiOutput("slr")),
    )
  )
)
)
