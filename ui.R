## Thanks for @timelyportfolio for this comment/fix
## https://github.com/timelyportfolio/functionplotR/issues/1#issuecomment-224369431
 getdeps <- function() {
   htmltools::attachDependencies(
     htmltools::tagList(),
     c(
       htmlwidgets:::getDependency("datatables", "DT"),
       htmlwidgets:::getDependency("renderPlotly","plotly")
      )
   )
 }

shinyUI(
  fluidPage(theme = shinytheme("sandstone"),
            withMathJax(
  navbarPage("FIRSTkit",
    tabPanel("Home", uiOutput("intro")),
    tabPanel("Descriptive Statistics", uiOutput("descriptive"),getdeps()),
#    tabPanel("Inference",cases.inference, uiOutput("Inference"), getdeps()),
    tabPanel("One-Sample Inference", uiOutput("OneSample"), getdeps()),
    tabPanel("Two-Sample Inference", uiOutput("TwoSample")),#, getdeps()),
    tabPanel("Three or more-Sample Inference", uiOutput("kSample")),#, getdeps()),
tabPanel("Categorical Data Analysis", uiOutput("chis")),
    tabPanel("Simple Linear Regression", uiOutput("slr")),#, getdeps()),
    tabPanel("Multiple Linear Regression", uiOutput("mlr"))
  )
  )
)
)

