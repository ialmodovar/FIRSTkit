## Thanks for @timelyportfolio for this comment/fix
## https://github.com/timelyportfolio/functionplotR/issues/1#issuecomment-224369431
getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(),
    c(
      htmlwidgets:::getDependency("functionplot", "functionplotR"),
      htmlwidgets:::getDependency("datatables", "DT")
    )
  )
}

shinyUI(
  navbarPage("FIRSTkit",
    tabPanel("Home", uiOutput("home")),
    tabPanel("Descriptive Statistics", uiOutput("descriptive")),
    tabPanel("One-Sample Inference", uiOutput("OneSample"), getdeps()),
    tabPanel("Two-Sample Inference", uiOutput("TwoSample"), getdeps()),
    tabPanel("Three or more-Sample Inference", uiOutput("kSample"), getdeps()),
    tabPanel("Simple Linear Regression", uiOutput("slr"), getdeps())
  )
)
