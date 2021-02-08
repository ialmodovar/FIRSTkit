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
    tabPanel("Home", uiOutput("Rmd/introduction")),
    tabPanel("Descriptive Statistics", uiOutput("descriptive")),
    tabPanel("One-Sample Inference", uiOutput("One_Sample_Inference"), getdeps())
  )
)
