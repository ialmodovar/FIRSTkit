shinyServer(function(input, output, session) {
  
  
  ## this add the significance level
  ## using renderUI here because Knitr will not create a slider
output$alpha_line <- renderUI({
    withMathJax(tagList(
      sliderInput(
        "alpha",
        "Significance level \\(\\alpha \\)",
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.001
      )
    ))
  })
  
  ## Modules
  
  #   output$FIRSTkitData <- renderUI({
  #     inclRmd("./Rmd/data_input.Rmd")
  #   })
  #
  output$intro <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/introduction.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
  })
  
  output$descriptive <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/descriptive-stat.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
  })
  output$VennDiagram <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/PT-VennDiagram.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
  })
  output$BayesTreeDiagram <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/Bayes_Tree_Diagram.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
  })
  output$ProbDistFnt <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/Distributions_Functions.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
  })
  output$OneSample <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/One_Sample_Inference.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
  })
  output$TwoSample <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/Two_Sample_Inference.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
    
  })
  output$kSamples <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/Three_Sample_or_more_Inference.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
    
  })
  
  output$slr <- renderUI({
    withMathJax(HTML(readLines(
      rmarkdown::render(
        input = "./Rmd/SLR.Rmd",
        output_format = rmarkdown::html_fragment(),
        quiet = TRUE
      )
    )))
    #	   withMathJax(inclRmd("./Rmd/SLR.Rmd"))
  })
})
