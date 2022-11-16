shinyServer(function(input, output, session) {

  ## this add the significance level 
  ## using renderUI here because Knitr will not create a slider
  output$alpha_line <- renderUI({
  withMathJax(
    tagList(
      sliderInput("alpha","Significance level \\(\\alpha \\)",
                  min = 0,
                  max = 1,
                  value = 0.05,step=0.001)
    )
  )
  })

  ## Modules
  
  output$FIRSTkitData <- renderUI({
    inclRmd("./Rmd/data_input.Rmd")
  })
  
  output$intro <- renderUI({
    inclRmd("./Rmd/introduction.Rmd")
  })
  output$descriptive <- renderUI({
	  inclRmd("./Rmd/descriptive-stat.Rmd")
	})
  output$BayesTreeDiagram <- renderUI({
    withMathJax(inclRmd("./Rmd/Bayes_Tree_Diagram.Rmd"))
  })
	
	 output$OneSample <- renderUI({
	 withMathJax(inclRmd("./Rmd/One_Sample_Inference.Rmd"))
	 })
	 output$TwoSample <- renderUI({
	   withMathJax(inclRmd("./Rmd/Two_Sample_Inference.Rmd"))
	 })
	output$kSample <- renderUI({
	   withMathJax(inclRmd("./Rmd/Three_Sample_or_more_Inference.Rmd"))
	 })
	
	 output$slr <- renderUI({
	   withMathJax(inclRmd("./Rmd/SLR.Rmd"))
	 })
})
