shinyServer(function(input, output, session) {

  ## this add the significance level 
  output$alpha_line <- renderUI({
  withMathJax(
    ## using renderUI here because Knitr will not create a slider
    tagList(
      sliderInput("alpha","Significance level \\(\\alpha \\)",
                  min = 0,
                  max = 1,
                  value = 0.05,step=0.001,
      )
    )
  )
  })

  output$intro <- renderUI({
    inclRmd("./Rmd/introduction.Rmd")
  })
  
	output$descriptive <- renderUI({
	  inclRmd("./Rmd/descriptive-stat.Rmd")
	})

	# output$Inference <- renderUI({
	#   withMathJax(inclRmd("./Rmd/One_Sample_Inference.Rmd"))
	#   withMathJax(inclRmd("./Rmd/Two_Sample_Inference.Rmd"))
	#  withMathJax(inclRmd("./Rmd/Three_Sample_or_more_Inference.Rmd"))	
	# })
	
	output$OneSample <- renderUI({
	withMathJax(inclRmd("./Rmd/One_Sample_Inference.Rmd"))
	})
	output$TwoSample <- renderUI({
	  withMathJax(inclRmd("./Rmd/Two_Sample_Inference.Rmd"))
	})
	output$kSample <- renderUI({
	  withMathJax(inclRmd("./Rmd/Three_Sample_or_more_Inference.Rmd"))
	})
	output$chis <- renderUI({
	  withMathJax(inclRmd("./Rmd/categorical.Rmd"))
	})
	output$slr <- renderUI({
	  withMathJax(inclRmd("./Rmd/SLR.Rmd"))
	})
	output$mlr <- renderUI({
	  withMathJax(inclRmd("./Rmd/MLR.Rmd"))
	})

})
