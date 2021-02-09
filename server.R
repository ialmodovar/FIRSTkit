shinyServer(function(input, output, session) {

	output$ui_line <- renderUI({
	  ## using renderUI here because Knitr will not create a slider
	  tagList(
	    sliderInput("nr_points", "", min = 10, max = 100, value = 50),
	    renderPlot({
	      nr <- if (is.null(input$nr_points)) 2 else input$nr_points
	      plot(1:nr, rnorm(nr))
	    })
	  )
	})

	output$home <- renderUI({
	  inclRmd("./Rmd/home.Rmd")
	})

	output$descriptive <- renderUI({
	  inclRmd("./Rmd/descriptive-stat.Rmd")
	})

	output$OneSample <- renderUI({
	  inclRmd("./Rmd/One_Sample_inference.Rmd")
	})
	output$TwoSample <- renderUI({
	  inclRmd("./Rmd/Two_Sample_Inference.Rmd")
	})
	output$kSample <- renderUI({
	  inclRmd("./Rmd/Three_Sample_or_more_Inference.Rmd")
	})
	output$slr <- renderUI({
	  inclRmd("./Rmd/SLR.Rmd")
	})
})
