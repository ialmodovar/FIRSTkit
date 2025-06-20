all.t.two.sample.test <- function(x, y, delta0 = 0,alpha = 0.05,...){
  twosided <- t.test(x = x, y = y,mu = delta0, 
                     alternative = "two.sided",
                     conf.level = 1-alpha)
  oneless <- t.test(x = x, y = y,mu = delta0, 
                    alternative = "less",
                    conf.level = 1-alpha)
  onegreater <- t.test(x = x, y = y,mu = delta0, 
                       alternative = "greater",
                       conf.level = 1-alpha)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 3) 
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),",",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(oneless$conf.int[1],digits=3),",",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),",",
                          round(onegreater$conf.int[2],digits=3),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt,Pval = pval, CI =ci)
  names(res) <- c("statistic","p-value",paste(as.character((1-alpha)*100),"% CI",sep=""))
  
  #   rownames(res) <- c(paste("\\(H_{1}: \\mu_{1}-\\mu_{2} =\\) ",delta0,sep=""),
  #                      paste("H_1: mu_1-mu_2 < ",delta0,sep=""),
  #                      paste("H_1: mu_1-mu_2 > ",delta0,sep=""))
  # res
  rownames(res) <- c(paste("\\(H_{1}: \\mu_{1}-\\mu_{2} \\neq \\) ",delta0,sep=""),
                     paste("\\(H_{1}: \\mu_{1}-\\mu_{2} <\\) ",delta0,sep=""),
                     paste("\\(H_{1}: \\mu_{1}-\\mu_{2} >\\) ",delta0,sep=""))
  res
  
}

##
all.wilcoxon.two.sample.test <- function(x,  y, delta0 = 0, alpha = 0.05,...){
  twosided <- wilcox.test(x = x, y= y, mu = delta0, 
                          alternative = "two.sided",
                          conf.level = 1-alpha,conf.int = TRUE)
  oneless <- wilcox.test(x = x, y = y,mu = delta0, 
                         alternative = "less",
                         conf.level = 1-alpha,conf.int = TRUE)
  onegreater <- wilcox.test(x = x, y= y, mu = delta0, 
                            alternative = "greater",
                            conf.level = 1-alpha,conf.int = TRUE)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 3) 
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),",",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(oneless$conf.int[1],digits=3),",",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),",",
                          round(onegreater$conf.int[2],digits=3),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt,Pval = pval, CI =ci)
  names(res) <- c("statistic","p-value",paste(as.character((1-alpha)*100),"% CI",sep=""))
  
  rownames(res) <- c(paste("\\(H_{1}: \\mu_{1}-\\mu_{2} \\neq \\) ",delta0,sep=""),
                     paste("\\(H_{1}: \\mu_{1}-\\mu_{2} <\\) ",delta0,sep=""),
                     paste("\\(H_{1}: \\mu_{1}-\\mu_{2} >\\) ",delta0,sep=""))
  res
}

all.var.two.sample.test <- function(x, y, ratio = 1,alpha = 0.05,...){
  twosided <- var.test(x = x, y = y,ratio=ratio, 
                       alternative = "two.sided",
                       conf.level = 1-alpha)
  oneless <- var.test(x = x, y = y,ratio=ratio, 
                      alternative = "less",
                      conf.level = 1-alpha)
  onegreater <- var.test(x = x, y = y,ratio=ratio, 
                         alternative = "greater",
                         conf.level = 1-alpha)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 3) 
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),",",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(oneless$conf.int[1],digits=3),",",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),",",
                          round(onegreater$conf.int[2],digits=3),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt,Pval = pval, CI =ci)
  names(res) <- c("statistic","p-value",paste(as.character((1-alpha)*100),"% CI",sep=""))
  
  # rownames(res) <- c(paste("H_1: sigma^2_1/sigma^2_2 = ",ratio,sep=""),
  #                    paste("H_1: sigma^2_1/sigma^2_2 < ",ratio,sep=""),
  #                    paste("H_1: sigma^2_1/sigma^2_2 > ",ratio,sep=""))
  # res
  
  rownames(res) <- c(paste("\\(H_{1}: \\sigma^{2}_{1}/\\sigma^{2}_{2} \\neq \\)",ratio,sep=""),
                     paste("\\(H_{1}: \\sigma^{2}_{1}/\\sigma^{2}_{2} <\\)",ratio,sep=""),
                     paste("\\(H_{1}: \\sigma^{2}_{1}/\\sigma^{2}_{2} >\\)",ratio,sep=""))
  res
  
}

all.ansari.two.sample.test <- function(x, y, ratio = 1,alpha = 0.05,...){
  twosided <- ansari.test(x = x, y = y, conf.int=TRUE, 
                          alternative = "two.sided",
                          conf.level = 1-alpha)
  oneless <- ansari.test(x = x, y = y,conf.int=TRUE,
                         alternative = "less",
                         conf.level = 1-alpha)
  onegreater <- ansari.test(x = x, y = y,conf.int=TRUE,
                            alternative = "greater",
                            conf.level = 1-alpha)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 3) 
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),",",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(oneless$conf.int[1],digits=3),",",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),",",
                          round(onegreater$conf.int[2],digits=3),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt,Pval = pval, CI =ci)
  names(res) <- c("statistic","p-value",paste(as.character((1-alpha)*100),"% CI",sep=""))
  
  # rownames(res) <- c(paste("H_1: sigma^2_1/sigma^2_2 = ",ratio,sep=""),
  #                    paste("H_1: sigma^2_1/sigma^2_2 < ",ratio,sep=""),
  #                    paste("H_1: sigma^2_1/sigma^2_2 > ",ratio,sep=""))
  # res
  
  rownames(res) <- c(paste("\\(H_{1}: \\sigma_{1}/\\sigma_{2} \\neq \\)",1,sep=""),
                     paste("\\(H_{1}: \\sigma_{1}/\\sigma_{2} <\\)",1,sep=""),
                     paste("\\(H_{1}: \\sigma_{1}/\\sigma_{2} >\\)",1,sep=""))
  res
  
}


two_sample_ui <- tabPanel("Two Sample",
                          sidebarLayout(
                            sidebarPanel(
                              sidebarPanel(
                                uiOutput("var_select_two_sample_ui"),
                                sliderInput("alpha", "Significance level \\(\\alpha\\)",
                                            min = 0, max = 1, value = 0.05, step = 0.001),
                                actionButton("run_two", "Submit")
                              ),
                            ),
                            mainPanel(
                              withMathJax(),
                              tabsetPanel(type = "tabs",
                                          tabPanel("Location Inference",
                                                   br(),
                                                   fluidRow(
                                                     column(6, tags$b("Two-Sample \\( t \\) test"), tableOutput("mean")),
                                                     column(6, tags$b("Mann-Whitney-Wilcoxon test"), tableOutput("loc"))
                                                   ),
                                                   textInput("delta0", label = withMathJax("\\( H_0: \\mu_1 - \\mu_2 = \\delta_0 \\)"),
                                                             value = "0", placeholder = "Enter null difference")
                                          ),
                                          tabPanel("Dispersion Inference",
                                                   br(),
                                                   fluidRow(
                                                     column(6, tags$b("Two-Sample Variance test"), tableOutput("variance")),
                                                     column(6, tags$b("Ansari-Bradley test"), tableOutput("ansari"))
                                                   ),
                                                   textInput("sigma0", label = withMathJax("\\( H_0: \\sigma_1^2 / \\sigma_2^2 = R \\)"),
                                                             value = "1", placeholder = "Enter variance ratio")
                                          ),
                                          tabPanel("Proportion Inference",
                                                   br(),
                                                   fluidRow(
                                                     column(6,
                                                            radioButtons("propsuccess1", NULL, choices = c(
                                                              "Proportion of success \\(\\hat{p}_1\\)" = "pr1",
                                                              "Number of successes \\(x_1\\)" = "success1")),
                                                            conditionalPanel("input.propsuccess1 == 'pr1'",
                                                                             numericInput("p_prop1", "Proportion \\(\\hat{p}_1\\)", 0.5, 0, 1, 0.001)),
                                                            conditionalPanel("input.propsuccess1 == 'success1'",
                                                                             numericInput("x_prop1", "Successes \\(x_1\\)", 20, 0, step = 1)),
                                                            numericInput("nprop1", "Sample size \\(n_1\\)", 50, 0, step = 1)
                                                     ),
                                                     column(6,
                                                            radioButtons("propsuccess2", NULL, choices = c(
                                                              "Proportion of success \\(\\hat{p}_2\\)" = "pr2",
                                                              "Number of successes \\(x_2\\)" = "success2")),
                                                            conditionalPanel("input.propsuccess2 == 'pr2'",
                                                                             numericInput("p_prop2", "Proportion \\(\\hat{p}_2\\)", 0.5, 0, 1, 0.001)),
                                                            conditionalPanel("input.propsuccess2 == 'success2'",
                                                                             numericInput("x_prop2", "Successes \\(x_2\\)", 20, 0, step = 1)),
                                                            numericInput("nprop2", "Sample size \\(n_2\\)", 50, 0, step = 1)
                                                     )
                                                   ),
                                                   hr(),
                                                   fluidRow(
                                                     column(6, tags$b("Two-Sample Proportion test"), tableOutput("proportion")),
                                                     column(6, tags$b("Wald CI for difference"), tableOutput("prop_ci"))
                                                   ),
                                                   textInput("p0", label = withMathJax("\\( H_0: p_1 - p_2 = p_0 \\)"),
                                                             value = "0", placeholder = "Enter null difference in proportions")
                                          )
                              )
                            )
                          )
)

two_sample_server <- function(input, output, session, firstkit.data) {
  
  output$var_select_two_sample_ui <- renderUI({
    df <- firstkit.data()
    req(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    group_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    tagList(
      radioButtons("two_sample_mode", "Select input mode:",
                   choices = c(
                     "Two separate numeric variables" = "vars",
                     "One numeric variable + grouping variable" = "group"
                   ),
                   selected = "vars"),
      conditionalPanel(
        condition = "input.two_sample_mode == 'vars'",
        selectInput("var1", "First Numeric Variable:", choices = num_vars),
        selectInput("var2", "Second Numeric Variable:", choices = num_vars)
      ),
      conditionalPanel(
        condition = "input.two_sample_mode == 'group'",
        selectInput("numvar_grouped", "Select Numeric Variable:", choices = num_vars),
        selectInput("groupvar", "Select Grouping Variable (must have 2 levels):", choices = group_vars)
      )
    )
  })
  
  df <- reactive({ req(firstkit.data()); firstkit.data() })
  
 xy_data <- eventReactive(input$run_two, {
  data <- df()
  mode <- req(input$two_sample_mode)
  
  if (mode == "group") {
    req(input$numvar_grouped, input$groupvar)
    data <- data[!is.na(data[[input$numvar_grouped]]) & !is.na(data[[input$groupvar]]), ]
    groups <- unique(data[[input$groupvar]])
    
    if (length(groups) > 2) {
      groups <- groups[1:2]
      data <- data[data[[input$groupvar]] %in% groups, ]
    }
    
    validate(need(length(groups) == 2, "Grouping variable must have exactly 2 levels"))
    
    x <- data[data[[input$groupvar]] == groups[1], input$numvar_grouped]
    y <- data[data[[input$groupvar]] == groups[2], input$numvar_grouped]
    
    # Convert to numeric safely:
    x <- as.numeric(as.character(x))
    y <- as.numeric(as.character(y))
    
    cat("Using groups:", groups[1], "and", groups[2], "\n")
  } else {
    req(input$var1, input$var2)
    x <- data[[input$var1]]
    y <- data[[input$var2]]
    
    # Convert to numeric safely:
    x <- as.numeric(as.character(x))
    y <- as.numeric(as.character(y))
  }
  
  list(x = x, y = y)
})

  
  output$mean <- renderTable({
    data <- xy_data()
    req(data$x, data$y)
    all.t.two.sample.test(data$x, data$y,
                          delta0 = as.numeric(input$delta0),
                          alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$loc <- renderTable({
    data <- xy_data()
    req(data$x, data$y)
    all.wilcoxon.two.sample.test(data$x, data$y,
                                 delta0 = as.numeric(input$delta0),
                                 alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$variance <- renderTable({
    data <- xy_data()
    req(data$x, data$y)
    all.var.two.sample.test(data$x, data$y,
                            ratio = as.numeric(input$sigma0),
                            alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$ansari <- renderTable({
    data <- xy_data()
    req(data$x, data$y)
    all.ansari.two.sample.test(data$x, data$y,
                               alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$proportion <- renderTable({
    p0 <- as.numeric(input$p0)
    n1 <- input$nprop1
    n2 <- input$nprop2
    x1 <- if (input$propsuccess1 == "pr1") round(input$p_prop1 * n1) else input$x_prop1
    x2 <- if (input$propsuccess2 == "pr2") round(input$p_prop2 * n2) else input$x_prop2
    
    all.proportion.two.sample.test(x1 = x1, x2 = x2,
                                   n1 = n1, n2 = n2,
                                   p0 = p0, alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$prop_ci <- renderTable({
    n1 <- input$nprop1
    n2 <- input$nprop2
    p1 <- if (input$propsuccess1 == "pr1") input$p_prop1 else input$x_prop1 / n1
    p2 <- if (input$propsuccess2 == "pr2") input$p_prop2 else input$x_prop2 / n2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    z <- qnorm(1 - input$alpha / 2)
    diff <- p1 - p2
    
    data.frame(
      Estimate = round(diff, 4),
      `Lower CI` = round(diff - z * se, 4),
      `Upper CI` = round(diff + z * se, 4)
    )
  })
  
  
}