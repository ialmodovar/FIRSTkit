

all.t.one.sample.test <- function(x, mu0 = 0, alpha = 0.05,...){
  twosided <- t.test(x = x, mu = mu0, 
                     alternative = "two.sided",
                     conf.level = 1-alpha)
  oneless <- t.test(x = x, mu = mu0, 
                    alternative = "less",
                    conf.level = 1-alpha)
  onegreater <- t.test(x = x, mu = mu0, 
                       alternative = "greater",
                       conf.level = 1-alpha)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 5)
  
  pval <- ifelse(pval < 0.001, "<0.001",as.character(pval))
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),", ",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste("\\(-\\infty\\)",", ",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),", ",
                          "\\(\\infty\\)",sep=""),")",sep=""))
  
  res <- data.frame(Test = tt, CI =ci,Pval = pval)
  names(res) <- c("statistic",paste(as.character((1-alpha)*100),"% CI",sep=""),"\\(p\\)-value")
  rownames(res) <- c(paste("\\( H_{1}: \\mu \\neq \\:  \\)",mu0,sep=""),
                     paste("\\( H_{1}: \\mu < \\: \\)",mu0,sep=""),
                     paste("\\( H_{1}: \\mu > \\: \\)",mu0,sep=""))
  res
}

all.wilcoxon.one.sample.test <- function(x, mu0 = 0, alpha = 0.05,...){
  twosided <- wilcox.test(x = x, mu = mu0, 
                          alternative = "two.sided",
                          conf.level = 1-alpha,conf.int = TRUE)
  oneless <- wilcox.test(x = x, mu = mu0, 
                         alternative = "less",
                         conf.level = 1-alpha,conf.int = TRUE)
  onegreater <- wilcox.test(x = x, mu = mu0, 
                            alternative = "greater",
                            conf.level = 1-alpha,conf.int = TRUE)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 5) 
  
  pval <- ifelse(pval < 0.001, "<0.001",as.character(pval))
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),", ",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste("\\(-\\infty\\)",", ",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),", ",
                          "\\(\\infty\\)",sep=""),")",sep=""))
  res <- data.frame(Test = tt, CI =ci,Pval = pval)
  names(res) <- c("statistic",paste(as.character((1-alpha)*100),"% CI",sep=""),"\\(p\\)-value")
  
  rownames(res) <- c(paste("\\( H_{1}: M \\neq \\: \\)",mu0,sep=""),
                     paste("\\( H_{1}: M < \\: \\)",mu0,sep=""),
                     paste("\\( H_{1}: M > \\: \\)",mu0,sep=""))
  
  res
}

one.sample.var.test <- function(x,sigma0=1,alpha=0.05,...){
  x <- x[!is.na(x)]
  df <- length(x)-1
  vv <- var(x,na.rm = TRUE)
  sigma02 <- sigma0^2
  X2 <- df * vv/sigma02 #test statistics
  ##
  pval <- c(pchisq(X2,df=df,lower.tail = FALSE),pchisq(X2,df=df,lower.tail = TRUE),pchisq(X2,df=df,lower.tail = FALSE))
  c1 <- qchisq(p=1-alpha/2,df = df)
  c2 <- qchisq(p=alpha/2,df = df)
  ci <- c(paste("(",round(sqrt(df*vv/c1),digits=3),",  ",round(sqrt(df*vv/c2),digits=3),")",sep=""),paste("(",0,",",round(sqrt(df*vv/qchisq(p=alpha,df = df)),digits=3),")",sep=""),
          paste("(",round(sqrt(df*vv/qchisq(p=alpha,df = df)),digits=3),", ","\\( \\infty \\)",")",sep=""))
  pval <- ifelse(pval < 0.001, "<0.001",as.character(pval))
  
  res <- data.frame(Test = X2,CI =ci,Pval = pval)
  names(res) <- c("statistic",paste(as.character((1-alpha)*100),"% CI",sep=""),"\\(p \\)-value")
  
  rownames(res) <- c(paste("\\( H_{1}: \\sigma \\neq \\: \\)",sigma0,sep=""),
                     paste("\\( H_{1}: \\sigma < \\: \\)",sigma0,sep=""),
                     paste("\\( H_{1}: \\sigma > \\: \\)",sigma0,sep=""))
  
  res
  
}

all.proportion.one.sample.test <- function(x = NULL,n, p = NULL, p0 = 0.5, alpha = 0.05){
  
  if(is.null(x)& is.null(p)){
    stop("You must either provide the number of success or the probability of success")
  }
  if(!is.null(x) & is.null(p)){
    p <- x/n
  } 
  if(is.null(x) & !is.null(p)){
    p <- p
    x <- round(p*n,digits = 0)
  }
  if(!is.null(x) & !is.null(p)){
    p <- x/n
  }
  twosided <- binom.test(x = x, n = n, p = p0, 
                         alternative = "two.sided",
                         conf.level = 1-alpha)
  oneless <- binom.test(x = x, n = n, p = p0, 
                        alternative = "less",
                        conf.level = 1-alpha)
  onegreater <- binom.test(x = x, n = n, p = p0, 
                           alternative = "greater",
                           conf.level = 1-alpha)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 5)
  pval <- ifelse(pval < 0.001, "<0.001",as.character(pval))
  
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),", ",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste("0",", ",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),", ",
                          "1",sep=""),")",sep=""))
  res <- data.frame(Test = tt, CI =ci,Pval = pval)
  names(res) <- c("statistic",paste(as.character((1-alpha)*100),"% CI",sep=""),"\\(p\\)-value")
  
  rownames(res) <- c(paste("\\( H_{1}: p \\neq \\: \\)",p0,sep=""),
                     paste("\\( H_{1}: p < \\: \\)",p0,sep=""),
                     paste("\\( H_{1}: p > \\: \\)",p0,sep=""))
  
  res
  
}

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

stats_inference_ui <- navbarMenu("Statistical Inference",
                            tabPanel("One Sample Inference",
                                    sidebarLayout(
                                      sidebarPanel(
                                        withMathJax(),
                                        uiOutput("var_select_one_sample_ui"),
                                        sliderInput("alpha", "Significance level \\(\\alpha\\)",
                                                    min = 0, max = 1, value = 0.05, step = 0.001),
                                        actionButton("run_desc", "Submit"),
                                          ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Location Inference",
                                                   withMathJax(),
                                                   br(),
                                                   uiOutput("tableUImean_all")),
                                          tabPanel("Dispersion Inference",
                                                   withMathJax(),
                                                   uiOutput("tableUIvar_all")),
                                          tabPanel("Proportion Inference",
                                                   withMathJax(),
                                                   fluidRow(
                                                     column(6,
                                                            radioButtons(
                                                              inputId = "propsuccess",
                                                              label = NULL,
                                                              choices = c(
                                                                "Proportion of success \\(\\hat{p}\\)" = "pr",
                                                                "Number of successes \\(x\\)" = "success"
                                                              )
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.propsuccess == 'pr'",
                                                              numericInput("p_prop", "Proportion of success (\\(\\hat{p} \\))",
                                                                           value = 0.5, min = 0, max = 1, step = 0.001)
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.propsuccess == 'success'",
                                                              numericInput("x_prop", "Number of successes (\\(x\\))",
                                                                           value = 20, min = 0, step = 1)
                                                            )
                                                     ),
                                                     column(6,
                                                            numericInput("nprop", "Sample size (\\(n\\))", value = 50, min = 0, step = 1)
                                                     )
                                                   ),
                                                   hr(),
                                                   tags$b("One-Sample Proportion test"),
                                                   uiOutput("tableUIprop_all"),
                                                   textInput("p0", label = withMathJax("\\( H_0: p = p_0 \\)"), value = "0.5",
                                                             placeholder = "Indicate the null value for the proportion"))
                                        )
                                      )
                                    )
),
tabPanel("Two Sample Inference",
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

)
stats_inference_server <- function(input, output, session, firstkit.data) {
  
  # UI for variable selection
  output$var_select_one_sample_ui <- renderUI({
    df <- firstkit.data()
    if (is.null(df)) return(NULL)
    varnames <- names(df)
    selectInput("num_vars", "Select Numeric Variables:",
                choices = varnames[sapply(df, is.numeric)],
                selected = NULL, multiple = TRUE)
  })
  
  output$mu0_inputs <- renderUI({
    req(input$num_vars)
    var_list <- input$num_vars
    
    inputs <- lapply(var_list, function(v) {
      inputId <- paste0("mu0_", v)
      textInput(inputId,
                label = withMathJax(paste0("Null hypothesis for ", v, ": \\( H_0: \\mu = \\mu_0 \\)")),
                value = "0",
                placeholder = "Enter \\(\\mu_0\\) for this variable")
    })
    
    tagList(inputs)
  })
  
  # Mean Test Table
  output$mean <- renderTable({
    req(input$num_vars)
    df <- firstkit.data()
    mu0 <- as.numeric(input$mu0)
    alpha <- as.numeric(input$alpha)
    x <- df[[input$num_vars[1]]]
    all.t.one.sample.test(x = x, mu0 = mu0, alpha = alpha)
  }, rownames = TRUE)
  
  output$loc <- renderTable({
    req(input$num_vars)
    df <- firstkit.data()
    mu0 <- as.numeric(input$mu0)
    alpha <- as.numeric(input$alpha)
    x <- df[[input$num_vars[1]]]
    all.wilcoxon.one.sample.test(x = x, mu0 = mu0, alpha = alpha)
  }, rownames = TRUE)
  
  output$variance <- renderTable({
    req(input$num_vars)
    df <- firstkit.data()
    sigma0 <- as.numeric(input$sigma0)
    alpha <- as.numeric(input$alpha)
    x <- df[[input$num_vars[1]]]
    one.sample.var.test(x = x, sigma0 = sigma0, alpha = alpha)
  }, rownames = TRUE)
  
  output$proportion <- renderTable({
    alpha <- as.numeric(input$alpha)
    p0 <- as.numeric(input$p0)
    
    # Choose x and p values based on selection
    if (input$propsuccess == "pr") {
      p_hat <- as.numeric(input$p_prop)
      x <- round(p_hat * input$nprop)
    } else {
      x <- input$x_prop
    }
    
    all.proportion.one.sample.test(
      x = x,
      n = input$nprop,
      p = ifelse(input$propsuccess == "pr", input$p_prop, x / input$nprop),
      p0 = p0,
      alpha = alpha
    )
  }, rownames = TRUE, digits = 4)
  
  # output$tableUImean <- renderUI({
  #   withMathJax(tableOutput("mean"))
  # })
  output$tableUImean_all <- renderUI({
    req(input$num_vars)
    df <- firstkit.data()
    alpha <- as.numeric(input$alpha)
    var_list <- input$num_vars
    
    output_ui_list <- lapply(var_list, function(varname) {
      t_output_id <- paste0("mean_", varname)
      w_output_id <- paste0("wilcox_", varname)
      mu_input_id <- paste0("mu0_", varname)
      
      # T-Test output
      output[[t_output_id]] <- renderTable({
        req(input[[mu_input_id]])
        mu0 <- as.numeric(input[[mu_input_id]])
        x <- df[[varname]]
        all.t.one.sample.test(x = x, mu0 = mu0, alpha = alpha)
      }, rownames = TRUE)
      
      # Wilcoxon Test output
      output[[w_output_id]] <- renderTable({
        req(input[[mu_input_id]])
        mu0 <- as.numeric(input[[mu_input_id]])
        x <- df[[varname]]
        all.wilcoxon.one.sample.test(x = x, mu0 = mu0, alpha = alpha)
      }, rownames = TRUE)
      
      # Build the side-by-side layout
      tagList(
        tags$h4(paste("Variable:", varname)),
        fluidRow(
          column(
            width = 6,
            tags$b("One-Sample \\( t\\) test"),
            tableOutput(t_output_id)
          ),
          column(
            width = 6,
            tags$b("Wilcoxon-signed rank test"),
            tableOutput(w_output_id)
          )
        ),
        textInput(mu_input_id,
                  label = withMathJax(paste0("\\( H_0: \\mu = \\mu_0 \\) for ", varname)),
                  value = "0"),
        tags$hr()
      )
    })
    
    tagList(output_ui_list)
  })
  
  
  # output$tableUIvar <- renderUI({
  #   withMathJax(tableOutput("variance"))
  # })
  
  output$tableUIvar_all <- renderUI({
    req(input$num_vars)
    df <- firstkit.data()
    alpha <- as.numeric(input$alpha)
    var_list <- input$num_vars
    
    output_ui_list <- lapply(var_list, function(varname) {
      chi_output_id <- paste0("variance_", varname)
      other_output_id <- paste0("othervar_", varname)
      sigma_input_id <- paste0("sigma0_", varname)
      
      # Chi-square test output
      output[[chi_output_id]] <- renderTable({
        req(input[[sigma_input_id]])
        sigma0 <- as.numeric(input[[sigma_input_id]])
        x <- df[[varname]]
        one.sample.var.test(x = x, sigma0 = sigma0, alpha = alpha)
      }, rownames = TRUE)
      
    
      tagList(
        tags$h4(paste("Variable:", varname)),
        fluidRow(
          column(6,
                 tags$b("One-Sample \\(\\chi^2\\) Variance Test"),
                 tableOutput(chi_output_id)),
        ),
        textInput(sigma_input_id,
                  label = withMathJax(paste0("\\( H_0: \\sigma = \\sigma_0 \\) for ", varname)),
                  value = "1"),
        tags$hr()
      )
    })
    
    tagList(output_ui_list)
  })
  
  
  output$tableUIprop_all <- renderUI({
    req(input$nprop)
    alpha <- as.numeric(input$alpha)
    p0 <- as.numeric(input$p0)
    
    test_output_id <- "prop_test"
    ci_output_id <- "prop_ci"
    
    # One-sample test output
    output[[test_output_id]] <- renderTable({
      if (input$propsuccess == "pr") {
        p_hat <- as.numeric(input$p_prop)
        x <- round(p_hat * input$nprop)
      } else {
        x <- input$x_prop
        p_hat <- x / input$nprop
      }
      
      all.proportion.one.sample.test(
        x = x,
        n = input$nprop,
        p = p_hat,
        p0 = p0,
        alpha = alpha
      )
    }, rownames = TRUE, digits = 4)
    
    # Wald CI output
    output[[ci_output_id]] <- renderTable({
      if (input$propsuccess == "pr") {
        p_hat <- as.numeric(input$p_prop)
      } else {
        p_hat <- input$x_prop / input$nprop
      }
      
      se <- sqrt(p_hat * (1 - p_hat) / input$nprop)
      z <- qnorm(1 - alpha / 2)
      ci_lower <- p_hat - z * se
      ci_upper <- p_hat + z * se
      
      data.frame(
        Estimate = round(p_hat, 4),
        `Lower CI` = round(ci_lower, 4),
        `Upper CI` = round(ci_upper, 4)
      )
    }, rownames = FALSE)
    
    # Output side-by-side layout
    tagList(
      fluidRow(
        column(6,
               tags$b("One-Sample Proportion Test"),
               tableOutput(test_output_id)),
        column(6,
               tags$b("Wald Confidence Interval"),
               tableOutput(ci_output_id))
      ),
      tags$hr()
    )
  })
  
  
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
##To learn more, see [FIRSTkit](https://github.com/ialmodovar/FIRSTkit). If you have any question or want to report to *israel.almodovar@upr.edu* or *maitra@iastate.edu*. 
