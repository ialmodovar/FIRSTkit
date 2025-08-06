##*********************************************
##*
##* @file: statistical_inference_module.R
##*
##* Statistical inference
##* one-sample, two sample and three-sample or more
##*
##* Author:
##* Israel Almodovar-Rivera PhD
##* Department of Mathematical Sciences
##* University of Puerto Rico at Mayaguez
##* israel.almodovar@upr.edu
##* Copyright June 2025
##*********************************************
{
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
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=4)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 5)
  
  pval <- ifelse(pval < 0.001, "<0.001",as.character(pval))
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=4),", ",
                          round(twosided$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste("\\(-\\infty\\)",", ",
                          round(oneless$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=4),", ",
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
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=4)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 5) 
  
  pval <- ifelse(pval < 0.001, "<0.001",as.character(pval))
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=4),", ",
                          round(twosided$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste("\\(-\\infty\\)",", ",
                          round(oneless$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=4),", ",
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
  pval <- c(2*min(pchisq(X2,df=df,lower.tail = FALSE),pchisq(X2,df=df,lower.tail = TRUE)),pchisq(X2,df=df,lower.tail = TRUE),pchisq(X2,df=df,lower.tail = FALSE))
  c1 <- qchisq(p=1-alpha/2,df = df)
  c2 <- qchisq(p=alpha/2,df = df)
  ci <- c(paste("(",round(sqrt(df*vv/c1),digits=4),",  ",round(sqrt(df*vv/c2),digits=3),")",sep=""),paste("(",0,",",round(sqrt(df*vv/qchisq(p=alpha,df = df)),digits=4),")",sep=""),
          paste("(",round(sqrt(df*vv/qchisq(p=alpha,df = df)),digits=4),", ","\\( \\infty \\)",")",sep=""))
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
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=4)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 5)
  pval <- ifelse(pval < 0.001, "<0.001",as.character(pval))
  
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=4),", ",
                          round(twosided$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste("0",", ",
                          round(oneless$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=4),", ",
                          "1",sep=""),")",sep=""))
  res <- data.frame(Test = tt, CI =ci,Pval = pval)
  names(res) <- c("statistic",paste(as.character((1-alpha)*100),"% CI",sep=""),"\\(p\\)-value")
  
  rownames(res) <- c(paste("\\( H_{1}: p \\neq \\: \\)",p0,sep=""),
                     paste("\\( H_{1}: p < \\: \\)",p0,sep=""),
                     paste("\\( H_{1}: p > \\: \\)",p0,sep=""))
  
  res
  
}

all.t.two.sample.test <- function(x, y, delta0 = 0,paired = FALSE, var.equal = FALSE, alpha = 0.05,...){
  twosided <- t.test(x = x, y = y,mu = delta0, paired=paired,
                     alternative = "two.sided", var.equal = var.equal,
                     conf.level = 1-alpha)
  oneless <- t.test(x = x, y = y,mu = delta0, paired=paired,
                    alternative = "less", var.equal = var.equal,
                    conf.level = 1-alpha)
  onegreater <- t.test(x = x, y = y,mu = delta0, paired=paired,
                       alternative = "greater",var.equal = var.equal,
                       conf.level = 1-alpha)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=4)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 4) 
  dfs <- c(twosided$parameter, oneless$parameter, onegreater$parameter)
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=4),",",
                          round(twosided$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste("\\( -\\infty\\),",
                          round(oneless$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=4),",",
                          round(onegreater$conf.int[2],digits=4),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt, df = dfs, CI =ci,Pvalue = pval)
  names(res) <- c("statistic","DF",paste(as.character((1-alpha)*100),"% CI",sep=""),"p-value")
  
  rownames(res) <- c(paste("\\(H_{1}: \\mu_{1}-\\mu_{2} \\neq \\) ",delta0,sep=""),
                     paste("\\(H_{1}: \\mu_{1}-\\mu_{2} <\\) ",delta0,sep=""),
                     paste("\\(H_{1}: \\mu_{1}-\\mu_{2} >\\) ",delta0,sep=""))
  res
  
}

all.wilcoxon.two.sample.test <- function(x,  y, delta0 = 0, paired = FALSE,alpha = 0.05,...){
  twosided <- wilcox.test(x = x, y= y, mu = delta0, paired=paired,
                          alternative = "two.sided",
                          conf.level = 1-alpha,conf.int = TRUE)
  oneless <- wilcox.test(x = x, y = y,mu = delta0, paired = paired,
                         alternative = "less",
                         conf.level = 1-alpha,conf.int = TRUE)
  onegreater <- wilcox.test(x = x, y= y, mu = delta0, paired = paired,
                            alternative = "greater",
                            conf.level = 1-alpha,conf.int = TRUE)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=4)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 4) 
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=4),",",
                          round(twosided$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(oneless$conf.int[1],digits=4),",",
                          round(oneless$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=4),",",
                          round(onegreater$conf.int[2],digits=4),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt,CI =ci,Pval=pval)
  names(res) <- c("statistic",paste(as.character((1-alpha)*100),"% CI",sep=""),"p-value")
  
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
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=4)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 4) 
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=4),",",
                          round(twosided$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(oneless$conf.int[1],digits=4),",",
                          round(oneless$conf.int[2],digits=4),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=4),",",
                          round(onegreater$conf.int[2],digits=4),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt,CI =ci,Pval = pval)
  names(res) <- c("statistic",paste(as.character((1-alpha)*100),"% CI",sep=""),Pval = pval )
  
  rownames(res) <- c(paste("\\(H_{1}: \\sigma_{1}/\\sigma_{2} \\neq \\)",1,sep=""),
                     paste("\\(H_{1}: \\sigma_{1}/\\sigma_{2} <\\)",1,sep=""),
                     paste("\\(H_{1}: \\sigma_{1}/\\sigma_{2} >\\)",1,sep=""))
  res
  
}

all.proportion.two.sample.test <- function(x = NULL,n, p = NULL, p0 = 0.5, alpha = 0.05){
  
  if(is.null(x)& is.null(p)){
    stop("You must either provide the number of success or the probability of success")
  }
  if(!is.null(x) & is.null(p)){
    x1 <- x[1]
    x2 <- x[2]
    n1 <- n[1]
    n2 <- n[2]
    p1 <- x1/n1
    p2 <- x2/n2
  } 
  if(is.null(x) & !is.null(p)){
    x1 <- round(p[1]*n[1],digits = 0)
    x2 <- round(p[1]*n[2],digits = 0)
  }
  if(!is.null(x) & !is.null(p)){
    x1 <- x[1]
    x2 <- x[2]
    n1 <- n[1]
    n2 <- n[2]
    p1 <- x1/n1
    p2 <- x2/n2
    
  }
  twosided <- binom.test(x = c(x1,x2), n = c(n1,n2), p = p0, 
                         alternative = "two.sided",
                         conf.level = 1-alpha)
  oneless <- binom.test(x = c(x1,x2), n = c(n1,n2), p = p0, 
                        alternative = "less",
                        conf.level = 1-alpha)
  onegreater <- binom.test(x = c(x1,x2), n = c(n1,n2), p = p0, 
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
  
  rownames(res) <- c(paste("\\( H_{1}: p_1 -p_2 \\neq \\: \\)",p0,sep=""),
                     paste("\\( H_{1}: p_1 -p_2 < \\: \\)",p0,sep=""),
                     paste("\\( H_{1}: p_1 -p_2 > \\: \\)",p0,sep=""))
  
  res
  
}
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
                                                         br(),
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
                                                         uiOutput("tableUIprop_all"),
                                                          textInput("p0", label = withMathJax("\\( H_0: p = p_0 \\)"), value = "0.5",
                                                                    placeholder = "Indicate the null value for the proportion")),
                                                tabPanel("R code", verbatimTextOutput("one_sample_code"))
                                              
                                            )
                                          )
                                          )
                                 ),
                                 tabPanel("Two Samples Inference",
                                          sidebarLayout(
                                              sidebarPanel(
                                                withMathJax(),
                                                uiOutput("var_select_two_sample_ui"),
                                                checkboxInput("paired", "Paired samples", value = FALSE),
                                                checkboxInput("equal_var", "Equal variance \\( (\\sigma^2_1 = \\sigma^2_2) \\)", value = FALSE),
                                                sliderInput("alpha", "Significance level \\(\\alpha\\)",
                                                            min = 0, max = 1, value = 0.05, step = 0.001),
                                                actionButton("run_two", "Run Analysis")
                                            ),
                                            mainPanel(
                                              withMathJax(),
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Location Inference",
                                                                   br(),
                                                                   textInput("delta0", label = withMathJax("\\( H_0: \\mu_1 - \\mu_2 = \\delta_0 \\)"),
                                                                             value = "0", placeholder = "Enter null difference"),
                                                                   fluidRow(
                                                                     withMathJax(),
                                                                     column(6, tags$b("Two-Samples \\( t \\) test"), tableOutput("twomean")),
                                                                     column(6, tags$b("Mann-Whitney-Wilcoxon test"), tableOutput("twoloc"))
                                                                   ),
                                                          ),
                                                          tabPanel("Dispersion Inference",
                                                                   br(),
                                                                   textInput("sigma0", label = withMathJax("\\( H_0: \\sigma_1^2 / \\sigma_2^2 = R \\)"),
                                                                             value = "1", placeholder = "Enter variance ratio"),
                                                                   fluidRow(
                                                                     column(6, tags$b("Two-Sample Variance test"), tableOutput("variance")),
                                                                     column(6, tags$b("Ansari-Bradley test"), tableOutput("ansari"))
                                                                   ),
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
                                                                   textInput("p0", label = withMathJax("\\( H_0: p_1 - p_2 = p_0 \\)"),
                                                                             value = "0", placeholder = "Enter null difference in proportions"),
                                                                   fluidRow(
                                                                     column(6, tags$b("Two-Samples Proportion test"), tableOutput("proportion")),
                                                                   ),
                                                          )
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel(title = "Three samples or more inference",
                                          sidebarLayout(
                                            sidebarPanel(
                                              withMathJax(), 
                                              uiOutput('var'),
                                              selectInput('type', 'Sums of Squares Type',
                                                          c(I = 'type1', II = 'type2', III = 'type3'), 'type1'),
                                              sliderInput("alpha", "Significance level \\(\\alpha\\)",
                                                          min = 0, max = 1, value = 0.05, step = 0.001)
                                            ),
                                            mainPanel(
                                              tabsetPanel(type = "tabs",
                                                          tabPanel("Location Inference",
                                                                   withMathJax(),
                                                                   fluidRow(
                                                                     column(6,
                                                                            h4('ANOVA Table'),
                                                                            tableOutput('aovSummary')
                                                                     ),
                                                                     column(6,
                                                                            withMathJax(),
                                                                            h4('Kruskal-Wallis Rank Sum Test'),
                                                                            tableOutput('KWSummary')
                                                                     )
                                                                   ),
                                                                   fluidRow(
                                                                     column(6,
                                                                            withMathJax(),
                                                                            h4('Tukey HSD'),
                                                                            tableOutput('tukeySummary')
                                                                     ),
                                                                     column(6,
                                                                            h4('Tukey HSD Plot'),
                                                                            plotlyOutput('tukeyPlot', height = "600px")
                                                                     )
                                                                   )
                                                          ),
                                                          tabPanel("Dispersion Inference",
                                                                   fluidRow(
                                                                     column(6,
                                                                            withMathJax(),
                                                                            h4("Bartlett test"),
                                                                            tableOutput("BartlettSummary")
                                                                     ),
                                                                     column(6,
                                                                            withMathJax(),
                                                                            h4("Fligner-Killeen test"),
                                                                            tableOutput("FlignerSummary")
                                                                     )
                                                                   )
                                                          )
                                              )
                                            )
                                          )
                                 )
                                 
)
stats_inference_server <- function(input, output, session, firstkit.data) {
  
  output$var_select_one_sample_ui <- renderUI({
    df <- firstkit.data()
    if (is.null(df)) return(NULL)
    varnames <- names(df)
    selectInput("num.vars", "Select Numeric Variables:",
                choices = varnames[sapply(df, is.numeric)],
                selected = NULL, multiple = TRUE)
  })
  
  output$mu0_inputs <- renderUI({
    req(input$num.vars)
    var.list <- input$num.vars
    
    inputs <- lapply(var.list, function(v) {
      inputId <- paste0("mu0_", v)
      textInput(inputId,
                label = withMathJax(paste0("Null hypothesis for ", v, ": \\( H_0: \\mu = \\mu_0 \\)")),
                value = "0",
                placeholder = "Enter \\(\\mu_0\\) for this variable")})
    
    tagList(inputs)
  })
  
  ## ---- one sample proportion inference ----
  output$proportion <- renderTable({
    alpha <- as.numeric(input$alpha)
    p0 <- as.numeric(input$p0)
    
    # Choose x and p values based on selection
    if (input$propsuccess == "pr") {
      phat <- as.numeric(input$p_prop)
      x <- round(phat*input$nprop)
    } else {
      x <- input$x_prop
    }
    
    all.proportion.one.sample.test(
      x = x,
      n = input$nprop,
      p = ifelse(input$propsuccess == "pr", input$p_prop, x/input$nprop),
      p0 = p0,
      alpha = alpha)
  }, rownames = TRUE, digits = 4)
  
  output$tableUImean_all <- renderUI({
    req(input$num.vars)
    df <- firstkit.data()
    alpha <- as.numeric(input$alpha)
    var.list <- input$num.vars
    
    output_ui_list <- lapply(var.list, function(varname) {
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
        textInput(mu_input_id,
                  label = withMathJax(paste0("\\( H_0: \\mu = \\mu_0 \\) for ", varname)),
                  value = "0"),
        tags$hr(),
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
      )
    })
    
    tagList(output_ui_list)
  })
  
  output$tableUIvar_all <- renderUI({
    req(input$num.vars)
    df <- firstkit.data()
    alpha <- as.numeric(input$alpha)
    var.list <- input$num.vars
    
    output_ui_list <- lapply(var.list, function(varname) {
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
        textInput(sigma_input_id,
                  label = withMathJax(paste0("\\( H_0: \\sigma = \\sigma_0 \\) for ", varname)),
                  value = "1"),
        fluidRow(
                 tags$b("One-Sample \\(\\chi^2\\) Variance Test"),
                 withMathJax(),
                 tableOutput(chi_output_id)),
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

    # One-sample test output
    output[[test_output_id]] <- renderTable({
      if (input$propsuccess == "pr") {
        phat <- as.numeric(input$p_prop)
        x <- round(phat * input$nprop)
      } else {
        x <- input$x_prop
        phat <- x/input$nprop
      }
      
      all.proportion.one.sample.test(
        x = x,
        n = input$nprop,
        p = phat,
        p0 = p0,
        alpha = alpha
      )
    }, rownames = TRUE, digits = 4)
    
    tagList(
      fluidRow(
        column(6,
               tags$b("One-Sample Proportion Test"),
               tableOutput(test_output_id))
      ),
      tags$hr()
    )
  })
  
  ## ---- two sample
  
  output$var_select_two_sample_ui <- renderUI({
    df <- firstkit.data()
    req(df)
    num.vars <- names(df)[sapply(df, is.numeric)]
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
        selectInput("var1", "First Numeric Variable:", choices = num.vars),
        selectInput("var2", "Second Numeric Variable:", choices = num.vars)
      ),
      conditionalPanel(
        condition = "input.two_sample_mode == 'group'",
        selectInput("numvar_grouped", "Select Numeric Variable:", choices = num.vars),
        selectInput("groupvar", "Select Grouping Variable (must have 2 levels):", choices = group_vars)
      )
    )
  })
  
  df <- reactive({ req(firstkit.data()); firstkit.data() })
  
  two.vars <- eventReactive(input$run_two, {
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

      x <- as.numeric(as.character(x))
      y <- as.numeric(as.character(y))

      cat("Using groups:", groups[1], "and", groups[2], "\n")
    } else {
      req(input$var1, input$var2)
      x <- data[[input$var1]]
      y <- data[[input$var2]]

      x <- as.numeric(as.character(x))
      y <- as.numeric(as.character(y))
    }

    list(x = x, y = y)
  })

  output$twomean <- renderTable({
    data <- two.vars()
    req(data$x, data$y)
    all.t.two.sample.test(data$x, data$y,
                          delta0 = as.numeric(input$delta0),
                          alpha = as.numeric(input$alpha),
                          paired=input$paired,
                          var.equal = input$equal_var)
    
  },rownames=TRUE)
  
  output$twoloc <- renderTable({
    data <- two.vars()
    req(data$x, data$y)
    all.wilcoxon.two.sample.test(data$x, data$y,
                                 delta0 = as.numeric(input$delta0),
                                 alpha = as.numeric(input$alpha),
                                 paired=input$paired)
  }, rownames = TRUE)
  
  output$variance <- renderTable({
    data <- two.vars()
    req(data$x, data$y)
    all.var.two.sample.test(data$x, data$y,
                            ratio = as.numeric(input$sigma0),
                            alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$ansari <- renderTable({
    data <- two.vars()
    req(data$x, data$y)
    all.ansari.two.sample.test(data$x, data$y,alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$proportion <- renderTable({
    p0 <- as.numeric(input$p0)
    n1 <- input$nprop1
    n2 <- input$nprop2
    x1 <- if (input$propsuccess1 == "pr1") round(input$p_prop1 * n1) else input$x_prop1
    x2 <- if (input$propsuccess2 == "pr2") round(input$p_prop2 * n2) else input$x_prop2
    
    all.proportion.two.sample.test(x = c(x1,x2),
                                   n = c(n1,n2),
                                   p0 = p0, 
                                   alpha = as.numeric(input$alpha))
  }, rownames = TRUE)
  
  output$prop_ci <- renderTable({
    n1 <- input$nprop1
    n2 <- input$nprop2
    p1 <- if (input$propsuccess1 == "pr1") input$p_prop1 else input$x_prop1/n1
    p2 <- if (input$propsuccess2 == "pr2") input$p_prop2 else input$x_prop2/n2
    se <- sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)
    z <- qnorm(1-input$alpha/2)
    diff <- p1-p2
    
    zstat <- diff/se
    pvalue <- 2*(pnorm(abs(zstat),lower.tail = FALSE))
    
    data.frame( Estimate = round(diff, 4),
      CI = paste0("(", round(diff - z * se, 4), ", ", round(diff + z * se, 4), ")"),
      pvalue = ifelse(pvalue < 0.001, "<0.001", round(pvalue, 3)),
      check.names = FALSE)
  })
  ##*********
  ##* Three samples or more analysis
  ##********
  
  output$var <- renderUI({
    df <- firstkit.data()
    req(df)
    nvars <- names(df)[sapply(df, is.numeric)]
    fvars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    withMathJax(
      tagList(
        selectInput("dvar", label = HTML("Response: \\( y \\)"), choices = nvars),
        selectInput("ivar", label = HTML("Fixed-effects: \\( \\alpha \\)"), choices = fvars))
    )
  })
  
  output$aovSummary <- renderTable({
    req(input$dvar, input$ivar)
    df <- firstkit.data()
    df[[input$ivar]] <- as.factor(df[[input$ivar]])
    fit <- lm(as.formula(paste(input$dvar, "~", input$ivar)), data = df)
    
    if (input$type == "type1") {
      anova(fit)
    } else if (input$type == "type2") {
      Anova(fit, type = 2)
    } else {
      Anova(fit, type = 3)
    }
  }, rownames = TRUE, digits = 4)
  
  output$tukeySummary <- renderTable({
    req(input$dvar, input$ivar)
    df <- firstkit.data()
    df[[input$ivar]] <- as.factor(df[[input$ivar]])
    conf <- 1 - as.numeric(input$alpha)
    
    tky <- TukeyHSD(aov(as.formula(paste(input$dvar, "~", input$ivar)), data = df), conf.level = conf)
    varname <- names(tky)[1]
    tky.df <- as.data.frame(tky[[varname]])
    tky.df$Comparison <- rownames(tky.df)
    
    res.tky <- data.frame(Comparison = tky.df$Comparison, Estimate = round(tky.df$diff, 3),
      CI = paste0("(", round(tky.df$lwr, 3), ", ", round(tky.df$upr, 3), ")"),
      pvalue = ifelse(tky.df$`p adj` < 0.001, "<0.001", round(tky.df$`p adj`, 3)),
      check.names = FALSE)
    
    names(res.tky)[3] <- paste(conf*100,"% CI",sep="")
    names(res.tky)[4] <- "\\(p\\)-value"
    
    res.tky
    
  }, rownames = FALSE,digits=4)
  
  output$KWSummary <- renderTable({
    req(input$dvar, input$ivar)
    df <- firstkit.data()
    df[[input$ivar]] <- as.factor(df[[input$ivar]])
    fit <- kruskal.test(as.formula(paste(input$dvar, "~", input$ivar)), data = df)
    kw.res <- data.frame(Statistic = round(fit$statistic, 4), DF = as.integer(fit$parameter),
      pvalue = ifelse(fit$p.value >= 0.001, round(fit$p.value, 4),"<0.001"),
      row.names = "Kruskal-Wallis")
    names(kw.res)[3] <- "\\(p\\)-value"
    kw.res
  }, rownames = TRUE,digits=4)
  
  output$BartlettSummary <- renderTable({
    req(input$dvar, input$ivar)
    df <- firstkit.data()
    df[[input$ivar]] <- as.factor(df[[input$ivar]])
    fit <- bartlett.test(as.formula(paste(input$dvar, "~", input$ivar)), data = df)
    bt.res <- data.frame(Statistic = round(fit$statistic, 4), DF = as.integer(fit$parameter),
      pvalue = ifelse(fit$p.value >= 0.001, round(fit$p.value, 4),"<0.001"),
      row.names = "Bartlett")
    names(bt.res)[3] <- "\\(p\\)-value"
    bt.res
  }, rownames = TRUE,digits=4)
  
  output$FlignerSummary <- renderTable({
    req(input$dvar, input$ivar)
    df <- firstkit.data()
    df[[input$ivar]] <- as.factor(df[[input$ivar]])
    fit <- fligner.test(as.formula(paste(input$dvar, "~", input$ivar)), data = df)
    flig.res <- data.frame( Statistic = round(fit$statistic, 4),
                            DF = as.integer(fit$parameter),
               pvalue = ifelse(fit$p.value >= 0.001, round(fit$p.value, 4), "<0.001"),row.names = "Fligner-Killeen")
    names(flig.res)[3] <- "\\(p\\)-value"
    flig.res
  }, rownames = TRUE,digits=4)
  
  output$tukeyPlot <- renderPlotly({
    req(input$dvar, input$ivar)
    df <- firstkit.data()
    df[[input$ivar]] <- as.factor(df[[input$ivar]])
    conf <- 1 - as.numeric(input$alpha)
    
    tky <- TukeyHSD(aov(as.formula(paste(input$dvar, "~", input$ivar)), data = df), conf.level = conf)
    varname <- names(tky)[1]
    tky.df <- as.data.frame(tky[[varname]])
    tky.df$Comparison <- rownames(tky.df)
    tky.df$Group <- factor(tky.df$Comparison, levels = rev(rownames(tky.df)))
    
    p <- ggplot(tky.df, aes(x = diff, y = Group)) +
      geom_point(color = "purple", size = 3) +
      geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.25, color = "gray30") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(x = "Mean Difference", y = "",
           title = paste0("Tukey HSD ", round(conf * 100), "% CI")) +
      theme_minimal(base_size = 16)
    
    ggplotly(p)
  })

  output$one_sample_code <- renderText({
    code_lines <- c("attach(mydata)",
                    '
one.sample.var.test <- function(x,sigma0=1,conf.level=0.95,alternative="two.sided"){
  x <- x[!is.na(x)]
  n <- length(x)
  nu <- n-1
  vv <- var(x)
  X2 <- nu * vv/(sigma0^2)
  alpha <- 1-conf.level
  if(alternative=="two.sided"){
  ci <- c(nu*vv/qchisq(p=1-alpha/2,df = nu), nu*vv/qchisq(p=alpha/2,df = nu))
  pval <- 2* min(pchisq(q = X2,df = nu,lower.tail = FALSE), pchisq(q = X2,df = nu,lower.tail = TRUE))
  } else if(alternative=="less"){
    ci <- c(0, nu*vv/qchisq(p=alpha,df = nu))
    pval <- pchisq(q = X2,df = nu,lower.tail = TRUE)
  } else if(alternative=="greater"){
    ci <- c(nu*vv/qchisq(p=1-alpha,df = nu),Inf)
    pval <- pchisq(q = X2,df = nu,lower.tail = FALSE)
  }

  list(estimate = vv, statistic=X2, pvalue= pval, conf.int = ci)
}\n')
    
    vars <- input$num.vars
    alpha <- as.numeric(input$alpha)
    if (is.null(vars) || length(vars) == 0) {
      return("No numeric variables selected for one-sample inference.")
    }
    
    
    for (v in vars) {
      mu_input_id <- paste0("mu0_", v)
      mu0_raw <- input[[mu_input_id]]
      if (is.null(mu0_raw) || mu0_raw == "") next
      mu0_val <- as.numeric(mu0_raw)
      if (is.na(mu0_val)) next
      
      code_lines <- c(
        code_lines,
        paste0("## One-sample inference for ", v),
        "## One Sample t-test",
        paste0("# two-sided"),
        paste0("t.test(x = ", v, ", mu = ", mu0_val, ", conf.level = ", 1 - alpha, ", alternative = \"two.sided\")"),
        paste0("# one-sided (less)"),
        paste0("t.test(x = ", v, ", mu = ", mu0_val, ", conf.level = ", 1 - alpha, ", alternative = \"less\")"),
        paste0("# one-sided (greater)"),
        paste0("t.test(x = ", v, ", mu = ", mu0_val, ", conf.level = ", 1 - alpha, ", alternative = \"greater\")"),
        "",
        "## Wilcoxon signed-rank test",
        paste0("# two-sided"),
        paste0("wilcox.test(x = ", v, ", mu = ", mu0_val, ", conf.level = ", 1 - alpha, ", alternative = \"two.sided\", conf.int = TRUE)"),
        paste0("# one-sided (less)"),
        paste0("wilcox.test(x = ", v, ", mu = ", mu0_val, ", conf.level = ", 1 - alpha, ", alternative = \"less\", conf.int = TRUE)"),
        paste0("# one-sided (greater)"),
        paste0("wilcox.test(x = ", v, ", mu = ", mu0_val, ", conf.level = ", 1 - alpha, ", alternative = \"greater\", conf.int = TRUE)"),
        ""
      )
    }
    
    if (length(code_lines) == 0) {
      "No valid null hypotheses provided."
    } else {
      paste(code_lines, collapse = "\n")
    }
  })
  
   
}
