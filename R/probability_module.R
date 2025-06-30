##*********************************************
##
## @file: probability_module.R
##
## Run FIRSTkit software
##
## Author:
## Israel Almodovar-Rivera PhD
## Department of Mathematical Sciences
## University of Puerto Rico at Mayaguez
## israel.almodovar@upr.edu
## Copyright June 2025
##*********************************************

bayes_probability_tree <- function(prior,sensitivity, specificity) {
  ## code from https://daranzolin.github.io/2018-01-07-probability-trees/
## modified to be a shiny app by Israel A. Almodóvar-Rivera.

  if (!all(c(prior,sensitivity, specificity) > 0) && !all(c(prior,sensitivity, specificity) < 1)) {
    stop("probabilities must be greater than 0 and less than 1.",
         call. = FALSE)
  }
  c_prior <- 1 - prior
  c_tp <- 1 - sensitivity
  c_tn <- 1 - specificity
  
  
  b1 <- prior * sensitivity
  b2 <- prior * c_tp
  b3 <- c_prior * c_tn
  b4 <- c_prior * specificity
  
  bp <- round(b1/(b1 + b3),digits=4)
  
  labs <- round(c(prior, c_prior, sensitivity, c_tp, specificity, c_tn, b1, b2, b4, b3),digits=4)
  labs <- c("",labs)
  
  tree <-
    create_graph() %>%
    add_n_nodes(
      n = 11,
      type = "path",
      label = labs,
      node_aes = node_aes(
        shape = "circle",
        height = 1,
        width = 1,
        fontsize=15,
        fillcolor = "#E5F5E0",
        fontcolor="black",
        x = c(0, 3, 3, 6, 6, 6, 6, 8, 8, 8, 8),
        y = c(0, 2, -2, 3, 1, -3, -1, 3, 1, -3, -1))) %>% 
    add_edge(
      from = 1,
      to = 2,
      edge_aes = edge_aes(
        label = "Prior",
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 1, 
      to = 3,
      edge_aes = edge_aes(
        label = "1-Prior",
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 4,
      edge_aes = edge_aes(
        label = "True Positive", ## Sensitivity=TRUE POSITIVE
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 5,
      edge_aes = edge_aes(
        label = "False Negative",
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 7,
      edge_aes = edge_aes(
        label = "False Positive",
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 6,
      edge_aes = edge_aes(
        label = "True Negative", ## Specificity = True Negative
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 4,
      to = 8,
      edge_aes = edge_aes(
        label = "=",
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 5,
      to = 9,
      edge_aes = edge_aes(
        label = "=",
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 7,
      to = 11,
      edge_aes = edge_aes(
        label = "=",
        fontsize=13,
      )
    ) %>% 
    add_edge(
      from = 6,
      to = 10,
      edge_aes = edge_aes(
        label = "=",
        fontsize=13,
      )
    ) 
  tree
}


# ---- Probability Theory ----
probability_ui <- navbarMenu("Probability Theory",
           # --- Venn diagram ----
           tabPanel("Venn Diagram",
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("sets", "Number of Sets", value = 1, min = 1, max = 7),
                        textInput("notation", "Insert Case", value = "A"),
                        actionButton("venn_submit", "Submit")
                      ),
                      mainPanel(plotOutput("vennPlot", height = "500px", width = "500px"))
                    )
           ),
           # ---- Bayes Tree Diagram ----
           tabPanel("Bayes Tree Diagram",
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("prior", "Prior Probability", value = 0.05, min = 0, max = 1, step = 0.01),
                        numericInput("tp", "Sensitivity (True Positive)", value = 0.95, min = 0, max = 1, step = 0.01),
                        numericInput("tn", "Specificity (True Negative)", value = 0.9, min = 0, max = 1, step = 0.01),
                        actionButton("bayes_submit", "Submit")
                      ),
                      mainPanel(
                        uiOutput('diagram', width = "500px", height = "500px"),
                        uiOutput("posterior")
                      )
                    )
           ),
            tabPanel("Distribution Functions",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           inputId = "distribution",
                           label = "Distribution:",
                           choices = c("Binomial","Poisson","Geometric",
                                       "Hypergeometric",
                                       "Normal","Chi-squared", "Student-t",
                                       "Snedecor-F"),
                           multiple = FALSE,
                           selected = "Normal"
                         ),
                         hr(),
                         conditionalPanel(
                           condition = "input.distribution == 'Binomial'",
                           numericInput("n_binomial", "Number of trials \\(n\\):",
                                        value = 10, min = 0, step = 1
                           ),
                           numericInput("p_binomial", "Probability of success \\(p\\):",
                                        value = 0.5, min = 0, max = 1, step = 0.001
                           )
                         ),
                        conditionalPanel(
                          condition = "input.distribution == 'Chi-squared'",
                          numericInput("df_chisq", "Degrees of freedom \\(df\\):",
                                       value = 1, min = 1, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Snedecor-F'",
                          numericInput("df1_f", "Degrees of freedom \\(df_1\\):",
                                       value = 10, min = 1, step = 1
                          ),
                          numericInput("df2_f", "Degrees of freedom \\(df_2\\):",
                                       value = 5, min = 1, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal'",
                          numericInput("mean_normal", "Mean \\(\\mu\\):",
                                       value = 0, step = 1
                          ),
                          radioButtons(
                            inputId = "variance_sd",
                            label = NULL,
                            choices = c(
                              "Variance \\(\\sigma^2\\)" = "variance_true",
                              "Standard deviation \\(\\sigma\\)" = "variance_false"
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal' && input.variance_sd == 'variance_true'",
                          numericInput("variance_normal", "Variance \\(\\sigma^2\\):",
                                       value = 1, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal' && input.variance_sd == 'variance_false'",
                          numericInput("sd_normal", "Standard deviation \\(\\sigma\\):",
                                       value = 1, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Poisson'",
                          numericInput("lambda_poisson", "Rate \\(\\lambda\\):",
                                       value = 4, min = 1, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Student-t'",
                          numericInput("df_t", "Degrees of freedom \\(df\\):",
                                       value = 3, min = 1, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Geometric'",
                          numericInput("p_geom", "Probability of success \\(p\\):",
                                       value = 0.5, min = 0, max = 1, step = 0.001,
                          )
                        ),

                        conditionalPanel(
                          condition = "input.distribution == 'Hypergeometric'",
                          numericInput("n_hypergeometric", "Sample size \\(n\\):",
                                       value = 50, min = 0, step = 1
                          ),
                          numericInput("N_hypergeometric", "Total number of objects \\(N\\):",
                                       value = 100, min = 0, step = 1
                          ),
                          numericInput("M_hypergeometric", "Number of successes \\(M\\):",
                                       value = 25, min = 0, step = 1
                          )
                        ),

                        hr(),
                         conditionalPanel(
                           condition = "input.distribution == 'Binomial'",
                           radioButtons(
                             inputId = "tail_binomial",
                             label = NULL,
                             choices = c(
                               "\\(\\mathbb{P}(X = x)\\)" = "equal",
                               "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                               "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                               "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                             )
                           )
                         ),
                        conditionalPanel(
                          condition = "input.distribution == 'Chi-squared'",
                          radioButtons(
                            inputId = "tail_chisq",
                            label = NULL,
                            choices = c(
                              "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                              "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                              "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Snedecor-F'",
                          radioButtons(
                            inputId = "tail_f",
                            label = NULL,
                            choices = c(
                              "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                              "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                              "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal'",
                          radioButtons(
                            inputId = "tail_normal",
                            label = NULL,
                            choices = c(
                              "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                              "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                              "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Poisson'",
                          radioButtons(
                            inputId = "tail_poisson",
                            label = NULL,
                            choices = c(
                              "\\(\\mathbb{P}(X = x)\\)" = "equal",
                              "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                              "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                              "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Student-t'",
                          radioButtons(
                            inputId = "tail_studentt",
                            label = NULL,
                            choices = c(
                              "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                              "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                              "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Geometric'",
                          radioButtons(
                            inputId = "tail_geom",
                            label = NULL,
                            choices = c(
                              "\\(\\mathbb{P}(X = x)\\)" = "equal",
                              "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                              "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                              "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Hypergeometric'",
                          radioButtons(
                            inputId = "tail_hypergeometric",
                            label = NULL,
                            choices = c(
                              "\\(\\mathbb{P}(X = x)\\)" = "equal",
                              "\\(\\mathbb{P}(X \\leq x)\\)" = "lower.tail",
                              "\\(\\mathbb{P}(X > x)\\)" = "upper.tail",
                              "\\(\\mathbb{P}(a \\leq X \\leq b)\\)" = "two.sided"
                            )
                          )
                        ),
                         hr(),
                         conditionalPanel(
                           condition = "input.distribution == 'Binomial' && input.tail_binomial == 'equal'",
                           numericInput("x1_binomial", "x:",
                                        value = 5, min = 0, step = 1
                           )
                         ),
                         conditionalPanel(
                           condition = "input.distribution == 'Binomial' && input.tail_binomial == 'lower.tail'",
                           numericInput("x1_binomial", "x:",
                                        value = 5, min = 0, step = 1
                           )
                         ),
                         conditionalPanel(
                           condition = "input.distribution == 'Binomial' && input.tail_binomial == 'upper.tail'",
                           numericInput("x2_binomial", "x:",
                                        value = 5, min = 0, step = 1
                           )
                         ),
                         conditionalPanel(
                           condition = "input.distribution == 'Binomial' && input.tail_binomial == 'two.sided'",
                           numericInput("a_binomial", "a",
                                        value = 3, min = 0, step = 1
                           ),
                           numericInput("b_binomial", "b \\( (a \\leq b) \\)",
                                        value = 11, min = 0, step = 1
                           )
                         ),
                        conditionalPanel(
                          condition = "input.distribution == 'Chi-squared' && input.tail_chisq == 'lower.tail'",
                          numericInput("x1_chisq", "x:",
                                       value = 3, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Chi-squared' && input.tail_chisq == 'upper.tail'",
                          numericInput("x2_chisq", "x:",
                                       value = 3, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Chi-squared' && input.tail_chisq == 'two.sided'",
                          numericInput("a_chisq", "a",
                                       value = 2, min = 0, step = 1
                          ),
                          numericInput("b_chisq", "b \\( (a \\leq b) \\)",
                                       value = 11, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Snedecor-F' && input.tail_f == 'lower.tail'",
                          numericInput("x1_f", "x:",
                                       value = 2, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Snedecor-F' && input.tail_f == 'upper.tail'",
                          numericInput("x2_f", "x:",
                                       value = 2, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Snedecor-F' && input.tail_f == 'two.sided'",
                          numericInput("a_f", "a",
                                       value = 2, min = 0, step = 1
                          ),
                          numericInput("b_f", "b \\( (a \\leq b) \\)",
                                       value = 11, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal' && input.tail_normal == 'lower.tail'",
                          numericInput("x1_normal", "x:",
                                       value = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal' && input.tail_normal == 'upper.tail'",
                          numericInput("x2_normal", "x:",
                                       value = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal' && input.tail_normal == 'two.sided'",
                          numericInput("a_normal", "a:",
                                       value = -3, step = 1
                          ),
                          numericInput("b_normal", "b \\( (a \\leq b) \\)",
                                       value = 3, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Poisson' && input.tail_poisson == 'equal'",
                          numericInput("x1_poisson", "x:",
                                       value = 2, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Poisson' && input.tail_poisson == 'lower.tail'",
                          numericInput("x1_poisson", "x:",
                                       value = 2, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Poisson' && input.tail_poisson == 'upper.tail'",
                          numericInput("x2_poisson", "x:",
                                       value = 2, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Poisson' && input.tail_poisson == 'two.sided'",
                          numericInput("a_poisson", "a",
                                       value = 2, min = 0, step = 1
                          ),
                          numericInput("b_poisson", "b \\( (a \\leq b) \\)",
                                       value = 5, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Geometric' && input.tail_geom == 'equal'",
                          helpText("Number of failures before the \\(1^{st}\\) success"),
                          numericInput("x1_geom", "x:",
                                       value = 1, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Geometric' && input.tail_geom == 'lower.tail'",
                          helpText("Number of failures before the \\(1^{st}\\) success"),
                          numericInput("x1_geom", "x:",
                                       value = 1, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Geometric' && input.tail_geom == 'upper.tail'",
                          helpText("Number of failures before the \\(1^{st}\\) success"),
                          numericInput("x2_geom", "x:",
                                       value = 1, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Geometric' && input.tail_geom == 'two.sided'",
                          helpText("Number of failures before the \\(1^{st}\\) success"),
                          numericInput("a_geom", "a",
                                       value = 2, min = 0, step = 1
                          ),
                          numericInput("b_geom", "b: \\( (a \\leq b) \\)",
                                       value = 5, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Student-t' && input.tail_studentt == 'lower.tail'",
                          numericInput("x1_t", "x:",
                                       value = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Student-t' && input.tail_studentt == 'upper.tail'",
                          numericInput("x2_t", "x:",
                                       value = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Student-t' && input.tail_studentt == 'two.sided'",
                          numericInput("a_t", "a",
                                       value = -3, step = 1
                          ),
                          numericInput("b_t", "b \\( (a \\leq b) \\)",
                                       value = 3, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Hypergeometric' && input.tail_hypergeometric == 'equal'",
                          numericInput("x1_hypergeometric", "x:",
                                       value = 3, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Hypergeometric' && input.tail_hypergeometric == 'lower.tail'",
                          numericInput("x1_hypergeometric", "x:",
                                       value = 3, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Hypergeometric' && input.tail_hypergeometric == 'upper.tail'",
                          numericInput("x2_hypergeometric", "x:",
                                       value = 3, min = 0, step = 1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Hypergeometric' && input.tail_hypergeometric == 'two.sided'",
                          numericInput("a_hypergeometric", "a",
                                       value = 2, min = 0, step = 1
                          ),
                          numericInput("b_hypergeometric", "b \\( (a \\leq b) \\)",
                                       value = 5, min = 0, step = 1
                          )
                        ),
                         actionButton("dist_submit", "Submit"),
                         hr()
                       ),
           #            
           #            # Show a plot of the generated distribution
                       mainPanel(
                         br(),
                         conditionalPanel(
                           condition = "input.distribution == 'Binomial'",
                           plotlyOutput("binomialPlot")
                         ),
                        conditionalPanel(
                          condition = "input.distribution == 'Poisson'",
                          plotlyOutput("PoissonPlot")
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Geometric'",
                          plotlyOutput("geometricPlot")
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Hypergeometric'",
                          plotlyOutput("HypergeometricPlot")
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Chi-squared' ",
                          plotlyOutput("chiSquarePlot")
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Snedecor-F'",
                          plotlyOutput("SnedecorFPlot")
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Normal'",
                          plotlyOutput("normalPlot")
                        ),
                        conditionalPanel(
                          condition = "input.distribution == 'Student-t'",
                          plotlyOutput("StudenttPlot")
                        ),
                         br(),
                         uiOutput("results_distribution"),
                         br()
                       )
                    )
           )
)

probability_server <- function(input, output, session) {


output$vennPlot <- renderPlot({
  req(input$notation)
  nsets <- max(1, input$sets)
  venn(input$notation, snames = LETTERS[1:nsets], sncs = 2)
})

output$diagram <- renderUI({
  graph <- bayes_probability_tree(prior = input$prior, sensitivity = input$tp, specificity = input$tn)
  print(render_graph(graph))
})

output$posterior <- renderUI({
  
  c_prior <- 1-input$prior
  c_tp <- 1-input$tp
  c_tn <- 1-input$tn
  
  b1 <- input$prior*input$tp
  b3 <- c_prior*c_tn
  
  bp <- round(b1/(b1+b3), 4)
  
  withMathJax(
    paste(paste0("The probability of having the disease (prior = ", input$prior, ")",
                 " after testing positive, i.e., $$\\mathbb{P}(\\mbox{Disease}|\\mbox{Testing Positive}) = "),
          sprintf("\\frac{%.4f \\times %.4f}{%.4f \\times %.4f + %.4f \\times %.4f} = %.4f $$",
                  input$tp, input$prior, input$tp, input$prior, c_prior, input$tn, bp))
  )
})
# 
 output$results_distribution <- renderUI({
   if(input$distribution == "Binomial") {
     withMathJax(
       paste0("\\(X \\sim \\mbox{Binomial}(n = \\)", " ", input$n_binomial, ", ", "\\(p = \\)", " ", input$p_binomial, "\\()\\)", " and ", case_when(
         input$tail_binomial == "equal" ~ paste0("\\( \\mathbb{P}(X = \\)", " ", input$x1_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(dbinom(input$x1_binomial, size = input$n_binomial, prob = input$p_binomial), 4)),
         input$tail_binomial == "lower.tail" ~ paste0("\\( \\mathbb{P}(X \\leq \\)", " ", input$x1_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x1_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)),
         input$tail_binomial == "upper.tail" ~ paste0("\\(\\mathbb{P}(X > \\)", " ", input$x2_binomial, "\\()\\)", " ", "\\( = \\)", " ", round(pbinom(input$x2_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE), 4)),
         input$tail_binomial == "two.sided" ~ paste0("\\(\\mathbb{P}(\\)", input$a_binomial, " ", "\\(\\leq X\\leq \\)", " ", input$b_binomial, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_binomial > input$b_binomial, "a must be less than or equal to b", round(pbinom(input$b_binomial, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE) - pbinom(input$a_binomial - 1, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE), 4)))
       ))
     ) 
  }else if(input$distribution == "Poisson") {
    withMathJax(
      paste0("\\(X \\sim \\mbox{Poisson}(\\lambda = \\)", " ", input$lambda_poisson, "\\()\\)", " and ", case_when(
        input$tail_poisson == "equal" ~ paste0("\\(\\mathbb{P}(X = \\)", " ", input$x1_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(dpois(input$x1_poisson, lambda = input$lambda_poisson), 4)),
        input$tail_poisson == "lower.tail" ~ paste0("\\(\\mathbb{P}(X \\leq \\)", " ", input$x1_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x1_poisson, lambda = input$lambda_poisson, lower.tail = TRUE), 4)),
        input$tail_poisson == "upper.tail" ~ paste0("\\(\\mathbb{P}(X > \\)", " ", input$x2_poisson, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(input$x2_poisson, lambda = input$lambda_poisson, lower.tail = FALSE), 4)),
        input$tail_poisson == "two.sided" ~ paste0("\\(\\mathbb{P}(\\)", input$a_poisson, " ", "\\(\\leq X\\leq \\)", " ", input$b_poisson, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_poisson > input$b_poisson, "a must be less than or equal to b", round(ppois(input$b_poisson, lambda = input$lambda_poisson, lower.tail = TRUE) - ppois(input$a_poisson - 1, lambda = input$lambda_poisson, lower.tail = TRUE), 4)))
      ))
    )
  }  else if(input$distribution == "Geometric") {
    withMathJax(
      paste0("\\(X \\sim \\mbox{Geometric}(p = \\)", " ", input$p_geom, "\\()\\)", " and ", case_when(
        input$tail_geom == "equal" ~ paste0("\\(\\mathbb{P}(X = \\)", " ", input$x1_geom, "\\()\\)", " ", "\\( = \\)", " ", round(dgeom(input$x1_geom, prob = input$p_geom), 4)),
        input$tail_geom == "lower.tail" ~ paste0("\\(\\mathbb{P}(X \\leq \\)", " ", input$x1_geom, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x1_geom, prob = input$p_geom, lower.tail = TRUE), 4)),
        input$tail_geom == "upper.tail" ~ paste0("\\(\\mathbb{P}(X > \\)", " ", input$x2_geom, "\\()\\)", " ", "\\( = \\)", " ", round(pgeom(input$x2_geom, prob = input$p_geom, lower.tail = FALSE), 4)),
        input$tail_geom == "two.sided" ~ paste0("\\(\\mathbb{P}(\\)", input$a_geom, " ", "\\(\\leq X\\leq \\)", " ", input$b_geom, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_geom > input$b_geom, "a must be less than or equal to b", round(pgeom(input$b_geom, prob = input$p_geom, lower.tail = TRUE) - pgeom(input$a_geom - 1, prob = input$p_geom, lower.tail = TRUE), 4)))
      ))
    )
  } else if(input$distribution == "Hypergeometric") {
    withMathJax(
      paste0("\\(X \\sim \\mbox{Hypergeometric}(n = \\)", " ", input$n_hypergeometric, ", ", "\\(N = \\)", " ", input$N_hypergeometric, ", ", "\\(M = \\)", " ", input$M_hypergeometric, "\\()\\)", " and ", case_when(
        input$tail_hypergeometric == "equal" ~ paste0("\\(\\mathbb{P}(X = \\)", " ", input$x1_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(dhyper(input$x1_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric), 4)),
        input$tail_hypergeometric == "lower.tail" ~ paste0("\\(\\mathbb{P}(X \\leq \\)", " ", input$x1_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x1_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)),
        input$tail_hypergeometric == "upper.tail" ~ paste0("\\(\\mathbb{P}(X > \\)", " ", input$x2_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", round(phyper(input$x2_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = FALSE), 4)),
        input$tail_hypergeometric == "two.sided" ~ paste0("\\( \\mathbb{P}(\\)", input$a_hypergeometric, " ", "\\(\\leq X\\leq \\)", " ", input$b_hypergeometric, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_hypergeometric > input$b_hypergeometric, "a must be less than or equal to b", round(phyper(input$b_hypergeometric, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE) - phyper(input$a_hypergeometric - 1, m = input$M_hypergeometric, n = (input$N_hypergeometric - input$M_hypergeometric), k = input$n_hypergeometric, lower.tail = TRUE), 4)))
      ))
    )
  } else if (input$distribution == "Chi-squared") {
    withMathJax(
      paste0("\\(X \\sim \\chi^2(\\)", " ", input$df_chisq, "\\()\\)", " and ", case_when(
        input$tail_chisq == "lower.tail" ~ paste0("\\(\\mathbb{P}(X \\leq \\)", " ", input$x1_chisq, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x1_chisq, df = input$df_chisq, lower.tail = TRUE), 4)),
        input$tail_chisq == "upper.tail" ~ paste0("\\(\\mathbb{P}(X > \\)", " ", input$x2_chisq, "\\()\\)", " ", "\\( = \\)", " ", round(pchisq(input$x2_chisq, df = input$df_chisq, lower.tail = FALSE), 4)),
        input$tail_chisq == "two.sided" ~ paste0("\\(\\mathbb{P}(\\)", input$a_chisq, " ", "\\(\\leq X\\leq \\)", " ", input$b_chisq, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_chisq > input$b_chisq, "a must be less than or equal to b", round(pchisq(input$b_chisq, df = input$df_chisq, lower.tail = TRUE) - pchisq(input$a_chisq, df = input$df_chisq, lower.tail = TRUE), 4)))
      ))
    )
  } else if (input$distribution == "Snedecor-F") {
    withMathJax(
      paste0("\\(X \\sim F(\\)", input$df1_f, ", ", input$df2_f, "\\()\\)", ", ", case_when(
        input$tail_f == "lower.tail" ~ paste0("\\(\\mathbb{P}(X \\leq \\)", " ", input$x1_f, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x1_f, df1 = input$df1_f, df2 = input$df2_f, lower.tail = TRUE), 4)),
        input$tail_f == "upper.tail" ~ paste0("\\(\\mathbb{P} (X > \\)", " ", input$x2_f, "\\()\\)", " ", "\\( = \\)", " ", round(pf(input$x2_f, df1 = input$df1_f, df2 = input$df2_f, lower.tail = FALSE), 4)),
        input$tail_f == "two.sided" ~ paste0("\\(\\mathbb{P}(\\)", input$a_f, " ", "\\(\\leq X\\leq \\)", " ", input$b_f, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_f > input$b_f, "a must be less than or equal to b", round(pf(input$b_f, df1 = input$df1_f, df = input$df2_f, lower.tail = TRUE) - pf(input$a_f, df1 = input$df1_f, df = input$df2_f, lower.tail = TRUE), 4)))
      ))
    )
  } else if (input$distribution == "Normal") {
    withMathJax(
      paste0("\\(X \\sim \\mbox{Normal}(\\mu = \\)", " ", input$mean_normal, ", ", ifelse(input$variance_sd == "variance_true", paste0("\\(\\sigma^2 = \\)", " ", input$variance_normal), paste0("\\(\\sigma^2 = \\)", " ", input$sd_normal^2)), "\\()\\)", " and ", case_when(
        input$tail_normal == "lower.tail" ~ paste0("\\( \\mathbb{P}(X \\leq \\)", " ", input$x1_normal, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x1_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)),
        input$tail_normal == "upper.tail" ~ paste0("\\(\\mathbb{P}(X > \\)", " ", input$x2_normal, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(input$x2_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = FALSE), 4)),
        input$tail_normal == "two.sided" ~ paste0("\\(\\mathbb{P}(\\)", input$a_normal, " ", "\\(\\leq X\\leq \\)", " ", input$b_normal, "\\()\\)", " ", "\\( = \\)", " ",  ifelse(input$a_normal > input$b_normal, "a must be less than or equal to b", round(pnorm(input$b_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE) - pnorm(input$a_normal, mean = input$mean_normal, sd = ifelse(input$variance_sd == "variance_true", sqrt(input$variance_normal), input$sd_normal), lower.tail = TRUE), 4)))
      ))
    )
  } else if (input$distribution == "Student-t") {
    withMathJax(
      paste0("\\(X \\sim t(\\)",input$df_t, "\\()\\)", " and ", case_when(
        input$tail_studentt == "lower.tail" ~ paste0("\\(\\mathbb{P}(X \\leq \\)", " ", input$x1_t, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x1_t, df = input$df_t, lower.tail = TRUE), 4)),
        input$tail_studentt == "upper.tail" ~ paste0("\\(\\mathbb{P}(X > \\)", " ", input$x2_t, "\\()\\)", " ", "\\( = \\)", " ", round(pt(input$x2_t, df = input$df_t, lower.tail = FALSE), 4)),
        input$tail_studentt == "two.sided" ~ paste0("\\(\\mathbb{P}(\\)", input$a_t, " ", "\\(\\leq X\\leq \\)", " ", input$b_t, "\\()\\)", " ", "\\( = \\)", " ", ifelse(input$a_t > input$b_t, "a must be less than or equal to b", round(pt(input$b_t, df = input$df_t, lower.tail = TRUE) - pt(input$a_t, df = input$df_t, lower.tail = TRUE), 4)))
      ))
    )
  } else {

  }
})

# ##**************************
# ##* Discrete distributions
# ##**************************

output$binomialPlot <- renderPlotly({
  xx <- qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):
    qbinom(0.99999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE)

  probs <- dbinom(xx, size = input$n_binomial, prob = input$p_binomial)

  df <- data.frame(x = xx, prob = probs)

  df$X <- switch(input$tail_binomial,
                 equal = ifelse(df$x <= input$x1_binomial, "2", "Other"),
                 lower.tail = ifelse(df$x <= input$x1_binomial, "2", "Other"),
                 upper.tail = ifelse(df$x >  input$x2_binomial, "2", "Other"),
                 two.sided  = ifelse(df$x >= input$a_binomial & df$x <= input$b_binomial, "2", "Other"))

  df$x <- as.factor(df$x)

  # Create ggplot
  p <- ggplot(df, aes(x = x, y = prob, fill = X, color = X, text = paste("x =", x, "<br>p =", round(prob, 4)))) +
    geom_col() +
    geom_text(aes(label = round(prob, 4), y = prob + 0.005), size = 3, vjust = 0) +
    scale_fill_manual(values = c("2" = "#656565", "Other" = "#ffffff")) +
    scale_color_manual(values = c("2" = "#000000", "Other" = "#000000")) +
    theme_bw() +
    theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5)) +
    ggtitle(paste0("Binomial(", input$n_binomial, ", ", input$p_binomial, ")")) +
    labs(x = "x", y = "Probability Mass Function")

  ggplotly(p, tooltip = "text")
})

output$PoissonPlot <- renderPlotly({
  lambda <- input$lambda_poisson
  x_vals <- qpois(0.99999, lambda, lower.tail = FALSE):qpois(0.99999, lambda, lower.tail = TRUE)
  df <- data.frame(x = x_vals, prob = dpois(x_vals, lambda = lambda))

  df$region <- switch(input$tail_poisson,
                      "equal" = ifelse(df$x <= input$x1_poisson, "Interest", "Other"),
                      "lower.tail" = ifelse(df$x <= input$x1_poisson, "Interest", "Other"),
                      "upper.tail" = ifelse(df$x > input$x2_poisson, "Interest", "Other"),
                      "two.sided" = ifelse(df$x >= input$a_poisson & df$x <= input$b_poisson, "Interest", "Other")
  )

  p <- ggplot(df, aes(x = factor(x), y = prob, fill = region)) +
    geom_col(color = "black") +
    geom_text(aes(label = round(prob, 4)), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("Interest" = "#656565", "Other" = "white")) +
    scale_color_manual(values = c("Interest" = "#000000", "Other" = "#000000")) +
    labs(x = "x", y = "Probability Mass Function",
         title = paste0("Poisson(", lambda, ")")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  ggplotly(p)
})

output$geometricPlot <- renderPlotly({
  p_geom <- input$p_geom
  xmax <- ceiling(p_geom + 5 * sqrt((1 - p_geom) / p_geom^2))
  x_vals <- 0:xmax
  df <- data.frame(x = x_vals, prob = dgeom(x_vals, prob = p_geom))

  df$region <- switch(input$tail_geom,
                      "equal" = ifelse(df$x <= input$x1_geom, "Interest", "Other"),
                      "lower.tail" = ifelse(df$x <= input$x1_geom, "Interest", "Other"),
                      "upper.tail" = ifelse(df$x > input$x2_geom, "Interest", "Other"),
                      "two.sided" = ifelse(df$x >= input$a_geom & df$x <= input$b_geom, "Interest", "Other")
  )

  p <- ggplot(df, aes(x = factor(x), y = prob, fill = region)) +
    geom_col(color = "black") +
    geom_text(aes(label = round(prob, 4)), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("Interest" = "#656565", "Other" = "white")) +
    scale_color_manual(values = c("Interest" = "#000000", "Other" = "#000000")) +
    labs(x = "x", y = "Probability Mass Function",
         title = paste0("Geometric(", p_geom, ")")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  ggplotly(p)
})

output$HypergeometricPlot <- renderPlotly({
  m <- input$M_hypergeometric
  N <- input$N_hypergeometric
  k <- input$n_hypergeometric

  x_vals <- qhyper(0.99999, m = m, n = N - m, k = k, lower.tail = FALSE):
    qhyper(0.99999, m = m, n = N - m, k = k, lower.tail = TRUE)

  df <- data.frame(x = x_vals, prob = dhyper(x_vals, m = m, n = N - m, k = k))

  df$region <- switch(input$tail_hypergeometric,
                      "equal" = ifelse(df$x <= input$x1_hypergeometric, "Interest", "Other"),
                      "lower.tail" = ifelse(df$x <= input$x1_hypergeometric, "Interest", "Other"),
                      "upper.tail" = ifelse(df$x > input$x2_hypergeometric, "Interest", "Other"),
                      "two.sided" = ifelse(df$x >= input$a_hypergeometric & df$x <= input$b_hypergeometric, "Interest", "Other")
  )

  p <- ggplot(df, aes(x = factor(x), y = prob, fill = region)) +
    geom_col(color = "black") +
    geom_text(aes(label = round(prob, 4)), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("Interest" = "#656565", "Other" = "white")) +
    scale_color_manual(values = c("Interest" = "#000000", "Other" = "#000000")) +
    labs(x = "x", y = "Probability Mass Function",
         title = paste0("Hypergeometric(n=", k, ", M=", m, ", N=", N, ")")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  ggplotly(p)
})


##****************************
##* Continuous distribution
##****************************

output$normalPlot <- renderPlotly({
  mean <- input$mean_normal
  sigma <- if (input$variance_sd == "variance_true") {
    sqrt(input$variance_normal)
  } else {
    input$sd_normal
  }

  x_vals <- seq(qnorm(0.0001, mean, sigma), qnorm(0.9999, mean, sigma), length.out = 500)
  df <- data.frame(x = x_vals, y = dnorm(x_vals, mean, sigma))

  df$region <- switch(input$tail_normal,
                      "lower.tail" = ifelse(df$x <= input$x1_normal, "Interest", "Other"),
                      "upper.tail" = ifelse(df$x > input$x2_normal, "Interest", "Other"),
                      "two.sided" = ifelse(df$x >= input$a_normal & df$x <= input$b_normal, "Interest", "Other")
  )

  p <- ggplot(df, aes(x = x, y = y, fill = region)) +
    geom_area(aes(group = region), color = "black") +
    scale_fill_manual(values = c("Interest" = "#656565", "Other" = "white")) +
    scale_color_manual(values = c("Interest" = "#000000", "Other" = "#000000")) +
    labs(title = paste0("Normal(", mean, ", ", sigma, ")"),
         x = "x", y = "Probability Density Function") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  ggplotly(p)
})

output$chiSquarePlot <- renderPlotly({
  df <- input$df_chisq
  x_vals <- seq(qchisq(0.0001, df), qchisq(0.9999, df), length.out = 500)
  df_data <- data.frame(x = x_vals, y = dchisq(x_vals, df))

  df_data$region <- switch(input$tail_chisq,
                           "lower.tail" = ifelse(df_data$x <= input$x1_chisq, "Interest", "Other"),
                           "upper.tail" = ifelse(df_data$x > input$x2_chisq, "Interest", "Other"),
                           "two.sided" = ifelse(df_data$x >= input$a_chisq & df_data$x <= input$b_chisq, "Interest", "Other")
  )

  p <- ggplot(df_data, aes(x = x, y = y, fill = region)) +
    geom_area(aes(group = region), color = "black") +
    scale_fill_manual(values = c("Interest" = "#656565", "Other" = "white")) +
    scale_color_manual(values = c("Interest" = "#000000", "Other" = "#000000")) +
    labs(title = paste0("Chi(", df, ")"), x = "x", y = "Probability Density Function") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

  ggplotly(p)
})

output$StudenttPlot <- renderPlotly({
  df <- input$df_t
  x_vals <- seq(qt(0.0005, df), qt(0.9995, df), length.out = 500)
  df_data <- data.frame(x = x_vals, y = dt(x_vals, df))

  df_data$region <- switch(input$tail_studentt,
                           "lower.tail" = ifelse(df_data$x <= input$x1_t, "Interest", "Other"),
                           "upper.tail" = ifelse(df_data$x > input$x2_t, "Interest", "Other"),
                           "two.sided" = ifelse(df_data$x >= input$a_t & df_data$x <= input$b_t, "Interest", "Other")
  )

  p <- ggplot(df_data, aes(x = x, y = y, fill = region)) +
    geom_area(aes(group = region), color = "black") +
    scale_fill_manual(values = c("Interest" = "#656565", "Other" = "white")) +
    scale_color_manual(values = c("Interest" = "#000000", "Other" = "#000000")) +
    labs(title = paste0("Student-t(", df, ")"), x = "x", y = "Probability Density Function") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

  ggplotly(p)
})

output$SnedecorFPlot <- renderPlotly({
  df1 <- input$df1_f
  df2 <- input$df2_f
  x_vals <- seq(qf(0.0005, df1, df2), qf(0.9995, df1, df2), length.out = 500)
  df_data <- data.frame(x = x_vals, y = df(x_vals, df1, df2))

  df_data$region <- switch(input$tail_f,
                           "lower.tail" = ifelse(df_data$x <= input$x1_f, "Interest", "Other"),
                           "upper.tail" = ifelse(df_data$x > input$x2_f, "Interest", "Other"),
                           "two.sided" = ifelse(df_data$x >= input$a_f & df_data$x <= input$b_f, "Interest", "Other")
  )

  p <- ggplot(df_data, aes(x = x, y = y, fill = region)) +
    geom_area(aes(group = region), color = "black") +
    scale_fill_manual(values = c("Interest" = "#656565", "Other" = "white")) +
    scale_color_manual(values = c("Interest" = "#000000", "Other" = "#000000")) +
    labs(title = paste0("Snedecor-F(", df1, ", ", df2, ")"), x = "x", y = "Probability Density Function") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

  ggplotly(p)
})

}
