
intro_ui <- tabPanel("Introduction",
           fluidPage(
             titlePanel("First Impressions R-based Statistics Toolkit (FIRSTkit)"),
             withMathJax(),
             h4("WELCOME"),
             h4("Overview"),
             p("FIRSTkit is a companion for introductory statistics courses requiring no prior R knowledge. It enables statistical analysis with minimal programming and helps students grasp fundamental concepts interactively."),
            
             h4("Modules Covered"),
             tags$ul(
               tags$li("Descriptive Statistics"),
               tags$li("Probability Theory"),
               tags$li("Distribution Functions"),
               tags$li("Statistical Inference"),
               tags$li("Simple and Multiple Linear Regression")
             ),
             
             h4("Data Input/Upload"),
             p("Supports .csv, .txt, .xls, .xlsx, and Google Sheets. Missing values are removed using na.rm = TRUE where possible."),
             
             h4("Descriptive Statistics"),
             tags$ul(
               tags$li("Location Measurements: Mean, Trimmed Mean, Median, Geometric Mean"),
               tags$li("Dispersion Measures: Standard Deviation, Variance, IQR, MAD, Range"),
               tags$li("Visualizations: Boxplot, Histogram, Stem-and-leaf plot, Density plot, Bar Graph, Scatter plot")
             ),
             
             h4("Probability Theory and Distribution Functions"),
             tags$ul(
               tags$li("Probability Events with Venn Diagram"),
               tags$li("Conditional Probability with Bayes Tree")
             ),
             
             h4("Discrete Distributions"),
             tags$ul(
               tags$li("Binomial Distribution"),
               tags$li("Poisson Distribution"),
               tags$li("Geometric Distribution")
             ),
             
             h4("Continuous Distributions"),
             tags$ul(
               tags$li("Normal Distribution"),
               tags$li("Chi-Squared Distribution"),
               tags$li("Student's t Distribution"),
               tags$li("Snedecor's F Distribution")
             ),
             
             h4("Inferential Statistics"),
             tags$ul(
               tags$li("One-Sample Tests: t-test, Wilcoxon, Chi-squared for variance, Proportion test"),
               tags$li("Two-Sample Tests: Independent & Paired t-tests, Wilcoxon-Mann-Whitney, F-test, Proportion test"),
               tags$li("Three or More Samples: ANOVA, Kruskal-Wallis, Chi-squared association test")
             ),
             
             h4("Linear Regression"),
             p("Fit simple or multiple linear regression models. The model assumes normally distributed residuals with constant variance:"),
             uiOutput("regression_equation"),
             
             h4("Authors"),
             p("[Israel A. Almodóvar-Rivera](https://github.com/ialmodovar/) is an Associate Professor in the Department of Mathematical Sciences at University of Puerto Rico at Mayaguez"),
             p("[Ranjan Maitra](https://www.stat.iastate.edu/people/ranjan-maitra) is an Professor in the Department of Statistics at Iowa State University."),
p("If you have any question or want to report something, or you want to contribute in this project you can send a message to
[*israel.almodovar@upr.edu*](mailto:israel.almodovar@upr.edu){.email} or [*maitra@iastate.edu*](mailto:maitra@iastate.edu){.email}."),

           )
           
  )

intro_server <- function(input, output, session) {
  output$regression_equation <- renderUI({
    withMathJax("$$Y_i = \\beta_0 + \\sum^p_{j=1} \\beta_j x_{ij} + \\varepsilon_i$$ where \\( \\varepsilon_i \\sim N(0, \\sigma^2) \\)")
  })
}

