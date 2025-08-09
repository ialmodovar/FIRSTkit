logo2 <- dataURI(file = "inst/app/www/firstkit-logo-final.png", mime = "image/png")

intro_ui <- tabPanel("Welcome",
                     fluidPage(
                       titlePanel("FIRST Impressions R-based Statistics Toolkit (FIRSTkit)"),
                       withMathJax(),
                       h4("Introduction"),
                       p("FIRSTkit is a companion for introductory statistics courses requiring no prior R knowledge."), 
                       p("It enables statistical analysis with minimal programming and helps students grasp fundamental concepts interactively."),
                       p(HTML("FIRSTkit is licensed under the <a href='https://www.gnu.org/licenses/gpl-3.0.txt'>GNU General Public License v3.0</a>.")),
                       
                       HTML(paste0(
                         '<div style="position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); opacity: 0.05; z-index: 0; pointer-events: none;">',
                         '<img src="', logo2, '" style="height: 200px;">',
                         '</div>'
                       )),
                       
                       h4("Modules Covered"),
                       tags$ul(
                         tags$li("Descriptive Statistics"),
                         tags$li("Probability Theory"),
                         tags$li("Distribution Functions"),
                         tags$li("Statistical Inference"),
                         tags$li("Linear Regression")
                       ),

                       h4("Data Input/Upload"),
                       p("User can input their own datasets. Supports .csv, .txt, .xls, .xlsx, .ods, and Google Sheets. Missing values are removed from the calculations."),

                       h4("Descriptive Statistics"),
                       tags$ul(
                         tags$li("Location Measurements: Mean, Trimmed Mean, Median, Geometric Mean"),
                         tags$li("Dispersion Measures: Standard Deviation, Variance, IQR, MAD, Range"),
                         tags$li("Visualizations: Boxplot, Histograms, Density plots and Bar Graph")
                       ),

                       h4("Probability Theory and Distribution Functions"),
                       tags$ul(
                         tags$li("Displaying Venn Diagram"),
                         tags$li("Conditional Probability with Bayes Tree")
                       ),

                       h4("Discrete Distributions"),
                       tags$ul(
                         tags$li("Binomial Distribution"),
                         tags$li("Poisson Distribution"),
                         tags$li("Geometric Distribution"),
                         tags$li("Hypergeometric Distribution")
                       ),

                       h4("Continuous Distributions"),
                       tags$ul(
                         withMathJax(),
                         tags$li("Normal Distribution"),
                         tags$li("Chi-Squared Distribution"),
                         tags$li("Student's \\(t\\) Distribution"),
                         tags$li("Snedecor's \\(F\\) Distribution")
                       ),

                       h4("Inferential Statistics"),
                       tags$ul(
                         withMathJax(),
                         tags$li("One-Sample Tests: \\(t\\)-test, Wilcoxon, Chi-squared test for variance, Proportion test"),
                         tags$li("Two-Sample Tests: Independent & Paired \\(t\\)-tests, Wilcoxon-Mann-Whitney, \\(F\\)-test, Proportion test"),
                         tags$li("Three or More Samples: ANOVA, Kruskal-Wallis, Multiple Comparison")
                       ),

                       h4("Linear Regression"),
                       p("Fit a linear regression models. The model assumes normally distributed residuals with constant variance:"),
                       uiOutput("regression_equation"),
                      
                       h4("Authors:"),
                       p("Israel A. Almodóvar-Rivera, PhD"),
                       p("email: ", tags$a(href = "mailto:israel.almodovar@upr.edu", "israel.almodovar@upr.edu")),
                       p("Ranjan Maitra, PhD"),
                       p("email: ", tags$a(href = "mailto:maitra@iastate.edu", "maitra@iastate.edu")),
                       p("Website: ", tags$a(href = "https://ialmodovar.github.io", "IAAR-Website", target = "_blank"))
                     )
)

intro_server <- function(input, output, session) {
  output$regression_equation <- renderUI({
    withMathJax("\\(Y_i = \\beta_0 + \\sum^p_{j=1} \\beta_j x_{ij} + \\varepsilon_i\\) where \\( \\varepsilon_i \\sim N(0, \\sigma^2) \\)")
  })
}
