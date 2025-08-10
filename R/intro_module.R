logo2 <- dataURI(file = "inst/app/www/firstkit-logo-final.png", mime = "image/png")

intro_ui <- tabPanel("Welcome",
                     fluidPage(
                       titlePanel(HTML(paste0(
                         '<div style="display: flex; align-items: center; justify-content: center; margin-top: 20px;">',
                         '<img src="', logo2, '" height="150" style="margin-top: 20px; margin-bottom: 20px;">',
                         '</div>'
                       ))),#"FIRST Impressions R-based Statistics Toolkit (FIRSTkit)"),
                       withMathJax(),
                       tags$h4(tags$strong("Introduction")),
                       p("FIRSTkit is a companion for introductory statistics courses requiring no knowledge of coding."), 
                       p("It enables statistical analysis with no programming and helps students grasp fundamental concepts interactively."),
                       p(" For the interested student, provides the basic R scripts that performed the calculations."),
                       p(HTML("FIRSTkit is licensed under the <a href='https://www.gnu.org/licenses/gpl-3.0.txt'>GNU General Public License v3.0</a>.")),
                       
                       HTML(paste0(
                         '<div style="position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); opacity: 0.05; z-index: 0; pointer-events: none;">',
                         '<img src="', logo2, '" style="height: 200px;">',
                         '</div>'
                       )),
                       tags$h4(tags$strong("Topics")),
                       tags$ul(
                         tags$li("Descriptive Statistics"),
                         tags$li("Probability Theory"),
                         tags$li("Distribution Functions"),
                         tags$li("Statistical Inference"),
                         tags$li("Linear Regression")
                       ),
                       
                       tags$h4(tags$strong("Data Input/Upload")),
                       p("Users can input their own datasets.  Input files supported are: .csv, .txt, .xls, .xlsx, .ods, and Google Sheets. Incomplete records are removed from the calculations."),
                       
                       tags$h4(tags$strong("Descriptive Statistics")),
                       tags$ul(
                         tags$li("Location Measures: Mean, Trimmed Mean, Median, Geometric Mean"),
                         tags$li("Dispersion Measures: Standard Deviation, Variance, IQR, MAD, Range"),
                         tags$li("Visualizations: Boxplot, Histograms, Density plots, Bar Graph, Stem-and-Leaf, and Scatterplot")
                       ),
                       
                       tags$h4(tags$strong("Probability Theory")),
                       tags$ul(
                         tags$li("Displaying Venn Diagram"),
                         tags$li("Conditional Probability with Bayes Tree"),
                         tags$li("Probability Distributions Functions"),
                       ),
                       
                       tags$h5(tags$strong("Discrete Distributions")),
                       tags$ul(
                         tags$li("Binomial Distribution"),
                         tags$li("Poisson Distribution"),
                         tags$li("Geometric Distribution"),
                         tags$li("Hypergeometric Distribution")
                       ),
                       
                       tags$h5(tags$strong("Continuous Distributions")),
                       tags$ul(
                         withMathJax(),
                         tags$li("Normal Distribution"),
                         tags$li("Chi-Squared Distribution"),
                         tags$li("Student's \\(t\\) Distribution"),
                         tags$li("Snedecor's \\(F\\) Distribution")
                       ),
                       
                       tags$h4(tags$strong("Statistical Inference")),
                       tags$ul(
                         withMathJax(),
                         tags$li("One-Sample Inference: \\(t\\)-test, Wilcoxon, Chi-squared test for variance, Proportion test"),
                         tags$li("Two-Sample Inference: Independent & Paired \\(t\\)-tests, Wilcoxon-Mann-Whitney, \\(F\\)-test, Proportion test"),
                         tags$li("Three or More Inference: ANOVA, Kruskal-Wallis, Multiple Comparison")
                       ),
                       
                       tags$h4(tags$strong("Linear Regression")),
                       p("Fit a linear regression models. The model assumes normally distributed residuals with constant variance:"),
                       uiOutput("regression_equation"),
                       p("Model summary, diagnostics of assumptions and outlier detection."),
                       
                       tags$h4(tags$strong("Author")),
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
