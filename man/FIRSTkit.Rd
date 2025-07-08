\name{FIRSTkit}
\alias{FIRSTkit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FIRST Impressions R-based Statistics Toolkit (FIRSTkit)
}
\description{
FIRSTkit is a Shiny App that is created to be a free companion to the teaching of introductory courses without any previous programming knowledge
}
\usage{
FIRSTkit()
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\strong{Modules}
 
  \describe{
    \item{\strong{Descriptive Statistics}}{
      \itemize{
        \item Location Summaries:
          \itemize{
            \item Sample Mean
            \item Sample Median
            \item Sample Geometric Mean
            \item Sample Trimmed Mean
          }
        \item Dispersion Summaries:
          \itemize{
            \item Sample Standard Deviation
            \item Sample Variance
            \item Sample Interquartile Range
            \item Median Absolute Deviation
            \item Sample Range
          }
        \item Univariate Data Visualization:
          \itemize{
            \item Boxplots
            \item Histograms
            \item Density Plots
            \item Bar Graphs
          }
      }
    }
 
    \item{\strong{Introduction to Probability Theory and Distribution Functions}}{
      \itemize{
        \item Set Theory
        \item Bayes Probability Tree
        \item Distribution Functions:
          \itemize{
            \item Binomial Distribution
            \item Poisson Distribution
            \item Hypergeometric Distribution
            \item Chi-Squared Distribution
            \item Normal Distribution
            \item Student's \eqn{t}-distribution
            \item Snedecor's \eqn{F}-distribution
          }
      }
    }
 
    \item{\strong{Inference Statistics}}{
      \itemize{
        \item One-Sample Inference:
          \itemize{
            \item One-Sample \eqn{t}-test for a population mean
            \item Wilcoxon signed-rank test for a location parameter
            \item One-Sample \eqn{\chi^2} test for the population variance
            \item One-Sample proportion test
          }
        \item Two-Sample Inference:
          \itemize{
            \item Two-Sample \eqn{t}-test to compare two population means (independent and dependent)
            \item Wilcoxon-Mann-Whitney test to compare two location parameters
            \item Two-Sample \eqn{F}-test to compare two population variances
            \item Ansari-Bradley test for comparing two scale parameters
            \item Two-Sample proportion test
          }
        \item Three-Sample or More Inference:
          \itemize{
            \item One-Way Analysis of Variance (ANOVA)
            \item Kruskal-Wallis rank sum test
            \item Bartlett's test for homogeneity of variances
            \item Fligner-Killeen test for homogeneity of variances
          }
      }
    }
 
    \item{\strong{Linear Regression}}{
      \itemize{
        \item Simple Linear Regression
        \item Model Diagnostics
        \item Studentized Residuals
        \item Quantile-Quantile Plot
        \item Cook's Distances
      }
    }
  }
}

\references{
Almodovar-Rivera and Maitra
}
\author{
Israel A. Almodovar-Rivera, PhD
}

\examples{
## Run FIRSTkit()
\dontrun{
FIRSTkit::FIRSTkit()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
