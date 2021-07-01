# First-timers Introductory R-based Statistics Toolkit (FIRSTkit)

## Introduction 

We all have seen the importance of statistics in sciences. Therefore, there is a tremendous need of teaching statistics as a introductory core course. A essential part in any statistics course is the use of a statistical software. On of the most popular statistical software is [**R**](https://www.r-project.org/). However, **R** can be challenging specially for those individuals who do not have any experience or just do not have the time to learn a programming language. [**FIRSTkit**](https://github.com/ialmodovar/FIRSTkit) was created with this in mind. **FIRSTkit** is Shiny App that is created to be free companion to introductory courses without required any previous programming knowledge. 

## FIRSTkit capabilities

*FIRSTkit* can do the following topics,

* **Descriptive statistics**

  + Location summary 
    + Mean 
    + Trimmed mean
    + Median 
    + Geometric Mean
  + Dispersion summary 
    + Standard Deviation
    + Minimum
    + Maximum
    + Variance 
    + Inter-quartile range
    + MAD
  + Univariate graphical display 
    + box-plot 
    + histograms
    + density plots
    + bar graphs

* **Inference statistics**

  + One-Sample Inference 
    + One-Sample *t* test for a mean
    + Wilcoxon signed-rank test a location
    + One-Sample $\chi^2$ Test for the Variance 
  + Two-Sample Inference
    + Two-Sample *t* test to Compare Two Means
    + Wilcoxon-Mann-Whitney to test Compare Two Location 
    + Two-Sample *F* Test to Compare Two Variances
  + Three-Sample or more Inference
    + One-Way Analysis of Variance (ANOVA)
    + Kruskal-Wallis Rank Sum test
    + Bartlett Test for Homogeneity of Variance

* **Linear Regression**

  + Simple linear regression
  + Multiple linear regression
  
## Local Installation
**FIRSTkit** is designed to be run from the browser and hosted on an institutional Shiny server so that students and users do not have any need for installation. 
However,the software can also be installed and run locally.

  + *Using RStudio:*
    + Download the file, uncompress and navigate to the directory.
    + Open the file ui.R.
    + Click on "Run App" 
  + *Using base R:*
    + Download the file, uncompress and navigate to the directory.
    + Note that the R package **shiny** is needed from CRAN.
    + Then run **shiny::runApp()**

If you have any questions e-mail *israel.almodovar@upr.edu* or *maitra@iastate.edu* or open an "issue" above (preferred).


