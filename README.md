# First Impressions R-based Statistics Toolkit (FIRSTkit)

## Introduction 

The importance of statistics in all disciplines is well-established and so is the need for teaching statistics introductory statistics as a class in all disciplines. An essential aspect of teaching statistics is the ability to do it practically, and therfore, through the use of software, that should therefore be included in instructional activities. On of the most popular statistical software is [**R**](https://www.r-project.org/). However, **R** can be challenging specially for those individuals who do not have any experience or the time to or desire to learn a programming language. Other software bring up costs and installation issues. [**FIRSTkit**](https://github.com/ialmodovar/FIRSTkit) was created with all this as the background. **FIRSTkit** is a shiny app that is created to be a free companion to introductory courses with no requirement of programming knowledge or experience. The software can be run on institutional servers so that there is no need for installation woes on the part of the students. 

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

