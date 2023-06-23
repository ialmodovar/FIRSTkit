# FIRST Impressions R-based Statistics Toolkit (FIRSTkit)

## Introduction 

We have all seen the increasing importance of statistical literacy in almost all disciplines. Consequently, there is tremendous need for teaching statistics as a introductory core course. Modern statistics is best learn and done with the aid of computational devices, so an essential part of any statistics course is the use of  statistical software. On of the most popular statistical software is [**R**](https://www.r-project.org/). However, **R** is challenging specially for those individuals who do not have experience or just do not have the time or need to learn a programming language. [**FIRSTkit**](https://github.com/ialmodovar/FIRSTkit) was created with this in mind. Under the hood, **FIRSTkit** is a Shiny App that is created to be a free companion to the teaching of introductory courses without  any previous programming knowledge. An added benefit is that students and users in the applied disciplines can use the software later as needed.

## Modules

### **Descriptive statistics**

  + Location summaries 
    + Sample Mean 
    + Sample Trimmed mean
    + Sample Median 
    + Sample Geometric Mean
  + Dispersion summaries 
    + Sample Standard Deviation
    + Minimum
    + Maximum
    + Variance 
    + Interquartile range
    + Median Absolute Deviation
  + Univariate Data Visualization 
    + Box-plots 
    + Histograms
    + Density plots
    + Bar graphs

### **Introduction to Probability Theory and Distribution Functions**

  + Probability events
  + Bayes Probability Tree
  + Distributions Functions
    + Binomial Distribution
    + Poisson Distribution
    + Chi-Squared Distribution
    + Normal Distribution
    + Student's $t$-distribution

### **Inference statistics**

  + One-Sample Inference 
    + One-Sample $t$-test for a population mean
    + Wilcoxon signed-rank test a location parameter
    + One-Sample $\chi^2$ Test for the Population Variance 
    + One-Sample proportion test
  + Two-Sample Inference
    + Two-Sample $t$-test to compare two population means (dependent and independent samples)
    + Wilcoxon-Mann-Whitney test to compare two location parameters 
    + Two-Sample $F$-test to compare two population variances
    + Two-Sample proportion test
  + Three-Sample or more Inference
    + One-Way Analysis of Variance (ANOVA)
    + Kruskal-Wallis Rank sum test
    + Bartlett Test for Homogeneity of Variance

### **Linear Regression**

  + Simple and multiple linear regression
  + Model Diagnostic
  
## Local Installation

**FIRSTkit** is designed to be run from the browser and hosted on an institutional Shiny server so that students and users do not have any need form of installation. 

However, the software can also be installed and run locally.

  + *Using RStudio:*
    + Download the file, uncompressed and navigate to the directory.
    + Open the file ui.R.
    + Click on "Run App" 
    
  + *Using base R:*
    + Download the file, uncompressed and navigate to the directory.
    + Note that the R package **shiny** and **googlesheets4** is needed from CRAN.
    + Then run **shiny::runApp()**

If you have any questions e-mail *israel.almodovar@upr.edu* or *maitra@iastate.edu* or open an "issue" above (preferred).


