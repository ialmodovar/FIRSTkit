# FIRST Impressions R-based Statistics Toolkit (FIRSTkit)

## Introduction 

We have all seen the increasing importance of statistical literacy in almost all disciplines. Consequently, there is tremendous need for teaching statistics as a introductory core course. Modern statistics is best learnt and done with the aid of computational devices, so an essential part of any statistics course is the use of  statistical software. On of the most popular statistical software is [**R**](https://www.r-project.org/). However, **R** is challenging specially for individuals who do not have experience or just do not have the time or need to learn a programming language. [**FIRSTkit**](https://github.com/ialmodovar/FIRSTkit) was created with this in mind. Under the hood, **FIRSTkit** is a Shiny App that is created to be a free companion to the teaching of introductory courses without  any previous programming knowledge. An added benefit is that students and users in the applied disciplines can use the software later as needed.

## Local Installation
**FIRSTkit** is designed to be run from the browser and hosted on an institutional Shiny server so that students and users do not have any need for installation. 
However,the software can also be installed and run locally.

  + *Using RStudio:*
    + Download the file, uncompress and navigate to the directory.
    + Open the file ui.R.
    + Click on "Run App" 
  + *Using base R:*
    + Download the file, uncompress and navigate to the directory.
    + Note that the R package **shiny** and **googlesheets** is needed from CRAN.
    + Then run **shiny::runApp()**

If you have any questions e-mail *israel.almodovar@upr.edu* or *maitra@iastate.edu* or open an "issue" above (preferred).


