##*********************************************
##*
##* @file: zzz.R
##*
##* options set up for FIRSTkit
##*
##* Author:
##* Israel Almodovar-Rivera PhD
##* Department of Mathematical Sciences
##* University of Puerto Rico at Mayaguez
##* israel.almodovar@upr.edu
##* Copyright June 2025
##*********************************************


.onLoad <- function(libname, pkgname) {
  options(shiny.maxRequestSize = 60 * 1024^2)  # Set max request size to 50 MB
}