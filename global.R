library("dplyr")
library("shiny")
library("knitr")
library("DT")
library("ggplot2")
library("htmltools")
library("htmlwidgets")
library("plotly")
library("gridExtra")
library("pander")
library("readxl")
library("readODS")
library("DiagrammeR") 
library("shinythemes")

##library("googlesheets")

encoding <- getOption("shiny.site.encoding", default = "UTF-8")

## options for knitting/rendering rmarkdown chunks
knitr::opts_chunk$set(
  echo = FALSE,
  comment = NA,
  cache = FALSE,
  message = FALSE,
  warning = FALSE
)

## function to render .md files to html
inclMD <- function(path) {
  markdown::markdownToHTML(
    path,
    fragment.only = TRUE,
    options = "",
    stylesheet = "",
    encoding = encoding
  )
}

## function to render .Rmd files to html - does not embed image or add css
inclRmd <- function(path, r_env = parent.frame()) {
  paste(
    readLines(path, warn = FALSE, encoding = encoding),
    collapse = '\n'
  ) %>%
  knitr::knit2html(
    text = .,
    fragment.only = TRUE,
    envir = r_env,
    options = "",
    stylesheet = "",
    encoding = encoding
  ) %>%
  gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
  gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
  HTML
}

## make html table
make_table <- function(dat, width = "50%") {
  knitr::kable(
    dat,
    align = "c",
    format = "html",
    table.attr = paste0("class='table table-condensed table-hover' style='width:", width, ";'")
  )
}


extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}
geo.mean <- function(x,na.rm=TRUE){
  x <- x[!is.na(x)]
  if(any(x < 0)){
    stop("geometric mean defined for positive values only\n")
  } else{
    exp(mean(log(x),na.rm=na.rm))
  }
}
#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)

