##***************************************
##
## @file: inference-one-sample.R
##
## Perform one sample inference for the location parameter
## FIRSTkit allow for one-sample t-test 
## and the Wilcoxon signed-rank test
## 
## Author:
## Israel A. Almodovar-Rivera, PhD
## israel.almodovar@upr.edu
## February 2021
##**************************************

all.t.one.sample.test <- function(x, mu0 = 0, alpha = 0.05,...){
  twosided <- t.test(x = x, mu = mu0, 
                     alternative = "two.sided",
                     conf.level = 1-alpha)
  oneless <- t.test(x = x, mu = mu0, 
                    alternative = "less",
                    conf.level = 1-alpha)
  onegreater <- t.test(x = x, mu = mu0, 
                       alternative = "greater",
                       conf.level = 1-alpha)
  
tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 3) 

ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),",",
        round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
        paste("(",paste(round(oneless$conf.int[1],digits=3),",",
                        round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
        paste("(",paste(round(onegreater$conf.int[1],digits=3),",",
                        round(onegreater$conf.int[2],digits=3),sep=""),")",sep=""))

res <- data.frame(Test = tt,Pval = pval, CI =ci)
names(res) <- c("statistic","p-value",paste(as.character((1-alpha)*100),"% CI",sep=""))

rownames(res) <- c("H_1: mu = mu_0","H_1: mu < mu_0", "H_1: mu > mu_0")
res
}



##

all.wilcoxon.one.sample.test <- function(x, mu0 = 0, alpha = 0.05,...){
  twosided <- wilcox.test(x = x, mu = mu0, 
                     alternative = "two.sided",
                     conf.level = 1-alpha,conf.int = TRUE)
  oneless <- wilcox.test(x = x, mu = mu0, 
                         alternative = "less",
                         conf.level = 1-alpha,conf.int = TRUE)
  onegreater <- wilcox.test(x = x, mu = mu0, 
                            alternative = "greater",
                            conf.level = 1-alpha,conf.int = TRUE)
  
  tt <- round(c(twosided$statistic, oneless$statistic, onegreater$statistic),digits=3)
  pval <- round(c(twosided$p.value, oneless$p.value, onegreater$p.value),digits = 3) 
  
  ci <- c(paste("(",paste(round(twosided$conf.int[1],digits=3),",",
                          round(twosided$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(oneless$conf.int[1],digits=3),",",
                          round(oneless$conf.int[2],digits=3),sep=""),")",sep=""),
          paste("(",paste(round(onegreater$conf.int[1],digits=3),",",
                          round(onegreater$conf.int[2],digits=3),sep=""),")",sep=""))
  
  res <- data.frame(Test = tt,Pval = pval, CI =ci)
  names(res) <- c("statistic","p-value",paste(as.character((1-alpha)*100),"% CI",sep=""))
  
  rownames(res) <- c("H_1: mu = mu_0","H_1: mu < mu_0", "H_1: mu > mu_0")
  res
}

