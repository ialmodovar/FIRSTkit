##*********************************************
##*
##* @file: linear_regression_module.R
##*
##* Linear regression for FIRSTkit
##* Perform diagnostic tools
##*
##* Author:
##* Israel Almodovar-Rivera PhD
##* Department of Mathematical Sciences
##* University of Puerto Rico at Mayaguez
##* israel.almodovar@upr.edu
##* Copyright June 2025
##*********************************************

linear_regression_ui <- tabPanel("Linear Regression",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("response_ui"),
                                     uiOutput("predictors_ui"),
                                     actionButton("run", "Submit")
                                   ),
                                   
                                   mainPanel(
                                     tabsetPanel(id = "tabs",  # <-- add id for conditionalPanel
                                                 tabPanel("Correlation Matrix",
                                                          withMathJax(),
                                                          sliderInput("corr_alpha", "Significance Level (\\( \\alpha \\))",
                                                                      value = 0.05, min = 0.001, max = 1, step = 0.001),
                                                          h4("Pearson Correlation Matrix"),
                                                          tableOutput("cor_matrix"),
                                                          br(),
                                                          uiOutput("corr_pvalue")
                                                 ),
                                                 
                                                 tabPanel("Regression Model Summary",
                                                          withMathJax(),
                                                          h4("Fitted Line Equation"),
                                                          uiOutput("model_eq"),
                                                          h4("Model Fit Statistics"),
                                                          tableOutput("fit_stats"), 
                                                          h4("Regression Model Summary Table"),
                                                          sliderInput("conf_level", "Significance Level (\\( \\alpha \\))",
                                                                      value = 0.05, min = 0, max = 1, step = 0.001),
                                                          tableOutput("model_table"),
                                                          h4("Residuals Summary Table"),
                                                          tableOutput("residual_summary")
                                                 ),
                                                 
                                                 tabPanel("ANOVA",
                                                          h4("Analysis of Variance (ANOVA) Table"),
                                                          withMathJax(),
                                                          uiOutput("anova_table")
                                                 ),
                                                 
                                                 tabPanel("Diagnostic Plots",
                                                          checkboxInput("typeofresiduals",label = "Use studentized residuals",value = TRUE),
                                                          #h4("Studentized Residuals vs Fitted Values"),
                                                          plotlyOutput("resid_plot"),
                                                          h4("Quantile-Quantile Plot"),
                                                          sliderInput("qq_alpha", "Significance Level (\\( \\alpha \\))",
                                                                      value = 0.05, min = 0, max = 1, step = 0.001),
                                                          plotlyOutput("qq_plot"),
                                                          plotlyOutput("residual_hist")
                                                 ),
                                                 
                                                 tabPanel("Cook's Distance",
                                                          h4("Cook's Distance Plot"),
                                                          conditionalPanel(
                                                            condition = "input.tabs == 'Cook\\'s Distance'",  # <-- properly escaped
                                                            numericInput("cook_thresh", "Cook's Distance Threshold",
                                                                         value = 0.04, min = 0, max = 1, step = 0.001)
                                                          ),
                                                          plotlyOutput("cook_plot"),
                                                          h4("Flagged Observations"),
                                                          tableOutput("cook_flagged")
                                                 ),
                                                 tabPanel("R code",
                                                          verbatimTextOutput("regression_code"))
                                     )
                                   )
                                 )
)


linear_regression_server <- function(input, output, session,firstkit.data) {
  
  output$response_ui <- renderUI({
    req(firstkit.data())
    selectInput("response", "Select Response Variable", 
                choices = names(firstkit.data()), selected = NULL)
  })
  
  output$predictors_ui <- renderUI({
    req(firstkit.data(), input$response)
    choices <- setdiff(names(firstkit.data()), input$response)
    selectInput("predictors", "Select Predictor Variables", 
                choices = choices, selected = NULL, multiple = TRUE)
  })
  
  cor.pmat <- function(mat) {
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    colnames(p.mat) <- colnames(mat)
    rownames(p.mat) <- colnames(mat)
    
    for (i in 1:n) {
      for (j in 1:n) {
        if (i == j) {
          p.mat[i, j] <- 0
        } else {
          test <- cor.test(mat[[i]], mat[[j]], method = "pearson")
          p.mat[i, j] <- test$p.value
        }
      }
    }
    p.mat
  }
  
  
  output$cor_matrix <- renderTable({
    req(firstkit.data(), input$response, input$predictors, input$corr_alpha)
    df <- firstkit.data()
    alpha <- input$corr_alpha
    
    selected_vars <- c(input$response, input$predictors)
    df_subset <- df[, selected_vars, drop = FALSE]
    num_df <- df_subset[sapply(df_subset, is.numeric)]
    
    if (ncol(num_df) < 2) {
      return(data.frame(Message = "Need at least 2 numerical variables to calculate the correlation."))
    }
    
    cor.mat <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")
    p_mat <- cor.pmat(num_df)
    
    formatted_mat <- matrix("", nrow = nrow(cor.mat), ncol = ncol(cor.mat),
                            dimnames = dimnames(cor.mat))
    
    for (i in 1:nrow(cor.mat)) {
      for (j in 1:ncol(cor.mat)) {
        rval <- round(cor.mat[i, j], 4)
        pval <- p_mat[i, j]
        sig <- if (!is.na(pval) && i != j && pval < alpha) " *" else ""
        formatted_mat[i, j] <- paste0(rval, sig)
      }
    }
    
    as.data.frame(formatted_mat)
  },   rownames = TRUE,digits = 3)
  
  output$corr_pvalue <- renderUI({
    withMathJax( HTML(paste0("* indicates statistically significant correlation \\(p \\)-value <", input$corr_alpha)))
  })
  
  
  model.fit <- eventReactive(input$run, {
    df <- firstkit.data()
    y <- input$response
    x <- input$predictors
    req(y, x)
    formula <- as.formula(paste(y, "~", paste(x, collapse = "+")))
    lm(formula, data = df)
  })
  
  output$model_summary <- renderPrint({
    req(model.fit())
    summary(model.fit())
  })
  
  output$fit_stats <- renderTable({
    req(model.fit())
    model <- model.fit()
    s <- summary(model)
    
    rsq <- round(s$r.squared, 5)
    adj_rsq <- round(s$adj.r.squared, 5)
    mse <- round(deviance(model)/df.residual(model), 5)
    
    df <- data.frame(
      Statistic = c("R-squared", "Adjusted R-squared", "AIC","BIC","Mean Sq"),
      Value = c(rsq, adj_rsq,AIC(model),BIC(model), mse),check.names = FALSE)
    
    colnames(df)[2] <- ""
    
    df
    
  },digits=4)
  
  
  output$model_table <- renderTable({
    req(model.fit(), input$conf_level)
    model <- model.fit()
    
    conf_level <- input$conf_level 
    alpha <- conf_level
    coefs <- summary(model)$coefficients
    ci <- confint(model, level = conf_level)
    
    pvals <- coefs[, 4]
    p2 <- ifelse(pvals < 0.001,"< 0.001",signif(pvals, 4))
    
    sig_marker <- ifelse(pvals < alpha, " *", "")
    p2 <- paste0(p2, sig_marker)
    
    ci_label <- paste0((1-input$conf_level)*100, "% CI")
    
    df <- data.frame( Estimate = round(coefs[, 1], 5), `Std. Error` = round(coefs[, 2], 5),check.names = FALSE)
    df[[ci_label]] <- paste0("(", round(ci[, 1], 4), ", ", round(ci[, 2], 4), ")")
    df[["p-value"]] <- p2

    df <- df[, c("Estimate", "Std. Error", ci_label, "p-value")]
    
    df
  }, rownames = TRUE,digits=4)
  
  output$residual_summary <- renderTable({
    req(model.fit())
    model <- model.fit()
    
    df1 <- as.data.frame(rbind(summary(model$residuals),summary(studres(model))))
    
    rownames(df1) <- c("Residuals","Studentized Residuals")
    df1
  },rownames=TRUE,digits=4)
  
  output$model_eq <- renderUI({
    req(model.fit())
    model <- model.fit()
    coefs <- coef(model)
    
    intercept <- round(coefs[1], 5)
    terms <- names(coefs)[-1]
    betas <- round(coefs[-1], 5)
    
    term.parts <- mapply(function(beta, var) {
      sign.str <- if (beta < 0) " - " else " + "
      paste0(sign.str, abs(beta), " \\cdot ", var)
    }, beta = betas, var = terms, SIMPLIFY = TRUE)
    
    eq.rhs <- paste0(term.parts, collapse = "")
    equation <- paste0("\\(\\hat{", input$response, "} = ", intercept, eq.rhs, "\\)")
    
    withMathJax(HTML(equation))
  })
  
 
  output$anova_table <- renderUI({
      req(model.fit(), input$conf_level)
      model <- model.fit()

    tbl <- anova(model)
    
    tbl$`Pr(>F)` <- ifelse(tbl$`Pr(>F)` < 0.001, "<0.001", round(tbl$`Pr(>F)`, 3))
    
    colnames(tbl)[4] <- "\\( F \\)-value"
    colnames(tbl)[5] <- "\\(p \\)-value"
    tbl[,1:4] <- round(tbl[,1:4],digits=3)

    HTML(
      htmlTable(
        tbl,
        rownames = TRUE,escape = FALSE,
        align = "lccccc",
        css.cell = "padding: 5px 10px;"
      )
    )
  })
    
  output$resid_plot <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    if(input$typeofresiduals){
      rr <- studres(model)
    } else{
      rr <- model$residuals
    }
    df <- data.frame(Fitted = fitted(model), Residuals = rr)
    
    p <- ggplot(df, aes(x = Fitted, y = Residuals)) +
      geom_point(color = "black", alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = ifelse(input$typeofresiduals,"Studentized Residuals vs Fitted Values","Residuals vs Fitted Values"),
           x = "Fitted Values", y = ifelse(input$typeofresiduals,"Studentized Residuals","Residuals")) +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$qq_plot <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    
    # Choose residual type
    if (input$typeofresiduals) {
      res <- studres(model)
    } else {
      res <- residuals(model)
    }
    
    n <- length(res)
    validate(need(n > 0, "No residuals available for the quantile-quantile plot."))
    
    sres <- sort((res - mean(res, na.rm = TRUE)) / sd(res, na.rm = TRUE))
    tq <- qnorm(ppoints(n))
    
    alpha <- input$qq_alpha
    se <- (1/dnorm(tq)) * sqrt(ppoints(n) * (1 - ppoints(n))/n)
    zcrit <- qnorm(1 - alpha/2)
    upper <- tq + zcrit * se
    lower <- tq - zcrit * se
    
    df <- data.frame(Theoretical = tq,Observed = sres,Upper = upper,Lower = lower,Label = "")
    
    outlier_idx <- order(abs(sres - tq), decreasing = TRUE)[1:3]
    df$Label[outlier_idx] <- outlier_idx
    
    p <- ggplot(df, aes(x = Theoretical, y = Observed)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray40", alpha = 0.5) +
      geom_point(color = "black") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      geom_text(aes(label = Label), vjust = -1, color = "darkred") +
      labs(
        title = "",
        x = "Theoretical Quantiles",
        y = ifelse(input$typeofresiduals, "Studentized Residuals", "Residuals")
      ) +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$residual_hist <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    
    #res <- studres(model)
    if(input$typeofresiduals){
      res <- studres(model)
    } else{
      res <- model$residuals
    }
    
    n <- length(res)
    validate(need(n > 0, "No residuals available for histogram."))
    
    df <- data.frame(Residual = res)
    
    p <- ggplot(df, aes(x = Residual)) +
      geom_histogram(aes(y = after_stat(density)),
                     fill = "white", color = "purple", linewidth = 1,
                     alpha = 0.6) +
      geom_density(color = "black", linewidth = 1.2) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs( title = "", x = ifelse(input$typeofresiduals,"Studentized Residuals","Residuals"), y = "Density") + theme_bw()
    
    ggplotly(p)
  })
  
  
  output$cook_plot <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    cooks <- cooks.distance(model)
    threshold <- input$cook_thresh %||% (4/length(cooks))
    
    df <- data.frame(Observation = seq_along(cooks),Cook = cooks,Flagged = cooks > threshold)
    
    p <- ggplot(df, aes(x = Observation, y = Cook, color = Flagged)) +
      geom_point(size = 2) +
      geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("FALSE" = "lightblue", "TRUE" = "darkorange")) +
      labs(title = "Cook's Distance",x = "Observation", y = "Cook's Distance",
           color = paste(">", round(threshold, 3))) + theme_bw()
    
    ggplotly(p)
  })
  
  output$cook_flagged <- renderTable({
    req(model.fit())
    model <- model.fit()
    cooks <- cooks.distance(model)
    threshold <- input$cook_thresh %||% (4/length(cooks))
    
    flagged <- which(cooks > threshold)
    if (length(flagged) == 0) return(data.frame(Message = "No observations flagged."))
    
    data.frame( Observation = flagged,`Cook's Distance` = round(cooks[flagged], 5))
  },digits=4)
  
  output$regression_code <- renderText({
    code_lines <- c("library(\"car\")\nattach(mydata)\n## Correlation Matrix")
    df <- firstkit.data()
    selected.vars <- c(input$response, input$predictors)
    df.subset <- df[, selected.vars, drop = FALSE]
    num.df <- df.subset[sapply(df.subset, is.numeric)]
    
    code_lines <- c(
      code_lines,
      paste0(
        "mydata.red <- mydata[, c(",
        paste0("\"", names(num.df), "\"", collapse = ", "),
        ")]"),
        "cor(mydata.red,use=\"pairwise.complete.obs\",method=\"pearson\")",
      "## Linear Regression model",
      paste0("m1 <- lm(",paste(input$response, "~", paste(input$predictors, collapse = " + ")),",data=mydata.red)"),
      "summary(m1) ## summary",
      "AIC(m1) ## Akaike's Information Criterion",
      "BIC(m1) ## Bayesian Information Criterion",
      "anova(m1) ## ANOVA Table",
      "summary(m1$residuals) ## summary residuals",
      "summary(studres(m1)) ## summary of studentized residuals using car package",
      ifelse(input$typeofresiduals,"res <- studres(m1)","res <- m1$residuals"),
      "cooks.distance(m1) ## Cook's distance"
    )
    
    # cor.mat <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")
    # p_mat <- cor.pmat(num_df)
    
    
    if (length(code_lines) == 0) {
      "No valid null hypotheses provided."
    } else {
      paste(code_lines, collapse = "\n")
    }
  })
  
}
