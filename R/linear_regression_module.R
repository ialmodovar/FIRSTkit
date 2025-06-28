
linear_regression_ui <- tabPanel("Linear Regression",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("response_ui"),
                                     uiOutput("predictors_ui"),
                                     actionButton("run", "Run Analysis")
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
                                                          p("* indicates statistically significant correlation (p-value < \\( \\alpha \\))",
                                                            style = "color:gray; font-style:italic;")
                                                 ),
                                                 
                                                 tabPanel("Model Summary",
                                                          withMathJax(),
                                                          h4("Fitted Line Equation"),
                                                          uiOutput("model_eq"),
                                                          h4("Model Fit Statistics"),
                                                          tableOutput("fit_stats"), 
                                                          h4("Model Summary Table"),
                                                          sliderInput("conf_level", "Significance Level (\\( \\alpha \\))",
                                                                      value = 0.05, min = 0, max = 1, step = 0.001),
                                                          tableOutput("model_table"),
                                                          h4("Residuals Summary Table"),
                                                          tableOutput("residual_summary")
                                                 ),
                                                 
                                                 tabPanel("ANOVA",
                                                          h4("Analysis of Variance (ANOVA) Table"),
                                                          withMathJax(),
                                                          tableOutput("anova_table")
                                                 ),
                                                 
                                                 tabPanel("Diagnostic Plots",
                                                          h4("Studentized Residuals vs Fitted"),
                                                          plotlyOutput("resid_plot"),
                                                          h4("QQ Plot"),
                                                          sliderInput("qq_alpha", "Significance Level (\\( \\alpha \\))",
                                                                      value = 0.05, min = 0, max = 1, step = 0.001),
                                                          plotlyOutput("qq_plot"),
                                                          h4("Histogram of the Studentized Residuals"),
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
                                                 )
                                     )
                                   )
                                 )
)


linear_regression_server <- function(input, output, session,firstkit.data) {
  
  observe({
    print("Data loaded:")
    print(str(firstkit.data()))
  })
  
  output$response_ui <- renderUI({
    req(firstkit.data())
    selectInput("response", "Select Response Variable", 
                choices = names(firstkit.data()), selected = NULL)
  })
  
  # UI for predictors
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
      return(data.frame(Message = "Need at least 2 numeric variables for correlation."))
    }
    
    cor.mat <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")
    p_mat <- cor.pmat(num_df)
    
    # Apply asterisk for p-values < alpha
    formatted_mat <- matrix("", nrow = nrow(cor.mat), ncol = ncol(cor.mat),
                            dimnames = dimnames(cor.mat))
    
    for (i in 1:nrow(cor.mat)) {
      for (j in 1:ncol(cor.mat)) {
        r_val <- round(cor.mat[i, j], 4)
        p_val <- p_mat[i, j]
        sig <- if (!is.na(p_val) && i != j && p_val < alpha) " *" else ""
        formatted_mat[i, j] <- paste0(r_val, sig)
      }
    }
    
    as.data.frame(formatted_mat)
  },   rownames = TRUE,digits = 3)
  
  # Fit model
  model.fit <- eventReactive(input$run, {
    df <- firstkit.data()
    y <- input$response
    x <- input$predictors
    req(y, x)
    formula <- as.formula(paste(y, "~", paste(x, collapse = "+")))
    lm(formula, data = df)
  })
  
  # Model summary
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
    mse <- round(deviance(model) / df.residual(model), 5)
    
    data.frame(
      Statistic = c("R-squared", "Adjusted R-squared", "AIC","BIC","$\\sigma^2$"),
      Value = c(rsq, adj_rsq,AIC(model),BIC(model), mse),
      check.names = FALSE)
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
    
    df <- data.frame(
      Estimate = round(coefs[, 1], 5),
      `Std. Error` = round(coefs[, 2], 5),
      check.names = FALSE
    )
    df[[ci_label]] <- paste0("(", round(ci[, 1], 4), ", ", round(ci[, 2], 4), ")")
    df[["p-value"]] <- p2
        # Reorder columns
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
    
    # Build terms with proper signs
    term_parts <- mapply(function(beta, var) {
      sign_str <- if (beta < 0) " - " else " + "
      paste0(sign_str, abs(beta), " \\cdot ", var)
    }, beta = betas, var = terms, SIMPLIFY = TRUE)
    
    eq_rhs <- paste0(term_parts, collapse = "")
    equation <- paste0("$$\\hat{", input$response, "} = ", intercept, eq_rhs, "$$")
    
    withMathJax(HTML(equation))
  })
  
  output$anova_table <- renderTable({
    req(model.fit(), input$conf_level)
    model <- model.fit()
    alpha <- 1 - (input$conf_level / 100)
    
    anova_tbl <- anova(model)
    anova_tbl <- data.frame(anova_tbl)
    
    # Rename F-value and p-value columns
    if ("F.value" %in% names(anova_tbl)) {
      names(anova_tbl)[names(anova_tbl) == "F.value"] <- "F-statistic"
    }
    if ("Pr..F." %in% names(anova_tbl)) {
      names(anova_tbl)[names(anova_tbl) == "Pr..F."] <- "$p$-value"
    }
    
    anova_tbl$Df <- as.integer(anova_tbl$Df)
    
    if ("Df" %in% names(anova_tbl)) {
      names(anova_tbl)[names(anova_tbl) == "Df"] <- "df"
    }
    
    for (col in names(anova_tbl)) {
      if (col != "df" && is.numeric(anova_tbl[[col]])) {
        if (col == "$p$-value") {
          anova_tbl[[col]] <- ifelse(
            anova_tbl[[col]] < 0.001,
            "< 0.001",
            signif(anova_tbl[[col]], 4)
          )
          # Append asterisk if p < alpha
          numeric_p <- suppressWarnings(as.numeric(anova_tbl[[col]]))
          sig_marker <- ifelse(!is.na(numeric_p) & numeric_p < alpha, " *", "")
          anova_tbl[[col]] <- paste0(anova_tbl[[col]], sig_marker)
          # Ensure "< 0.001" gets asterisk too
          anova_tbl[[col]] <- ifelse(
            grepl("< 0.001", anova_tbl[[col]]) & (0.001 < alpha),
            "< 0.001 *",
            anova_tbl[[col]]
          )
        } else {
          anova_tbl[[col]] <- round(anova_tbl[[col]], 4)
        }
      }
    }
    
    anova_tbl
  }, rownames = TRUE,digits=3,na="")
  
  
  # Residuals vs Fitted
  output$resid_plot <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    df <- data.frame(Fitted = fitted(model), Residuals = studres(model))
    
    p <- ggplot(df, aes(x = Fitted, y = Residuals)) +
      geom_point(color = "black", alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Studentized Residuals vs Fitted Values",
           x = "Fitted Values", y = "Studentized Residuals") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$qq_plot <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    
    # Studentized residuals
    res <- studres(model)
    n <- length(res)
    validate(need(n > 0, "No residuals available for QQ plot."))
    
    # Sort residuals
    sorted_res <- sort(res)
    
    # Theoretical quantiles (standard normal)
    theoretical_q <- qnorm(ppoints(n))
    
    # Confidence bands (based on standard errors of order statistics)
    alpha <- input$qq_alpha
    se <- (1 / dnorm(theoretical_q)) * sqrt(ppoints(n) * (1 - ppoints(n)) / n)
    z_crit <- qnorm(1 - alpha / 2)
    upper <- theoretical_q + z_crit * se
    lower <- theoretical_q - z_crit * se
    
    # Create data frame
    df <- data.frame(
      Theoretical = theoretical_q,
      Observed = sorted_res,
      Upper = upper,
      Lower = lower,
      Label = ""
    )
    
    # Identify top 3 outliers as in car::qqPlot
    outlier_idx <- order(abs(res - theoretical_q), decreasing = TRUE)[1:3]
    df$Label[outlier_idx] <- outlier_idx
    
    # Plot
    p <- ggplot(df, aes(x = Theoretical, y = Observed)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray90", alpha = 0.5) +
      geom_point(color = "black") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      geom_text(aes(label = Label), vjust = -1, color = "darkred") +
      labs(
        title = "",
        x = "Theoretical Quantiles", y = "Studentized Residuals"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$residual_hist <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    
    res <- studres(model)
    n <- length(res)
    validate(need(n > 0, "No residuals available for histogram."))
    
    df <- data.frame(Residual = res)
    
    p <- ggplot(df, aes(x = Residual)) +
      # Histogram with density scale
      geom_histogram(aes(y = after_stat(density)),
                     fill = "white", color = "purple", linewidth = 1,
                     alpha = 0.6) +
      # Add empirical density curve
      geom_density(color = "black", linewidth = 1.2) +
      # Vertical line at zero
      geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(
        title = "",
        x = "Studentized Residual", y = "Density"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  # Cook's Distance plot
  output$cook_plot <- renderPlotly({
    req(model.fit())
    model <- model.fit()
    cooks <- cooks.distance(model)
    threshold <- input$cook_thresh %||% (4 / length(cooks))
    
    df <- data.frame(
      Observation = seq_along(cooks),
      Cook = cooks,
      Flagged = cooks > threshold
    )
    
    p <- ggplot(df, aes(x = Observation, y = Cook, color = Flagged)) +
      geom_point(size = 2) +
      geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("FALSE" = "lightblue", "TRUE" = "darkorange")) +
      labs(title = "Cook's Distance",
           x = "Observation", y = "Cook's Distance",
           color = paste(">", round(threshold, 3))) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$cook_flagged <- renderTable({
    req(model.fit())
    model <- model.fit()
    cooks <- cooks.distance(model)
    threshold <- input$cook_thresh %||% (4 / length(cooks))
    
    flagged <- which(cooks > threshold)
    if (length(flagged) == 0) return(data.frame(Message = "No observations flagged."))
    
    data.frame(
      Observation = flagged,
      `Cook's Distance` = round(cooks[flagged], 5)
    )
  },digits=4)
  
}
