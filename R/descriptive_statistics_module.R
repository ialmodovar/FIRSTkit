##*********************************************
##*
##* @file: descriptive_statistics_module.R
##*
##* Descriptive Statistics UI an Server module
##* for FIRSTkit
##*
##*
##* Author:
##* Israel Almodovar-Rivera PhD
##* Department of Mathematical Sciences
##* University of Puerto Rico at Mayaguez
##* israel.almodovar@upr.edu
##* Copyright June 2025
##*********************************************

## geometric mean
geo.mean <- function(x, na.rm = TRUE) { 
  exp(mean(log(x[x > 0]), na.rm = na.rm))
}

descriptive_stats_ui <- tabPanel("Descriptive Statistics",
                          sidebarLayout(
                            sidebarPanel(
                              withMathJax(),
                              uiOutput("var_select_ui"),
                              checkboxGroupInput("location_stats", "Location Statistics:",
                                                 choices = c("Sample mean (\\(\\bar{x}\\))" = "mean", "Sample median (\\( M \\))" = "median", "Sample geometric mean  (\\(g \\))" = "geo.mean","Truncated sample mean ( \\( \\tilde{x} \\))" = "tmean")),
                              conditionalPanel("input.location_stats.indexOf('tmean') >= 0",
                                               numericInput("trim_level", "Trim Level", value = 0, min = 0, max = 0.5, step = 0.01)
                              ),
                              checkboxGroupInput("dispersion_stats", "Dispersion Statistics:",
                                                 choices = c("Sample variance (\\( s^2\\))" = "var","Sample standard deviation (\\( s\\))" = "sd", "Interquartile range (\\( IQR\\))" = "iqr", "Median absolute deviation (\\( MAD\\))" = "mad", "Range (\\( R \\))" = "range")),
                              textInput("quantiles", "Enter Quantiles (comma-separated, between 0 and 1)", "0, 0.25, 0.5, 0.75,1"),
                              selectInput("group_by", "Group by (optional):", choices = NULL),
                              selectInput("plot_type", "Plot Type:", choices = c("None", "Histogram","Density" ,"Boxplot", "Barplot")),
                              actionButton("run_desc", "Submit")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Numerical Summary",
                                         uiOutput("latex_table_ui")),
                                tabPanel("Frequency", 
                                         uiOutput("cat_summary")),
                                tabPanel("Quantiles", 
                                         tableOutput("qtable")),
                                tabPanel("Visualization", 
                                         uiOutput("plots"))
                              )
                            )
                          )
                 )

descriptive_stats_server <- function(input, output, session,firstkit.data) {
  
  
  output$var_select_ui <- renderUI({
    df <- firstkit.data()
    if (is.null(df)) return(NULL)
    varnames <- names(df)
    tagList(
      selectInput("num_vars", "Select Numeric Variables:",
                  choices = varnames[sapply(df, is.numeric)], selected = NULL, multiple = TRUE),
      selectInput("cat_vars", "Select Variables for Frequency:",
                  choices = varnames, selected = NULL, multiple = TRUE)
    )
  })
  
  observe({
    if (!is.null(firstkit.data())) {
      gvars <- names(firstkit.data())[sapply(firstkit.data(), function(x) is.factor(x) || is.character(x))]
      updateSelectInput(session, "group_by", choices = c("None", gvars), selected = "None")
    }
  })
  
  desc_trigger <- reactiveVal(0)
  
  observeEvent(input$run_desc, {
    desc_trigger(desc_trigger() + 1)
  })
  
  numeric_summary_data <- reactive({
    req(desc_trigger())
    df <- firstkit.data()
    req(df)
    numeric.vars <- input$num_vars
    group_var <- if (input$group_by != "None") input$group_by else NULL
    
    stat.funs <- list()
    if ("mean" %in% input$location_stats) stat.funs$`\\( \\bar{x} \\)` <- function(x, na.rm=TRUE) mean(x, na.rm=na.rm)
    if ("median" %in% input$location_stats) stat.funs$`\\( M \\)` <- function(x, na.rm=TRUE) median(x, na.rm=na.rm)
    if ("geo.mean" %in% input$location_stats) stat.funs$`\\(g\\)` <- function(x, na.rm=TRUE) geo.mean(x, na.rm=na.rm)
    if ("tmean" %in% input$location_stats) stat.funs$`\\( \\tilde{x} \\)` <- function(x, na.rm=TRUE) mean(x, na.rm=na.rm,trim=input$trim_level)
    if ("var" %in% input$dispersion_stats) stat.funs$`\\( s^2 \\)` <- function(x, na.rm=TRUE) var(x, na.rm=na.rm)
    if ("sd" %in% input$dispersion_stats) stat.funs$`\\( s \\)` <- function(x, na.rm=TRUE) sd(x, na.rm=na.rm)
    if ("iqr" %in% input$dispersion_stats) stat.funs$`\\( IQR \\)` <- function(x, na.rm=TRUE) IQR(x, na.rm=na.rm)
    if ("mad" %in% input$dispersion_stats) stat.funs$`\\( MAD \\)` <- function(x, na.rm=TRUE) mad(x, constant = 1, na.rm=na.rm)
    if ("range" %in% input$dispersion_stats) stat.funs$`\\( R \\)` <- function(x, na.rm=TRUE) max(x, na.rm=na.rm) - min(x, na.rm=na.rm)
    
    if (length(stat.funs) == 0 || length(numeric.vars) == 0)
      return(data.frame(Message = "No statistics selected or no numeric variables chosen."))
    
    if (!is.null(group_var) && group_var %in% names(df)) {
      grouped_data <- df %>%
        dplyr::select(all_of(c(group_var, numeric.vars))) %>%
        group_by(.data[[group_var]])
      
      summary_list <- lapply(numeric.vars, function(v) {
        stats <- grouped_data %>%
          summarise(across(all_of(v), stat.funs, .names = "{.fn}"), .groups = "drop")
        stats <- stats %>% mutate(Variable = v)
        stats[, c(group_var, "Variable", setdiff(names(stats), c(group_var, "Variable")))]
      })
      
      bind_rows(summary_list)
    } else {
      summary_list <- lapply(numeric.vars, function(v) {
        values <- df[[v]]
        stats <- sapply(stat.funs, function(f) f(values))
        data.frame(Variable = v, t(stats), row.names = NULL, check.names = FALSE)
      })
      bind_rows(summary_list)
    }
  })
  
  output$num_summary <- renderTable({
    numeric_summary_data()
  })
  
  output$latex_table_ui <- renderUI({
    df <- numeric_summary_data()
    if (nrow(df) == 0) return(NULL)
    
    output$num_summary <- renderTable({df}, sanitize.text.function = identity)  
    
    tagList(
      withMathJax(),
      tableOutput("num_summary")
    )
  })
  
  
  cat_summary_data <- reactive({
    req(desc_trigger())
    df <- firstkit.data()
    req(df)
    cat_vars <- input$cat_vars
    if (length(cat_vars) == 0) return(list(data.frame(Message = "No variable selected.")))
    
    lapply(cat_vars, function(var) {
      vec <- df[[var]]
      tbl <- table(vec, useNA = "ifany")
      pp <- prop.table(tbl)
      rr <- ifelse(sum(round(pp,digits=1))==1, 1, 
                   ifelse(sum(round(pp,digits=2))==1,2,
                          ifelse(sum(round(pp,digits=3))==1,3,4)))
      
      percent_tbl <- round(100*pp, rr)
      
      vars.summary <- data.frame(Category = names(tbl), Count = as.vector(tbl), Percentage = as.vector(percent_tbl), stringsAsFactors = FALSE)
      total.row <- data.frame(Category = "Total:", Count = sum(tbl), Percentage = round(sum(percent_tbl), rr),stringsAsFactors = FALSE)
      list(title = paste("Frequency Table for:", var), table = rbind(vars.summary, total.row))
    })
  })
  
  output$cat_summary <- renderTable({
    NULL  
  })
  
  output$cat_summary <- renderUI({
    summaries <- cat_summary_data()
    if (is.null(summaries) || length(summaries) == 0) return(NULL)
    
    output_list <- lapply(summaries, function(tbl_info) {
      tagList(
        h4(tbl_info$title),
        tableOutput(outputId = paste0("tbl_", gsub("[^A-Za-z0-9]", "_", tbl_info$title)))
      )
    })
    
    for (i in seq_along(summaries)) {
      local({
        idx <- i
        output_id <- paste0("tbl_", gsub("[^A-Za-z0-9]", "_", summaries[[idx]]$title))
        output[[output_id]] <- renderTable({
          summaries[[idx]]$table
        })
      })
    }
    
    do.call(tagList, output_list)
  })
  
  observeEvent(input$run_desc, {
    req(input$num_vars, input$quantiles)
    
    # Parse quantile values
    probs <- as.numeric(strsplit(input$quantiles, ",")[[1]])
    probs <- probs[!is.na(probs) & probs >= 0 & probs <= 1]
    
    if (length(probs) == 0) {
      showNotification("Please enter valid quantile values between 0 and 1.", type = "error")
      return(NULL)
    }
    
    df <- firstkit.data()
    vars <- input$num_vars
    group_var <- input$group_by
    qnames <- paste0(probs * 100, "%")
    
    sdf <- data.frame()
    
    if (group_var == "None") {
      for (v in vars) {
        qvals <- quantile(df[[v]], probs = probs, na.rm = TRUE)
        sdf <- rbind(sdf, data.frame(Variable = v,t(qvals)))
      }
      colnames(sdf) <- c("Variable", qnames)
      
    } else {
      req(group_var %in% names(df))
      df[[group_var]] <- as.factor(df[[group_var]])
      
      for (g in levels(df[[group_var]])) {
        subset_df <- df[df[[group_var]] == g, ]
        for (v in vars) {
          qvals <- quantile(subset_df[[v]], probs = probs, na.rm = TRUE)
          sdf <- rbind(sdf, data.frame(Group = g,Variable = v,t(qvals)))
        }
      }
      colnames(sdf) <- c("Group", "Variable", qnames)
    }
    
    output$qtable <- renderTable({
      sdf
    })
  })
  
  output$plots <- renderUI({
    req(desc_trigger())
    req(input$plot_type)
    numeric.vars <- input$num_vars
    if (length(numeric.vars) == 0 || input$plot_type == "None") return(NULL)
    plot_output_list <- lapply(numeric.vars, function(var) {
      plotlyOutput(outputId = paste0("plot_", var))
    })
    do.call(tagList, plot_output_list)
  })
  
  observe({
    req(desc_trigger())
    df <- firstkit.data()
    numeric.vars <- input$num_vars
    cat.vars <- input$cat_vars
    group_var <- if (input$group_by != "None") input$group_by else NULL
    plot_type <- isolate(input$plot_type)
    
    for (vv in numeric.vars) {
      local({
        v <- vv
        output[[paste0("plot_", v)]] <- renderPlotly({
          p <- NULL
          
          if (plot_type == "Histogram") {
            
            if (!is.null(group_var) && group_var %in% names(df)) {
              p <- ggplot(df, aes(x = .data[[v]],fill=.data[[group_var]])) +
                geom_histogram(alpha=0.5) +
                labs(title = paste("Histogram of", v, "by", group_var), x = group_var, y = v) +theme_bw()
            } else {
              p <- ggplot(df, aes(x = .data[[v]])) +
                geom_histogram(fill = "white", color = "purple", bins = 30) +
                labs(title = paste("Histogram of", v), x = v, y = "Count")  +theme_bw()
            }
            
            
          } else if (plot_type == "Density") {
            
            if (!is.null(group_var) && group_var %in% names(df)) {
              p <- ggplot(df, aes(x = .data[[v]],y=after_stat(density),color=.data[[group_var]],fill=.data[[group_var]])) +
                geom_density(alpha=0.5) +
                labs(title = paste("Density of", v, "by", group_var), x = group_var, y = v) +theme_bw()
            } else {
              p <- ggplot(df, aes(x = .data[[v]],y=after_stat(density))) +
                geom_density(fill = "white", color = "purple", bins = 30) +
                labs(title = paste("Density of", v), x = v, y = "Prob")  +theme_bw()
            }
            
            
          } else if (plot_type == "Boxplot") {
            if (!is.null(group_var) && group_var %in% names(df)) {
              p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[v]])) +
                geom_boxplot(outlier.colour = "black", fill = "white", color = "darkorange") +
                labs(title = paste("Boxplot of", v, "by", group_var), x = group_var, y = v) +theme_bw()
            } else {
              p <- ggplot(df, aes(y = .data[[v]])) +
                geom_boxplot(outlier.colour = "black", fill = "white", color = "darkorange") +
                labs(title = paste("Boxplot of", v), y = v)  +theme_bw()
            }
            
          }
          else if (plot_type == "Barplot") {
            if (!is.null(group_var) && group_var %in% names(df)) {
              df2 <- df[!is.na(df[[v]]) & !is.na(df[[group_var]]), ]
              p <- ggplot(df2, aes(x = .data[[v]], fill = .data[[group_var]])) +
                geom_bar(position = "dodge", color = "black") +
                labs(title = paste("Grouped Barplot of", v, "by", group_var),
                     x = v, y = "Count", fill = group_var) +
                theme_bw()
            } else {
              freq <- as.data.frame(table(df[[v]]))
              colnames(freq) <- c("Category", "Count")
              p <- ggplot(freq, aes(x = Category, y = Count)) +
                geom_bar(stat = "identity", fill = "white", color = "purple") +
                labs(title = paste("Barplot of", v), x = v, y = "Count") +
                theme_bw()
            }
          }
          ggplotly(p)
        })
      })
    }
  })
}

