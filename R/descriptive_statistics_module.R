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
                              textInput("quantiles", "Enter quantiles (comma-separated, between 0 and 1)", "0, 0.25, 0.5, 0.75,1"),
                              selectInput("group_by", "Group by (optional):", choices = NULL),
                              selectInput("plot_type", "Plot Type:", choices = c("None","Stem-and-Leaf" ,"Histogram","Density" ,"Boxplot", "Barplot","Scatterplot")),
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
                                         conditionalPanel("input.plot_type == 'Stem-and-Leaf'",
                                                          numericInput(inputId="sc",label = "scale",value = 1,min = 0,step = 0.05)
                                                          ),
                                         conditionalPanel("input.plot_type == 'Scatterplot'",
                                                          plotOutput("legend_scatterplot", height = "100px")),
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
        select(all_of(c(group_var, numeric.vars))) %>%
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
    
    df <- df[order(df[[1]]), , drop = FALSE]
    
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
    req(desc_trigger(), input$plot_type)
    if (length(input$num_vars) == 0 || input$plot_type == "None") return(NULL)
    
    if (input$plot_type == "Scatterplot") {
      return(uiOutput("grid.ui"))  
    }
    
    tagList(lapply(input$num_vars, function(v) {
      if (input$plot_type == "Stem-and-Leaf") {
        plotOutput(paste0("stem_", v))
      } else {
        plotlyOutput(paste0("plot_", v))  
      }
    }))
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
  
  output$grid.ui <- renderUI({
    req(input$num_vars)
    vars <- input$num_vars
    n <- length(vars)
    validate(need(n >= 2, "Please select at least two variables."))
    
    tagList(lapply(1:n, function(i) {
      fluidRow(
        lapply(1:n, function(j) {
          id <- paste0("cell_", i, "_", j)
          if (i == j) {
            column(3, div(style = "height:100px; display:flex; align-items:center; justify-content:center;",strong(vars[i])))
          } else if (i > j) {
            column(3, plotlyOutput(id, height = "300px"))
          } else {
            column(3, HTML("&nbsp;"))
          }
        }))
      }))
  })
  
  observe({
    req(input$plot_type == "Scatterplot")
    req(input$num_vars)
    df <- firstkit.data()
    vars <- input$num_vars
    n <- length(vars)
    group_var <- if (!is.null(input$group_by) && input$group_by != "None") input$group_by else NULL
    
    for (i in 1:n) {
      for (j in 1:n) {
        if (i > j) {
          local({
            xx <- vars[j]
            yy <- vars[i]
            id <- paste0("cell_", i, "_", j)
            
            output[[id]] <- renderPlotly({
              p <- ggplot(df, aes_string(x = xx, y = yy)) +
                geom_point(aes_string(color = group_var), size = 2) +
                theme_bw() +
                theme(legend.position = "none")
              ggplotly(p) 
            })
          })
        }
      }
    }
  })
  
  output$legend_scatterplot <- renderPlot({
    req(input$group_by)
    gvar <- input$group_by
    req(gvar != "None")
    glevels <- levels(factor(firstkit.data()[[gvar]]))
    df.leg <- data.frame(x = 1:length(glevels), y = 1, group = factor(glevels))
    
    ggplot(df.leg, aes(x = x, y = y, color = group)) +
      geom_point(size = 5) + scale_color_discrete(name = gvar) +
      theme_void() + ylim(c(2, 3)) +
      theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(2, "cm")) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE))
  })
  
  observe({
    req(input$plot_type == "Stem-and-Leaf")
    req(input$num_vars)
    
    sc <- as.numeric(input$sc)
    fkit <- firstkit.data()
    nvars <- input$num_vars
    gvar <- input$group_by
    
    for (vv in nvars) {
      local({
        v <- vv
        output[[paste0("stem_", v)]] <- renderPlot({
          req(fkit[[v]])
          
          if (!is.null(gvar) && gvar %in% names(fkit)) {
            fkit.nonan <- na.omit(fkit[, c(v, gvar)])
            fkit.nonan[[gvar]] <- as.factor(fkit.nonan[[gvar]])
            
            glist <- split(fkit.nonan, fkit.nonan[[gvar]])
            stem.list <- lapply(names(glist), function(g) {
              lines <- capture.output(stem(glist[[g]][[v]], scale = sc))
              data.frame(tmp = lines, rr = seq_along(lines), group = g, stringsAsFactors = FALSE)
            })
            stemdf <- do.call(rbind, stem.list)
            
            ggplot(stemdf) +
              geom_text(aes(x = rr, y = 0, label = tmp), size = max(10 / sc, 3), hjust = 0) +
              coord_flip() +
              facet_wrap(~group, scales = "free_y") +
              theme_classic() +
              scale_x_continuous(breaks = NULL) +
              scale_y_continuous(breaks = NULL, limits = c(0, 1)) +
              theme(
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                axis.line = element_blank(),
                legend.position = "none"
              )
            
          } else {
            tmp <- capture.output(stem(fkit[[v]], scale = sc))
            stemdf <- data.frame(tmp = tmp, rr = seq_along(tmp))
            
            ggplot(stemdf) +
              geom_text(aes(x = rr, y = 0, label = tmp), size = max(10 / sc, 3), hjust = 0) +
              coord_flip() +
              theme_classic() +
              scale_x_continuous(breaks = NULL) +
              scale_y_continuous(breaks = NULL, limits = c(0, 1)) +
              theme(
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                axis.line = element_blank(),
                legend.position = "none"
              )
          }
        })
      })
    }
  })
  
  
}

