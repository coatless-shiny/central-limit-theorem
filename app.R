library(shiny)
library(bslib)
library(ggplot2)
library(moments)
library(scales)
library(kableExtra)
library(DT)

# Define distribution colors
dist_colors <- list(
  normal = "#636363",      # Gray
  uniform = "#159947",     # Green
  t = "#159799",          # Teal
  cauchy = "#994715",     # Orange
  beta_right = "#155999", # Blue
  beta_left = "#997315",  # Gold
  chisq = "#991515",      # Dark Red
  gamma = "#701599",      # Purple
  weibull = "#159947",    # Green
  f = "#636363",         # Gray
  lognormal = "#159799",  # Teal
  pareto = "#994715",     # Orange
  bimodal = "#701599",    # Purple
  mixture = "#155999"     # Blue
)



# UI definition
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    primary = "#8C1515"  # Stanford cardinal
  ),
  
  title = "Central Limit Theorem",
  
  # Sidebar with controls
  sidebar = sidebar(
    title = "Controls",
    
    # Distribution selection with new options
    selectInput("dist_type", "Population Distribution:",
                choices = c(
                  "Normal" = "normal",
                  "Student's t" = "t",
                  "Uniform" = "uniform",
                  "Cauchy" = "cauchy",
                  "Beta (Left Skewed)" = "beta_left",
                  "Beta (Right Skewed)" = "beta_right",
                  "Chi-squared (Right Skewed)" = "chisq",
                  "F (Right Skewed)" = "f",
                  "Gamma (Right Skewed)" = "gamma",
                  "Log-normal (Right Skewed)" = "lognormal",
                  "Pareto (Right Skewed)" = "pareto",
                  "Weibull (Right Skewed)" = "weibull",
                  "Bimodal Normal" = "bimodal",
                  "Mixture Normal" = "mixture"
                )),
    
    
    # Sample size controls
    numericInput("obs_per_sample", "Observations per Sample:", 
                 value = 30, min = 2, max = 100),
    
    numericInput("samples_per_draw", "Samples per Draw:", 
                 value = 1, min = 1, max = 10),
    
    # Manual sampling control
    actionButton("manual_sample", "Add Samples", 
                 class = "btn-primary w-100"),
    
    br(), br(),
    
    # Automated sampling controls
    uiOutput("control_button"),
    br(),
    actionButton("reset", "Reset", class = "btn-danger w-100")
  ),
  
  # Main panel
  card(
    # Population Distribution
    card(
      card_header("Population Distribution"),
      layout_columns(
        col_widths = c(8, 4),
        plotOutput("population_plot", height = "400px"),
        DTOutput("pop_stats")
      )
    ),
    
    # Current Sample
    card(
      card_header("Current Sample"),
      layout_columns(
        col_widths = c(8, 4),
        plotOutput("current_sample_plot", height = "400px"),
        DTOutput("sample_stats")
      )
    ),
    
    # Sampling Distribution
    card(
      card_header("Sampling Distribution"),
      layout_columns(
        col_widths = c(8, 4),
        plotOutput("sampling_plot", height = "400px"),
        DTOutput("sampling_stats")
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    is_running = FALSE,
    button_state = "start",
    samples = numeric(),
    current_sample = numeric(),
    last_update = Sys.time(),
    total_draws = 0  # Add counter for total draws
  )
  
  # Add observer for distribution change
  observeEvent(input$dist_type, {
    values$samples <- numeric()
    values$current_sample <- numeric()
    values$is_running <- FALSE
    values$button_state <- "start"
    values$total_draws <- 0  # Reset total draws counter
  })
  
  # Function to create statistics data frame
  get_stats_df <- function(x, extra_info = NULL) {
    if (length(x) > 0) {
      stats <- data.frame(
        Statistic = c("Mean", "Median", "Std. Dev.", "Skewness", "Kurtosis"),
        Value = format(round(c(
          mean(x),
          median(x),
          sd(x),
          skewness(x),
          kurtosis(x)
        ), 3), nsmall = 3)
      )
      
      if (!is.null(extra_info)) {
        stats <- rbind(extra_info, stats)
      }
      
      return(stats)
    } else {
      data.frame(
        Statistic = character(),
        Value = character()
      )
    }
  }
  
  
  # Helper functions
  get_population_data <- function(n = 1000) {
    switch(input$dist_type,
           # Symmetric distributions
           "normal" = rnorm(n, mean = 0, sd = 1),
           "t" = rt(n, df = 3),
           "uniform" = runif(n, -3, 3),
           "cauchy" = rcauchy(n, location = 0, scale = 0.5),
           
           # Skewed distributions
           "beta_left" = {
             x <- rbeta(n, 7, 2)
             (x - 0.5) * 6
           },
           "beta_right" = {
             x <- rbeta(n, 2, 7)
             (x - 0.5) * 6
           },
           "chisq" = {
             x <- rchisq(n, df = 4)
             (x - 4) * 0.7  # Center and scale
           },
           "f" = {
             x <- rf(n, df1 = 4, df2 = 8)
             (x - 1.5) * 1.5  # Center and scale
           },
           "gamma" = {
             x <- rgamma(n, shape = 2, rate = 1)
             (x - 2) * 1.5
           },
           "lognormal" = {
             x <- rlnorm(n, meanlog = 0, sdlog = 0.5)
             (x - exp(0.125)) * 2
           },
           "pareto" = {
             # Using shape = 3 for finite variance
             x <- (1/runif(n)^(1/3) - 1) * 0.8  # Transform uniform to Pareto
             pmin(x, 6)  # Limit extreme values for better visualization
           },
           "weibull" = {
             x <- rweibull(n, shape = 1.5, scale = 1)
             (x - 0.9) * 2
           },
           
           # Multimodal distributions
           "bimodal" = c(rnorm(n/2, -1.5, 0.5), 
                         rnorm(n/2, 1.5, 0.5)),
           "mixture" = {
             components <- sample(1:3, n, replace = TRUE, 
                                  prob = c(0.4, 0.3, 0.3))
             means <- c(-2, 0, 2)
             sds <- c(0.5, 1, 0.5)
             rnorm(n, means[components], sds[components])
           })
  }
  
  # Function to draw multiple samples and update values
  draw_samples <- function() {
    # Draw the specified number of samples
    for(i in 1:input$samples_per_draw) {
      new_sample <- get_population_data(input$obs_per_sample)
      values$current_sample <- new_sample  # Update current sample view
      values$samples <- c(mean(new_sample), values$samples)
      values$total_draws <- values$total_draws + 1  # Increment total draws counter
    }
  }
  
  # Function to get new sample(s) for automatic sampling
  get_new_sample <- function() {
    if (values$is_running && difftime(Sys.time(), values$last_update, units = "secs") >= 1) {
      draw_samples()
      values$last_update <- Sys.time()
    }
  }
  
  # Statistics table outputs with DT
  output$pop_stats <- renderDT({
    pop_data <- get_population_data(1000)
    stats_df <- get_stats_df(pop_data)
    datatable(stats_df, 
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE) |>
      formatStyle(columns = 1:2, fontSize = '90%')
  })
  
  output$sample_stats <- renderDT({
    req(length(values$current_sample) > 0)
    extra_info <- data.frame(
      Statistic = "Sample Size",
      Value = format(length(values$current_sample), big.mark = ",")
    )
    stats_df <- get_stats_df(values$current_sample, extra_info)
    datatable(stats_df, 
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE) |>
      formatStyle(columns = 1:2, fontSize = '90%')
  })
  
  output$sampling_stats <- renderDT({
    req(length(values$samples) > 0)
    extra_info <- data.frame(
      Statistic = "Total Draws",
      Value = format(values$total_draws, big.mark = ",")
    )
    stats_df <- get_stats_df(values$samples, extra_info)
    datatable(stats_df, 
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE) |>
      formatStyle(columns = 1:2, fontSize = '90%')
  })
  
  # Graph population, sample, and sampling distributions.
  output$population_plot <- renderPlot({
    pop_data <- get_population_data(1000)
    data_frame <- data.frame(value = pop_data)
    
    ggplot(data_frame, aes(x = value)) +
      geom_histogram(aes(y = after_stat(density)), 
                     fill = dist_colors[[input$dist_type]], alpha = 0.7,
                     color = "white", bins = 30) +
      geom_density(color = "#4D4F53", linewidth = 1) +
      labs(x = "Value",
           y = "Density") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
  
  output$current_sample_plot <- renderPlot({
    invalidateLater(1000)
    get_new_sample()
    
    if (length(values$current_sample) > 0) {
      data_frame <- data.frame(value = values$current_sample)
      mean_val <- mean(values$current_sample)
      
      ggplot(data_frame, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), 
                       fill = dist_colors[[input$dist_type]], alpha = 0.7,
                       color = "white", bins = 30) +
        geom_density(color = "#4D4F53", linewidth = 1) +
        geom_vline(xintercept = mean_val, 
                   color = "#4D4F53", linewidth = 1, linetype = "dashed") +
        annotate("text", x = mean_val, y = 0, 
                 label = sprintf("Mean: %.2f", mean_val),
                 vjust = -0.5) +
        labs(x = "Value",
             y = "Density") +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    }
  })
  
  output$sampling_plot <- renderPlot({
    if (length(values$samples) > 0) {
      data_frame <- data.frame(value = values$samples)
      
      ggplot(data_frame, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), 
                       fill = "#8C1515", alpha = 0.7,
                       color = "white", bins = min(30, length(values$samples))) +
        geom_density(color = "#4D4F53", linewidth = 1) +
        stat_function(fun = dnorm, 
                      args = list(mean = mean(values$samples), 
                                  sd = sd(values$samples)),
                      color = "black", linetype = "dashed", linewidth = 1) +
        labs(x = "Sample Mean",
             y = "Density") +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    }
  })
  
  observeEvent(input$manual_sample, {
    draw_samples()
  })
  
  output$control_button <- renderUI({
    btn_class <- switch(values$button_state,
                        "start" = "btn-success",
                        "pause" = "btn-warning",
                        "resume" = "btn-success")
    
    btn_label <- switch(values$button_state,
                        "start" = "Start Sampling",
                        "pause" = "Pause",
                        "resume" = "Resume")
    
    actionButton("control_button", btn_label, 
                 class = paste("btn", btn_class, "w-100"))
  })
  
  observeEvent(input$control_button, {
    if (values$button_state == "start") {
      values$is_running <- TRUE
      values$button_state <- "pause"
    } else if (values$button_state == "pause") {
      values$is_running <- FALSE
      values$button_state <- "resume"
    } else {
      values$is_running <- TRUE
      values$button_state <- "pause"
    }
  })
  
  observeEvent(input$reset, {
    values$samples <- numeric()
    values$current_sample <- numeric()
    values$is_running <- FALSE
    values$button_state <- "start"
    values$total_draws <- 0 
  })
}

# Run the application
shinyApp(ui = ui, server = server)
