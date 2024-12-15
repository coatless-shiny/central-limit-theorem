library(shiny)
library(bslib)
library(ggplot2)
library(moments)
library(scales)
library(kableExtra)
library(DT)

# Stanford colors ----
stanford_colors <- list(
  cardinal = "#8C1515", 
  dark_red = "#820000",
  bright_red = "#B1040E",
  cool_grey = "#4D4F53",
  black = "#2E2D29",
  foggy_grey = "#E3DED1",
  sandstone = "#D2C295",
  light_sage = "#C7D1C5",
  lime = "#009B76",
  palo_verde = "#175E54",
  digital_blue = "#006CB8",
  poppy = "#E98300",
  purple = "#53284F",
  plum = "#620059"
)

# App color scheme ----
app_colors <- list(
  # Action colors 
  add = stanford_colors$lime,                 # Green for adding
  start = stanford_colors$palo_verde,         # Darker green for start/resume
  pause = stanford_colors$poppy,              # Orange for pause
  reset = stanford_colors$bright_red,         # Red for reset
  main_theme = stanford_colors$cardinal,      # Stanford cardinal for main theme
  
  # Distribution colors
  continuous = stanford_colors$digital_blue,  # Blue for continuous
  discrete = stanford_colors$purple           # Purple for discrete
)

# Distribution colors using Stanford palette
dist_colors <- list(
  # Continuous distributions
  normal = stanford_colors$digital_blue,
  t = stanford_colors$digital_blue,
  uniform = stanford_colors$digital_blue,
  cauchy = stanford_colors$digital_blue,
  beta = stanford_colors$digital_blue,
  chisq = stanford_colors$digital_blue,
  gamma = stanford_colors$digital_blue,
  weibull = stanford_colors$digital_blue,
  f = stanford_colors$digital_blue,
  lognormal = stanford_colors$digital_blue,
  pareto = stanford_colors$digital_blue,
  bimodal = stanford_colors$digital_blue,
  mixture = stanford_colors$digital_blue,
  
  # Discrete distributions
  binomial = stanford_colors$purple,
  poisson = stanford_colors$purple,
  geometric = stanford_colors$purple,
  negative_binomial = stanford_colors$purple,
  hypergeometric = stanford_colors$purple
)

# UI definition ----
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    primary = "#8C1515"  # Stanford cardinal
  ),
  
  title = "Central Limit Theorem",
  
  ## Sidebar ----
  sidebar = sidebar(
    ### Distribution Settings Section ----
    card(
      card_header("Distribution Settings"),
      numericInput("pop_size", "Population Size:", 
                   value = 1000, min = 100, max = 10000, step = 100),
      selectInput("dist_type", "Population Distribution:",
                  choices = list(
                    "Continuous" = list(
                      "Normal" = "normal",
                      "Student's t" = "t",
                      "Uniform" = "uniform",
                      "Cauchy" = "cauchy",
                      "Beta" = "beta",
                      "Chi-squared" = "chisq",
                      "F" = "f",
                      "Gamma" = "gamma",
                      "Log-normal" = "lognormal",
                      "Pareto" = "pareto",
                      "Weibull" = "weibull",
                      "Bimodal Normal" = "bimodal",
                      "Mixture Normal" = "mixture"
                    ),
                    "Discrete" = list(
                      "Binomial" = "binomial",
                      "Poisson" = "poisson",
                      "Geometric" = "geometric",
                      "Negative Binomial" = "negative_binomial",
                      "Hypergeometric" = "hypergeometric"
                    )
                  )),
      
      #### Dynamic UI for distribution parameters
      uiOutput("dist_params")
    ),
    
    ### Sampling Settings Section ----
    card(
      card_header("Sampling Settings"),
      
      numericInput("obs_per_sample", "Sample Size (n):", 
                   value = 30, min = 2, max = 100),
      
      numericInput("samples_per_draw", "Samples to Draw:", 
                   value = 1, min = 1, max = 10)
    ),
    
    ### Sampling Controls Section ----
    card(
      card_header("Sampling Controls"),
      
      actionButton("manual_sample", "Add Sample", 
                   class = paste("btn w-100"),
                   style = sprintf("background-color: %s; color: white;", 
                                   app_colors$add)),
      
      uiOutput("control_button"),
      
      actionButton("reset", "Reset", 
                   class = "btn-danger w-100")
    )
  ),
  
  ## Main panel ----
  card(
    ### Population Distribution  ----
    card(
      card_header("Population Distribution"),
      layout_columns(
        col_widths = c(8, 4),
        plotOutput("population_plot", height = "400px"),
        DTOutput("pop_stats")
      )
    ),
    
    ### Current Sample  ----
    card(
      card_header("Current Sample"),
      layout_columns(
        col_widths = c(8, 4),
        plotOutput("current_sample_plot", height = "400px"),
        DTOutput("sample_stats")
      )
    ),
    
    ### Sampling Distribution  ----
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

# Server ----
server <- function(input, output, session) {
  ## Reactive values  ----
  values <- reactiveValues(
    is_running = FALSE,
    button_state = "start",
    samples = numeric(),
    current_sample = numeric(),
    last_update = Sys.time(),
    total_draws = 0  # Add counter for total draws
  )
  
  ## Observers ----
  
  ## Distribution change  ----
  observeEvent(input$dist_type, {
    values$samples <- numeric()
    values$current_sample <- numeric()
    values$is_running <- FALSE
    values$button_state <- "start"
    values$total_draws <- 0  # Reset total draws counter
  })

  ## Automatically draw samples ----
  observeEvent(input$manual_sample, {
    draw_samples()
  })
  
  
  ## Control button state changes ----
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
  
  ## Handle reset ----
  observeEvent(input$reset, {
    values$samples <- numeric()
    values$current_sample <- numeric()
    values$is_running <- FALSE
    values$button_state <- "start"
    values$total_draws <- 0 
  })
  
  ## Create distribution statistics data frame ----
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
  
  ## Initialize parameters ----
  output$dist_params <- renderUI({
    switch(input$dist_type,
           "normal" = list(
             numericInput("normal_mean", "Mean:", value = 0, step = 0.1),
             numericInput("normal_sd", "Standard Deviation:", value = 1, min = 0.1, step = 0.1)
           ),
           "t" = list(
             numericInput("t_df", "Degrees of Freedom:", value = 3, min = 1, step = 1)
           ),
           "uniform" = list(
             numericInput("uniform_min", "Minimum:", value = -3, step = 0.1),
             numericInput("uniform_max", "Maximum:", value = 3, step = 0.1)
           ),
           "cauchy" = list(
             numericInput("cauchy_location", "Location:", value = 0, step = 0.1),
             numericInput("cauchy_scale", "Scale:", value = 0.5, min = 0.1, step = 0.1)
           ),
           "beta" = list(
             numericInput("beta_shape1", "a:", value = 2, min = 0.1, step = 0.1),
             numericInput("beta_shape2", "b:", value = 2, min = 0.1, step = 0.1)
           ),
           "chisq" = list(
             numericInput("chisq_df", "Degrees of Freedom:", value = 4, min = 1, step = 1)
           ),
           "f" = list(
             numericInput("f_df1", "Degrees of Freedom 1:", value = 4, min = 1, step = 1),
             numericInput("f_df2", "Degrees of Freedom 2:", value = 8, min = 1, step = 1)
           ),
           "gamma" = list(
             numericInput("gamma_shape", "Shape:", value = 2, min = 0.1, step = 0.1),
             numericInput("gamma_rate", "Rate:", value = 1, min = 0.1, step = 0.1)
           ),
           "lognormal" = list(
             numericInput("lognormal_meanlog", "Mean Log:", value = 0, step = 0.1),
             numericInput("lognormal_sdlog", "SD Log:", value = 0.5, min = 0.1, step = 0.1)
           ),
           "pareto" = list(
             numericInput("pareto_shape", "Shape:", value = 3, min = 1, step = 0.1)
           ),
           "weibull" = list(
             numericInput("weibull_shape", "Shape:", value = 1.5, min = 0.1, step = 0.1),
             numericInput("weibull_scale", "Scale:", value = 1, min = 0.1, step = 0.1)
           ),
           "bimodal" = list(
             numericInput("bimodal_mean1", "Mean 1:", value = -1.5, step = 0.1),
             numericInput("bimodal_mean2", "Mean 2:", value = 1.5, step = 0.1),
             numericInput("bimodal_sd", "Standard Deviation:", value = 0.5, min = 0.1, step = 0.1)
           ),
           "mixture" = list(
             numericInput("mixture_mean1", "Mean 1:", value = -2, step = 0.1),
             numericInput("mixture_mean2", "Mean 2:", value = 0, step = 0.1),
             numericInput("mixture_mean3", "Mean 3:", value = 2, step = 0.1),
             numericInput("mixture_sd1", "SD 1:", value = 0.5, min = 0.1, step = 0.1),
             numericInput("mixture_sd2", "SD 2:", value = 1, min = 0.1, step = 0.1),
             numericInput("mixture_sd3", "SD 3:", value = 0.5, min = 0.1, step = 0.1),
             numericInput("mixture_prob1", "Probability 1:", value = 0.4, min = 0, max = 1, step = 0.1),
             numericInput("mixture_prob2", "Probability 2:", value = 0.3, min = 0, max = 1, step = 0.1)
           ),
           "binomial" = list(
             numericInput("binom_size", "Number of Trials (n):", 
                          value = 20, min = 1, step = 1),
             numericInput("binom_prob", "Success Probability (p):", 
                          value = 0.5, min = 0, max = 1, step = 0.1)
           ),
           "poisson" = list(
             numericInput("pois_lambda", "Rate (λ):", 
                          value = 5, min = 0, step = 0.5)
           ),
           "geometric" = list(
             numericInput("geom_prob", "Success Probability (p):", 
                          value = 0.3, min = 0, max = 1, step = 0.1)
           ),
           "negative_binomial" = list(
             numericInput("nb_size", "Number of Successes (r):", 
                          value = 5, min = 1, step = 1),
             numericInput("nb_prob", "Success Probability (p):", 
                          value = 0.5, min = 0, max = 1, step = 0.1)
           ),
           "hypergeometric" = list(
             numericInput("hyper_m", "Population Successes (M):", 
                          value = 50, min = 0, step = 1),
             numericInput("hyper_n", "Population Failures (N):", 
                          value = 50, min = 0, step = 1),
             numericInput("hyper_k", "Sample Size (k):", 
                          value = 10, min = 1, step = 1)
           )
    )
  })
  
  
  ## Generate population-level data ----
  get_population_data <- function(n) {
    
    req(input$pop_size)
  
    switch(input$dist_type,
           "normal" = rnorm(n, mean = input$normal_mean, sd = input$normal_sd),
           "t" = rt(n, df = input$t_df),
           "uniform" = runif(n, min = input$uniform_min, max = input$uniform_max),
           "cauchy" = rcauchy(n, location = input$cauchy_location, scale = input$cauchy_scale),
           "beta" = {
             x <- rbeta(n, input$beta_shape1, input$beta_shape2)
             (x - 0.5) * 6
           },
           "chisq" = {
             x <- rchisq(n, df = input$chisq_df)
             (x - input$chisq_df) * 0.7
           },
           "f" = {
             x <- rf(n, df1 = input$f_df1, df2 = input$f_df2)
             (x - (input$f_df2/(input$f_df2 - 2))) * 1.5
           },
           "gamma" = {
             x <- rgamma(n, shape = input$gamma_shape, rate = input$gamma_rate)
             (x - (input$gamma_shape/input$gamma_rate)) * 1.5
           },
           "lognormal" = {
             x <- rlnorm(n, meanlog = input$lognormal_meanlog, sdlog = input$lognormal_sdlog)
             (x - exp(input$lognormal_meanlog + input$lognormal_sdlog^2/2)) * 2
           },
           "pareto" = {
             x <- (1/runif(n)^(1/input$pareto_shape) - 1) * 0.8
             pmin(x, 6)
           },
           "weibull" = {
             x <- rweibull(n, shape = input$weibull_shape, scale = input$weibull_scale)
             (x - input$weibull_scale * gamma(1 + 1/input$weibull_shape)) * 2
           },
           "bimodal" = c(
             rnorm(n/2, input$bimodal_mean1, input$bimodal_sd),
             rnorm(n/2, input$bimodal_mean2, input$bimodal_sd)
           ),
           "mixture" = {
             # Normalize probabilities
             probs <- c(input$mixture_prob1, input$mixture_prob2,
                        1 - input$mixture_prob1 - input$mixture_prob2)
             probs <- pmax(0, pmin(1, probs))
             probs <- probs / sum(probs)
             
             components <- sample(1:3, n, replace = TRUE, prob = probs)
             means <- c(input$mixture_mean1, input$mixture_mean2, input$mixture_mean3)
             sds <- c(input$mixture_sd1, input$mixture_sd2, input$mixture_sd3)
             rnorm(n, means[components], sds[components])
           },
           "binomial" = {
             req(input$binom_size, input$binom_prob)
             x <- rbinom(n, size = input$binom_size, prob = input$binom_prob)
             # Center around mean for better visualization
             x - input$binom_size * input$binom_prob
           },
           "poisson" = {
             req(input$pois_lambda)
             x <- rpois(n, lambda = input$pois_lambda)
             # Center around lambda
             x - input$pois_lambda
           },
           "geometric" = {
             req(input$geom_prob)
             x <- rgeom(n, prob = input$geom_prob)
             # Center around mean (1-p)/p
             x - (1 - input$geom_prob)/input$geom_prob
           },
           "negative_binomial" = {
             req(input$nb_size, input$nb_prob)
             x <- rnbinom(n, size = input$nb_size, prob = input$nb_prob)
             # Center around mean r(1-p)/p
             x - input$nb_size * (1 - input$nb_prob)/input$nb_prob
           },
           "hypergeometric" = {
             req(input$hyper_m, input$hyper_n, input$hyper_k)
             x <- rhyper(n, m = input$hyper_m, n = input$hyper_n, k = input$hyper_k)
             # Center around mean k*M/(M+N)
             x - input$hyper_k * input$hyper_m/(input$hyper_m + input$hyper_n)
           }
           )
  }
  
  ## Validate distribution parameters are present ----
  validate_dist_params <- function() {
    req(input$dist_type)  # Always require distribution type
    
    switch(input$dist_type,
           "normal" = req(input$normal_mean, input$normal_sd),
           "t" = req(input$t_df),
           "uniform" = req(input$uniform_min, input$uniform_max),
           "cauchy" = req(input$cauchy_location, input$cauchy_scale),
           "beta" = req(input$beta_shape1, input$beta_shape2),
           "chisq" = req(input$chisq_df),
           "f" = req(input$f_df1, input$f_df2),
           "gamma" = req(input$gamma_shape, input$gamma_rate),
           "lognormal" = req(input$lognormal_meanlog, input$lognormal_sdlog),
           "pareto" = req(input$pareto_shape),
           "weibull" = req(input$weibull_shape, input$weibull_scale),
           "bimodal" = req(input$bimodal_mean1, input$bimodal_mean2, input$bimodal_sd),
           "mixture" = req(input$mixture_mean1, input$mixture_mean2, input$mixture_mean3,
                           input$mixture_sd1, input$mixture_sd2, input$mixture_sd3,
                           input$mixture_prob1, input$mixture_prob2),
           "binomial" = req(input$binom_size, input$binom_prob),
           "poisson" = req(input$pois_lambda),
           "geometric" = req(input$geom_prob),
           "negative_binomial" = req(input$nb_size, input$nb_prob),
           "hypergeometric" = req(input$hyper_m, input$hyper_n, input$hyper_k)
    )
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
  
  ## Draw new sample(s) for automatic sampling ----
  get_new_sample <- function() {
    if (values$is_running && difftime(Sys.time(), values$last_update, units = "secs") >= 1) {
      draw_samples()
      values$last_update <- Sys.time()
    }
  }
  
  ## Statistics table outputs with DT ----
  output$pop_stats <- renderDT({
    # Require distribution parameters before trying to render
    validate_dist_params()
    
    pop_data <- get_population_data(input$pop_size)

    extra_info <- data.frame(
      Statistic = "Population Size",
      Value = format(length(pop_data), big.mark = ",")
    )
    
    stats_df <- get_stats_df(pop_data, extra_info)
    datatable(stats_df, 
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE) |>
      formatStyle(columns = 1:2, fontSize = '90%')
  })
  
  output$sample_stats <- renderDT({
    # Check if we have a sample before trying to render
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
    req(length(values$samples) > 1)
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
  
  ## Graph population, sample, and sampling distributions. ----
  
  ## Render the population plot ----
  output$population_plot <- renderPlot({    
    
    validate_dist_params()
    
    pop_data <- get_population_data(input$pop_size)
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
  
  ## Render the current sample plot ----
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
  
  ## Render the sampling distribution plot ----
  output$sampling_plot <- renderPlot({
    
    # Check if we have at least 2 samples before trying to plot
    req(length(values$samples) >= 2)
    
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
  
  ## Automatic sampling control button ----
  output$control_button <- renderUI({
    btn_class <- switch(values$button_state,
                        "start" = paste("btn w-100"),
                        "pause" = paste("btn w-100"),
                        "resume" = paste("btn w-100"))
    
    btn_style <- switch(values$button_state,
                        "start" = sprintf("background-color: %s; color: white;", 
                                          app_colors$start),
                        "pause" = sprintf("background-color: %s;", 
                                          app_colors$pause),
                        "resume" = sprintf("background-color: %s; color: white;", 
                                           app_colors$start))
    
    btn_label <- switch(values$button_state,
                        "start" = "Start Sampling",
                        "pause" = "Pause",
                        "resume" = "Resume")
    
    actionButton("control_button", btn_label, 
                 class = btn_class,
                 style = btn_style)
  })

}

# Run the application ----
shinyApp(ui = ui, server = server)
