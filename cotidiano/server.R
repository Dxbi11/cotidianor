library(shiny)
library(ggplot2)

server <- function(input, output) {
  
  # Dynamic UI components
  output$dynamic_inputs <- renderUI({
    if(input$test_type == "Single Mean"){
      tagList(
        numericInput("mu0", "Population Mean (μ₀):", value = 100),
        numericInput("xbar", "Sample Mean:", value = 98),
        numericInput("sigma", "Population SD:", value = 15),
        numericInput("n", "Sample Size:", value = 30)
      )
      
    }
    else if(input$test_type == "Single Proportion"){
      tagList(
        numericInput("p0", "Population Proportion:", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("p_hat", "Sample Proportion:", value = 0.55, min = 0, max = 1, step = 0.01),
        numericInput("n_prop", "Sample Size:", value = 100)
      )
    }
    else if(input$test_type == "Difference of Means"){
      tagList(
        h4("Group 1:"),
        numericInput("xbar1", "Sample Mean:", value = 75),
        numericInput("sigma1", "Population SD:", value = 10),
        numericInput("n1", "Sample Size:", value = 30),
        
        h4("Group 2:"),
        numericInput("xbar2", "Sample Mean:", value = 72),
        numericInput("sigma2", "Population SD:", value = 12),
        numericInput("n2", "Sample Size:", value = 35)
      )
    }
    else if(input$test_type == "Difference of Proportions"){
      tagList(
        h4("Group 1:"),
        numericInput("p1", "Sample Proportion:", value = 0.6, min = 0, max = 1, step = 0.01),
        numericInput("n1_prop", "Sample Size:", value = 100),
        
        h4("Group 2:"),
        numericInput("p2", "Sample Proportion:", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("n2_prop", "Sample Size:", value = 120)
      )
    }
  })
  
  # Calculation logic
  test_results <- eventReactive(input$calculate, {
    req(input$test_type)
    
    alpha <- input$alpha
    results <- list()
    
    # Calculate test statistic and p-value
    if(input$test_type == "Single Mean"){
      se <- input$sigma / sqrt(input$n)
      z <- (input$xbar - input$mu0) / se
    }
    else if(input$test_type == "Single Proportion"){
      se <- sqrt(input$p0 * (1 - input$p0) / input$n_prop)
      z <- (input$p_hat - input$p0) / se
    }
    else if(input$test_type == "Difference of Means"){
      se <- sqrt((input$sigma1^2/input$n1) + (input$sigma2^2/input$n2))
      z <- (input$xbar1 - input$xbar2) / se
    }
    else if(input$test_type == "Difference of Proportions"){
      p_pool <- (input$p1*input$n1_prop + input$p2*input$n2_prop)/(input$n1_prop + input$n2_prop)
      se <- sqrt(p_pool*(1-p_pool)*(1/input$n1_prop + 1/input$n2_prop))
      z <- (input$p1 - input$p2)/se
    }
    
    # Determine critical values
    if(input$test_tails == "Two-tailed"){
      z_crit <- qnorm(1 - alpha/2)
      p_value <- 2 * (1 - pnorm(abs(z)))
    } else {
      if(input$tail_direction == "Left-tailed"){
        z_crit <- qnorm(alpha)
        p_value <- pnorm(z)
      } else {
        z_crit <- qnorm(1 - alpha)
        p_value <- 1 - pnorm(z)
      }
    }
    
    # Decision
    decision <- if(p_value < alpha) "Reject H₀" else "Fail to reject H₀"
    
    list(z = z, z_crit = z_crit, p_value = p_value, decision = decision)
  })
  
  # Results output
  output$results <- renderPrint({
    res <- test_results()
    cat(paste(
      "Test Statistic (Z):", round(res$z, 4), "\n",
      "Critical Value:", round(res$z_crit, 4), "\n",
      "P-value:", round(res$p_value, 4), "\n",
      "Decision:", res$decision
    ))
  })
  
  # Distribution plot
  output$dist_plot <- renderPlot({
    res <- test_results()
    req(res$z_crit)
    
    df <- data.frame(x = seq(-4, 4, length.out = 200))
    df$y <- dnorm(df$x)
    
    p <- ggplot(df, aes(x, y)) +
      geom_line(color = "steelblue") +
      geom_vline(xintercept = res$z, color = "red", linetype = "dashed") +
      labs(title = "Normal Distribution with Test Results",
           x = "Z-value", y = "Density") +
      theme_minimal()
    
    if(input$test_tails == "Two-tailed"){
      p <- p + 
        geom_area(data = subset(df, x < -res$z_crit), aes(y = y), fill = "orange", alpha = 0.3) +
        geom_area(data = subset(df, x > res$z_crit), aes(y = y), fill = "orange", alpha = 0.3) +
        geom_vline(xintercept = c(-res$z_crit, res$z_crit), color = "blue", linetype = "dotted")
    } else {
      if(input$tail_direction == "Left-tailed"){
        p <- p + 
          geom_area(data = subset(df, x < res$z_crit), aes(y = y), fill = "orange", alpha = 0.3) +
          geom_vline(xintercept = res$z_crit, color = "blue", linetype = "dotted")
      } else {
        p <- p + 
          geom_area(data = subset(df, x > res$z_crit), aes(y = y), fill = "orange", alpha = 0.3) +
          geom_vline(xintercept = res$z_crit, color = "blue", linetype = "dotted")
      }
    }
    p
  })
}