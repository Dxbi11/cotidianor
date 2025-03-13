library(shiny)
library(ggplot2)

server <- function(input, output, session) {
  observeEvent(input$calc, {
    output$result <- renderPrint({
      alpha <- input$alpha
      test_side <- input$test_side
      unilateral_direction <- if(test_side == "Unilateral") input$unilateral_direccion else NA
      
      # Inicializar variables
      z <- NA
      z_crit <- NA
      p_value <- NA
      result_text <- ""
      
      # Cálculos para Media
      if(input$test_type == "Media"){
        xbar <- input$xbar
        mu0 <- input$mu0
        sigma <- input$sigma
        n <- input$n
        SE <- sigma / sqrt(n)
        z <- (xbar - mu0) / SE
        
        if(test_side == "Bilateral"){
          p_value <- 2 * (1 - pnorm(abs(z)))
          z_crit <- qnorm(1 - alpha/2)
        } else {
          if(unilateral_direction == "Izquierda"){
            p_value <- pnorm(z)
            z_crit <- qnorm(alpha)
          } else if(unilateral_direction == "Derecha"){
            p_value <- 1 - pnorm(z)
            z_crit <- qnorm(1 - alpha)
          }
        }
        result_text <- paste0("Prueba para la Media: \n",
                              "Z calculado = ", round(z, 3), "\n",
                              "Z crítico = ±", round(z_crit, 3), "\n",
                              "Valor-p = ", round(p_value, 4))
      }
      
      # Cálculos para Proporción
      else if(input$test_type == "Proporción"){
        p_hat <- input$p_hat
        n <- input$n_prop
        p0 <- input$p0
        SE <- sqrt(p0 * (1 - p0) / n)
        z <- (p_hat - p0) / SE
        
        if(test_side == "Bilateral"){
          p_value <- 2 * (1 - pnorm(abs(z)))
          z_crit <- qnorm(1 - alpha/2)
        } else {
          if(unilateral_direction == "Izquierda"){
            p_value <- pnorm(z)
            z_crit <- qnorm(alpha)
          } else if(unilateral_direction == "Derecha"){
            p_value <- 1 - pnorm(z)
            z_crit <- qnorm(1 - alpha)
          }
        }
        result_text <- paste0("Prueba para Proporción: \n",
                              "Z calculado = ", round(z, 3), "\n",
                              "Z crítico = ±", round(z_crit, 3), "\n",
                              "Valor-p = ", round(p_value, 4))
      }
      
      # Cálculos para Diferencia de Medias
      else if(input$test_type == "Diferencia de Medias"){
        xbar1 <- input$xbar1
        sigma1 <- input$sigma1
        n1 <- input$n1
        xbar2 <- input$xbar2
        sigma2 <- input$sigma2
        n2 <- input$n2
        SE <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
        z <- (xbar1 - xbar2) / SE
        
        if(test_side == "Bilateral"){
          p_value <- 2 * (1 - pnorm(abs(z)))
          z_crit <- qnorm(1 - alpha/2)
        } else {
          if(unilateral_direction == "Izquierda"){
            p_value <- pnorm(z)
            z_crit <- qnorm(alpha)
          } else if(unilateral_direction == "Derecha"){
            p_value <- 1 - pnorm(z)
            z_crit <- qnorm(1 - alpha)
          }
        }
        result_text <- paste0("Prueba para Diferencia de Medias: \n",
                              "Z calculado = ", round(z, 3), "\n",
                              "Z crítico = ±", round(z_crit, 3), "\n",
                              "Valor-p = ", round(p_value, 4))
      }
      
      cat(result_text)
    })
  })
}