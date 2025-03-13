library(shiny)
library(ggplot2)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Prueba de Hipótesis para la Media"),
  sidebarLayout(
    sidebarPanel(
      numericInput("media_p", "Media Poblacional (μ₀):", value = 58),
      numericInput("media_m", "Media Muestral (x̄):", value = 57),
      numericInput("desv", "Desviación Estándar (s):", value = 2),
      numericInput("n", "Tamaño de Muestra (n):", value = 36),
      selectInput("tipo_prueba", "Tipo de Prueba:",
                  choices = c("Una cola izquierda" = "izq",
                              "Una cola derecha" = "der",
                              "Dos colas" = "dos")),
      numericInput("alfa", "Nivel de Significancia (α):", 
                   value = 0.05, min = 0, max = 1, step = 0.01),
      actionButton("calcular", "Realizar Prueba")
    ),
    
    mainPanel(
      h3("Resultados de la Prueba:"),
      verbatimTextOutput("resultados"),
      plotOutput("grafico")
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  
  datos <- eventReactive(input$calcular, {
    # Calcular error estándar
    error_estandar <- input$desv / sqrt(input$n)
    
    # Calcular Z-score
    z_calculado <- (input$media_m - input$media_p) / error_estandar
    
    # Determinar Z crítico
    alfa <- input$alfa
    if(input$tipo_prueba == "izq") {
      z_critico <- qnorm(alfa)
    } else if(input$tipo_prueba == "der") {
      z_critico <- qnorm(1 - alfa)
    } else {
      z_critico <- qnorm(c(alfa/2, 1 - alfa/2))
    }
    
    # Decisión
    decision <- if(input$tipo_prueba == "izq" && z_calculado < z_critico ||
                   input$tipo_prueba == "der" && z_calculado > z_critico ||
                   input$tipo_prueba == "dos" && (abs(z_calculado) > abs(z_critico[2]))) {
      "Rechazar Ho"
    } else {
      "No rechazar Ho"
    }
    
    list(
      z_calculado = z_calculado,
      z_critico = z_critico,
      error_estandar = error_estandar,
      decision = decision
    )
  })
  
  output$resultados <- renderPrint({
    res <- datos()
    cat("Z calculado:", round(res$z_calculado, 4), "\n")
    cat("Z crítico:", round(res$z_critico, 4), "\n")
    cat("Error estándar:", round(res$error_estandar, 4), "\n")
    cat("Decisión:", res$decision)
  })
  
  output$grafico <- renderPlot({
    res <- datos()
    
    # Crear data frame para el gráfico
    df <- data.frame(x = seq(-4, 4, length.out = 200))
    df$y <- dnorm(df$x)
    
    # Configurar el gráfico base
    p <- ggplot(df, aes(x, y)) +
      geom_line(color = "steelblue") +
      geom_vline(xintercept = res$z_calculado, color = "red", linetype = "dashed") +
      labs(title = "Distribución Normal Estándar",
           x = "Z-score",
           y = "Densidad") +
      theme_minimal()
    
    # Añadir regiones críticas
    if(input$tipo_prueba == "izq") {
      p <- p + geom_area(data = subset(df, x < res$z_critico),
                         aes(y = y), fill = "orange", alpha = 0.3)
    } else if(input$tipo_prueba == "der") {
      p <- p + geom_area(data = subset(df, x > res$z_critico),
                         aes(y = y), fill = "orange", alpha = 0.3)
    } else {
      p <- p + geom_area(data = subset(df, x < res$z_critico[1]),
                         aes(y = y), fill = "orange", alpha = 0.3) +
        geom_area(data = subset(df, x > res$z_critico[2]),
                  aes(y = y), fill = "orange", alpha = 0.3)
    }
    
    p
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
