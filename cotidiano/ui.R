library(shiny)

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