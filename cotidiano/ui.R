library(shiny)

ui <- fluidPage(
  titlePanel("Pruebas de Hipótesis Interactivas con visualización de Curva Normal"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("test_type", "Seleccione el tipo de prueba:",
                  choices = c("Media", "Proporción", "Diferencia de Medias", "Diferencia de Proporciones")),
      
      radioButtons("test_side", "Tipo de prueba:",
                   choices = c("Bilateral", "Unilateral"), inline = TRUE),
      
      conditionalPanel(
        condition = "input.test_side == 'Unilateral'",
        radioButtons("unilateral_direccion", "Dirección de la prueba unilateral:",
                     choices = c("Izquierda", "Derecha"), inline = TRUE)
      ),
      
      # Panel para Media
      conditionalPanel(
        condition = "input.test_type == 'Media'",
        numericInput("mu0", "Media poblacional (μ₀):", value = 50),
        numericInput("xbar", "Media muestral (x̄):", value = 52),
        numericInput("sigma", "Desviación estándar (σ):", value = 8),
        numericInput("n", "Tamaño de muestra (n):", value = 30)
      ),
      
      # Panel para Proporción
      conditionalPanel(
        condition = "input.test_type == 'Proporción'",
        numericInput("p_hat", "Proporción muestral (p̂):", value = 0.6, min = 0, max = 1, step = 0.01),
        numericInput("n_prop", "Tamaño de muestra (n):", value = 100),
        numericInput("p0", "Proporción poblacional (p₀):", value = 0.5, min = 0, max = 1, step = 0.01)
      ),
      
      # Panel para Diferencia de Medias
      conditionalPanel(
        condition = "input.test_type == 'Diferencia de Medias'",
        h4("Grupo 1"),
        numericInput("xbar1", "Media muestral (x̄₁):", value = 100),
        numericInput("sigma1", "Desviación estándar (σ₁):", value = 10),
        numericInput("n1", "Tamaño de muestra (n₁):", value = 30),
        h4("Grupo 2"),
        numericInput("xbar2", "Media muestral (x̄₂):", value = 95),
        numericInput("sigma2", "Desviación estándar (σ₂):", value = 12),
        numericInput("n2", "Tamaño de muestra (n₂):", value = 35)
      ),
      
      # Panel para Diferencia de Proporciones
      conditionalPanel(
        condition = "input.test_type == 'Diferencia de Proporciones'",
        h4("Grupo 1"),
        numericInput("p1", "Proporción muestral (p̂₁):", value = 0.8, min = 0, max = 1, step = 0.01),
        numericInput("n1_prop", "Tamaño de muestra (n₁):", value = 100),
        h4("Grupo 2"),
        numericInput("p2", "Proporción muestral (p̂₂):", value = 0.7, min = 0, max = 1, step = 0.01),
        numericInput("n2_prop", "Tamaño de muestra (n₂):", value = 100)
      ),
      
      numericInput("alpha", "Nivel de significancia (α):", 
                   value = 0.05, min = 0, max = 1, step = 0.01),
      actionButton("calc", "Calcular")
    ),
    
    mainPanel(
      h3("Resultados"),
      verbatimTextOutput("result"),
      plotOutput("curvePlot")
    )
  )
)