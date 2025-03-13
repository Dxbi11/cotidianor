library(shiny)

ui <- fluidPage(
  titlePanel("Statistical Hypothesis Test Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("test_type", "Select Test Type:",
                  choices = c("Single Mean", "Single Proportion",
                              "Difference of Means", "Difference of Proportions")),
      
      radioButtons("test_tails", "Test Type:",
                   choices = c("Two-tailed", "One-tailed"), inline = TRUE),
      
      conditionalPanel(
        condition = "input.test_tails == 'One-tailed'",
        radioButtons("tail_direction", "Direction:",
                     choices = c("Left-tailed", "Right-tailed"), inline = TRUE)
      ),
      
      # Dynamic inputs based on test type
      uiOutput("dynamic_inputs"),
      
      numericInput("alpha", "Significance Level (α):",
                   value = 0.05, min = 0.001, max = 0.2, step = 0.005),
      
      actionButton("calculate", "Calculate")
    ),
    
    mainPanel(
      h3("Test Results:"),
      verbatimTextOutput("results"),
      plotOutput("dist_plot")
    )
  )
)