library(shiny)
library(shinythemes)
library(rsconnect)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    div("Statistical Hypothesis Test Calculator", 
        style = "color: #2C3E50; font-weight: bold; padding-bottom: 10px")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #F8F9FA; border-radius: 8px; padding: 20px;",
      
      wellPanel(
        style = "background-color: white;",
        selectInput("test_type", "Select Test Type:",
                    choices = c("Single Mean", "Single Proportion",
                                "Difference of Means", "Difference of Proportions"),
                    width = "100%")
      ),
      
      wellPanel(
        style = "background-color: white; margin-top: 15px;",
        radioButtons("test_tails", "Test Type:",
                     choices = c("Two-tailed", "One-tailed"), 
                     inline = TRUE,
                     selected = "Two-tailed")
      ),
      
      conditionalPanel(
        condition = "input.test_tails == 'One-tailed'",
        wellPanel(
          style = "background-color: white; margin-top: 10px;",
          radioButtons("tail_direction", "Direction:",
                       choices = c("Left-tailed", "Right-tailed"), 
                       inline = TRUE)
        )
      ),
      
      uiOutput("dynamic_inputs"),
      
      wellPanel(
        style = "background-color: white; margin-top: 15px;",
        numericInput("alpha", "Significance Level (α):",
                     value = 0.05, min = 0.001, max = 0.2, 
                     step = 0.005, width = "100%")
      ),
      
      actionButton("calculate", "Calculate",
                   style = "background-color: #18BC9C; color: white; 
                            width: 100%; margin-top: 20px;
                            border: none; border-radius: 4px;
                            padding: 10px; font-weight: bold;")
    ),
    
    mainPanel(
      style = "padding-left: 30px;",
      
      div(h3("Test Results:", 
             style = "color: #2C3E50; margin-bottom: 20px;"),
          wellPanel(
            style = "background-color: white; border-radius: 8px;
                     box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            verbatimTextOutput("results")
          )
      ),
      
      div(h3("Distribution Plot:", 
             style = "color: #2C3E50; margin: 20px 0;"),
          wellPanel(
            style = "background-color: white; border-radius: 8px;
                     padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            plotOutput("dist_plot", height = "400px")
          )
      )
    )
  ),
  
  tags$style(HTML("
    .shiny-input-container:not(.shiny-input-container-inline) {
      width: 100%;
    }
    .control-label {
      font-weight: 500 !important;
      color: #2C3E50 !important;
    }
    .well {
      margin-bottom: 10px !important;
    }
  "))
)