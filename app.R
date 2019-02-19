library(shiny)
library(dplyr)

source("MRC_all_shiny.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("MRC Power"),
  
  # 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "predictors",
                  label   = "Number of Predictors: ",
                  min     = 1,
                  max     = 3,
                  value   = 1,
                  step    = 1),
      numericInput(inputId = "n",
                   label   = "Sample Size:",
                   min     = 1,
                   max     = Inf,
                   value   = 100,
                   step    = 1),
      sliderInput(inputId = "alpha",
                  label   = "Alpha: ",
                  min     = .001,
                  max     = 1,
                  value   = 0.05),
      numericInput(inputId = "nruns",
                   label   = "Number of Iterations",
                   min     = 1,
                   max     = 10000,
                   value   = 10000),
      sliderInput(inputId  = "ry1",
                  label   = "Correlation between Y and 1st Predictor",
                  min     = 0,
                  max     = 1,
                  value   = 0.5,
                  step    = 0.01),
      sliderInput(inputId  = "ry2",
                  label   = "Correlation between Y and 2nd Predictor",
                  min     = 0,
                  max     = 1,
                  value   = 0.5,
                  step    = 0.01),
      sliderInput(inputId  = "ry3",
                  label   = "Correlation between Y and 3rd Predictor",
                  min     = 0,
                  max     = 1,
                  value   = 0.5,
                  step    = 0.01),
      sliderInput(inputId  = "r12",
                  label   = "Correlation between 1st and 2nd Predictor",
                  min     = 0,
                  max     = 1,
                  value   = 0.5,
                  step    = 0.01),
      sliderInput(inputId  = "r13",
                  label   = "Correlation between 1st and 3rd Predictor",
                  min     = 0,
                  max     = 1,
                  value   = 0.5,
                  step    = 0.01),
      sliderInput(inputId  = "r23",
                  label   = "Correlation between 2nd and 3rd Predictor",
                  min     = 0,
                  max     = 1,
                  value   = 0.5,
                  step    = 0.01),
      numericInput(inputId = "my",
                   label   = "Mean of Y",
                   min     = 0,
                   max     = Inf,
                   value   = 0,
                   step    = 5),
      numericInput(inputId = "m1",
                   label   = "Mean of 1st Predictor",
                   min     = 0,
                   max     = Inf,
                   value   = 0,
                   step    = 5),
      numericInput(inputId = "m2",
                   label   = "Mean of 2nd Predictor",
                   min     = 0,
                   max     = Inf,
                   value   = 0,
                   step    = 5),
      numericInput(inputId = "m3",
                   label   = "Mean of 3rd Predictor",
                   min     = 0,
                   max     = Inf,
                   value   = 0,
                   step    = 5),
      numericInput(inputId = "sy",
                   label   = "SD of Y",
                   min     = 0,
                   max     = Inf,
                   value   = 1,
                   step    = 5),
      numericInput(inputId = "s1",
                   label   = "SD of 1st Predictor",
                   min     = 0,
                   max     = Inf,
                   value   = 1,
                   step    = 5),
      numericInput(inputId = "s2",
                   label   = "SD of 2nd Predictor",
                   min     = 0,
                   max     = Inf,
                   value   = 1,
                   step    = 5),
      numericInput(inputId = "s3",
                   label   = "SD of 3rd Predictor",
                   min     = 1,
                   max     = Inf,
                   value   = 1,
                   step    = 5),
      
      
      actionButton(inputId = "run_function",
                   label   = "Calculate"),
      
      # SET WIDTH FOR SIDEBAR PANEL
      width = 7
    ),
    
    mainPanel(
      verbatimTextOutput(outputId = "power_results"),
      
      # SET WIDTH FOR MAIN PANEL
      width = 5
    )
  )
)

# Define server logic to print output returned by MRC_all()
server <- function(input, output) {
  
  power_vals <- reactive({
    MRC_all(ry1 = input$ry1, ry2 = input$ry2, ry3 = input$ry3, r12 = input$r12, r13 = input$r13, r23 = input$r23, 
            my = input$my, m1 = input$m1, m2 = input$m2, m3 = input$m3, sy = input$sy, s1 = input$s1, s2 = input$s2, s3 = input$s3, 
            n = input$n, alpha = input$alpha, nruns = input$nruns, predictors = input$predictors)
  })
  
  output$power_results <- eventReactive(input$run_function, {
     
    
    
     # 1 predictor
     if (input$predictors == 1){
      paste0(power_vals()$n_print, "\n",
             power_vals()$power_r2_print, "\n",
             power_vals()$power_b1_print, "\n", 
             power_vals()$power_none_print, "\n", 
             power_vals()$power_one_print, "\n")
    } else if (input$predictors == 2){
      paste0(power_vals()$n_print, "\n",
             power_vals()$power_r2_print, "\n",
             power_vals()$power_b1_print, "\n",
             power_vals()$power_b2_print, "\n", 
             power_vals()$power_none_print, "\n", 
             power_vals()$power_one_print, "\n", 
             power_vals()$power_all_print)
    } else if (input$predictors == 3){
      paste0(power_vals()$n_print, "\n",
             power_vals()$power_r2_print, "\n",
             power_vals()$power_b1_print, "\n",
             power_vals()$power_b2_print, "\n", 
             power_vals()$power_b3_print, "\n", 
             power_vals()$power_none_print, "\n", 
             power_vals()$power_one_print, "\n", 
             power_vals()$power_two_print, "\n", 
             power_vals()$power_all_print)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

