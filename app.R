library(shiny)
library(ggplot2)

predictor_choices <- c(
  "Weight (1000 lbs)"     = "wt",
  "Horsepower"            = "hp",
  "Displacement (cu.in.)" = "disp",
  "Rear axle ratio"       = "drat"   # added to meet requirements
)

ui <- fluidPage(
  titlePanel("mtcars Explorer: Predicting Fuel Efficiency"),
  sidebarLayout(
    sidebarPanel(
      selectInput("predictor", "Choose a predictor:",
                  choices  = predictor_choices,
                  selected = "wt"),
      # Enhancement: cylinder filter
      # Cylinders is a strong confounder; the predictor-mpg relationship
      # shifts substantially within 4-, 6-, and 8-cyl subgroups. This lets
      # users isolate each group without needing separate plots.
      sliderInput("cyl_filter", "Filter by cylinders:",
                  min = 4, max = 8, value = c(4, 8), step = 2),
      checkboxInput("show_line", "Show regression line", value = TRUE)
    ),
    mainPanel(
      plotOutput("scatter"),
      br(),
      h4("Model Summary"),
      verbatimTextOutput("model_summary")   # full summary(), not just R²
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    mtcars[mtcars$cyl >= input$cyl_filter[1] &
             mtcars$cyl <= input$cyl_filter[2], ]
  })
  
  fit <- reactive({
    req(input$predictor)
    lm(as.formula(paste("mpg ~", input$predictor)),
       data = filtered_data())
  })
  
  output$scatter <- renderPlot({
    df   <- filtered_data()
    xvar <- input$predictor
    p <- ggplot(df, aes(x = .data[[xvar]], y = mpg)) +
      geom_point(aes(color = factor(cyl)), size = 3, alpha = 0.8) +
      scale_color_manual(name   = "Cylinders",
                         values = c("4" = "#009E73",
                                    "6" = "#E69F00",
                                    "8" = "#CC79A7")) +
      labs(x = xvar, y = "MPG") +
      theme_classic(base_size = 13)
    if (input$show_line)
      p <- p + geom_smooth(method = "lm", formula = y ~ x,
                           se = TRUE, color = "black", linewidth = 0.9)
    p
  })
  
  output$model_summary <- renderPrint({
    summary(fit())             # full summary as required
  })
}

shinyApp(ui, server)