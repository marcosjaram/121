# Load p# Load Packages
library(shiny)
library(ggplot2)

# Data
data <- data.frame(
  month = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5.0, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Model multiple linear regression
model <- lm(y ~ x2 + x5, data = data)

# Shiny UI
ui <- fluidPage(
  titlePanel("Multiple Linear Regression"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x2_input", "Enter x2:", value = 10000),
      numericInput("x5_input", "Enter x5:", value = 25000),
      actionButton("predict_button", "Predict"),
      hr(),
      h4("Model Coefficients:"),
      verbatimTextOutput("coefficients_text")
    ),
    mainPanel(
      style = "background-color: #C0B8B8;",  # Ganti warna latar belakang sidebar menjadi pink
      plotOutput("regression_plot"),
      h4("Predicted Sales (Y):"),
      verbatimTextOutput("predicted_sales")
    )
  )
)

# Shiny server
server <- function(input, output) {
  # Predict function
  predict_sales <- function(x2, x5) {
    new_data <- data.frame(x2 = x2, x5 = x5)
    predicted_sales <- predict(model, newdata = new_data)
    return(predicted_sales)
  }
  
  # Render combined plot for x2 and x5
  output$regression_plot <- renderPlot({
    ggplot(data, aes(x = x2, y = y)) +
      geom_point(aes(color = "x2"), size = 3) +
      geom_smooth(method = "lm", formula = y ~ x2 + x5, se = FALSE) +
      geom_point(aes(x = x5, color = "x5"), size = 3) +
      labs(title = "Multiple Linear Regression",
           x = "x2 and x5",
           y = "Sales (Y)",
           color = "Variable") +
      theme_minimal()
  })
  
  # Render coefficients
  output$coefficients_text <- renderText({
    paste("Intercept:", round(coef(model)[1], 4),
          "\nx2 Coefficient:", round(coef(model)[2], 4),
          "\nx5 Coefficient:", round(coef(model)[3], 4))
  })
  
  # Event handler for prediction button
  observeEvent(input$predict_button, {
    x2_input <- input$x2_input
    x5_input <- input$x5_input
    predicted_sales <- predict_sales(x2_input, x5_input)
    output$predicted_sales <- renderText({
      paste("Predicted Sales (Y):", round(predicted_sales, 2))
    })
  })
}

# Run Shiny app
shinyApp(ui, server)