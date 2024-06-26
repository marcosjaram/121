library(readxl)
library(car)


# Get the data
ExamenesData <- read_excel("ExamenesData.xlsx")
shapiro.test(Examenes_Data$Rendimiento)

modelo<- lm(Rendimiento ~ Exdig+Femenino+(Exdig*Femenino), data = Examenes_Data)
vif_values <- car::vif(modelo)
print(vif_values)

library(lmtest)
bptest(modelo)

summary(modelo)

# Load Packages
library(shiny)
library(ggplot2)

data2 <- data.frame(
  Estudiante = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"),
  GeneroF = c("1","1","0","0","1","1","1","1","0","0","0","1","1","0","0","1","0","0","1","0","0","0","1","0","0","1","1","1","0","1","0")
  Rendimiento = c("3.3","3.3","3.3","4.2","3.8","4.2","3.8","3.8","4.6","3.3","4.2","5","5","4.6","3.3","1.7","2.9","4.2","2.5","2.9","2.5","2.9","3.8","4.2","2.9","4.2","2.5","2.5","3.8","3.8")
  ExamenDig = c("1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0")
  Edad = c("21",	"19",	"20",	"22",	"23",	"21",	"20",	"22",	"20",	"21",	"20",	"21",	"17",	"21",	"19",	"23",	"18",	"19",	"23",	"19",	"21",	"18",	"22",	"20",	"23",	"21",	"19",	"17",	"18",	"23")
  Estrato = c("5",	"4",	"4",	"4",	"5",	"5",	"5",	"6",	"3",	"6",	"6",	"6",	"6",	"4",	"5",	"4",	"6",	"6",	"6",	"4",	"4",	"4",	"4",	"5",	"3",	"6",	"3",	"4",	"4",	"5")
  
  modelo<- lm(Rendimiento ~ Exdig+Femenino+(Exdig*Femenino)+Edad+Estrato, data = data2)
  
  # Shiny UI
  ui <- fluidPage(
    titlePanel("Regresion Linear Multiple"),
    sidebarLayout(
      sidebarPanel(
        numericInput("Genero_input", "Genero:", value = 1, min = 0, max = 1),
        numericInput("TEX_input", "Tipo de examen:", value = 1, min = 0, max = 1),
        numericInput("Edad_input", "Edad:", value = 23, min = 17, max = 23),
        numericInput("Est_input", "Estrato socioeconomico:", value = 6, min = 3, max = 6),
        actionButton("predict_button", "Predecir"),
        hr(),
        h4("Model Coefficients:"),
        verbatimTextOutput("coefficients_text")
      ),
      mainPanel(
        style = "background-color: #C0B8B8;",  # Ganti warna latar belakang sidebar menjadi pink
        plotOutput("regression_plot"),
        h4("Rendimiento predecido (y) :"),
        verbatimTextOutput("Rendimiento predecido")
      )
    )
  )
  
  # Shiny server
  server <- function(input, output) {
    # Predict function
    predict_knowledge <- function(GeneroF, ExamenDig, Edad, Estrato) {
      new_data <- data.frame(GeneroF = GeneroF, ExamenDig = ExamenDig, Edad = Edad, Estrato = Estrato)
      predicted_sales <- predict(modelo, newdata = new_data)
      return(predicted_knowledge)
    }
    
    # Render combined plot for x2 and x5
    output$regression_plot <- renderPlot({
      ggplot(data, aes(x = x2, y = y)) +
        geom_point(aes(color = "x2"), size = 3) +
        geom_smooth(method = "lm", formula = Rendimiento ~ Exdig+Femenino+(Exdig*Femenino)+Edad+Estrato, se = FALSE) +
        geom_point(aes(x = x5, color = "x5"), size = 3) +
        labs(title = "Multiple Linear Regression",
             x = "x2 and x5",
             y = "Rendimiento (Y)",
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