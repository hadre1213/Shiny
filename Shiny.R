library(shiny)
library("RColorBrewer")

data <- read.csv("TitanicCleaned.csv",
                 header = T,
                 sep = '\t',
                 dec = '.')
ui <- fluidPage(titlePanel("Titanic"),
                
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(
                      "radio2",
                      label = h3("Axis X category"),
                      choices = list(
                        "Sex" = 1,
                        "Pclass" = 2,
                        "Survived" = 3,
                        "Age" = 4,
                        "Siblings" = 5
                      ),
                      selected = 1
                    ),
                    radioButtons(
                      "radio",
                      label = h3("Fill category"),
                      choices = list(
                        "Sex" = 1,
                        "Pclass" = 2,
                        "Survived" = 3,
                        "Age" = 4,
                        "Siblings" = 5
                      ),
                      selected = 1
                    )
                    
                    
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "distPlot"),
                    plotOutput(outputId = "classificationPlot")
                  )
                ))

server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (input$radio == 1) {
      x <- data$Sex
    }
    else if (input$radio == 2) {
      x <- data$Pclass
    }
    else if (input$radio == 3) {
      x <- data$Survived
    }
    else if (input$radio == 4) {
      x <- data$Age
    }
    else if (input$radio == 5) {
      x <- data$SibSp
    }
    
    if (input$radio2 == 1) {
      y <- data$Sex
    }
    else if (input$radio2 == 2) {
      y <- data$Pclass
    }
    else if (input$radio2 == 3) {
      y <- data$Survived
    }
    else if (input$radio2 == 4) {
      y <- data$Age
    }
    else if (input$radio2 == 5) {
      y <- data$SibSp
    }
    survivors <- table(x, y)
    barplot(survivors, col = brewer.pal(n = 3, name = "RdBu"))
    legend("topleft",  fill = brewer.pal(n = 3, name = "RdBu"), rownames(survivors))
    
  })
  
  output$classificationPlot <- renderPlot({
    library(rpart)
    library(rpart.plot)
    library(randomForest)
    wyb <- c(sample(1:450, 446))
    titanic_train <- data[wyb,]
    titanic_test <- data[wyb,]
    rpart.control(maxdepth = 3)
    result <-
      rpart(Survived ~ Age + Pclass, data = titanic_train, method = "class")
    rpart.plot(result)
  })
}

shinyApp(ui = ui, server = server)