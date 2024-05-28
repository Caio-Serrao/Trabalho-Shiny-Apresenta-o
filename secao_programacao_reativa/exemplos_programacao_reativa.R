library(shiny)




##################### exemplo 1 - programação reativa ########################


ui <- fluidPage(
  selectInput(
    inputId = "variavel_A",
    label = "Variável A",
    choices = names(mtcars)
  ),
  plotOutput(outputId = "histograma_A"),
  selectInput(
    inputId = "variavel_B",
    label = "Variável B",
    choices = names(mtcars)
  ),
  plotOutput(outputId = "histograma_B")
)

server <- function(input, output, session) {
  
  output$histograma_A <- renderPlot({
    print("Gerando histograma A...")
    hist(mtcars[[input$variavel_A]], main = "Histograma A")
  })
  
  output$histograma_B <- renderPlot({
    print("Gerando histograma B...")
    hist(mtcars[[input$variavel_B]], main = "Histograma B")
  })
  
}

shinyApp(ui, server)









##################### exemplo 2 - programação reativa ########################

ui <- fluidPage(
  "INCRÍVEL DADO",
  sliderInput(
    inputId = "tamanho",
    label = "Selecione o número de lançamentos",
    min = 1,
    max = 1000,
    value = 500
  ),
  plotOutput(outputId = "distribuicao"),
  textOutput(outputId = "frase")
)

server <- function(input, output, session) {
  
  lancamentos <- sample(1:6, input$tamanho, replace = TRUE)
  
  
  output$distribuicao <- renderPlot({
    lancamentos |> 
      table() |> 
      barplot()
  })
  
  output$frase <- renderText({
    contagem <- table(lancamentos)
    mais_freq <- names(contagem[which.max(contagem)])
    num_ap <- contagem[mais_freq]
    paste("O valor mais sorteado foi o", mais_freq, "com ", num_ap, "aparições")
  })
}

shinyApp(ui, server)







##################### exemplo 3 - programação reativa ########################

ui <- fluidPage(
  "INCRÍVEL DADO",
  sliderInput(
    inputId = "tamanho",
    label = "Selecione o número de lançamentos",
    min = 1,
    max = 1000,
    value = 500
  ),
  plotOutput(outputId = "distribuicao"),
  textOutput(outputId = "frase")
)

server <- function(input, output, session) {
  
  # função reactive()
  lancamentos <- reactive({
    sample(1:6, input$tamanho, replace = TRUE)
  })
  
  output$distribuicao <- renderPlot({
    lancamentos() |> 
      table() |> 
      barplot()
  })
  
  output$frase <- renderText({
    contagem <- table(lancamentos())
    mais_freq <- names(contagem[which.max(contagem)])
    num_ap <- contagem[mais_freq]
    paste("O valor mais sorteado foi o", mais_freq, "com ", num_ap, "aparições")
  })
}

shinyApp(ui, server)








##################### exemplo 4 - programação reativa ########################

ui <- fluidPage(
  "INCRÍVEL DADO",
  sliderInput(
    inputId = "tamanho",
    label = "Selecione o número de lançamentos",
    min = 1,
    max = 1000,
    value = 500
  ),
  # função actionButton()
  actionButton(inputId = "botao", label = "Simular"),
  plotOutput(outputId = "distribuicao"),
  textOutput(outputId = "frase"),

)

server <- function(input, output, session) {
  
  # função eventReactive()
  lancamentos <- eventReactive(input$botao, {
    sample(1:6, input$tamanho, replace = TRUE)
  })
  
  output$distribuicao <- renderPlot({
    lancamentos() |> 
      table() |> 
      barplot()
  })
  
  output$frase <- renderText({
    contagem <- table(lancamentos())
    mais_freq <- names(contagem[which.max(contagem)])
    num_ap <- contagem[mais_freq]
    paste("O valor mais sorteado foi o", mais_freq, "com ", num_ap, "aparições")
  })
}

shinyApp(ui, server)


