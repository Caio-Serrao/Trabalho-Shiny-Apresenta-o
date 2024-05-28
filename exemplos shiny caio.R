

############################### Tipos de Input #################################

#### selectInput() input

library(shiny)

ui <- fluidPage(
  titlePanel("Exemplo de Shiny App com selectInput"),
  sidebarLayout(
    # Sidebar com o selectInput
    sidebarPanel(
      selectInput("variable", "Escolha uma variavel:",
                  choices = c("Cilindros" = "cyl",
                              "Transmissao" = "am",
                              "Marchas" = "gear"),
                  selected = "cyl")
    ),
    
    # Main panel para exibir a tabela
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

server <- function(input, output) {
  output$dataTable <- renderTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
}

#  Executando o App Shiny
shinyApp(ui = ui, server = server)

#### checkBoxGroupInput() input

# Definir a interface do usuário (UI)
ui <- fluidPage(
  # Título do aplicativo
  titlePanel("Exemplo de Shiny App com checkboxGroupInput"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "variables",
                         label = "Escolha as variaveis:",
                         choices = c("Cilindros" = "cyl",
                                     "Transmissao" = "am",
                                     "Marchas" = "gear"),
                         selected = "cyl")
    ),
    
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

server <- function(input, output) {
  output$dataTable <- renderTable({
    req(input$variables)  # Requer que pelo menos uma variável seja selecionada
    mtcars[, c("mpg", input$variables), drop = FALSE]
  }, rownames = TRUE)
}

#  Executando o App Shiny
shinyApp(ui = ui, server = server)



## actionButton() input


# Definir a interface do usuário (UI)
ui <- fluidPage(
  # Título do aplicativo
  titlePanel("Exemplo de Shiny App com actionButton"),
  
  # Layout com um sidebar e main panel
  sidebarLayout(
    # Sidebar com checkboxGroupInput e actionButton
    sidebarPanel(
      checkboxGroupInput(inputId = "variables",
                         label = "Escolha as variaveis:",
                         choices = c("Cilindros" = "cyl",
                                     "Transmissao" = "am",
                                     "Marchas" = "gear"),
                         selected = "cyl"),
      actionButton("updateButton", "Atualizar Tabela")
    ),
    
    # Main panel para exibir a tabela
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

# Definir a lógica do servidor
server <- function(input, output, session) {
  # Reatividade isolada para atualizar a tabela somente quando o botão é clicado
  dataTable <- eventReactive(input$updateButton, {
    req(input$variables)  # Requer que pelo menos uma variável seja selecionada
    mtcars[, c("mpg", input$variables), drop = FALSE]
  })
  
  # Renderizar a tabela
  output$dataTable <- renderTable({
    dataTable()
  }, rownames = TRUE)
}

# Rodar o aplicativo Shiny
shinyApp(ui = ui, server = server)



#### sliderInput()


ui <- fluidPage(
  titlePanel("Exemplo de Shiny App com sliderInput"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "mpgRange", 
                  label = "Escolha o intervalo de mpg:",
                  min = min(mtcars$mpg), 
                  max = max(mtcars$mpg), 
                  value = c(min(mtcars$mpg), max(mtcars$mpg)))
    ),
    
    # Main panel para exibir a tabela
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

server <- function(input, output) {
  output$dataTable <- renderTable({
    mtcars[mtcars$mpg >= input$mpgRange[1] & mtcars$mpg <= input$mpgRange[2], ]
  }, rownames = TRUE)
}

# Executando o App Shiny
shinyApp(ui = ui, server = server)

######################## Tipos de Output ###############################

#### plotOutput()

ui <- fluidPage(
  titlePanel("Exemplo de Shiny App com Gráfico como Output"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mpgRange", 
                  "Escolha o intervalo de mpg:",
                  min = min(mtcars$mpg), 
                  max = max(mtcars$mpg), 
                  value = c(min(mtcars$mpg), max(mtcars$mpg)))
    ),
    
    # Main panel para exibir o grafico
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)


server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    filteredData <- mtcars[mtcars$mpg >= input$mpgRange[1] & mtcars$mpg <= input$mpgRange[2], ]
    
    ggplot(filteredData, aes(x = wt, y = mpg)) +
      geom_point() +
      labs(title = "Grafico de Dispersao de MPG vs Peso",
           x = "Peso (1000 lbs)",
           y = "Milhas por Galao (mpg)") +
      theme_minimal()
  })
}

# Executando o App Shiny
shinyApp(ui = ui, server = server)
