
library(shiny)
library(ggplot2)


### Alguns Tipos de Input

## actionButtonInput()

if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("obs", "Número de observações", 0, 1000, 500),
    actionButton("goButton", "Go!"),
    plotOutput("distPlot")
  )
  
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      # Criar uma dependência em input$goButton. Isso será executado uma vez inicialmente,
      # porque o valor muda de NULL para 0.
      input$goButton
      
      # Usar isolate() para evitar dependência de input$obs
      dist <- isolate(rnorm(input$obs))
      hist(dist)
    })
  }
  
  shinyApp(ui, server)
}

## checkBoxGroupInput() widget

if (interactive()){
  
  ui <- fluidPage(
    checkboxGroupInput("variable", "Variáveis para mostrar:",
                       c("Cilindros" = "cyl",
                         "Transmissão" = "am",
                         "Marchas" = "gear")),
    tableOutput("data")
  )
  
  server <- function(input, output) {
    output$data <- renderTable({
      mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
  
  shinyApp(ui, server)
}

## radioButtons() widget

## Executar exemplos apenas em sessões interativas do R
if (interactive()) {
  
  ui <- fluidPage(
    radioButtons("dist", "Tipo de distribuição:",
                 c("Normal" = "norm",
                   "Uniforme" = "unif",
                   "Log-normal" = "lnorm",
                   "Exponencial" = "exp")),
    plotOutput("distPlot")
  )
  
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     lnorm = rlnorm,
                     exp = rexp,
                     rnorm)
      
      hist(dist(500))
    })
  }
  
  shinyApp(ui, server)
}

## selectInput() widget

ui <- fluidPage(
  selectInput("variable", "Variable:",
              c("Cylinders" = "cyl",
                "Transmission" = "am",
                "Gears" = "gear")),
  tableOutput("data")
)

server <- function(input, output) {
  output$data <- renderTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
}

shinyApp(ui, server)

# dateInput() Widget  

## Executar exemplos apenas em sessões interativas do R
if (interactive()) {
  
  ui <- fluidPage(
    dateInput("date1", "Data:", value = "2012-02-29"),
    
    # O valor padrão é a data no fuso horário do cliente
    dateInput("date2", "Data:"),
    
    # O valor é sempre yyyy-mm-dd, mesmo se o formato de exibição for diferente
    dateInput("date3", "Data:", value = "2012-02-29", format = "mm/dd/yy"),
    
    # Passar um objeto Date
    dateInput("date4", "Data:", value = Sys.Date()-10),
    
    # Usar idioma diferente e primeiro dia da semana diferente
    dateInput("date5", "Data:",
              language = "ru",
              weekstart = 1),
    
    # Iniciar com a visão de década em vez da visão padrão de mês
    dateInput("date6", "Data:",
              startview = "decade")
  )
  
  shinyApp(ui, server = function(input, output) { })
}

## dateRangeInput()

## Executar exemplos apenas em sessões interativas do R
if (interactive()) {
  
  ui <- fluidPage(
    dateRangeInput("daterange1", "Intervalo de datas:",
                   start = "2001-01-01",
                   end   = "2010-12-31"),
    
    # O início e fim padrão é a data atual no fuso horário do cliente
    dateRangeInput("daterange2", "Intervalo de datas:"),
    
    # O início e fim são sempre especificados em yyyy-mm-dd, mesmo se o formato de exibição for diferente
    dateRangeInput("daterange3", "Intervalo de datas:",
                   start  = "2001-01-01",
                   end    = "2010-12-31",
                   min    = "2001-01-01",
                   max    = "2012-12-21",
                   format = "mm/dd/yy",
                   separator = " - "),
    
    # Passar objetos Date
    dateRangeInput("daterange4", "Intervalo de datas:",
                   start = Sys.Date()-10,
                   end = Sys.Date()+10),
    
    # Usar idioma diferente e primeiro dia da semana diferente
    dateRangeInput("daterange5", "Intervalo de datas:",
                   language = "de",
                   weekstart = 1),
    
    # Iniciar com a visão de década em vez da visão padrão de mês
    dateRangeInput("daterange6", "Intervalo de datas:",
                   startview = "decade")
  )
  
  shinyApp(ui, server = function(input, output) { })
}

## fileInput

## Executar exemplos apenas em sessões interativas do R
if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Escolha um arquivo CSV",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Cabeçalho", TRUE)
      ),
      mainPanel(
        tableOutput("contents")
      )
    )
  )
  
  server <- function(input, output) {
    output$contents <- renderTable({
      # input$file1 será NULL inicialmente. Depois que o usuário selecionar
      # e carregar um arquivo, será um data frame com as colunas 'name',
      # 'size', 'type' e 'datapath'. A coluna 'datapath'
      # conterá os nomes dos arquivos locais onde os dados podem ser encontrados.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header)
    })
  }
  
  shinyApp(ui, server)
}

## numericInput() widget

## Executar exemplos apenas em sessões interativas do R
if (interactive()) {
  
  ui <- fluidPage(
    numericInput("obs", "Observações:", 10, min = 1, max = 100),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$obs })
  }
  shinyApp(ui, server)
}

## SliderInput() widget

## Executar exemplos apenas em sessões interativas do R
if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("obs", "Número de observações:",
                min = 0, max = 1000, value = 500
    ),
    plotOutput("distPlot")
  )
  
  # Lógica do servidor
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }
  
  # Aplicativo completo com componentes UI e servidor
  shinyApp(ui, server)
}

## passwordInput() widget

if (interactive()) {
  
  ui <- fluidPage(
    passwordInput("password", "Senha:"),
    actionButton("go", "Go"),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({
      req(input$go)
      isolate(input$password)
    })
  }
  shinyApp(ui, server)
}

### Exemplo de App que reúne vários dos widgets mencionados, além
# de recursos adicionais como expressões reativas

# Definir UI para o aplicativo visualizador de conjunto de dados ----
ui <- fluidPage(
  
  # Título do aplicativo ----
  titlePanel("Mais Widgets"),
  
  # Layout de barra lateral com definições de entrada e saída ----
  sidebarLayout(
    
    # Painel lateral para entradas ----
    sidebarPanel(
      
      # Entrada: Selecionar um conjunto de dados ----
      selectInput("dataset", "Escolha um conjunto de dados:",
                  choices = c("rock", "pressure", "cars")),
      
      # Entrada: Especificar o número de observações a serem visualizadas ----
      numericInput("obs", "Número de observações a visualizar:", 10),
      
      # Incluir texto de esclarecimento ----
      helpText("Nota: enquanto a visualização dos dados mostrará apenas o",
               "número especificado de observações, o resumo ainda será baseado",
               "no conjunto de dados completo."),
      
      # Entrada: actionButton() para adiar a renderização da saída ----
      # até que o usuário clique explicitamente no botão (em vez de
      # fazer isso imediatamente quando as entradas mudam). Isso é útil se
      # os cálculos necessários para renderizar a saída forem excessivamente
      # demorados.
      actionButton("update", "Atualizar Visualização")
      
    ),
    
    # Painel principal para exibir saídas ----
    mainPanel(
      
      # Saída: Cabeçalho + resumo da distribuição ----
      h4("Resumo"),
      verbatimTextOutput("summary"),
      
      # Saída: Cabeçalho + tabela de distribuição ----
      h4("Observações"),
      tableOutput("view")
    )
    
  )
)

# Definir a lógica do servidor para resumir e visualizar o conjunto de dados selecionado ----
server <- function(input, output) {
  
  # Retornar o conjunto de dados solicitado ----
  # Note que usamos eventReactive() aqui, que cria uma expressão reativa
  # que será inválida quando input$update for clicado
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  }, ignoreNULL = FALSE)
  
  # Gerar um resumo do conjunto de dados ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Mostrar o número solicitado de observações ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

# Criar o aplicativo Shiny ----
shinyApp(ui, server)


### Exemplo de App que demonstra computacionalmente o Teorema do Limite Central

ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)
server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}

shinyApp(ui, server)

## Criando App que permita a implementação de múltiplas abas

ui <- fluidPage(
  titlePanel('Titulo'),
  sidebarLayout(
    sidebarPanel(
      textOutput('Aba')
    ),
    mainPanel(
      tabsetPanel(
        id = 'aba_atual',
        tabPanel('1', 'Aba 1'),
        tabPanel('2', 'Aba 2'),
        tabPanel('3', 'Aba 3'))
    )
  )
)

server <- function(input, output, session) {
  output$Aba <- renderText({
    paste0("Aba Atual: ", input$aba_atual)
  })
}

shinyApp(ui, server)


### Usando temas do bslib

ui <- fluidPage(
  theme = bslib::bs_theme()
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
