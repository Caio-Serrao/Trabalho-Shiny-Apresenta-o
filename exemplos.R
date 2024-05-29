pacman::p_load("shiny",
               "tidyverse",
               "babynames",
               "DT",
               "shinythemes",
               "gapminder",
               "shinydashboard")

######################## Estrutura básica ###################################

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)

######################## Definição UI ###################################

ui <- fluidPage(
  titlePanel("Aplicativo Teste"),
  sidebarLayout(
    sidebarPanel(
      textInput("nome", "Digite seu nome")
    ),
    mainPanel(
      textOutput('nome_usuario')
    )
  )
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)

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


#### Text Input() com actionButton()

ui <- fluidPage(
  # Titulo do aplicativo
  titlePanel("Popularidade de um nome ao longo dos Anos"),
  
  # Layout com um sidebar e main panel
  sidebarLayout(
    # Sidebar com o textInput e actionButton
    sidebarPanel(
      textInput("nameInput", 
                "Digite um nome de bebe:", 
                value = "John"),
      actionButton("updateButton", "Atualizar Grafico")
    ),
    
    # Main panel para exibir o grafico
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Definir a logica do servidor
server <- function(input, output) {
  # Usar reactiveVal para armazenar o nome de bebe atual
  nameToPlot <- reactiveVal("John")
  
  # Atualizar o nome quando o botao for clicado
  observeEvent(input$updateButton, {
    nameToPlot(input$nameInput)
  })
  
  output$linePlot <- renderPlot({
    # Filtrar os dados com base no nome fornecido
    filteredData <- babynames[babynames$name == nameToPlot(), ]
    
    # Verificar se ha dados filtrados
    if(nrow(filteredData) > 0) {
      ggplot(filteredData, aes(x = year, y = n)) +
        geom_line() +
        labs(title = paste("Popularidade do nome", nameToPlot(), "ao longo do tempo"),
             x = "Ano",
             y = "Numero de nascimentos") +
        theme_minimal()
    } else {
      ggplot() +
        annotate("text", x = 1950, y = 1000, label = "Nome nao encontrado", size = 6, color = "red") +
        theme_minimal() +
        labs(title = "Popularidade do nome ao longo do tempo",
             x = "Ano",
             y = "Numero de nascimentos")
    }
  })
}

# Rodar o aplicativo Shiny
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

######################## Renders ###################################

#plot

ui <- fluidPage(
  titlePanel("Histogram"),
  sidebarLayout(
    sidebarPanel(sliderInput('nb_bins', '# Bins', 5, 10 , 5)),
    mainPanel(
      tabsetPanel(
        tabPanel('Waiting', 
                 plotOutput('hist_waiting')),
        tabPanel('Eruptions', 
                 plotOutput('hist_eruptions'))
      )
    )
  )
)

server <- function(input, output, session){
  output$hist_waiting <- renderPlot({
    hist(faithful$waiting,
         breaks = input$nb_bins,
         col = 'steelblue')
  })
  output$hist_eruptions <- renderPlot({
    hist(faithful$eruptions,
         breaks = input$nb_bins,
         col = 'steelblue')
  })
}

shinyApp(ui = ui, server = server)

# table

ui <- fluidPage(
  titlePanel("What's in a Name?"),
  selectInput('sex', 'Select Sex', choices = c("M", "F")),
  sliderInput('year', 'Select Year', min = 1900, max = 2010, value = 1900),
  DT::DTOutput('table_top_10_names')
)

server <- function(input, output, session){
  top_10_names <- function(){
    babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      slice_max(prop, n = 10)
  }
  output$table_top_10_names <- DT::renderDT({
    top_10_names() %>%
      DT::datatable()
  })
}

shinyApp(ui = ui, server = server)

######################## Layout ###################################

# básico

ui2 <- fluidPage(
  titlePanel("Baby Name Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput('name', 'Enter Name', 'David')),
    mainPanel(  
      plotOutput('trend')))
)

server2 <-  function(input, output, session){
  output$trend <- renderPlot({
    data_names <- subset(
      babynames, name == input$name
    )
    ggplot(data_names) +
      geom_line(
        aes(x = year, y = prop, colour = sex)
      )
  })
}

shinyApp(ui = ui2, server = server2)

# sem o tabPanel

ui <- fluidPage(
  titlePanel("Histogram"),
  sidebarLayout(
    sidebarPanel(sliderInput('nb_bins', '# Bins', 5, 10 , 5)),
    mainPanel(
      plotOutput('hist_waiting'), 
      plotOutput('hist_eruptions'))
      )
    )
  


server <- function(input, output, session){
  output$hist_waiting <- renderPlot({
    hist(faithful$waiting,
         breaks = input$nb_bins,
         col = 'steelblue')
  })
  output$hist_eruptions <- renderPlot({
    hist(faithful$eruptions,
         breaks = input$nb_bins,
         col = 'steelblue')
  })
}

shinyApp(ui = ui, server = server)

#completo

ui <- fluidPage(
  titlePanel("Histogram"),
  sidebarLayout(
    sidebarPanel(sliderInput('nb_bins', '# Bins', 5, 10 , 5)),
    mainPanel(
      tabsetPanel(
        tabPanel('Waiting', 
                 plotOutput('hist_waiting')),
        tabPanel('Eruptions', 
                 plotOutput('hist_eruptions'))
      )
    )
  )
)

server <- function(input, output, session){
  output$hist_waiting <- renderPlot({
    hist(faithful$waiting,
         breaks = input$nb_bins,
         col = 'steelblue')
  })
  output$hist_eruptions <- renderPlot({
    hist(faithful$eruptions,
         breaks = input$nb_bins,
         col = 'steelblue')
  })
}

shinyApp(ui = ui, server = server)





############################ PROGRAMAÇÃO REATIVA ######################################

########################### exemplo 1 - programação reativa ###########################


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





########################### exemplo 2 - programação reativa ###########################

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





########################### exemplo 3 - programação reativa ###########################

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





########################### exemplo 4 - programação reativa ###########################

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

##############################################################################################



######################## Dashboards ###################################

##### parte 01 ######


header <- dashboardHeader(title = "My first dashboard")

sidebar <- dashboardSidebar()

body <- dashboardBody()

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output){
  
}

shinyApp(ui, server)

##### parte 02 ######

# Definindo novos parâmetros no header
header <- dashboardHeader(title = "My first dashboard",
                          titleWidth = 300,
                          dropdownMenu(type = "messages"),
                          dropdownMenu(type = "notifications")
                          
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
}

shinyApp(ui, server)

##### parte 03 ######

# Definindo a sidebar e seus parâmetros
sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              id = "pages",
                              menuItem("Many charts", tabName = "charts",
                                       icon = icon("chart-line")),
                              menuItem("Statistics", tabName = "stats",
                                       icon = icon("file-excel"))
                            ))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
}

shinyApp(ui, server)

##### parte 04 ######

# Adicionando subtabs na sidebar
sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              id = "pages",
                              menuItem("Many charts", tabName = "charts",
                                       icon = icon("chart-line")),
                              menuItem("Statistics", tabName = "stats",
                                       icon = icon("file-excel"),
                                       menuSubItem("Team 1", tabName = "team1",
                                                   icon = icon("user")))
                            ))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
}

shinyApp(ui, server)

##### parte 05 ######

# Adicionando inputs na sidebar

sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              id = "pages",
                              menuItem("Many charts", tabName = "charts",
                                       icon = icon("chart-line")),
                              menuItem("A couple of checkboxes",
                                       checkboxGroupInput("checkboxes",
                                                          "Days of the week",
                                                          choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
                            ))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
}

shinyApp(ui, server)

##### parte 06 ######

# Desativando a sidebar

sidebar <- dashboardSidebar(disable = TRUE)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
}

shinyApp(ui, server)

##### parte 07 ######

# Modificando parametros no body - criando linhas e caixas

header <- dashboardHeader(title = "My first dashboard")

sidebar <- dashboardSidebar()

body <- dashboardBody(
  fluidRow(
    box("row 1, box 1"),
    box("row 1, box 2")
  ),
  fluidRow(
    box("row 2, box 1")
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output){
  
}

shinyApp(ui, server)

##### parte 08 ######

# Adicionando conteúdo às caixas

body <- dashboardBody(
  fluidRow(
    box("row 1, box 1",
        plotOutput("plot")),
    box("row 1, box 2")
  ),
  fluidRow(
    box("row 2, box 1",
        selectInput("select", "select a number:",
                    choices = c(1, 2, 3, 4, 5)))
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output){
  
}

shinyApp(ui, server)

##### parte 09 ######

# Alterando a largura das caixas

body <- dashboardBody(
  fluidRow(
    box("row 1, box 1",
        plotOutput("plot")),
    box("row 1, box 2", width = 4)
  ),
  fluidRow(
    box("row 2, box 1",
        selectInput("select", "select a number:",
                    choices = c(1, 2, 3, 4, 5)),
        width = 12)
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output){
  
}

shinyApp(ui, server)

##### parte 10 ######

# Adicionando tipo especificos de caixas: valueBox

body <- dashboardBody(
  fluidRow(
    valueBox(value = 3,
             subtitle = "Total of cars",
             icon = icon("car"),
             color = "red")
  ))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output){
  
}

shinyApp(ui, server)

##### parte 11 ######

# Adicionando tipo especificos de caixas: infoBox

body <- dashboardBody(
  fluidRow(
    infoBox(value = 3,
            title = "Total of bicycles",
            icon = icon("bicycle"),
            color = "green",
            width = 6)
  ))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output){
  
}

shinyApp(ui, server)

##### parte 12 ######

# Adicionando valueBoxOutput
#
#body <- dashboardBody(
#  fluidRow(
#    valueBoxOutput(outputId = "valueBox1")
#  ))
#
#ui <- dashboardPage(header, sidebar, body)
#
#server <- function(input, output){
#  output$valuebox1 <- renderValueBox(
#    valueBox(value = 3,
#             subtitle = "Total of cars",
#             icon = icon("car"),
#             color = "red")
#  )
#  
#}
#
#shinyApp(ui, server)

##### parte 13 ######

# Adicionando infoBoxOutput

#body <- dashboardBody(
#  fluidRow(
#    infoBoxOutput(outputId = "infobox1")
#  ))
#
#ui <- dashboardPage(header, sidebar, body)
#
#server <- function(input, output){
#  output$infobox1 <- renderInfoBox(
#    infoBox(value = 3,
#            title = "Total of bicycles",
#            icon = icon("bicycle"),
#            color = "green",
#            width = 12)
#  )
#  
#}
#
#shinyApp(ui, server)

##### parte 14 ######

# Adicionando tabItems

sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              id = "pages",
                              menuItem("Many charts", tabName = "charts",
                                       icon = icon("chart-line")),
                              menuItem("Statistics", tabName = "statistics",
                                       icon = icon("file-excel"))
                            ))
body <-dashboardBody(
  tabItems(
    tabItem("charts", "Charts go here."),
    tabItem("statistics", "Statistics go here.")
    
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
}

shinyApp(ui, server)

##### parte 15 ######

# Adicionando tabPanels

sidebar <- dashboardSidebar()

body <-dashboardBody(
  tabsetPanel(
    tabPanel("Distribution",
             box(plotOutput("dist"))),
    tabPanel("Calendar",
             dateInput("matchdate",
                       "Enter the date:",
                       value = "2024-05-28")
             
    )))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
}

shinyApp(ui, server)

##### parte 16 ######

# Montando o Dashboard completo

# Helper function

library(gapminder)

plot_life_exp <- function(territorio,
                          ...) {
  paises <- gapminder
  
  print(
    paises %>% 
      filter(country == territorio) %>% 
      ggplot() +
      geom_line(aes(year, lifeExp),
                ...) +
      xlab("Período") + 
      ylab(str_c("Expectativa de vida em anos: ", territorio))
  )
}


# Dashboard

header <- dashboardHeader(title = "Expectativa de vida dos países",
                          titleWidth = 300)

sidebar <- dashboardSidebar(sidebarMenu(menuItem("Gráfico",
                                                 tabName = "graph"),
                                        menuItem("Vazio",
                                                 tabName = "vazia")),
                            width = 300)

body <- dashboardBody(tabItems(
  tabItem("graph",
          fluidRow(box(selectInput("country", "País",
                                   choices = unique(gapminder$country))) 
          ),
          fluidRow(box(plotOutput("plot1")))),
  tabItem("vazia")))


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  output$plot1 <- renderPlot(plot_life_exp(input$country)) }

shinyApp(ui, server)



