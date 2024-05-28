pacman::p_load("shiny",
               "tidyverse",
               "babynames",
               "DT",
               "shinythemes",
               "gapminder")

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
      textInput("name", "Digite seu nome")
    ),
    mainPanel(
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
