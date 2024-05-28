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
