if(!require("pacman")) install.packages("pacman")
pacman::p_load("shiny",
               "tidyverse",
               "shinydashboard")

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



