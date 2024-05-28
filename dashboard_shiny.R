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



