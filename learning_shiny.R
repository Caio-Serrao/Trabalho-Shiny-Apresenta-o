install.packages("shiny")
install.packages("babynames")
install.packages("DT")
install.packages("shinythemes")
install.packages("recipes")
install.packages("tidytext")
install.packages("forcats")
install.packages("pacman")
library("pacman")
library(forcats)
library(shiny)
library(tidyverse)
library(babynames)
library(DT)
library(shinythemes)
library(gapminder)
library(recipes)
library(tidytext)
##########################  AULA 01  ##################################

ui <- fluidPage( 
  textInput("name", "Enter a name:"),
  textOutput("q")
)  # ui = user interface

server <- function(input, output){
  output$q <- renderText({
    paste("Do you prefer dogs or cats,",
          input$name, "?")
  })
}

shinyApp(ui = ui, server = server)

##########################  AULA 02  ##################################

# A good tip is to sketch my app before beginning de code, in order to get a preview
# about the final resoult.

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

##########################  AULA 03 - Inputs! ##################################

##### Inputs!

## select inputs
 
selectInput("inputId",
            "label",
            choices = c("A", "B", "C"))

## slider inputs

sliderInput("inputId",
            "label",
            value = 1925,
            min = 1900,
            max = 2000)

## dica de caso tenha dúvida

?dateRangeInput
help(checkboxInput)

## exemplo (remember to always use unique label to the input)

ui3 <- fluidPage( 
  textInput("name", "Enter a name:"),
  selectInput("animal", "Dogs or cats?", choices = c("Dogs", "Cats")), # always in the UI
  textOutput("greeting"),
  textOutput("answer")
)

server3 <- function(input, output, session){
  output$greeting <- renderText({
    paste("Do you prefer dogs or cats,", input$name, "?")
  })
  output$answer <- renderText({
    paste("I prefer", input$animal, "!")
  })
}
  
shinyApp(ui = ui3, server = server3)

## exemplo do datacamp

ui <- fluidPage(
  titlePanel("What's in a Name?"),
  # Add select input named "sex" to choose between "M" and "F"
  selectInput('sex', 'Select Sex', choices = c("F", "M")),
  # CODE BELOW: Add slider input named 'year' to select years  (1900 - 2010)
  sliderInput('year', "label", value = 1900, min = 1900, max = 2010),
  # Add plot output to display top 10 most popular names
  plotOutput('plot_top_10_names')
)

server <- function(input, output, session){
  # Render plot of top 10 most popular names
  output$plot_top_10_names <- renderPlot({
    # Get top 10 names by sex and year
    top_10_names <- babynames %>% 
      filter(sex == input$sex) %>% 
      # MODIFY CODE BELOW: Filter for the selected year
      filter(year == input$year) %>% 
      slice_max(prop, n = 10)
    # Plot top 10 names by sex and year
    ggplot(top_10_names, aes(x = name, y = prop)) +
      geom_col(fill = "#263e63")
  })
}

shinyApp(ui = ui, server = server)

##########################  AULA 04 - RENDERS ##################################

#### RENDERS

# renderTable() <-> tableOutput() or dataTableOutput()
# renderImage() <-> imageOutput()
# renderPlot() <-> plotOutput()

## available to use non-shiny outputs and render functions, like DT, PLotly, Leaflet
## ex1

ui <- fluidPage(
  DT::DTOutput("babynames_table")
)

server <- function(input, output){
  output$babynames_table <- DT::renderDT({
    babynames %>% 
      dplyr::slice_sample(prop = .1) %>% 
      DT::datatable()
  })
}

shinyApp(ui = ui, server = server)
## ex2

ui <- fluidPage(
  titlePanel("What's in a Name?"),
  # Add select input named "sex" to choose between "M" and "F"
  selectInput('sex', 'Select Sex', choices = c("M", "F")),
  # Add slider input named "year" to select year between 1900 and 2010
  sliderInput('year', 'Select Year', min = 1900, max = 2010, value = 1900),
  # MODIFY CODE BELOW: Add a DT output named "table_top_10_names"
  DT::DTOutput('table_top_10_names')
)

server <- function(input, output, session){
  top_10_names <- function(){
    babynames %>% 
      filter(sex == input$sex) %>% 
      filter(year == input$year) %>% 
      slice_max(prop, n = 10)
  }
  # MODIFY CODE BELOW: Render a DT output named "table_top_10_names"
  output$table_top_10_names <- DT::renderDT({
    top_10_names() %>%
      DT::datatable()
  })
}

shinyApp(ui = ui, server = server)

## ex3

ui <- fluidPage(
  selectInput('name', 'Select Name', top_trendy_names$name), #esse top veio de algum lugar que noa sei
  # CODE BELOW: Add a plotly output named 'plot_trendy_names'
  plotly::plotlyOutput("plot_trendy_names")
)

server <- function(input, output, session){
  # Function to plot trends in a name
  plot_trends <- function(){
    babynames %>% 
      filter(name == input$name) %>% 
      plotly::plot_ly(, x = ~year, y = ~n, type = "bar")
  }
  # CODE BELOW: Render a plotly output named 'plot_trendy_names'
  output$plot_trendy_names <- plotly::renderPlotly(plot_trends())
  
  
}

shinyApp(ui = ui, server = server)

##########################  AULA 05 - Layouts and themes ##################################

#### Layouts and themes

# na aula 1 já sabiamos como usar o sidebarpanel e o main panel, porém agora vamos usar
# outra função pra gerar mais de uma tab em um mesmo app, deixando a opção pro usuário escolher
# qual saída ele quer visualizar naquele momento. Fariamo assim:

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

#abordando os temas, primeiro usamos o themeSelector pra identificar qual usar


ui <- fluidPage(
  titlePanel("Histogram"),
  shinythemes::themeSelector(),
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

# escolhido qual será o tema:

ui <- fluidPage(
  titlePanel("Histogram"),
  theme = shinythemes::shinytheme('united'),
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

##########################  AULA 06 - steps to build an app  ##################################

#### Steps to build a shiny app (the way that data camp strongly recommends)
## 1. Add inputs in the UI
## 2. Add outputs (UI/SERVER) 
## 3. Update layout(UI)
## 4. Upgrade de code of the outputs in the SERVER

## 1º: Add inputs in the UI

ui <- fluidPage(
  titlePanel("Life Expectation vs. GDP Per Capita"),
  selectInput('continet', 'Select Continet', unique(gapminder$continent)),
  sliderInput('year', 'Select Year', 1952, 2007, 1990, step = 5)
)

server <- function(input, output, session){
  
}

shinyApp(ui = ui, server = server)

## 2º: Add outputs (UI/SERVER)

ui <- fluidPage(
  titlePanel("Life Expectation vs. GDP Per Capita"),
  selectInput('continet', 'Select Continet', unique(gapminder$continent)),
  sliderInput('year', 'Select Year', 1952, 2007, 1990, step = 5),
  plotOutput('plot'),
  DT::DTOutput('table')
)

server <- function(input, output, session){
  output$plot <- renderPlot({
    ggplot()
  })
  output$table <- DT::renderDT({
    gapminder
  })
}

shinyApp(ui = ui, server = server)

## 3. Update layout(UI)

ui <- fluidPage(
  titlePanel("Life Expectation vs. GDP Per Capita"),
  sidebarLayout(
    sidebarPanel(
      selectInput('continet', 'Select Continet', unique(gapminder$continent)),
      sliderInput('year', 'Select Year', 1952, 2007, 1990, step = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('plot', plotOutput('plot')),
        tabPanel('table',DT::DTOutput('table'))
      )
    )
  )
)
  
server <- function(input, output, session){
  output$plot <- renderPlot({
    ggplot()
  })
  output$table <- DT::renderDT({
    gapminder
  })
}

shinyApp(ui = ui, server = server) 

## 4. Upgrade de code of the outputs in the SERVER

ui <- fluidPage(
  titlePanel("Life Expectation vs. GDP Per Capita"),
  sidebarLayout(
    sidebarPanel(
      selectInput('continent', 'Select Continet', unique(gapminder$continent)),
      sliderInput('year', 'Select Year', 1952, 2007, 1990, step = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("plot", plotOutput('plot')),
        tabPanel("table", DT::DTOutput('table'))
      )
    )
  )
)

server <- function(input, output, session){
  output$plot <- renderPlot({
    data <- gapminder %>% 
      filter(year == input$year) %>% 
      filter(continent == input$continent) 
    print(data)
    ggplot(data, aes(x = gdpPercap, y = lifeExp))+
      geom_point()
  })
  output$table <- DT::renderDT({
    gapminder %>% 
      filter(year == input$year) %>% 
      filter(continent == input$continent)
  })
}

shinyApp(ui = ui, server = server) 

##########################  AULA 07 - Reactivity 101 ##################################

##### Reactivity 101

## discuss the basics of reactor programming

## reactivity at its best means, the output is notified whenever any of its dependencies change

## Reative Source
# User input that comes through a browser interface, typically

ui <- fluidPage(
  titlePanel("Greeting"),
  textInput("name", "Enter a name:")
)
 server <- function(input, output, session){
   
 }
 
 shinyApp(ui = ui, server = server)

## Reactive endpoint
 # OUTPUT that tipically appears in the browser window, such as plot or a table of values

 ui <- fluidPage(
   titlePanel("Greeting"),
   textInput("name", "Enter a name:"),
   textOutput("greeting")
 )
 server <- function(input, output, session){
   output$greeting <- renderText({    # the output object is an observer
     paste("Hello", input$name)       # that uses a reactive expression(input$name)
   })
 }
 
 shinyApp(ui = ui, server = server)

## REactive Conductor (lazy and cached):
 #Lazy: Evaluated only when it is called, typically by a reactive endpoint.
 #Cached: Evaluated only when the value of one of its underlying dependencies changes.
 # An intermediatre that depends os reactive sources, and/or updates reactive endpoints
 
 server <- function(input, output, session){
   output$plot_trendy_names <- plotly::renderPlotly({
     babynames %>% 
       filter(name == input$name) %>% 
       ggplot(val_bnames, aes(x = year, y = n)) +
       geom_col()
   })
   output$table_trendy_names <- DT::renderDT({
     babynames %>% 
       filter(name == input$name)      #dependent of one or more reactive sources or endpoints
   })                                 # the code is repeated twice and evaluated twice
 }
 
 shinyApp(ui = ui, server = server) 

 
### BEST CODE:
 
 server <- function(input, output, session){
   rval_babynames <- reactive({
     babynames %>% 
       filter(name == input$name)
   })
   output$plot_trendy_names <- plotly::renderPlotly({
     rval_babynames() %>% 
       ggplot(val_bnames, aes(x = year, y = n)) +   #it's only valuated when its underline 
       geom_col()                                   # sources change
   })
   output$table_trendy_names <- DT::renderDT({
     rval_babynames()    
   })                                 
 }
 
## in this case, below, the expression "Computing cars_1..." appear only once.
 ui <- fluidPage(
   numericInput('nrows', 'Number of Rows', 10, 5, 30),
   tableOutput('table'), 
   plotOutput('plot')
 )
 server <- function(input, output, session){
   cars_1 <- reactive({
     print("Computing cars_1 ...")
     head(cars, input$nrows)
   })
   cars_2 <- reactive({
     print("Computing cars_2 ...")
     head(cars, input$nrows*2)
   })
   output$plot <- renderPlot({
     plot(cars_1())
   })
   output$table <- renderTable({
     cars_1()
   })
 }
 shinyApp(ui = ui, server = server)
 
# Since cars_1 is used by the outputs, it will be executed. 
#However, it will be executed only once, since it is a reactive expression, and 
#hence gets cached. As cars_2 is NOT used by any of the outputs, it will never be executed.
 
 ##########################  AULA 08 - Observes and reactives ################################## 
 
 ##### Observes and reactives
 
 ui <- fluidPage(
   titlePanel("Greeting"),
   textInput("name", "Enter a name:")
 )
 server <- function(input, output, session){
   observe(
     showModal(modalDialog(
       paste("Hello", input$name)
     ))
   )
 }
 
 shinyApp(ui = ui, server = server)

 ## or
 
 ui <- fluidPage(
   titlePanel("Greeting"),
   textInput("name", "Enter a name:")
 )
 server <- function(input, output, session){
   observe(
     showNotification(
       paste("You entered the name", input$name)
     )
   )
 }
 
 shinyApp(ui = ui, server = server)

 ##########################  AULA 09 - STOP - DELAY - TRIGGER ##################################
 
 ####### STOP - DELAY - TRIGGER
 
 #### 1. Isolating actions
 
 ## in this case, the input when is changed, updates de code twice
 
 ui <- fluidPage(
   titlePanel("Greeting"),
   textInput("name", "Enter a name:"),
   selectInput('greeting', 'Select a greeting:', choices = c("Hello", "Bonjour")),
   textOutput("greeting")
 )
 server <- function(input, output, session){
   output$greeting <- renderText({    
     paste(input$greeting, input$name, sep = ", ")       
   })
 }
 
 shinyApp(ui = ui, server = server)
 
 # if we do not want do update twice (when the greeting and the name are changed),
 # so the code would be updated only once:
 
 ui <- fluidPage(
   titlePanel("Greeting"),
   textInput("name", "Enter a name:"),
   selectInput('greeting', 'Select a greeting:', choices = c("Hello", "Bonjour")),
   textOutput("greeting")
 )
 server <- function(input, output, session){
   output$greeting <- renderText({    
     paste(isolate({
       input$greeting
       }), input$name, sep = ", ")   
   })
 }
 
 shinyApp(ui = ui, server = server)

 #### 2. Delaying actions
 
 ### If i want to delay the actions only when the user clicks on a button:
 
 ui <- fluidPage(
   titlePanel("Greeting"),
   textInput("name", "Enter a name:"),
   selectInput('greeting', 'Select a greeting:', choices = c("Hello", "Bonjour")),
   actionButton("showgreeting", "Show Greeting!"),
   textOutput("greeting")
 )
 
 server <- function(input, output, session){
   rv_greeting <- eventReactive(input$showgreeting, {
     paste('Hello', input$name)
   })
   output$greeting <- renderText({
     rv_greeting()
   })
 }
 
 ## example
 
 ui <- fluidPage(
   titlePanel('BMI Calculator'),
   sidebarLayout(
     sidebarPanel(
       textInput('name', 'Enter your name'),
       numericInput('height', 'Enter height (in m)', 1.5, 1, 2, step = 0.1),
       numericInput('weight', 'Enter weight (in Kg)', 60, 45, 120),
       actionButton("show_bmi", "Show BMI")
     ),
     mainPanel(
       textOutput("bmi")
     )
   )
 )
 
 server <- function(input, output, session) {
   # MODIFY CODE BELOW: Use eventReactive to delay the execution of the
   # calculation until the user clicks on the show_bmi button (Show BMI)
   rval_bmi <- eventReactive(input$show_bmi, {
     input$weight/(input$height^2)
   })
   output$bmi <- renderText({
     bmi <- rval_bmi()
     paste("Hi", input$name, ". Your BMI is", round(bmi, 1))
   })
 }
 
 shinyApp(ui = ui, server = server)
 
 #### 3. triggering actions
 
 ui <- fluidPage(
   titlePanel("Greeting"),
   textInput("name", "Enter a name:"),
   selectInput('greeting', 'Select a greeting:', choices = c("Hello", "Bonjour")),
   textOutput("greeting")
 )
 server <- function(input, output, session){
   observeEvent(input$greeting, {
     showModal(modalDialog(paste('Hello', input$name)))
   })
 }
 
 shinyApp(ui = ui, server = server)
 
 ##########################  AULA 10  ##################################
 
 ui <- fluidPage(
   titlePanel("UFO Sightings"),
   sidebarPanel(
     selectInput("state", "Choose a U.S. state:", choices = unique(usa_ufo_sightings$state)),
     dateRangeInput("dates", "Choose a date range:",
                    start = "1920-01-01",
                    end = "1950-01-01"
     )
   ),
   mainPanel(
     tabsetPanel(
       tabPanel("Plot", plotOutput("shapes")),
       tabPanel("Table", tableOutput("duration_table"))
     )
   )
 )
 
 server <- function(input, output) {
   rval_choices <- reactive({
     usa_ufo_sightings %>%
       filter(
         state == input$state,
         date_sighted >= input$dates[1],
         date_sighted <= input$dates[2]
       )
   })
   output$shapes <- renderPlot({
     rval_choices() %>%
       ggplot(aes(shape)) +
       geom_bar() +
       labs(
         x = "Shape",
         y = "# Sighted"
       )
   })
   
   output$duration_table <- renderTable({
     rval_choices() %>%
       group_by(shape) %>%
       summarize(
         nb_sighted = n(),
         avg_duration_min = mean(duration_sec) / 60,
         median_duration_min = median(duration_sec) / 60,
         min_duration_min = min(duration_sec) / 60,
         max_duration_min = max(duration_sec) / 60
       )
   })
 }
 
 shinyApp(ui, server)
 
#You could add a theme, write a custom CSS stylesheet to add pictures of aliens, 
 #and use the data to add even more information about alien sightings the world over.
 
 ##########################  AULA 11 - Custom error messages ##################################
 
 
 #### Custom error messages
 
 ## TIP : pickerinput
 
 #example:

 server <- function(input, output, session){
   output$age <- renderTable({
     validate(
       need(input$age != "", "Be sure to select an age")
     )
     mental_health_survey %>% 
       summarise(avg_age = mean(Age))
   })
 }

 #### shinyWidgets
 
install.packages("shinywidgetsGallery") 
library(shinywidgetsGallery)

##########################  AULA 12 - APPS EXAMPLES ##################################
 
#### EXAMPLES OF APPS
 
ui <- fluidPage(
  # CODE BELOW: Add an appropriate title
  titlePanel("2014 Mental Health in Tech Survey"),
  sidebarPanel(
    # CODE BELOW: Add a checkboxGroupInput
    checkboxGroupInput(
      inputId = "mental_health_consequence",
      label = "Do you think that discussing a mental health issue with your employer would have negative consequences?",
      choices = c("Maybe", "Yes", "No"),
      selected = "Maybe"
    ),
    # CODE BELOW: Add a pickerInput
    pickerInput(
      inputId = "mental_vs_physical",
      label = "Do you feel that your employer takes mental health as seriously as physical health?",
      choices = c("Don't Know", "No", "Yes"),
      multiple = TRUE
    )
  ),
  mainPanel(
    # CODE BELOW: Display the output
    plotOutput("age")
  )
)

server <- function(input, output, session) {
  # CODE BELOW: Build a histogram of the age of respondents
  # Filtered by the two inputs
  output$age <- renderPlot({
    mental_health_survey %>%
      filter(
        mental_health_consequence %in% input$mental_health_consequence,
        mental_vs_physical %in% input$mental_vs_physical
      ) %>%
      ggplot(aes(Age)) +
      geom_histogram()
  })
}

shinyApp(ui, server)
 
## example 2

ui <- fluidPage(
  titlePanel("2014 Mental Health in Tech Survey"),
  sidebarPanel(
    sliderTextInput(
      inputId = "work_interfere",
      label = "If you have a mental health condition, do you feel that it interferes with your work?", 
      grid = TRUE,
      force_edges = TRUE,
      choices = c("Never", "Rarely", "Sometimes", "Often")
    ),
    checkboxGroupInput(
      inputId = "mental_health_consequence",
      label = "Do you think that discussing a mental health issue with your employer would have negative consequences?", 
      choices = c("Maybe", "Yes", "No"),
      selected = "Maybe"
    ),
    pickerInput(
      inputId = "mental_vs_physical",
      label = "Do you feel that your employer takes mental health as seriously as physical health?", 
      choices = c("Don't Know", "No", "Yes"),
      multiple = TRUE
    )    
  ),
  mainPanel(
    plotOutput("age")  
  )
)

server <- function(input, output, session) {
  output$age <- renderPlot({
    # MODIFY CODE BELOW: Add validation that user selected a 3rd input
    validate(
      need(input$mental_vs_physical != "", "Select Menta vs. Physical")
    )
    mental_health_survey %>%
      filter(
        work_interfere == input$work_interfere,
        mental_health_consequence %in% input$mental_health_consequence,
        mental_vs_physical %in% input$mental_vs_physical
      ) %>%
      ggplot(aes(Age)) +
      geom_histogram()
  })
}

shinyApp(ui, server)
 
# ex3 

ui <- fluidPage(
  titlePanel('Explore Cuisines'),
  sidebarLayout(
    sidebarPanel(
      selectInput('cuisine', 'Select Cuisine', unique(recipes$cuisine)), #pacote recipes
      sliderInput('nb_ingredients', 'Select No. of Ingridients', 5, 100, 20)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Word Cloud', d3wordcloudOutput('wc_ingredients')),
        tabPanel('Plot', plotly::plotlyOutput('plot_top_ingredients')),
        tabPanel('Table', DT::DTOutput('dt_top_ingredients'))
      )
    )
  )
)

server <- function(input, output, session){
    recipes %>% 
      filter(cuisine == input$cuisine) %>% 
      count(ingredient, name = 'nb_recipes') %>% 
      arrange(desc(nb_recipes)) %>% 
      head(input$nb_ingredients)
  # in this case we compute ingredients that are common to all countries, like salt
  # in order to fix it, we use:
  recipes_enriched <- recipes %>% 
    count(cuisine, ingredient, name = 'nb_recipes') %>% 
    tidytext::bind_tf_idf(ingredient, cuisine, nb_recipes)
  
  rval_top_ingredients <- reactive({
    recipes_enriched %>% 
      filter(cuisine == input$cuisine) %>% 
      arrange(desc(tf_idf)) %>% 
      head(input$nb_ingredients) %>% 
      mutate(ingredient = forcats::fct_reorder(ingredient, tf_idf))
  })
  output$dt_top_ingredients <- DT::renderDT({
    rval_top_ingredients
  })
    
}
 
#or 
ui <- fluidPage(
  titlePanel('Explore Cuisines'),
  sidebarLayout(
    sidebarPanel(
      selectInput('cuisine', 'Select Cuisine', unique(recipes$cuisine)),
      sliderInput('nb_ingredients', 'Select No. of Ingredients', 5, 100, 10),
    ),
    mainPanel(
      tabsetPanel(
        # CODE BELOW: Add a plotly output named "plot_top_ingredients"
        tabPanel('Plot', plotly::plotlyOutput('plot_top_ingredients')),
        tabPanel('Table', DT::DTOutput('dt_top_ingredients'))
      )
    )
  )
)

server <- function(input, output, session) {
  # CODE BELOW: Add a reactive expression named `rval_top_ingredients` that
  # filters `recipes_enriched` for the selected cuisine and top ingredients
  # based on the tf_idf value.
  rval_top_ingredients <- reactive({
    recipes_enriched %>% 
      filter(cuisine == input$cuisine) %>% 
      arrange(desc(tf_idf)) %>% 
      head(input$nb_ingredients)
  })
  # CODE BELOW: Render a horizontal bar plot of top ingredients and 
  # the tf_idf of recipes they get used in, and assign it to an output named 
  # `plot_top_ingredients` 
  output$plot_top_ingredients <- plotly::renderPlotly({
    rval_top_ingredients() %>%
      ggplot(aes(x = ingredient, y = tf_idf)) +
      geom_col() +
      coord_flip()
  })
  
  output$dt_top_ingredients <- DT::renderDT({
    recipes %>% 
      filter(cuisine == input$cuisine) %>% 
      count(ingredient, name = 'nb_recipes') %>% 
      arrange(desc(nb_recipes)) %>% 
      head(input$nb_ingredients)
  })
}

shinyApp(ui, server)
 
# last app

ui <- bootstrapPage(
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput('map', width = '100%', height = '100%'),
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput('nb_fatalities', 'Minimum Fatalities', 1, 40, 10),
                dateRangeInput(
                  'date_range', 'Select Date', "2010-01-01", "2019-12-01"
                ),
                # CODE BELOW: Add an action button named show_about
                actionButton('show_about', "About")
  ),
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  ")
)

server <- function(input, output, session) {
  # CODE BELOW: Use observeEvent to display a modal dialog
  # with the help text stored in text_about.
  observeEvent(input$show_about, {
    showModal(modalDialog(text_about, title = "About"))
  })
  
  
  output$map <- leaflet::renderLeaflet({
    mass_shootings %>% 
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        fatalities >= input$nb_fatalities
      ) %>% 
      leaflet() %>% 
      setView( -98.58, 39.82, zoom = 5) %>% 
      addTiles() %>% 
      addCircleMarkers(
        popup = ~ summary, radius = ~ sqrt(fatalities)*3,
        fillColor = 'red', color = 'red', weight = 1
      )
  })
}

shinyApp(ui, server)
 
 
 
 
 
 
 
 
 
 
 
 