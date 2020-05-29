library(shiny)
library(tidyverse)
library(vroom)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput('code', 'Product', 
# the product name in the UI and
# returns the product code to the server.
                       setNames(products$prod_code, products$title)))
  ),
  fluidRow(
    column(2, selectInput("y", "Y axis", c('rate', 'count')))
  ),
  fluidRow(
    column(12, plotOutput('age_sex'))
  ),
  fluidRow(
    column(4, tableOutput('diag')),
    column(4, tableOutput('body_part')),
    column(4, tableOutput('location'))
  ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    # selected() %>% count(diag, wt = weight, sort = T)
    count_top(selected(), diag), with = "100%"
  )
  
  output$body_part <- renderTable(
    # selected() %>% count(body_part, wt = weight, sort = T)
    count_top(selected(), body_part), with = "100%"
  )
  
  output$location <- renderTable(
    # selected() %>% count(location, wt = weight, sort = T)
    count_top(selected(), location), with = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c('age', 'sex')) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, color = sex)) +
        geom_line() +
        labs(y = 'Estimated number of injuries')
    } else {
      summary() %>%
        ggplot(aes(age, rate, color = sex)) +
        geom_line(na.rm = T) +
        labs(y = 'Injuries per 10,000 people')
    }
  }, res = 96)
  
  output$narrative <- renderText({
    input$story
    selected() %>% pull(narrative) %>% sample(1)
  })
  
}

shinyApp(ui, server)