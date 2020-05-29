library(shiny)
library(ggplot2)
library(tidyverse)

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput('code', 'Product', 
# the product name in the UI and
# returns the product code to the server.
                       setNames(products$prod_code, products$title)))
  ),
  fluidRow(
    column(12, plotOutput('age_sex'))
  ),
  fluidRow(
    column(4, tableOutput('diag')),
    column(4, tableOutput('body_part')),
    column(4, tableOutput('location'))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = T)
  )
  
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = T)
  )
  
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = T)
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c('age', 'sex')) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, color = sex)) +
      geom_line() +
      labs(y = 'Estimated number of injuries')
  }, res = 96)
  
}

shinyApp(ui, server)