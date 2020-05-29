library(shiny)
library(tidyverse)
library(vroom)
# library(data.table)

# kwa_list <- vroom('./data/kwa_list.csv')
# pt_list <- vroom('./data/pt_list.csv')
# pt_list %>%
#   count(W01SEX, AGES) %>%
#   ggplot(aes(AGES, n, color = W01SEX)) + 
#   geom_line()
# suga_list <- vroom('./data/suga_list.zip')
# suga_list %>%
#   filter(KWA == 'IM', BHCD == 'C1') %>%
#   mutate(SUGACD = fct_lump(fct_infreq(SUGACD), n = 10)) %>%
#   count(SUGACD)

# suga_list %>%
#   filter(KWA == 'IM', BHCD == 'C1', (str_detect(SUGACD, '^A'))) 


ui <- fluidPage(
  fluidRow(
    column(6, selectInput('kwa', 'Departments', setNames(kwa_list$KWA, kwa_list$KWANM)))
  ),
  fluidRow(
    column(12, plotOutput('age_sex'))
  ),
  fluidRow(
    column(4, tableOutput('c1'))
  )
)

server <- function(input, output, session) {
  selected_pt_list <- reactive(pt_list %>% filter(W03KWA == input$kwa) %>% count(W01SEX, AGES))
  selected_suga_list <- reactive({
    suga_list %>%
      filter(KWA == input$kwa, BHCD == 'C1') %>%
      mutate(SUGACD = fct_lump(fct_infreq(SUGACD), n = 10)) %>%
      count(SUGACD)
  })
  
  output$age_sex <- renderPlot({
    selected_pt_list() %>% ggplot(aes(AGES, n, color = W01SEX)) + 
      geom_line()
  })
  
  output$c1 <- renderTable(selected_suga_list(), width = "100%")
}

shinyApp(ui, server)