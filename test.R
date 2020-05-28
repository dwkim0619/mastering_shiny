library(shiny)
library(vroom)
library(tidyverse)
library(tidymodels)
# devtools::install_github("tidymodels/tidymodels")

injuries <- vroom::vroom('neiss/injuries.tsv.gz')
glimpse(injuries)

products <- vroom::vroom('neiss/products.tsv')
products

population <- vroom::vroom('neiss/population.tsv')
population

selected <- injuries %>% filter(prod_code == 1842)
selected %>% count(diag, wt = weight, sort = T)
selected %>% count(body_part, wt = weight, sort = T)
selected %>% count(location, wt = weight, sort = T)

summary <- selected %>%
  count(age, sex, wt = weight)

summary %>%
  ggplot(aes(x = age, y = n, color = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")

summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c('age', 'sex')) %>%
  mutate(rate = n / population * 1e4)
summary %>%
  ggplot(aes(age, rate, color = sex)) +
  geom_line(na.rm = T) +
  labs(y = 'Injuries per 10,000 people')

selected %>%
  sample_n(10) %>%
  pull(narrative)

head(products)
