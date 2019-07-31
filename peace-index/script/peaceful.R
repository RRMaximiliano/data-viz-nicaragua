library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(hrbrthemes)

## set wd 
setwd("~/Datos/peaceful-countries/")

## web scraping
url <- read_html("http://worldpopulationreview.com/countries/most-peaceful-countries/")

peace <- url %>% 
  html_nodes('table') %>% 
  html_table(header = TRUE) %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  select(-flag) %>% 
  rename(country = name)

## save csv 
write.csv(peace, "data/peace.csv", row.names = FALSE)

## peace index, central america 
peace %>% 
  filter(country %in% c("Nicaragua", "Costa Rica", "El Salvador", "Honduras", "Guatemala", "Panama")) %>% 
  mutate(country = reorder(country, peace_index)) %>% 
  ggplot(aes(country, peace_index, color = country)) + 
  geom_point(size = 4) +
  geom_segment(aes(x = country, xend = country,
                   y = 0, yend = peace_index), 
               size = 2) +
  coord_flip(ylim = c(1,3), expand = TRUE, clip = "on") + 
  theme_ipsum_rc() + 
  theme(legend.position = "none") + 
  labs(title = "Global Peace Index in 2019", 
       subtitle = "Central American Region", 
       y = "Peace Index", 
       x = NULL, 
       caption = "Data: Institute for Economics and Peace | @rrmaximiliano")

ggsave("./figs/fig1.png", plot = last_plot(), dpi = 800, units = "cm")





