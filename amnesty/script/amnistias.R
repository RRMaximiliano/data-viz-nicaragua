library(tidyverse)
library(reshape2)
library(ggthemes)
library(hrbrthemes)
library(doBy)
library(extrafont)
loadfonts(device = "win")

setwd("~/Datos/amnistias/")

#--Open 
data <- read.csv("amnistias.csv", fileEncoding = "UTF-8-BOM")

data %>%  
  count(concedida, sort = TRUE) %>% 
  head(20) %>%  
  mutate(concedida = fct_reorder(concedida, n)) %>%
  ggplot(aes(concedida, n)) +
  geom_col() +
  coord_flip() + 
  labs(x = "", 
       y = "Cantidad de amnistías concedidas", 
       caption = "@rrmaximiliano", 
       title = "Amnistías otorgadas") + 
  theme_ipsum_rc()

ggsave("fig1.png", plot = last_plot(), dpi = 300, units = "cm")

#--Por decadas
decade <- data %>%  
  distinct(year, .keep_all = TRUE) %>%  
  mutate(decade = 10 * (year %/% 10)) %>% 
  group_by(decade) %>%  
  count(decade) 

decade %>%  
  ggplot(aes(decade, n)) + 
  geom_col() +
  theme_ipsum_rc() + 
  labs(title = "Aministías otorgadas por década", 
       x = "Decada", 
       y = "Amnistía otorgadas", 
       caption = "@rrmaximiliano")

ggsave("fig2.png", plot = last_plot(), dpi = 300, units = "cm")


