library(gganimate)
library(zoo)
library(tidyverse)
library(scales)

setwd("D:/Documents/Datos/remesas-nic/")
data <- read_csv("./datos/remesas.csv")

data$year <- format(as.Date(data$date), "%Y")
data$year_month <- format(as.Date(data$date), "%Y-%m")


data %>%  
  filter(year >= 2018) %>% 
  ggplot(aes(year_month, remesas, group = 1)) + 
  geom_point() +
  geom_line() + 
  theme_ipsum_rc() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, 
       y = "Millions of dollars", 
       title = "Personal remittances in Nicaragua", 
       caption = "Data: Central Bank of Nicaragua | @rrmaximiliano")

ggsave("./figs/fig1.png", plot = last_plot(), dpi = 800, units = "cm")


