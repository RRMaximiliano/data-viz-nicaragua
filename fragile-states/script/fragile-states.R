library(tidyverse)
library(hrbrthemes)
library(janitor)
library(countrycode)
library(gganimate)

#--set wd
setwd("~/Datos/fragile-states//")

#--read csv 
data <- read.csv("data/fsi-2019.csv", stringsAsFactors = FALSE)

data <- data %>%  
  clean_names()

data %>%  
  filter(country == "Nicaragua" | country == "Costa Rica" | country == "El Salvador" | 
         country == "Honduras") %>%  
  ggplot(aes(year, p1_state_legitimacy, color = country)) + 
  geom_point(alpha = 0.7) + 
  geom_line() + 
  theme_ipsum_rc() + 
  theme(legend.position = "bottom") + 
  labs(color = "")

data %>%  
  filter(country == "Nicaragua" | country == "Costa Rica" | country == "El Salvador" | 
           country == "Honduras") %>%  
  ggplot(aes(year, e3_human_flight_and_brain_drain, color = country)) + 
  geom_point(alpha = 0.7) + 
  geom_line() + 
  theme_ipsum_rc() + 
  theme(legend.position = "bottom") + 
  labs(color = "")


data %>%  
  filter(country == "Nicaragua" | country == "Costa Rica" | country == "El Salvador" | 
           country == "Honduras") %>%  
  ggplot(aes(year, p3_human_rights, color = country)) + 
  geom_point(alpha = 0.7) + 
  geom_line() + 
  theme_ipsum_rc() + 
  theme(legend.position = "bottom") + 
  labs(color = "", 
       y= "Human rights index", 
       x = "Year", 
       title = "Human Rights and Rule of Law Index in Central America", 
       subtitle = "High is worse | 2006-2019", 
       caption = "Data: The Fund for Peace - Fragile States Index | @rrmaximiliano") 


ggsave("./figs/fig1.png", plot = last_plot(), dpi = 800, units = "cm")

