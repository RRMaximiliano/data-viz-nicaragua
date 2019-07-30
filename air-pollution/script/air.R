library(tidyverse)
library(janitor)
library(hrbrthemes)
library(gghighlight)
library(ggrepel)

#--set wd
setwd("~/Datos/air-pollution/")

#--read csv 
data <- read.csv("data/PM25-air-pollution.csv", stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  rename(country = entity, 
         pm25 = pm2_5_air_pollution_mean_annual_exposure_micrograms_per_cubic_meter_micrograms_per_cubic_meter)

#--plot
data %>% 
  filter(country %in% c("Nicaragua", "Costa Rica", "El Salvador", "Honduras",
                        "Guatemala", "Panama", "Belize")) %>% 
  mutate(label = if_else(year == max(year), as.character(country), NA_character_)) %>% 
  ggplot(aes(year, pm25, color = country)) + 
  geom_point(size = 2, alpha = 0.8) + 
  geom_line(size = 1) + 
  geom_label_repel(aes(label = label),
                   size = rel(4),
                   nudge_x = 1,
                   label.padding = 0.2,
                   box.padding = 0.1,
                   segment.color = NA,
                   na.rm = TRUE) + 
  scale_y_continuous(labels = function(x) paste0(x,"\U003BCg")) + 
  theme_ipsum_rc() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) + 
  labs(color = NULL, 
       x = "Año",
       title = "Contaminación del aire PM2.5, exposición media anual",
       subtitle = "microgramos por metro cúbico",
       caption = "Fuente: Banco Mundial | @rrmaximiliano")

ggsave("./figs/fig1.png", plot = last_plot(), dpi = 800, 
       width = 12, 
       height = 9, 
       units = "in")
