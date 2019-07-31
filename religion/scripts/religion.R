library(tidyverse)
library(reshape2)
library(hrbrthemes)

#--set wd 
setwd("D:/Documents/Datos/latinobarometro 2018/")

#--read cvs
data <- read.csv("./data/religion_latinobarometro.csv", fileEncoding = "UTF-8-BOM")
catolico <- read.csv("./data/catolicos.csv", fileEncoding = "UTF-8-BOM")
 
#--reshape data 
data_reshaped <- reshape(data, idvar = "country", timevar = "year", direction = "wide")
data_reshaped <- data_reshaped %>%  
      rename(religion1 = religion_none.2007, 
             religion2 = religion_none.2017)

#--graph 
data_reshaped %>%  
  ggplot(aes(x = country)) + 
  geom_segment(aes(x = reorder(country, religion2), xend=country, 
                   y = religion1, yend=religion2), 
               color = "black", size = 2) + 
  geom_point(aes(x = country, y = religion1, color=c("#56B4E9")), size = 4) + 
  geom_point(aes(x = country, y = religion2, color=c("#E69F00")), size = 4) +
  coord_flip() + 
  scale_y_continuous(labels = function(x) paste0(round(x/100*100,1), "%")) + 
  scale_color_manual(name = "Year", 
                       values = c("#56B4E9", "#E69F00"), 
                       labels = c("2007", "2017")) + 
  # Theme
  theme_ipsum_rc() + 
  theme(panel.grid.major.x = element_blank(), 
        legend.position = "bottom") + 
  # Labels
  labs(x = "", 
       y= "Porcentaje", 
       caption = "Data: Latinobarómetro 2007 - 2017 | @rrmaximiliano", 
       title = "Proporción de personas que contestaron no tener una religión",
       subtitle = "2007 - 2017") 


ggsave("./figs/fig1.png", plot = last_plot(), dpi = 800, units = "cm")


#--graph catolicos 
catolico %>%  
  ggplot(aes(year, catolicos)) +
  geom_point(size = 3, color = c("#56B4E9"), alpha = 0.7) + 
  geom_line(color=c("#56B4E9")) + 
  theme_ipsum_rc() + 
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank()) + 
  scale_y_continuous(labels = function(x) paste0(x/100*100, "%")) + 
  labs(x = "", 
       y = "% de católicos", 
       caption = "Data: Latinobarómetro 1996 - 2017 | @rrmaximiliano", 
       title = "Proporción de católicos en Nicaragua", 
       subtitle = "1996-2017")
  
ggsave("./figs/fig2.png", plot = last_plot(), dpi = 800, units = "cm")
