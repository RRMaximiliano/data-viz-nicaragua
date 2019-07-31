library(WDI) 
library(tidyverse)
library(hrbrthemes)
library(scales)
library(plotly)

#--setwd 
setwd("~/Datos/wdi/")

#--search data
WDIsearch('gdp.*capita.*constant') %>%  
  view()

#--get data
data <- WDI(indicator = "NY.GDP.PCAP.KD", country = c("NI", "CR", "HN", "SV", "PA", "BZ", "GT"), 
            start = 1990, end = 2018) %>%  
        rename(gdp = "NY.GDP.PCAP.KD")

#--first plot
data %>%  
  ggplot(aes(year, gdp, color = country)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = dollar) + 
  theme_ipsum_rc() + 
  labs(y = "GDP per capita",
       x = "Year",
       color = NULL, 
       title = "GDP per capita (constant 2011 US $)", 
       subtitle = "1990 - 2018",
       caption = "Source: World Bank Development Indicators | @rrmaximiliano")

ggsave("./figs/pib-ca-comp.png", dpi = 800, units = "in", device='png')

#--interactive  
position = c("Nicaragua", "Honduras", "El Salvador", "Guatemala", "Belize", "Costa Rica", "Panama")

data %>%  
  filter(year %in% c(1990, 2018)) %>%  
  ggplot(aes(x = country, y = gdp, fill = as.factor(year))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  coord_flip() + 
  theme_ipsum_rc() + 
  theme(legend.title = element_blank()) + 
  scale_y_continuous(labels = dollar) + 
  scale_x_discrete(limits = position) + 
  labs(y = "GDP per capita", 
       x = "Countries", 
       title = "GDP per capita (constant 2011 US $)",  
       subtitle = "Central American Countries", 
       caption = "Source: World Bank Development Indicators | @rrmaximilliano") 

ggsave("./figs/pib-ca-fixed.png", dpi = 300, units = "in", device='png')

ggplotly(plot, tooltip = c("gdp", "country"), 
         dynamicTicks = TRUE)

#--solo Nicaragua
data %>%  
  filter(country == "Nicaragua") %>% 
  ggplot(aes(year, gdp, color = country)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits = c(1000, 2000), labels = dollar) + 
  theme_ipsum_rc() + 
  theme(legend.position = "none") + 
  labs(y = "GDP per capita",
       x = "Year",
       color = NULL, 
       title = "GDP per capita in Nicaragua (constant 2011 US $)", 
       subtitle = "1990 - 2018",
       caption = "Source: World Bank Development Indicators | @rrmaximiliano")

ggsave("./figs/pib-nic.png", dpi = 800, units = "in", device='png')



##-Remesas
remesas <- WDI(indicator = "BX.TRF.PWKR.DT.GD.ZS", country = c("NI")) %>% 
  rename(remesas = "BX.TRF.PWKR.DT.GD.ZS")

remesas %>% 
  filter(!is.na(remesas)) %>%  
  ggplot(aes(year, remesas)) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  theme_ipsum_rc() + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  labs(title = "Remesas personales como porcentaje del PIB en Nicaragua", 
       subtitle = "1977 - 2018", 
       caption = "Data: WDI | @ rrmaximiliano",
       x = "Año", 
       y = "% del PIB") 
  
ggsave("./figs/remesas.png", dpi = 800, units = "in", device='png')









