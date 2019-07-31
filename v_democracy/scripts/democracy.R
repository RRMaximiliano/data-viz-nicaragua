library(tidyverse)
library(hrbrthemes)
library(janitor)
library(countrycode)
library(gganimate)

#--set wd
setwd("~/Datos/v_demoacry/")

#--read data
data <- readRDS("data/V-Dem-CY-Full+Others-v9.rds")

#--select Nicaragua
nicaragua <- data %>%  
  filter(country_name == "Nicaragua")

nicaragua %>%  
  ggplot(aes(x = year, y = v2x_polyarchy)) + 
  geom_point(alpha = 0.7) + 
  scale_x_continuous(limits = c(1900, 2018), 
                     breaks = seq(1900, 2018, 10)) +
  # Theme
  theme_ipsum_rc() + 
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(hjust = 1),
        axis.title.x = element_text(hjust = 1),
        plot.margin = margin(1,1,1,1, "cm")) + 
  # Labs 
  labs(title = "Electoral democracy index", 
       subtitle = "Low to High (0 - 1)", 
       caption = "Data: V-Dem 9.0 | @rrmaximiliano", 
       x = "Year", 
       y = "Index")

# Análisis por década 
decade <- data %>%  
  filter(country_name == "Nicaragua") %>%  
  distinct(year, .keep_all = TRUE) %>%  
  mutate(decade = 10 * (year %/% 10)) %>%  
  select(country_name, decade, year, v2x_polyarchy) %>%  
  group_by(decade) %>%  
  summarize(mean = mean(v2x_polyarchy, na.rm = TRUE))

decade <- decade %>% 
  mutate(category = case_when(
                    decade <= 1936 ~ "Periodo Liberal",  
                    decade >= 1937 & decade < 1979 ~ "Dictadura Somocista", 
                    decade >= 1979 & decade < 1990 ~ "Revolución Sandinista", 
                    decade >= 1990 & decade < 2020 ~ "Periodo Actual")) 

decade$category <- factor(decade$category, levels = c("Periodo Liberal", 
                                                      "Dictadura Somocista", 
                                                      "Revolución Sandinista", 
                                                      "Periodo Actual"))  

decade %>%  
  ggplot(aes(decade, mean, color = category)) + 
  geom_point(alpha = 0.7, size = 3) + 
  scale_x_continuous(limits = c(1900, 2010), 
                     breaks = seq(1900, 2010, 10)) +
  # Theme
  theme_ipsum_rc() + 
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(hjust = 1),
        axis.title.x = element_text(hjust = 1),
        plot.margin = margin(1,1,1,1, "cm"), 
        legend.position = "bottom") + 
  # Labs 
  labs(title = "Electoral democracy index", 
       subtitle = "Low to High (0 - 1)", 
       caption = "Data: V-Dem 9.0 | @rrmaximiliano", 
       x = "Year", 
       y = "Index", 
       color = NULL)

ggsave("./figs/decades.png", plot = last_plot(), dpi = 800, units = "cm")

#--education
nicaragua %>%  
  select(year, e_peaveduc) %>% 
  filter(!is.na(e_peaveduc), year >= 1900 & year <= 2010) %>% 
  ggplot(aes(year, e_peaveduc)) + 
  geom_point(alpha = 0.4)
  
#--Freedom of expression
nicaragua %>%  
  select(year, v2x_freexp_altinf) %>% 
  filter(!is.na(v2x_freexp_altinf), year >= 1960) %>% 
  ggplot(aes(year, v2x_freexp_altinf)) + 
  geom_point(size = 2, alpha = 0.8, color = "#337CA0") + 
  # Theme
  theme_ipsum_rc() + 
  theme(panel.grid.minor = element_blank()) + 
  # Labs
  labs(title = "Freedom of expression index in Nicaragua (1960 - 2018)", 
       subtitle = "Low to high (0 - 1)", 
       x = "Year", 
       y = "Index", 
       caption = "Data: Varieties of Democracy Project v9 | @rrmaximiliano\nPlease refer to the codebook for the variable description") + 
  # Scales
  scale_x_continuous(limits = c(1960, 2020), 
                     breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(limits = c(0, 1))

ggsave("./figs/foe.png", plot = last_plot(), dpi = 800, units = "cm")

nicaragua %>% 
  select(year, v2clrelig)

#--Freedom of expression en en Centroamerica
ca <- data %>%
  filter(country_name == "Nicaragua" | country_name =="Costa Rica" | 
         country_name == "El Salvador" | country_name =="Guatemala" | 
         country_name == "Panama" | country_name == "Honduras" | country_name == "Belize")

tabyl(data$country_name, sort = TRUE)

ca %>%  
  filter(year >= 1960) %>%  
  ggplot(aes(year, v2x_freexp_altinf, color = country_name)) + 
  geom_point(size = 0.5, alpha = 0.7) + 
  geom_line(alpha = 0.7, na.rm = TRUE) + 
  # Theme
  theme_ipsum_rc() + 
  theme(legend.position = "none") + 
  # Labels
  labs(color = NULL, 
       x= "Year", 
       y = "Index", 
       title = "Freedom of expression index in Central America (1960 - 2018)", 
       subtitle = "Low to high (0 - 1)",
       caption = "Data: Varieties of Democracy Project v9 | @rrmaximiliano\nPlease refer to the codebook for the variable description") + 
  facet_wrap(~country_name)

ggsave("./figs/ca_foe.png", plot = last_plot(), dpi = 800, units = "cm")


#--Continent level
data$continent <- countrycode(sourcevar = data[, "country_name"],
                            origin = "country.name",
                            destination = "continent")

americas <- data %>%  
  filter(continent == "Americas") 

plot <- americas %>%  
  ggplot(aes(year, v2x_freexp_altinf, color = country_name)) 

plot + 
  geom_line(na.rm = TRUE) + 
  # Theme
  theme_ipsum_rc() + 
  theme(legend.position = "none") + 
  labs(color = NULL, 
       x= "Year", 
       y = "Index", 
       title = "Freedom of expression index in America (1800 - 2018)", 
       subtitle = "Low to high (0 - 1)",
       caption = "Data: Varieties of Democracy Project v9 | @rrmaximiliano\nPlease refer to the codebook for the variable description") + 
  facet_wrap(~country_name, ncol = 5)

ggsave("./figs/americas_foe.png", plot = last_plot(), dpi = 800, units = "cm")


#-- GGanimate
ca %>%
  filter(year > 1980) %>%  
  ggplot(aes(year, v2x_freexp_altinf, color = country_name)) + 
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_line() + 
  facet_wrap(~country_name) + 
  scale_color_viridis_d() + 
  theme_ipsum_rc()+ 
  transition_reveal(year)

#--bivariate analisis
p <- data %>% 
  filter(year >= 1980, !is.na(e_migdppcln), 
          country_name == "Nicaragua" | country_name =="Costa Rica" | 
          country_name == "El Salvador" | country_name =="Guatemala" | 
          country_name == "Panama" | country_name == "Honduras" | country_name == "Belize") %>% 
  ggplot(aes(v2x_freexp_altinf, e_migdppcln)) + 
  geom_point(aes(color = country_name), size = 10, na.rm = TRUE) + 
  geom_text(aes(label = country_text_id), nudge_y = .001, na.rm = TRUE) + 
  labs( # set titles
    title = "Relación PIB per capita y libertad de expresión en {closest_state}",
    subtitle = "Centroamérica: 1960 to 2016",
    y = "Log PIB per capita",
    x = "Índice de libertad dee expresión",
    caption = "\nData: Varities of Democracy v.9 | @rrmaximiliano"
    ) + 
  theme_ipsum_rc() + 
  theme(legend.position = "none")

p <- p + transition_states(year, transition_length = 3, state_length = 3) +  #animate the plot
      ease_aes('cubic-in-out')

animate(p, nframes = 500, start_pause = 15, end_pause = 15, fps=15, width=600, height=600)
anim_save("anim.gif", animation = last_animation(), path = "figs/")



##-Periodo de Somoza
# Educación
ca %>%  
  filter(year >= 1934, year <= 1979) %>%  
  ggplot(aes(year, e_peaveduc, color = country_name)) +  
  geom_point(size = 0.5, alpha = 0.7) + 
  geom_line(alpha = 0.7, na.rm = TRUE) + 
  facet_wrap(~country_name) + 
  theme_ipsum_rc() + 
  theme(legend.position = "none") + 
  labs(x= "Year", 
       y = "Average years of education", 
       title = "Average years of education among citizens older than 15", 
       caption = "Data: UNESCO | @rrmaximiliano") 

ggsave("./figs/ca_education.png", plot = last_plot(), dpi = 800, units = "cm")

# Gdp per capita
plotdata <- ca %>%
  filter(!is.na(e_migdppc)) %>%  
  filter(year >= 1934) %>%  
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-e_migdppc) * 1) %>%
  ungroup()

# Periodo solo de Somoza
# plotdata <- ca %>%
#   filter(year >= 1934, year <= 1979) %>%  
#   group_by(year) %>%
#   # The * 1 makes it possible to have non-integer ranks while sliding
#   mutate(rank = min_rank(-e_migdppc) * 1) %>%
#   ungroup()

p <- ggplot(plotdata, aes(rank, group = country_name, 
                     fill = as.factor(country_name), color = as.factor(country_name))) +
  geom_tile(aes(y = e_migdppc/2,
                height = e_migdppc,
                width = 0.9), alpha = 0.8, color = NA) +
  # text in x-axis (requires clip = "off" in coord_*)
  # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1 
  #   leads to weird artifacts in text spacing.
  geom_text(aes(y = 0, 
                label = paste(country_name, " ")), 
            vjust = 0.2, 
            hjust = 1,
            size = 8) +
  coord_flip(clip = "off", expand = FALSE)  +
  scale_y_continuous(labels=scales::dollar) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title='GDP per capita in {closest_state}',
       subtitle = "Central American Region", 
       x = "", 
       y = "Real GDP per capita $2012", 
       caption = "Data: Maddison Project Database | @rrmaximiliano") +
  theme_ipsum_rc() + 
  theme(plot.title = element_text(hjust = 0, size = 25),
        plot.subtitle = element_text(size = 20), 
        axis.text.y =  element_blank(), 
        axis.text.x =  element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm"), 
        plot.caption = element_text(size = 14)) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, nframes = 400, duration = 25, fps = 16, height = 800, width = 1100)

animate(p, nframes = 300, fps = 10, end_pause = 10, height = 800, width = 1000)

anim_save("ca_gif.gif", animation = last_animation(), path = "figs/")

