library(tidyverse)
library(hrbrthemes)
library(janitor)
library(reshape2)
library(ggrepel)
library(ggthemes)
library(gghighlight)
library(viridis)

#--set wd
setwd("~/Datos/internet-nicaragua/")

# INDIVIDUALS USING INTERNET
  #--read data
  internet <- read.csv("data/internet.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", check.names = FALSE)
  
  #--from wide to long
  internet_long <- melt(internet, id.vars = c("country"))
  
  #--rename variables
  internet_long <- internet_long %>%
    rename(year = variable, internet = value) %>%
    arrange(country, year) %>%
    mutate(year = as.numeric(as.character(year)))

# BROADBAND SUBSCRIPTIONS
  #--read data
  broadband <- read.csv("data/broadband.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", check.names = FALSE)
  
  #--from wide to long
  broadband_long <- melt(broadband, id.vars = c("country"))
  
  #--rename variables
  broadband_long <- broadband_long %>%
    rename(year = variable, broadband = value) %>%
    arrange(country, year) %>%
    mutate(year = as.numeric(as.character(year)))
  
# FIXED TELEPHONE SUBSCRIPTIONS
  #--read data
  telephone <- read.csv("data/fixed-telephone.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", check.names = FALSE)
  
  #--from wide to long
  telephone_long <- melt(telephone, id.vars = c("country"))
  
  #--rename variables
  telephone_long <- telephone_long %>%
    rename(year = variable, telephone = value) %>%
    arrange(country, year) %>%
    mutate(year = as.numeric(as.character(year)))


# JOINING ALL DATASETS
data <- left_join(internet_long, broadband_long, by = c("country", "year")) %>% 
  left_join(., telephone_long, by = c("country", "year"))

data <- data %>%  
  gather(indicator, value, internet:telephone, -country, -year)

#--plot
data %>%
  filter(country %in% c("Nicaragua", "Costa Rica", "Panama", "Honduras", "El Salvador"),
         indicator == "internet") %>% 
  mutate(label = if_else(year == max(year), as.character(country), NA_character_)) %>%
  ggplot(aes(year, value, color = country)) +
  geom_point(size = 2) +
  geom_line() +
  geom_label_repel(aes(label = label),
                   size = rel(4),
                   nudge_x = 1,
                   label.padding = 0.2,
                   box.padding = 0.1,
                   segment.color = NA,
                   na.rm = TRUE) +
  # scales and labels
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Percentage of individuals using internet",
       subtitle = "2000 - 2017",
       x = "Year",
       y = "Percentage",
       caption = "Source: International Telecommunication Union | @rrmaximiliano") +
  theme_ipsum_rc() +
  theme(legend.position = "none",
        plot.title = element_text(size = 18), 
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10)) 

ggsave("./figs/fig1.png", plot = last_plot(), dpi = 800, 
       width = 12, 
       height = 9, 
       units = "in")

#--plot all the variables
data %>%  
  filter(country %in% c("Nicaragua", "Costa Rica", "Panama", "Belize", 
                        "Honduras", "El Salvador", "Guatemala"),
         !is.na(value), indicator != "internet") %>% 
  ggplot(aes(year, value, color = country)) +
  geom_point(size = 2) + 
  geom_line() + 
  facet_wrap(~ indicator, labeller = label_parsed) + 
  theme_ipsum_rc() + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18), 
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10)) + 
  guides(col = guide_legend(nrow = 1)) + 
  labs(x = "Year", 
       y = "Per 100 inhabitants", 
       caption = "Source: International Telecommunication Union | @rrmaximiliano", 
       title = "Broadband internet and fixed telephone subscriptions per 100 inhabitants", 
       subtitle = "2000 - 2017")

ggsave("./figs/fig2.png", plot = last_plot(), dpi = 800, 
       width = 12, 
       height = 9, 
       units = "in")
