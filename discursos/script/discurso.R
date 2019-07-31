library(tidyverse)
library(tidytext)
library(janitor)
library(hrbrthemes)
library(viridis)
library(tm)                       # for spanish stopwords

#--setwd
setwd("~/Datos/discurso-daniel/")

data <- read.csv("data/discurso.csv", stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  clean_names()

#--unnesting
stop_words <- bind_rows(stop_words,
                        tibble(word = tm::stopwords("spanish"),
                                   lexicon = "custom"))

discurso <- data %>%
  filter(year == 2019) %>% 
  mutate(id = row_number()) %>%
  unnest_tokens(word, discurso) %>%
  anti_join(stop_words, by = "word")

discurso <- discurso %>%  
  filter(!word %in% c("de", "la", "el", "los", "en", "es", "para", "con", "por",
                      "las", "del", "se", "lo", "una", "aquí", "este", "su", 
                      "al", "les", "como", "ni", "hemos", "ellos", "también", "nos",
                      "ese", "tod", "desde", "está", "unidos", "tiene", "hay", "ha",
                      "esta", "qué")) 

discurso$word[discurso$word == "estados"] <- "estados unidos"

discurso %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  theme_ipsum_rc() +
  labs(x = "Palabras",
       y = "N",
       title = "Sustantivos que más se repitieron en el\ndiscurso de Daniel el 19 de Julio, 2019",
       caption = "@rrmaximiliano")

ggsave("./figs/sustantivos.png", plot = last_plot(), dpi = 800, units = "cm")

discurso %>% 
  filter(word == "yanqui") %>%  
  count(word, sort = TRUE)


#--2018
discurso <- data %>%
  filter(year == 2018) %>% 
  mutate(id = row_number()) %>%
  unnest_tokens(word, discurso) %>%
  anti_join(stop_words, by = "word")

discurso <- discurso %>%  
  filter(!word %in% c("de", "la", "el", "los", "en", "es", "para", "con", "por",
                      "las", "del", "se", "lo", "una", "aquí", "este", "su", 
                      "al", "les", "como", "ni", "hemos", "ellos", "también", "nos",
                      "ese", "tod", "desde", "está", "unidos", "tiene", "hay", "ha",
                      "esta", "qué")) 

discurso$word[discurso$word == "estados"] <- "estados unidos"

discurso %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  theme_ipsum_rc() +
  labs(x = "Palabras",
       y = "N",
       title = "Sustantivos que más se repitieron en el\ndiscurso de Daniel el 19 de Julio, 2018",
       caption = "@rrmaximiliano")

#--grouped
grouped <- data %>%
  group_by(year) %>% 
  mutate(id = row_number()) %>%
  unnest_tokens(word, discurso) %>%
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>%
  ungroup

grouped$word[grouped$word == "unidos"] <- "estados unidos"

grouped %>%  
  filter(!word %in% c("ahí", "tod", "aquí", "quién", "decir", "mató", "entonces")) %>% 
  group_by(year) %>%  
  top_n(15) %>%
  ungroup %>% 
  mutate(year = as.factor(year), 
         word = reorder_within(word, n, year)) %>%
  ggplot(aes(word, n, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, scales = "free_y") + 
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_ipsum_rc() +
  labs(x = NULL,
       y = "N",
       title = "Sustantivos que Daniel Ortega repitió más en sus discursos del 19 de Julio",
       subtitle = "Top 15",
       caption = "Nota: verbos, adjetivos, adverbios, y pronombres fueron filtrados | @rrmaximiliano") 

ggsave("./figs/comparacion.png", plot = last_plot(), dpi = 1000, units = "cm")

#--tf-idf
grouped %>% 
  bind_tf_idf(word, year, n) %>% 
  group_by(year) %>%  
  top_n(10) %>%
  ungroup %>% 
  mutate(year = as.factor(year), 
         word = reorder_within(word, tf_idf, year)) %>%
  ggplot(aes(word, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, scales = "free_y") + 
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_ipsum_rc() +
  labs(x = NULL,
       y = "Valor TF-IDF",
       title = "Palabras más importantes en el discurso de Daniel Ortega según TF-IDF" ,
       subtitle = "Top 10",
       caption = "Nota: TF-IDF se refiere a la frecuencia inversa de los términos que expresa que tan relevante es una palabra para un documento. | @rrmaximiliano") 

ggsave("./figs/tf-idf.png", plot = last_plot(), dpi = 1000, units = "cm")


