pacman::p_load(tidyverse, data.table, rvest, rio, magrittr)
source("AnyDecentMusic/FONCTIONS.R")

Temps = list(
  Recent = "",
  Last_3months = "?time=1",
  Last_6months = "?time=2",
  Last_12months = "?time=3",
  All_time = "?time=4"
)

Chronologie =
  Temps %>%
  names() %>%
  as_factor()

genres =
  "http://www.anydecentmusic.com" %>%
  FETCHEZ_LES_GENRES()

genres =
  Temps %>%
  map(.f = ~ str_c(genres, .))

input =
  genres %>%
  map(FETCH_TEMPS) %>%
  bind_rows(.id = "Temps")

input$genre =
  str_split_fixed(string = input$genre,
                  pattern = ".aspx",
                  n = 2) %>% .[, 1] %>%
  str_remove_all(pattern = "http://www.anydecentmusic.com/genre/") %>%
  str_replace_all(pattern = "/",
                  replacement = "-") %>%
  str_replace_all(pattern = "%20",
                  replacement = " ")

saveRDS(object = input,
        file = "AnyDecentMusic/Selection.rds")

input =
  "AnyDecentMusic/Selection.rds" %>%
  read_rds()

output =
  input %>%
  group_by(Temps,
           genre) %>%
  top_n(n = 1,
        wt = Score) %>%
  ungroup() %>%
  group_by(Artist,
           Title) %>%
  slice(1:1) %>%
  ungroup() %>%
  semi_join(x = input,
            by = join_by(Temps, Artist, Title))

output$Temps %<>% factor(levels = Chronologie)

output %>%
  group_by(Temps,
           Album = paste(Artist, Title,
                         sep = " - "),
           Score) %>%
  summarise(Genres = paste(genre,
                           collapse = " / ")) %>%
  filter(Temps != "All_time") %>%
  arrange(-Score, Temps) %>%
  view()


# Print -------------------------------------------------------------------
output %>%
  filter(Temps != "All_time") %>%
  group_nest(Temps, Artist, Title, Score) %>%
  group_nest(Temps, Score) %>%
  group_nest(Score) %>%
  arrange(-Score) %>%
  jsonlite::write_json(path = "AnyDecentMusic/Selection.json")
