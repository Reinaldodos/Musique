pacman::p_load(tidyverse, data.table, rvest, rio, magrittr)
source("FONCTIONS.R")

Temps = list(
  Recent = "",
  Last_3months = "?time=1",
  Last_6months = "?time=2",
  Last_12months = "?time=3",
  All_time = "?time=4"
)

Chronologie = Temps %>% names() %>% as_factor()

genres =
  "http://www.anydecentmusic.com" %>%
  FETCHEZ_LES_GENRES()

genres = Temps %>% map(.f = ~ str_c(genres, .))

input =
  genres %>% map(FETCH_TEMPS) %>%
  bind_rows(.id = "Temps")

input$genre =
  str_split_fixed(string = input$genre,
                  pattern = ".aspx",
                  n = 2) %>% .[, 1] %>%
  str_remove_all(pattern = "http://www.anydecentmusic.com/genre/") %>%
  str_replace_all(pattern = "/", replacement = "-") %>%
  str_replace_all(pattern = "%20", replacement = " ")

output =
  input %>%
  group_by(Temps, genre) %>%
  top_n(n = 1, wt = Score) %>%
  ungroup() %>%
  group_by(genre) %>%
  slice(1:1) %>% ungroup()

output$Temps %<>% factor(levels = Chronologie)

saveRDS(object = output, file = "Selection")

# Print -------------------------------------------------------------------
"Selection" %>% read_rds() %>%
  group_nest(Temps, Artist, Title, Score) %>%
  group_nest(Temps, Score) %>%
  arrange(Temps, -Score) %>%
  group_nest(Temps) %>%
  jsonlite::write_json(path = "Selection.json")
