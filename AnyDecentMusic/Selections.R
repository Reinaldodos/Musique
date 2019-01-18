pacman::p_load(tidyverse, data.table, rvest, rio)
source("FONCTIONS.R")

Temps = list(
  Recent = "",
  Last_3months = "?time=1",
  Last_6months = "?time=2",
  Last_12months = "?time=3",
  All_time = "?time=4"
)

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
  group_by(Temps) %>% nest() %>%
  mutate(Selection = map(
    .x = data,
    .f = ~ group_by(.data = ., genre) %>%
      filter(Score == max(Score))
  )) %>%
  mutate(Liste = map(
    .x = Selection,
    .f = ~ paste(.$Artist, .$Title, sep = " - ") %>% unique %>% sort
  ))

saveRDS(object = output, file = "Selection")


# Print -------------------------------------------------------------------
"Selection" %>% read_rds %>% select(Temps, Liste) %>%  unnest() %>%
  split(x = .$Liste, f = .$Temps)

