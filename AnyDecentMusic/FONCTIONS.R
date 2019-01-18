FETCHEZ_LES_GENRES <- function(url) {
  require(rvest)
  Genres =
    url %>%
    read_html() %>%
    html_nodes(css = "#ctl00_ctl00_ctl00_ContentPlaceHolderDefault_ctl11_SubGenres") %>%
    html_children() %>%
    html_text() %>%
    str_replace_all(replacement = "n", pattern = "&") %>%
    str_replace_all(replacement = "%20", pattern = " ")

  Genres =
    paste(url, "/genre/", Genres[2:length(Genres)], ".aspx", sep = "") %>%
    return()
}

FETCHEZ <- function(genre) {
  require(rvest)
  print(genre)
  CODE =
    genre %>%
    read_html()

  Infos =
    CODE %>%
    html_nodes(css = ".album_info a") %>%
    html_text() %>%
    as.character()

  if (length(Infos) == 0)
  {
    return(NULL)
  }

  Scores =
    CODE %>%
    html_nodes(css = "p.score") %>%
    html_text() %>%
    as.numeric()

  cbind.data.frame(Artist = Infos[seq(1, length(Infos), 2)],
                   Title = Infos[seq(2, length(Infos), 2)],
                   Score = Scores) %>%
    return()
}

FETCH_TEMPS <- function(genres) {
  safe_FETCH = safely(FETCHEZ)
  input =
    genres %>% set_names %>%
    map(.f = safe_FETCH)

  if (input %>% map(.f = ~ .$error) %>% compact %>% length(.) == 0)
    input =
      input %>% map(.f = ~ .$result) %>%
      bind_rows(.id = "genre")

  return(input)
}
