test = list()
for (albums in ids)
{
  lo = paste("https://api.spotify.com/v1/albums/", albums, "/tracks?market=FR", sep="")
  coco = try(content(GET(url= lo,config(accept_json(), token = tauken))))

  for (n in 1:length(coco$items))
  {
    test = c(test, list(as.data.frame(coco$items[[n]])))
  }
}
foo = rbindlist(test, use.names = TRUE, fill = TRUE)
# backup = as.list(foo)
# playlist = c(playlist, backup) %>% unique()
playlist = foo
# tauken = gettoken()
toto = list()
for (n in 1:nrow(playlist))
{
  track = playlist[n]$id
  lo = paste("https://api.spotify.com/v1/tracks/", track, "?market=FR", sep="")
  coco = try(content(GET(url= lo,config(accept_json(), token = tauken ))), silent = TRUE)
  poo = as.data.frame(coco)

  if(is.null(poo$artists.name)){next}

  test = tryCatch(subset(poo, select = c(artists.name, popularity, uri)), poo = TRUE)
  if (poo != TRUE)
  {
    toto = c(toto, list(data.table(test)))
  }
}

liste = rbindlist(toto, use.names = TRUE, fill = TRUE)
liste = liste[order(artists.name, -popularity)]
URIlist = data.table(liste$uri)

write.table(URIlist, file = "Playlist.txt",
            quote = FALSE, row.names = FALSE, col.names = FALSE)
