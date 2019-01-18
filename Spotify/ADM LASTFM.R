library(httr);library(XML); library(data.table)

# saveRDS(playlist, file = "Playlist")

# playlist = readRDS("Playlist")

playlist = lapply(playlist, function(x) gsub("&", "and", x))
playlist = lapply(playlist, function(x) gsub("'", "", x))
playlist = lapply(playlist, function(x) gsub("-", "", x))

seurch = list()
playlist = as.data.table(playlist)
for (i in 1:nrow(playlist))
{
  toto = gsub(" ", "%20", paste("artist:%22",playlist$artist[i],"%22 album:%22", playlist$title[i],"%22", sep=""))
  url = paste("https://api.spotify.com/v1/search?q=",toto,"&type=album&market=FR", sep="")
  seurch = c(seurch, list(url))
}

ids = list()
# tauken = gettoken()
for (lo in seurch)
{
  coco = try(content(GET(url= lo,config(accept_json(), token = tauken))))
  if (class(coco) != "list"){next}
  if (length(coco$albums$items)==0) {next}
  for (n in 1:length(coco$albums$items))
  {
    ids = c(ids, list(coco$albums$items[[n]]$id))
  }
}

