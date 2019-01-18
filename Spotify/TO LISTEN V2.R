library(data.table);library(PKI);library(httr);library(base64enc);library(doParallel)

# récupérer la playlist "TO LISTEN" ---------------------------------------
appname = "The Playlist Generator"
key = "722e11750a63437fade1015849ab9f82"
secretID = "7cfd510cf62245e9a19c86d74256ff90"

# Your application requests authorization
gettoken = function()
{
  endpoint = oauth_endpoint(request = NULL, authorize = "https://accounts.spotify.com/authorize", access = "https://accounts.spotify.com/api/token")
  app = oauth_app(appname, key, secret = secretID)
  tauken = oauth2.0_token(
    endpoint, app, scope = NULL, type = NULL,
    use_oob = FALSE, as_header = TRUE,
    cache = FALSE
  )
  return(tauken)
}
tauken = gettoken()
url = "https://api.spotify.com/v1/users/reinaldodos/playlists/1Bdy7OQd5bE2YKUAaoensd/tracks?market=FR"
cont = content(GET(url = url, config(accept_json(), token = tauken)))


# en extraire les morceaux sauvegardés ------------------------------------
tracks = cont$items

isSaved = function(yack)
{
  id = yack$track$id
  isSaved = content(GET(
    url = paste(
      "https://api.spotify.com/v1/me/tracks/contains?ids=", id, sep = ""
    ), config(accept_json(), token = tauken)
  ))
  toto = cbind(id, isSaved)
  return(toto)
}

issaved = data.table(rbindlist(lapply(tracks, isSaved)))
saved_tracks = as.list(issaved[issaved$V2 == "TRUE"]$V1)

# récupérer les albums de référence ---------------------------------------
isAlbum <- function (yack)
{
  url = paste("https://api.spotify.com/v1/tracks/?ids=", yack, sep = "")
  cont = content(GET(url = url, config(accept_json(), token = tauken)))
  albums = cont$tracks[[1]]$album$id
  return(albums)
}

Albums = unique(lapply(saved_tracks, isAlbum))


# récupérer les morceaux de chaque album par popularité -------------------


# retirer les morceaux déja sauvegardés
# ne conserver que 3 morceaux max par album
# réarranger par artiste et par ordre d'album
# coller dans la playlist "TO LISTEN"
