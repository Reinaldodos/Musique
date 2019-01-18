library(data.table);library(PKI);library(httr);library(base64enc);library(doParallel)

morceaux = read.table("Morceaux.txt")
morceaux = gsub("spotify:track:", "", apply(morceaux, 2, paste, collapse=","))

appname = "The Playlist Generator"
key = "722e11750a63437fade1015849ab9f82"
secretID = "ee56893475204bcb8a9d72cdfd74224c"

# Your application requests authorization
gettoken = function()
  {
  endpoint = oauth_endpoint(request = NULL, authorize = "https://accounts.spotify.com/authorize", access = "https://accounts.spotify.com/api/token")
  app = oauth_app(appname, key, secret = secretID)
  tauken = oauth2.0_token(endpoint, app, scope = NULL, type = NULL,
                          use_oob = FALSE, as_header = TRUE,
                          cache = FALSE)
  return(tauken)
  }
tauken = gettoken()
url = paste("https://api.spotify.com/v1/tracks?ids=",morceaux,"&market=FR", sep="")
cont=content(GET(url = url, config(accept_json(), token = tauken)))

playlist <- list()
for (n in 1:length(cont$tracks))
{
  toto = cont$tracks[[n]]
  title = toto$album$name
  artist = toto$artists[[1]]$name
  toto = cbind(artist,title)
  playlist = c(playlist,list(data.table(toto)))
}

playlist = unique(rbindlist(playlist, use.names = TRUE, idcol = NULL))
