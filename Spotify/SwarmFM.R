library(PlayerRatings);library(RSelenium);library(rvest);library(data.table);library(stringr)
# RSelenium::checkForServer(update = TRUE)
checkForServer()
# example of commandline passing
startServer(args = c("-port 4455"), log = FALSE, invisible = FALSE)
remDr <- remoteDriver(browserName = "firefox", port = 4455)
remDr$open(silent = TRUE)
appURL = "http://app.swarm.fm/#/releases/explore/myartists/trending/albums"
remDr$navigate(appURL)

remDr$findElement("css", "#loginbutton")$clickElement()
# cliquer sur "J'ai un compte Spotify"
# remDr$findElement("css", "#login-username")$sendKeysToElement(list("reinaldodos"))
# remDr$findElement("css", "#login-password")$sendKeysToElement(list("gFw5QAHza8aGGMj"))
# remDr$findElement("css", ".btn-green")$clickElement()

remDr$navigate(appURL)
webElems = remDr$findElements(using = 'css selector', ".icon-spotify")
toto = lapply(webElems, function(x) x$getElementAttribute("href"))
toto = lapply(toto, function(x) gsub("spotify:album:", "", x))

test = list()
for (albums in c(toto, ids))
{
  lo = paste("https://api.spotify.com/v1/albums/", albums, "/tracks?market=FR", sep="")
  coco = try(content(GET(url= lo,config(accept_json(), token = tauken))))

  for (n in 1:length(coco$items))
  {
    test = c(test, list(as.data.frame(coco$items[[n]]$id)))
  }
}
foo = rbindlist(test, use.names = TRUE, fill = TRUE)
colnames(foo) = "id"
ids = c(ids, list(foo))
remDr$closeall()
