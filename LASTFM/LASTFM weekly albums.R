library(package='XML')
library(package='DescTools')
setwd("~/Dropbox/Carto & Stats/R/Musique/LASTFM")

doc <- htmlParse(file = "http://www.last.fm/fr/user/reinaldodos/library/albums?date_preset=LAST_7_DAYS")

artists <- xpathSApply(doc, "//*[@id='top-albums-section']/table/tbody/tr/td[3]/span/span[1]/a", xmlValue)
albums <- xpathSApply(doc, "//*[@id='top-albums-section']/table/tbody/tr/td[3]/span/a", xmlValue)
counter <- xpathSApply(doc, "//*[@id='top-albums-section']/table/tbody/tr/td[4]/span/span/a", xmlValue)

LIB <- NULL
LIB <- cbind(counter,artists, albums)
LIB[,] <- gsub("&", "and", LIB[,])
LIB <- data.frame(LIB)
LIB$counter <- as.numeric(gsub("\\D", "",as.character(LIB$counter)))
LIB$counter

LIB=subset(LIB,LIB$counter>3)

ALB <- NULL

for (n in 1:nrow(LIB))
{
  show(LIB[n,2:3])
  doc <- xmlInternalTreeParse(file = paste("http://ws.audioscrobbler.com/2.0/?method=album.getinfo&artist=",LIB[n,2],"&album=",LIB[n,3], "&autocorrect=1&api_key=35c710f49c77939574619a7de41f7267", sep=""))
  r <- xmlRoot(doc)
  album <- r[[1]]
  album.longueur <- xmlSize(albums)
  
  url <- xmlValue(album[["url"]])
  x <- htmlParse(url)
  DatedeSortie <- try(xpathSApply(x, "//*[@id='mantle_skin']//li/p", xmlValue), silent=TRUE)
  DatedeSortie=DatedeSortie[1]
  toto <- try(cbind(LIB[n,], DatedeSortie), silent=TRUE)
  if(class(toto)=="try-error")
  {
    DatedeSortie=""
  }
  toto <- cbind(LIB[n,], DatedeSortie)
  ALB <- rbind(ALB,toto)
}
ALB=subset(ALB, select = -c(counter))
colnames(ALB) <- c("Artiste", "Album", "Date")
ALB <- data.frame(ALB)
write.csv(ALB,"LASTFM weekly albums.csv", row.names = FALSE, quote = FALSE)
