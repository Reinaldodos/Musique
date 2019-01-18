library(package='XML')
require(package='XML')

doc <- xmlInternalTreeParse(file = "http://ws.audioscrobbler.com/2.0/?method=user.gettopalbums&user=Reinaldodos&period=12month&limit=100&api_key=35c710f49c77939574619a7de41f7267")

r <- xmlRoot(doc)
albums <- r[[1]]
albums.longueur <- xmlSize(albums)

LIB <- matrix(data ="0", nrow = albums.longueur, ncol=3)
colnames(LIB) <- c("ID", "Artiste", "Album")

#première boucle

for (n in 1:as.integer(albums.longueur))
{
	LIB[n,1] <- xmlValue(albums[[n]][["mbid"]])
	LIB[n,3] <- xmlValue(albums[[n]][["name"]])
	LIB[n,2] <- xmlValue(albums[[n]][["artist"]][["name"]])
}

LIB[,] <- gsub("&", "and", LIB[,]) 

LIB <- data.frame(LIB)

ALB <- NULL

for (n in 1:as.integer(albums.longueur))
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
ALB=subset(ALB, select = -c(ID))
colnames(ALB) <- c("Artiste", "Album", "Date")
ALB <- data.frame(ALB)

write.table(ALB, file = "Albums of the year.txt", quote = FALSE, sep = " - ", row.names = FALSE)
