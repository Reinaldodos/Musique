library(package='XML')
require(package='XML')

doc <- xmlInternalTreeParse(file = "http://ws.audioscrobbler.com/2.0/?method=user.gettoptracks&user=Reinaldodos&period=12month&limit=1000&api_key=35c710f49c77939574619a7de41f7267")
r <- xmlRoot(doc)
tracks <- r[[1]]
tracks.longueur <- xmlSize(tracks)

doc <- xmlInternalTreeParse(file = "http://ws.audioscrobbler.com/2.0/?method=user.getlovedtracks&user=Reinaldodos&limit=1000&api_key=35c710f49c77939574619a7de41f7267")
r <- xmlRoot(doc)
loved <- r[[1]]
loved.longueur <- xmlSize(loved)

LOVE <- NULL
LIB <- NULL


#première boucle
for (m in 1:as.integer(loved.longueur))
{
	mbid <- xmlValue(loved[[m]][["mbid"]])
	track <- xmlValue(loved[[m]][["name"]])
	artist <- xmlValue(loved[[m]][["artist"]][["name"]])
	
	toto <- cbind(mbid, artist, track)
	LOVE <- rbind(LOVE, toto)
}	
	

for (n in 1:as.integer(tracks.longueur))
{
	mbid <- xmlValue(tracks[[n]][["mbid"]])
	track <- xmlValue(tracks[[n]][["name"]])
	artist <- xmlValue(tracks[[n]][["artist"]][["name"]])
	
	toto <- cbind(mbid, artist, track)
	LIB <- rbind(LIB, toto)
}

Selection <- merge(LOVE,LIB)
BEST_OF <- Selection[,c(2,3)]

write.table(Selection, "Selection.txt", quote=TRUE, row.names=FALSE, col.names=FALSE)
write.table(BEST_OF, "BEST OF.txt", quote=TRUE, row.names=FALSE, col.names=FALSE)