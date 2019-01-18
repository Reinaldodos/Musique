source("LASTFM loved2.R")

FINAL <- NULL

for(p in 1:nrow(Selection))
{
	cho <-Selection[p,c(2:3)]
	show(cho)
	
	mbid <- as.character(cho$mbid)
	artist <- as.character(cho$artist)
	title <- as.character(cho$title)
	artist <- gsub("&", "and", artist)
	
	'on récupère les titres'
	file <- paste("http://ws.audioscrobbler.com/2.0/?method=artist.getinfo&artist=",artist,"&autocorrect=1&api_key=35c710f49c77939574619a7de41f7267", sep="")
	doc <- NULL
	try(doc <- xmlInternalTreeParse(file))

	if(is.null(doc))
	{
		next
	}
	r <- xmlRoot(doc)
	morceau <- r[[1]]
	tags <- morceau[["tags"]]
	nb.tags <- xmlSize(tags)
	
	if(nb.tags>0)
	{
		tag <- NULL
		tho <- NULL
		
		for (n in 1:nb.tags)
		{
			tag[n] <- 1
			tho[n] <- xmlValue(tags[[n]][["name"]])
			
		}
		
		tag <- data.frame(t(tag))
		colnames(tag) <- tho
		tho <- cbind(cho,tag)
		FINAL <- merge(FINAL, tho, all=TRUE)
'FAIRE UN UPDATE'		
		
	}
}
FINAL[is.na(FINAL)] <- 0

name <- paste(FINAL$artist,FINAL$track, sep=" - ")
rownames(FINAL) <- name
FINAL <- subset(FINAL, select=-c(artist, track))
