
#source("LASTFM loved2.R")
#colnames(Selection) <- c("ID", "Artiste", "Titre")
m <- 1
Playlist <- matrix(data ="0", nrow = 1, ncol=ncol(Selection))
colnames(Playlist) <- c("ID", "Artiste", "Titre")
Playlist <- Selection

#première boucle
for (m in m:nrow(Selection))
{
	id <- Selection[m, "ID"]
	title <- Selection[m, "Titre"]
	artist <- Selection[m, "Artiste"]
#	show(paste(m, " / ", nrow(Selection) ": ",artist, " - ", title))
	#récupérer la table de titles similaires au title m
	if (id == "")
	{
		url.track <- paste("http://ws.audioscrobbler.com/2.0/?method=track.getsimilar&track=", title, "&artist=", artist, "&autocorrect=1&api_key=35c710f49c77939574619a7de41f7267", sep ="")
		track <- xmlInternalTreeParse(file = url.track)
		r <- xmlRoot(track)
		track <- r[[1]]
	}

	if (id != "")
	{
		url.similar <- paste("http://ws.audioscrobbler.com/2.0/?method=track.getsimilar&track=", title, "&artist=", artist, "&autocorrect=1&api_key=35c710f49c77939574619a7de41f7267", sep ="")
		similar <- xmlInternalTreeParse(file = url.similar)
		r <- xmlRoot(similar)
		track <- r[[1]]
	}
#ATTENTION: CHOISIR FINEMENT LE NB DE TITRES SIMILAIRES
	track.longueur <- 5
	Adjacence <- matrix(data ="0", nrow = track.longueur, ncol=4)
	colnames(Adjacence) <- 	c("ID", "Artiste", "Titre", title)

	for (n in 1:track.longueur)
	{
		Adjacence[n,1] <- xmlValue(track[[n]][["mbid"]])
		Adjacence[n,3] <- xmlValue(track[[n]][["name"]])
		Adjacence[n,2] <- xmlValue(track[[n]][["artist"]][["name"]])
		Adjacence[n,4] <- xmlValue(track[[n]][["match"]])
	}
	Adjacence <- data.frame(Adjacence, stringsAsFactors=FALSE )
	Playlist <- merge(Playlist, Adjacence, all = TRUE)
	Adja <- data.frame(Playlist[, seq(4,ncol(Playlist))], stringsAsFactors=FALSE )
	for (k in 1:nrow(Adja))
	{
		for (n in 1:ncol(Adja))
		{
			if (is.na(Adja[k,n]))
			{
				p <- n+3
				Playlist[k,p] <- 0
			}
		}
	}
	while (ncol(Playlist)>4)
	{
		for (n in 1:nrow(Playlist))
		{
			Playlist[n,4] <- sum(as.numeric(Playlist[n,4]),as.numeric(Playlist[n,5]))
		}
		Playlist <- Playlist [,-c(5)]
	}	
	Playlist <- Playlist[order(Playlist[,4],decreasing =T),]
	toto <- Playlist[1:25,2:3]
#	show(toto)
}