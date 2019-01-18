
source("LASTFM loved2.R")

Selection <- NULL

doc <- xmlInternalTreeParse(file = "http://ws.audioscrobbler.com/2.0/?method=geo.getevents&location=Europe&festivalsonly=1&limit=600&api_key=35c710f49c77939574619a7de41f7267")
r <- xmlRoot(doc)
events <- r[[1]]
events.longueur <- xmlSize(events)




for (m in 1:as.integer(events.longueur))
{
	lineup <- events[[m]][["artists"]]
	for (p in 1:as.integer(xmlSize(lineup)))
	{
			event.artist <- xmlValue(lineup[[p]])

			ID <- xmlValue(events[[m]][["id"]])
			title <- xmlValue(events[[m]][["title"]])
			city <- xmlValue(events[[m]][["venue"]][["location"]][["city"]])
			country <- xmlValue(events[[m]][["venue"]][["location"]][["country"]])
			date <- xmlValue(events[[m]][["startDate"]])
			artists <- event.artist
			
			Test <- cbind(ID, title, city, country, date, artists)
			Selection <- rbind(Selection,Test)					
	}

}

Selection <- data.frame(Selection)
LOVE <- data.frame(LOVE)
artist <- unique(LOVE$artist)

FESTOCHES <- merge(Selection, LOVE, by.x = c("artists"), by.y = c("artist")) 
FESTOCHES <- unique(subset(FESTOCHES, select = -c(mbid, track)))

liste <- FESTOCHES$title
liste <- table(liste)
liste <- data.frame(liste)
cut <- max(liste$Freq)*.5
liste <- subset(liste, Freq>cut)

festivals <- merge(FESTOCHES, liste, by.x = c("title"), by.y = c("liste")) 
festivals <- unique(festivals)

liste <- unique(subset(festivals, select=c(date, title)))

show(liste)