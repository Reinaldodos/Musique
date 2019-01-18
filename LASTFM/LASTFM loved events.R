
source("LASTFM loved2.R")


doc <- xmlInternalTreeParse(file = "http://ws.audioscrobbler.com/2.0/?method=geo.getevents&location=Paris&limit=10000&api_key=35c710f49c77939574619a7de41f7267")
r <- xmlRoot(doc)
events <- r[[1]]
events.longueur <- xmlSize(events)

Selection <- NULL

for (m in 1:as.integer(events.longueur))
{
	cancelled <- xmlValue(events[[m]][["cancelled"]])
 	if (cancelled != "1")
	{
		country <- xmlValue(events[[m]][["venue"]][["location"]][["country"]])
		if (country == "France")
		{
			mbid <- xmlValue(events[[m]][["id"]])
			title <- xmlValue(events[[m]][["title"]])
			name <- xmlValue(events[[m]][["venue"]][["name"]])
			date <- xmlValue(events[[m]][["startDate"]])
			artist <- xmlValue(events[[m]][["artists"]][["artist"]])
			
			Test <- cbind(mbid, title, name, date, artist)
			Selection <- rbind(Selection,Test)					
		}
	}
}

Selections <- merge(BEST_OF, Selection)
Selections <- unique(subset(Selections, select = c(title, name, date, artist)))

show(Selections)