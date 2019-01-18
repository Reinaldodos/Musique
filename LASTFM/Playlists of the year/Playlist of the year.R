AlbumList <- read.table("Albums of the year.txt", header = FALSE, quote = "", sep = ";")
AlbumList = AlbumList[,1:2]
colnames(AlbumList) <- c("Artist", "Album")

Selection <- AlbumList

FINAL <- NULL

for(p in 1:nrow(Selection))
{
  cho <-Selection[p,]
  show(cho)
  
  artist <- as.character(cho$Artist)
  album <- as.character(cho$Album)
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
    tho <- cbind(cho,tho,tag)
    FINAL <- merge(FINAL, tho, all=TRUE)
    'FAIRE UN UPDATE'		
    
  }
}
FINAL[is.na(FINAL)] <- 0

backup <- FINAL
# name <- paste(FINAL$artist,FINAL$track, sep=" - ")
# rownames(FINAL) <- name
# FINAL <- subset(FINAL, select=-c(artist, track))

library(sqldf)

FINAL <- backup
TAGS <- read.table("Playlists of the year/TAGS.txt", sep = ";")

for(n in 1:nrow(TAGS))
{
  TAG = as.character(TAGS[n,1])
  selected <- subset(FINAL, FINAL$tho == TAG, select = c(Artist, Album))
  FINAL <- subset(FINAL, ! (FINAL$Artist %in% selected$Artist))
  
  tags <- sqldf('
                select tho, count(tho) as NB from FINAL
                group by tho
                order by NB DESC
                ')
  tags <- subset(tags, tags$NB>2)
  write.table(selected, file = paste("Playlists of the year/",TAG, ".txt", sep = ""), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

BILAN <- subset(backup, backup$tho %in% TAGS[,1])
