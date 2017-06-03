library(rvest)
library(dplyr)
library(jsonlite)

lastfm_key = 'ad1902e583803953df32c4655a5aace7'

getListOfSongsInTop40 = function() {
  currentUrl = "http://www.billboard.com/charts/hot-100/1958-08-16"
  
  songData = data.frame(song = character(), artist = character())
  
  repeat {
    page = read_html(currentUrl)
    
    weekNode = html_node(page, "time")
    week = html_text(weekNode)[[1]]
    print(paste("Scraping data from the week of", week))
    
    titleNodes = html_nodes(page, ".chart-row__song")
    titles = html_text(titleNodes)
    titles = titles[1:40]
    
    artistNodes = html_nodes(page, ".chart-row__artist")
    artists = html_text(artistNodes)
    artists = artists[1:40]
    
    # For comparisons, put everything in lower case and remove apostrophes and commas, as Billboard has consistency issues with those
    titles = tolower(titles)
    titles = gsub(",", " ", titles)
    titles = gsub("'", " ", titles)
    titles = gsub("\n", "", titles)
    titles = gsub("  ", " ", titles)
    titles = gsub("  ", " ", titles)
    trimws(titles)
    
    artists = tolower(artists)
    artists = gsub(",", " ", artists)
    artists = gsub("'", " ", artists)
    artists = gsub("\n", "", artists)
    artists = gsub("  ", " ", artists)
    artists = gsub("&", "and", artists)
    trimws(artists)
    
    newSongData = data.frame(song = titles, artist = artists)
    
    # Only add songs that are not already in the data
    newSongData = anti_join(newSongData, songData, by = c("song", "artist"))
    songData = rbind(songData, newSongData)
    
    disabledArrows = html_nodes(page, ".chart-nav__link--disabled")
    if (length(disabledArrows) > 0) {
      break
    }
    else {
      nextArrow = html_nodes(page, ".chart-nav__link[title=\"Next Week\"]")
      currentUrl = paste("http://www.billboard.com", html_attr(nextArrow, "href"), sep = "")
    }
  }
  
  return(songData)
}

getNumberOfAppearances = function(song_data) {
  appearancesTable = table(song_data$artist)
  appearances = as.data.frame(appearancesTable)
  return(appearances)
}

removeFeaturing = function(artists) {
  actualArtists = Filter(function(x) !any(grepl("featuring", x)), artists)
  return(actualArtists)
}

getWonderScoreOfTopSongs = function(artist) {
  getTopSongs = function(artist) {
    topSongsUrl = paste("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks&artist=", artist, "&api_key=", lastfm_key,"&format=json&limit=30", sep = "")
    topSongsUrl = URLencode(topSongsUrl, repeated = FALSE, reserved = FALSE)
    topSongsFullData = fromJSON(topSongsUrl)
    
    tracks = as.vector(topSongsFullData$toptracks$track$name)
    tracks = as.character(tracks)
    playcounts = as.vector(topSongsFullData$toptracks$track$playcount)
    playcounts = as.numeric(playcounts)
    #mbid = as.vector(topSongsFullData$toptracks$track$mbid)
    #mbid = as.character(mbid)
    
    return(data.frame(song = tracks, plays = playcounts))
  }
  
  removeRemixesFromTopSongs = function(top_songs) {
    topSong = as.character(top_songs$song[1])
    
    topSong = gsub("\\(", "", topSong)
    topSong = gsub("\\)", "", topSong)
    
    remixes = grepl(topSong, top_songs$song)
    remixes[1] = FALSE
    newTopSongs = filter(top_songs, !remixes)
    return(newTopSongs)
  }
  
  calculateIqrScore = function(plays) {
    if (length(plays) < 2) {
      return(0)
    }
    else {
      score = (max(plays) - median(plays)) / IQR(plays)
      return(score)
    }
  }
  
  calculateSdRatio = function(plays) {
    if (length(plays) < 2) {
      return(0)
    }
    else {
      score = sd(plays) / sd(plays[plays < max(plays)])
      return(score)
    }
  }
  
  print(paste("Getting score for",artist))
  topSongs = getTopSongs(artist)
  topSongs = removeRemixesFromTopSongs(topSongs)
  topSongs = head(topSongs, 10)
  
  topSong = as.character(topSongs$song[1])
  #topSongId = as.character(topSongs$id[1])
  
  playCounts = as.numeric(paste(topSongs$plays))
  iqrScore = calculateIqrScore(playCounts)
  sdRatioScore = calculateSdRatio(playCounts)
  
  #Sys.sleep(0.3)
  return(list("iqrScore" = iqrScore, "sdRatioScore" =  sdRatioScore, "song" = topSong))
}

getYearsFromId = function(songId) {
  songDataUrl = paste("http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key=", lastfm_key, "&mbid=", songId, "&format=json", sep = '')
  songData = fromJSON(songDataUrl)
  
  albumId = as.character(songData$track$album$mbid)
  albumUrl = paste("http://ws.audioscrobbler.com/2.0/?method=album.getInfo&api_key=", lastfm_key, "&mbid=", albumId, "&format=json", sep = '')
  albumData = fromJSON(albumUrl)
}

# Get every song that has ever appeared in the Top 40, and then get the number of times each artist has appeared (once per song)
songData = getListOfSongsInTop40()    # or load("AllSongsInTop40")
appearances = getNumberOfAppearances(songData)

# Get a list of every artist that has had three or fewer appearances on the Top 40
threeOrLessHits = filter(appearances, Freq < 4)
possibleWonders = as.vector(threeOrLessHits$Var1)

# Remove "x featuring y"
possibleWonders = removeFeaturing(possibleWonders)

scoresAndSongs = lapply(possibleWonders, FUN = getWonderScoreOfTopSongs)
iqrScores = lapply(scoresAndSongs, FUN = function(x) {return(x[["iqrScore"]])})
sdRatioScores = lapply(scoresAndSongs, FUN = function(x) {return(x[["sdRatioScore"]])})
songs = lapply(scoresAndSongs, FUN = function(x) {return(x[["song"]])})

finalData = data.frame(artists = possibleWonders, iqrScore = unlist(iqrScores), sdRatioScore = unlist(sdRatioScores), hit = unlist(songs))

finalData = filter(finalData, sdRatioScore != 0)
finalData = filter(finalData, sdRatioScore != Inf)
finalData = filter(finalData, iqrScore != Inf)
finalData = filter(finalData, iqrScore != 0)

exportData = filter(finalData, sdRatioScore > 25)
exportData$iqrScore = NULL

missedCollabs = c("kungs vs cookin on 3 burners", "peter cetera with amy grant", "johnny cash and the tennessee three", "j. frank wilson and the cavaliers", 
                  "joe cocker and jennifer warnes", "machine gun kelly x camila cabello", "b.m.u. (black men united)", "usa for africa", "kygo x selena gomez",
                  "philip bailey with phil collins", "ludacris co-starring t-pain", "shaggy feat. ricardo \"rikrok\" ducent", "bobby vee and the strangers",
                  "luther vandross and janet jackson", "amy grant with vince gill", "rod stewart with ronald isley", "stevie nicks with tom petty and the heartbreakers",
                  "g-eazy x bebe rexha", "new boyz feat. ray j", "zach sobiech", "julia michaels", "band-aid", "the little dippers")

exportData = filter(exportData, !(artists %in% missedCollabs))
exportData = arrange(exportData, desc(sdRatioScore))

exportData = head(exportData, 30)

write.csv(exportData, "OneHitWonders.csv")
