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

filterArtistRemoveCollabs = function(song, artist) {
  # Checks to see if an artist title might be a collaboration
  wordsThatIndicateCollab = "&|and|feat|with|/|by"
  if (grepl(wordsThatIndicateCollab, artist)) {
    queryUrl = paste("http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key=", lastfm_key, "&artist=", artist, "&track=", song, "&format=json", sep = "")
    trackData = fromJSON(URLencode(queryUrl, repeated = FALSE, reserved = FALSE))
    primaryArtist = trackData$track$artist$name
    
    print(paste(artist, " to ", primaryArtist))
    return(primaryArtist)
  }
  else {
    print(paste("Keeping ", artist))
    return(artist)
  }
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
    topSongsUrl = paste("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks&artist=", artist, "&api_key=", lastfm_key,"&format=json&limit=40", sep = "")
    topSongsUrl = URLencode(topSongsUrl, repeated = FALSE, reserved = FALSE)
    topSongsFullData = fromJSON(topSongsUrl)
    
    # This clears up any formatting issues, like all lowercase or missing aposthropehe
    actualArtist = as.character(topSongsFullData$toptracks$track$artist$name[1])
    if (length(actualArtist) > 0) {
      assign("actualArtist", actualArtist, envir = parent.frame())
    }
    else {
      assign("actualArtist", artist, envir = parent.frame())
    }
    
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
  
  topSong = topSongs[which.max(topSongs$plays), 1]
  topSong = as.character(topSong)

  playCounts = as.numeric(paste(topSongs$plays))
  sdRatioScore = calculateSdRatio(playCounts)
  
  #Sys.sleep(0.3)
  return(list("actualArtist" = actualArtist, "sdRatioScore" =  sdRatioScore, "song" = topSong))
}

# Get every song that has ever appeared in the Top 40, and then get the number of times each artist has appeared (once per song)
songData = getListOfSongsInTop40()    # or load("AllSongsInTop4.RData")
save(songData, file = "AllSongsInTop40.RData")
appearances = getNumberOfAppearances(songData)

# Get a list of every artist that has had three or fewer appearances on the Top 40
threeOrLessHits = dplyr::filter(appearances, Freq < 4)

# Try and remove collaborations
threeOrLessHits = dplyr::filter(threeOrLessHits, !(grepl(" and ", Var1)&!grepl("and the", Var1)&!grepl("and his", Var1)))
threeOrLessHits = dplyr::filter(threeOrLessHits, !(grepl("&", Var1)))
threeOrLessHits = dplyr::filter(threeOrLessHits, !(grepl("with |ft. |ft |featuring|/|feat. |featruing", Var1)))

possibleWonders = as.vector(threeOrLessHits$Var1)

scoresAndSongs = lapply(possibleWonders, FUN = getWonderScoreOfTopSongs) # Or skip a few lines and load("ScriptResults.RData")
sdRatioScores = lapply(scoresAndSongs, FUN = function(x) {return(x[["sdRatioScore"]])})
songs = lapply(scoresAndSongs, FUN = function(x) {return(x[["song"]])})
actualArtists = lapply(scoresAndSongs, FUN = function(x) {return(x[["actualArtist"]])})

songsAndScoresDf = data.frame(artists = unlist(actualArtists), sdRatioScore = unlist(sdRatioScores), hit = unlist(songs))
save(songsAndScoresDf, file = "ScriptResults.RData")

exportData = dplyr::filter(songsAndScoresDf)

