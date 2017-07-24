library(dplyr)
library(jsonlite)

#Gets songsAndScoresDf
load("ScriptResults.RData")

removeArtists = c("Johnny Cash And The Tennessee Three", "USA for Africa", "Black Men United", "LUDACRIS CO-STARRING T-PAIN", "Band Aid"
                 , "Bobby Vee and the Strangers", "henhouse five plus too", "Jerry Butler & The Impressions", "Joe South & The Believers",
                 "Roy Orbison and The Candy Men", "David A. Stewart introducing Candy Dulfer", "raymond lefevre and his orchestra", "The Little Dippers")

wonders = dplyr::filter(songsAndScoresDf, !(artists %in% removeArtists))

# Add missing years
wonders$year[10] = 1993
wonders$year[152] = 1959
wonders$year[160] = 1997
wonders$year[164] = 2000

# Shorten this absurdly long song title
wonders$hit = as.character(wonders$hit)
wonders$hit[15] = "San Fransisco"

inJson = jsonlite::toJSON(wonders)
write(inJson, "viz/OneHitWonders.json")
