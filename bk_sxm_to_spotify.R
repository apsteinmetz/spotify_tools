# make playlist for spotify from Bill Kelly's
# SiriusXM playlist posts on Facebook
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
# relies on spotify credentials stored in system environment variables
play_date <- "2020-07-26"
song_file <- paste0("raw_bk_playlists/bk_",play_date,".txt")
show_name <-paste0("Blackhole_",play_date)

ac = get_spotify_access_token() # local override of environment variables

# -------------------------------------------------------
get_spotify_track <- function(q){
   # local auth token in ac to override environment variables
   result1 <- search_spotify(q,type="track",market="US",authorization = ac)
   if(is_empty(result1)){
      retval <-tibble(spot_artist=NA,spot_name = NA,track_uri=NA)
   }else{
      retval <- tibble(spot_artist=result1$artists[[1]]$name,
                       spot_name= result1[1,]$name,
                       track_uri=result1[1,]$uri)
   }
   return(retval[1,])
}
# ------------------------------------------------------
get_track_uri <- function(q){
   result1 <- search_spotify(q,type="track",market="US",authorization = ac)
   if(is_empty(result1)){
      retval <- NA
   }else{
      retval <- track_uri=result1[1,]$uri[1]
   }
   return(retval)
}
# --------------------------------------------------------
# load and clean up the playlist
raw_playlist <- read_delim(song_file,delim='\"',
                           col_names = c("artist","song","dummy")) %>% 
   as_tibble() %>% 
   mutate(dummy = as.character(dummy)) %>% 
   mutate(song=if_else(!is.na(dummy),dummy,song)) %>%
   select(-dummy) %>%
   # don't know what CSW is
   mutate(artist = str_remove_all(artist,"^CSW")) %>%
   mutate(artist = str_replace_all(artist,"-"," ")) %>%
   #alt search term stripping "and the..." kinds of names
   mutate(alt_artist = str_remove(artist,"( and .+)|(&.+)|(w\\/.+)")) %>%
   mutate(song = str_replace_all(song,"-"," ")) %>%
   mutate(song = str_replace_all(song,"/"," ")) %>%
   mutate_all(str_remove_all,"[[:punct:]]") %>%
   mutate_all(str_trim) %>%
   rowid_to_column(var="id") %>%
   {.}

# MAIN FLOW OF PROGRAM --------------------------------------------------
# SEARCH FOR SONG AT SPOTIFY AND RETRIEVE URI FOR PLAYLIST
#why doesn't this work?
#new_playlist <- playlist %>% 
#   mutate(track_uri = get_track_uri(paste0("track:",song," artist:",artist)))
# so do it the old-fashioned way
print("Getting Track URIs")


source("spotify fuzzy search.r")
playlist <- raw_playlist
uri_list <- tibble()
for (n in 1:nrow(playlist)) {
   cat(n," ")
   result <- fuzzy_search_spotify(playlist$alt_artist[n],playlist$song[n])
   if (!is.na(result[1])){
      uri_list <- bind_rows(uri_list,result)
   } else {
      # pad with empty row
      uri_list <- add_row(uri_list)
   }
}
playlist <- bind_cols(playlist,uri_list)

available_tracks <- playlist %>% filter(!is.na(track_uri)) %>% pull(track_uri)
missing_tracks <- playlist %>% filter(is.na(track_uri)) %>% 
   mutate(a_s = paste0(artist,", ",song))
missing_tracks %>% select(a_s) %>% print(n=25)

show_desc <-paste0("Bill Kelly's Blackhole Bandstand playlist for ",play_date," SXM show. ",
                   nrow(missing_tracks), " of ", nrow(playlist),
                   " songs missing.  See Bill's Facebook page for complete track list.")

# CHANGE THINGS IN SPOTIFY USER ACCOUNT.  CAUTION. ------------------------------------
print("Making Playlist at Spotify")

# spot_playlist <- create_playlist(user_id = get_my_profile()$id,
#                                 name = show_name,
#                                 description = show_desc)
# 
# spotifyr::add_tracks_to_playlist(spot_playlist$id,available_tracks)

# ----------------------------------------------------------------------
# make text for facebook post and copy to clipboard
spot_playlist$external_urls$spotify
fb <- paste("Spotify playlist is up.","",
            "Playlist URL:",
            spot_playlist$external_urls$spotify,"",
            "Missing Tracks:",
            paste(missing_tracks$a_s,collapse = "\n"),"",
            sep = "\n")

writeClipboard(fb)
