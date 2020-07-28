# make spotify playlist from discogs tracklist URL
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(rvest)
library(polite)
# relies on spotify credentials stored in system environment variables

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
multi_search_spotify <- function(playlist1){
   uri_list <- tibble()
   for (n in 1:nrow(playlist1)) {
      uri_list <- bind_rows(uri_list,get_spotify_track(paste0("track:",playlist1[n,]$song,
                                                              " artist:",playlist1[n,]$artist)))
   }
   return(uri_list)
}
# --------------------------------------------------------
# load and clean up the playlist
# MAIN FLOW OF PROGRAM --------------------------------------------------
discogs_album_url <- "https://www.discogs.com/Various-20-Of-Another-Kind-Volume-Two/master/326726"

cat("Getting Track List")
tracklist_raw <- read_html(discogs_album_url) %>% 
   html_node("#tracklist > div > table") %>% 
   html_table()

cat("Cleaning Track List")
tracklist <- tracklist_raw %>% 
   rename(artist=X1,song=X2) %>% 
   select(-X3) %>% 
   mutate(artist=str_remove(artist,"^\\–")) %>% 
   mutate(artist=str_remove(artist,"\\([0-9]{1,2}\\)")) %>% 
   mutate_all(str_trim)
   {.}
   
   
cat("Getting Track URIs")


uri_list <- multi_search_spotify(tracklist)
playlist <- bind_cols(tracklist,uri_list)

# make another pass to see if alt_artist gets a hit
# alt_playlist <- playlist %>% 
#    filter(is.na(spot_artist)) %>% 
#    filter(artist != alt_artist) %>%
#    mutate(artist = alt_artist) %>% 
#    select(id,artist,song)
# alt_uri_list <- multi_search_spotify(alt_playlist)
# alt_playlist <- bind_cols(alt_playlist,alt_uri_list) %>% 
#    filter(!is.na(spot_artist)) %>% 
#    mutate(alt_artist = artist)
# 
# playlist <- playlist %>%
#    anti_join(alt_playlist, by = "id") %>%
#    bind_rows(alt_playlist) %>%
#    arrange(id)


available_tracks <- playlist %>% filter(!is.na(track_uri)) %>% pull(track_uri)
missing_tracks <- playlist %>% filter(is.na(track_uri)) %>% 
   mutate(a_s = paste0(artist,", ",song))
missing_tracks %>% select(a_s) %>% print(n=25)

show_desc <-paste0("Bill Kelly's Blackhole Bandstand playlist for ",play_date," SXM show. ",
                   nrow(missing_tracks), " of ", nrow(playlist),
                   " songs missing.  See Bill's Facebook page for complete track list.")

# CHANGE THINGS IN SPOTIFY USER ACCOUNT.  CAUTION. ------------------------------------
print("Making Playlist at Spotify")

spot_playlist <- create_playlist(user_id = get_my_profile()$id,
                                name = list_name,
                                description = list_desc)

spotifyr::add_tracks_to_playlist(spot_playlist$id,available_tracks)

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
