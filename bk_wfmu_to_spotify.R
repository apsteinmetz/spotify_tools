#Create Spotify Playlists from Bill Kelly's WFMU playlists
# make playlist for spotify
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(tidyverse)
library(lubridate)
# relies on spotify credentials stored in system environment variables

load("../wfmu/playlists.rdata")
bk <- filter(playlists,DJ=="TW") %>% 
   rename(artist = Artist,song = Title) %>% 
   mutate(a_s = paste(artist,";" ,song)) %>% 
   mutate(y = year(AirDate)) %>% 
   mutate(era = cut(y,4,labels = c("I","II","III","IV")))
   

bk_top_100 <- bk %>% 
   group_by(era,a_s) %>% 
   tally %>% 
   #ultimately we will take top 100 but get extra to allow for missing songs
   slice_max(order_by = n,n=150) %>%
   separate(a_s,into=c("artist","song"),sep=";",remove = FALSE) %>% 
   mutate(artist = str_replace_all(artist,"-"," ")) %>% 
   mutate(song = str_replace_all(song,"-"," ")) %>% 
   mutate_all(str_remove_all,"[[:punct:]]") %>% 
   mutate_all(str_trim) %>% 
   mutate(n = as.numeric(n))
   

# -------------------------------------------------------
get_spotify_track <- function(q){
   result1 <- search_spotify(q,type="track",market="US")
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
   result1 <- search_spotify(q,type="track",market="US")
   if(is_empty(result1)){
      retval <- NA
   }else{
      retval <- track_uri=result1[1,]$uri[1]
   }
   return(retval)
}
# --------------------------------------------------------

playlist <- bk_top_100

playlist_for_era <- function(era1){
   filter(bk_top_100,era == era1)
}


# SEARCH FOR SONG AT SPOTIFY AND RETRIEVE URI FOR PLAYLIST
#why doesn't this work?
#new_playlist <- playlist %>% 
#   mutate(track_uri = get_track_uri(paste0("track:",song," artist:",artist)))
# so do it the old-fashioned way
uri_list <- tibble() 
for (n in 1:nrow(playlist)) {
   uri_list <- bind_rows(uri_list,get_spotify_track(paste0("track:",playlist[n,]$song,
                                                           " artist:",playlist[n,]$artist)))
}

get_spotify_track("track:Where Evil Grows artist:The Gore Gore Girls")

playlist <- bind_cols(playlist,uri_list)

available_tracks <- playlist %>% filter(!is.na(track_uri)) %>% 
   slice_max(order_by = n,n=100,with_ties=FALSE)
missing_tracks <- playlist %>% filter(is.na(track_uri)) %>% 
   mutate(a_s = paste0(artist,", ",song))
missing_tracks %>% select(a_s) %>% print(n=25)

# -----------------------------------------------------------------
make_playlist_at_spotify <- function(era1){
   years = switch (era1,
                   "I" = "2001-2005",
                   "II" = "2006- 2010",
                   "III" = "2010-2015",
                   "IV" = "2016-2020"
   )
   show_desc <-paste0("Bill Kelly's Teenage Wasteland on WFMU. Top 100 by era. Era ", era1,":",years,
                      ". Top song list created using the WFMU playlist analyzer at wfmu.servebeer.com. ",
                      "NOTE: MANY of the top songs are not on Spotify. ",
                      "See Bill's DJ page at WFMU.org for complete track list."
   )
   show_name <- paste("Bill Kelly's Teenage Wasteland Top 100:",years)
   tracks_to_add <-  available_tracks %>% 
      filter(era == era1) %>% 
      pull(track_uri)
   # CHANGE THINGS IN SPOTIFY USER ACCOUNT.  CAUTION. ------------------------------------
   
   spot_playlist <- create_playlist(user_id = get_my_profile()$id,
                                    name = show_name,
                                    description = show_desc)
   
   spotifyr::add_tracks_to_playlist(spot_playlist$id,tracks_to_add)
   
   return(spot_playlist$external_urls$spotify)
   
}


playlist_url <- make_playlist_at_spotify("II")
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

