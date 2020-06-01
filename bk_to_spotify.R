# make playlist for spotify
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
# relies on spotify credentials stored in system environment variables
play_date <- "2020-05-23"
song_file <- paste0("raw_bk_playlists/bk_",play_date,".txt")
show_name <-paste0("Blackhole_",play_date)
show_desc <-paste0("Bill Kelly's Blackhole Bandstand playlist for ",play_date,", SXM show")

# -------------------------------------------------------
get_track_uri <- function(q){
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
# --------------------------------------------------------
playlist <- read.delim(song_file,sep='"',col.names = c("artist","song","dummy")) %>% 
   as_tibble() %>% 
   select(-dummy) %>% 
   mutate_all(str_trim) %>% 
   mutate(artist = str_replace_all(artist,"-"," ")) %>% 
   mutate(song = str_replace_all(song,"-"," ")) %>% 
   mutate_all(str_remove_all,"[[:punct:]]")

#why doesn't this work?
#new_playlist <- playlist %>% 
#   mutate(track_uri = get_track_uri(paste0("track:",song," artist:",artist)))

uri_list <- tibble() 
for (n in 1:nrow(playlist)) {
   uri_list <- bind_rows(uri_list,get_track_uri(paste0("track:",playlist[n,]$song,
                                       " artist:",playlist[n,]$artist)))
}

playlist <- bind_cols(playlist,uri_list)


new_playlist <- create_playlist(user_id = get_my_profile()$id,
                                name = show_name,
                                description = show_desc)
available_tracks <- playlist %>% filter(track_uri != "not found") %>% pull(track_uri)
spotifyr::add_tracks_to_playlist(new_playlist$id,available_tracks)
