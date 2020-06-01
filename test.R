# make playlist for spotify
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)

View(Sys.getenv() %>% enframe())


access_token <- get_spotify_access_token()

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
   select(name, genres) %>% 
   rowwise %>% 
   mutate(genres = paste(genres, collapse = ', ')) %>% 
   ungroup

get_my_profile()$id

create_playlist(user_id = get_my_profile()$id,"api_test")


playlist <- read.delim("raw_bk_playlists/bk_2020-05-29.txt",sep='"',col.names = c("artist","song","dummy")) %>% 
   as_tibble() %>% 
   select(-dummy) %>% 
   mutate_all(str_trim) %>% 
   mutate(artist = str_replace_all(artist,"-"," ")) %>% 
   mutate(song = str_replace_all(song,"-"," ")) %>% 
   mutate_all(str_remove_all,"[[:punct:]]")

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

test_q <- "track:Dont Come Close artist:Ramones"
test_q <-"track:The Devil Came from Kansas artist:Procol Harum"

test_q <- paste0("track:",playlist[11,]$song," artist:",playlist[11,]$artist)
print(test_q)
temp <- search_spotify(test_q,type="track",market="US")

temp <- get_track_uri(q=test_q)

#why doesn't this work?
#new_playlist <- playlist %>% 
#   mutate(track_uri = get_track_uri(paste0("track:",song," artist:",artist)))

uri_list <- tibble() 
for (n in 1:nrow(playlist)) {
   uri_list <- bind_rows(uri_list,get_track_uri(paste0("track:",playlist[n,]$song,
                                       " artist:",playlist[n,]$artist)))
}

playlist <- bind_cols(playlist,uri_list)


show_name <-"Blackhole_5-30-2020"
show_desc <-"Bill Kelly's Blackhole Bandstand playlist for 5/30/2020, SXM show # 1032"
new_playlist <- create_playlist(user_id = get_my_profile()$id,
                                name = show_name,
                                description = show_desc)
available_tracks <- playlist %>% filter(spotify_uri != "not found") %>% pull(spotify_uri)
spotifyr::add_tracks_to_playlist(new_playlist$id,available_tracks)
