#Create Spotify Playlists from Bill Kelly's WFMU playlists
# make playlist for spotify
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(tidyverse)
library(lubridate)
library(spotfuzz) # local package

# relies on spotify credentials stored in system environment variables

load("../wfmu/playlists.rdata")
load("../wfmu/djkey.rdata")
dj_name = "Greasy"
dj_abb <- filter(DJKey,str_detect(ShowName,dj_name)) %>% pull(DJ)

dj_song_list <- filter(playlists,DJ==dj_abb) %>%
   rename(artist = Artist,song = Title) %>%
   mutate_all(str_to_title) %>% 
   mutate(artist = str_remove(artist,"^The ")) %>% 
   mutate(a_s = paste(artist,";" ,song)) %>% 
   #mutate(y = year(AirDate)) %>% 
   #mutate(era = cut(y,4,labels = c("I","II","III","IV"))) %>% 
   {.}
   
dj_top_songs <- dj_song_list %>% 
   group_by(artist,song) %>% 
   tally() %>% 
   ungroup() %>% 
   #ultimately we will take top 100 but get extra to allow for missing songs
#   slice_max(order_by = n,n=150 %>% 
   filter(n >9) %>% 
   ungroup() %>% 
   arrange(desc(n)) %>% 
   rowid_to_column(var = "id") %>% 
   {.}
   
# --------------------------------------------------------

playlist <- dj_top_songs

uri_list <- fuzzy_search_spotify(playlist$artist,
                                      playlist$song,
                                      progress = TRUE)

playlist <- bind_cols(playlist,uri_list)

available_tracks <- playlist %>% filter(!is.na(track_uri)) %>% 
   slice_max(order_by = n,n=100,with_ties=FALSE)
tracks_to_add <- available_tracks  %>% pull(track_uri)

missing_tracks <- playlist %>% filter(is.na(track_uri)) %>% 
   mutate(a_s = paste0(artist,", ",song))
missing_tracks %>% select(a_s) %>% print(n=25)

# CHANGE THINGS IN SPOTIFY USER ACCOUNT.  CAUTION. ------------------------------------
   
show_name <- "Greasy Kid Stuff on WFMU - Greatest Hits"
show_desc <- "The top songs played by Belinda and Hova Saturday Mornings on WFMU from 1997 to 2006. Alas, many aren't available on Spotify. See wfmu.org for complete playlists."
spot_playlist <- create_playlist(user_id = get_my_profile()$id,
                                 name = show_name,
                                 description = show_desc)

spotifyr::add_tracks_to_playlist(spot_playlist$id,tracks_to_add)

spot_playlist$external_urls$spotify

