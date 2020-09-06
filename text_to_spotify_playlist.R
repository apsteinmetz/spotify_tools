# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(spotfuzz)
# relies on spotify credentials stored in system environment variables

song_file <- paste0("wdub playlist 79-80.csv")
playlist_name= "WDUB Class of 1980"
playlist_desc = "From Art Steinmetz's WDUB playlists at Denison University"

ac = get_spotify_access_token() # local override of environment variables
# --------------------------------------------------------
# load and clean up the playlist
raw_playlist <- read_csv(song_file)
playlist <- raw_playlist %>% as_tibble() %>% mutate_all(as.character)

# MAIN FLOW OF PROGRAM --------------------------------------------------
# SEARCH FOR SONG AT SPOTIFY AND RETRIEVE URI FOR PLAYLIST
#why doesn't this work?
#new_playlist <- playlist %>% 
#   mutate(track_uri = get_track_uri(paste0("track:",song," artist:",artist)))
# so do it the old-fashioned way
print("Getting Track URIs")


uri_list <- spotfuzz::fuzzy_search_spotify(playlist$artist,playlist$title)
playlist <- bind_cols(raw_playlist,uri_list)


available_tracks <- playlist %>% filter(!is.na(track_uri)) %>% pull(track_uri)
missing_tracks <- playlist %>% filter(is.na(track_uri)) %>% 
   mutate(a_s = paste0(artist,", ",title))
missing_tracks %>% select(a_s) %>% print(n=25)

# CHANGE THINGS IN SPOTIFY USER ACCOUNT.  CAUTION. ------------------------------------
print("Making Playlist at Spotify")

spot_playlist <- create_playlist(user_id = get_my_profile()$id,
                                name = playlist_name,
                                description = playlist_desc)



available_tracks <- playlist %>% 
   filter(!is.na(track.uri)) %>% 
   pull(track.uri)

# load 100 tracks at a time to playlist
cuts <- cut_width(1:length(available_tracks),100,labels=FALSE)
for (n in 1:max(cuts)){
   spotifyr::add_tracks_to_playlist(spot_playlist$id,available_tracks[which(cuts==n)])
}

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
