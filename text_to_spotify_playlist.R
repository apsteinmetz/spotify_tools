# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(spotfuzz)
# relies on spotify credentials stored in system environment variables

song_file <- paste0("C:\\Users\\Apste\\Documents\\R Projects\\wfmu\\data\\efd_songs_PPPNW.csv")
playlist_name= 'Evan "Funk" Davies Punk/Post-Punk/Power Pop/New Wave'
playlist_desc = "Singles from the late 70s and early 80s via WFMU."

ac = get_spotify_access_token() # local override of environment variables
auth = get_spotify_authorization_code(scope = c("playlist-modify-public","user-read-private"))

                                      



# --------------------------------------------------------
# load and clean up the playlist
raw_playlist <- read_csv(song_file)
playlist <- raw_playlist %>% 
   as_tibble() %>% 
   mutate_all(as.character) |> 
   transmute(artist = Artist,title = Title)

# one off fixes
fix_artist <- function(playlist,old_artist,new_artist){
   playlist |> 
      mutate(artist = if_else(artist == old_artist,new_artist,artist))
}

# playlist <- playlist |> 
#    fix_artist("MoDettes","Mo-Dettes") |> 
#    fix_artist("SleaterKinney","Sleater-Kinney") |> 
#    fix_artist("Lords Of The Underground","Lords of the New Church")

# MAIN FLOW OF PROGRAM --------------------------------------------------
# SEARCH FOR SONG AT SPOTIFY AND RETRIEVE URI FOR PLAYLIST
#why doesn't this work?
#new_playlist <- playlist %>% 
#   mutate(track_uri = get_track_uri(paste0("track:",song," artist:",artist)))
# so do it the old-fashioned way
print("Getting Track URIs")


# uri_list <- spotfuzz::fuzzy_search_spotify(playlist$artist,playlist$title)

playlist_s <- map(1:nrow(playlist),function(n){
   uri_list <- spotfuzz::quick_search_spotify(playlist$artist[n],playlist$title[n])
   print(uri_list)
   }) |> 
   bind_rows()

playlist_full <- bind_cols(playlist,playlist_s) |> 
   select(1,3,everything())



available_tracks <- playlist_full %>% filter(!is.na(track_uri)) %>% pull(track_uri)
missing_tracks <- playlist_full %>% filter(is.na(track_uri)) %>% 
   mutate(a_s = paste0(artist,", ",title))
missing_tracks %>% select(a_s) %>% print(n=25)

# CHANGE THINGS IN SPOTIFY USER ACCOUNT.  CAUTION. ------------------------------------
print("Making Playlist at Spotify")

spot_playlist <- create_playlist(user_id = get_my_profile(auth)$id,
                                name = playlist_name,
                                description = playlist_desc,
                                authorization = auth)



available_tracks <- playlist_full %>% 
   filter(!is.na(track_uri)) %>% 
   pull(track_uri) |> 
   unique()

# load 100 tracks at a time to playlist
cuts <- cut_width(1:length(available_tracks),100,labels=FALSE)
# divide available_tracks into groups of 100 each
available_tracks <- split(available_tracks,cuts)

for (n in 1:length(available_tracks)){
   spotifyr::add_tracks_to_playlist(spot_playlist$id,available_tracks[[n]],authorization = auth)
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
