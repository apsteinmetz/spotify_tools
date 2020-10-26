# make playlist for spotify from Bill Kelly's
# SiriusXM playlist posts on Facebook
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(spotfuzz)
# relies on spotify credentials stored in system environment variables
play_date <- "2020-10-24"
song_file <- paste0("raw_bk_playlists/bk_",play_date,".txt")
show_name <-paste0("Blackhole_",play_date)

# local override of environment variables
ac = get_spotify_access_token(
   client_id = Sys.getenv("SPOTIFY_BK_ID"),
   client_secret = Sys.getenv("SPOTIFY_BK_SECRET")
)
# --------------------------------------------------------
# load and clean up the playlist
raw_playlist <- read_delim(song_file,delim='\"',
                           col_names = c("artist","song","dummy")) %>% 
   as_tibble() %>% 
   mutate(dummy = as.character(dummy)) %>% 
   mutate(song=if_else(!is.na(dummy),dummy,song)) %>%
   select(-dummy) %>%
   # don't know what CSW is
   mutate(artist = str_remove_all(artist,"^CSW:")) %>%
   mutate(artist = str_replace_all(artist,"-"," ")) %>%
   mutate(song = str_replace_all(song,"-"," ")) %>%
   mutate(song = str_replace_all(song,"/"," ")) %>%
   mutate_all(str_replace_all,"&","and") %>%
   #mutate_all(str_remove_all,"[[:punct:]]") %>%
   mutate_all(str_trim) %>%
   {.}

# MAIN FLOW OF PROGRAM --------------------------------------------------
# SEARCH FOR SONG AT SPOTIFY AND RETRIEVE URI FOR PLAYLIST
#why doesn't this work?
#new_playlist <- playlist %>% 
#   mutate(track_uri = get_track_uri(paste0("track:",song," artist:",artist)))
# so do it the old-fashioned way
print("Getting Track URIs")


playlist <- raw_playlist
uri_list <- fuzzy_search_spotify(playlist$artist,
                                 playlist$song,
                                 progress = TRUE)
playlist <- bind_cols(playlist,uri_list) %>% select(song,spot_track,everything())

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
                                name = show_name,
                                description = show_desc)

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
