# make playlist for spotify
# devtools::install_github('charlie86/spotifyr')
library(tidyverse)
library(spotifyr)
library(tuber)


yt_oauth(Sys.getenv("goog_Rstats_client"),Sys.getenv("goog_Rstats_client"))
yt_token()

get_stats(video_id="N708P-A45D0")
list_my_videos()
View(Sys.getenv() %>% enframe())
Sys.getenv("goog_Rstats_client")
tuber::yt_oauth(app_id = Sys.getenv("goog_Rstats_client"),app_secret = Sys.getenv("goog_Rstats_secret"))


access_token <- get_spotify_access_token()

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
   select(name, genres) %>% 
   rowwise %>% 
   mutate(genres = paste(genres, collapse = ', ')) %>% 
   ungroup

get_my_profile()$id
