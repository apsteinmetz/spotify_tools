library(spotifyr)

ac <- get_spotify_access_token(client_id = '8db2b0a52b7045758fa428f8b2f8fdb8',
                                         client_secret = 'e283b6aafe8e42a6aeb94112314f9615')

Sys.setenv(SPOTIFY_CLIENT_ID = '8db2b0a52b7045758fa428f8b2f8fdb8')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e283b6aafe8e42a6aeb94112314f9615')

