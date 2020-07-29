# spotify track fuzzy search

require(tidyverse)
require(spotifyr)


#artist = "Kid Gulliver"
#track = "I Wanna Be a Popstar"
artist = "Little Steven and the Disciples of Soul	"
track = "Undefeated"

# Return a tibble with a subset of spotify metadata -----------------
my_search_spotify <- function(artist,track){
  search_string = paste0("track:",track," artist:",artist)
  result <- search_spotify(search_string,market="US")
  metadata <- result$tracks$items %>% as_tibble()
  if(nrow(metadata)==0){
    # nothing found
    return(tibble(spot_artist=NA,
                           spot_track=NA,
                           spot_album=NA,
                           track_number= NA,
                           release_date=NA,
                           track_uri=NA)
                 )
  } else{
    metadata <- metadata[1,] %>% 
      mutate(artist = artists[[1]]$name[1]) %>% 
      transmute(spot_artist=artist,
                spot_track=name,
                spot_album=album.name,
                track_number,
                release_date=album.release_date,
                track_uri=uri)
    
    
  }
  return(metadata)
}

# try different strategies to get tracks -----------------
vfss <- Vectorize(fuzzy_search_spotify,c("artist","track"))

fuzzy_search_spotify <- function(artist,track,progress=FALSE){
  # spotify won't do fuzzy searchs but does partial completions well
  # so we can fake fuzzy
  if(progress) cat(artist," ",track,"\n")
  # Try 1
  retval <- my_search_spotify(artist,track)
  if(is.na(retval[1])){
    # Try 2
    #alt artist search term stripping "and the..." kinds of names
    artist <-str_remove(artist,"( and the .+)|(&.+)|(w\\/.+)|(featuring .+)")
    retval <- my_search_spotify(artist,track)
    if(is.na(retval[1])){
      # Try 3. Reduce song name to longest word in song name
      # order words in track name by length
      track_words <- tibble(token = unlist(strsplit(track,split = " "))) %>% 
        mutate(len = str_length(token)) %>% 
        arrange(desc(len))
      retval <- my_search_spotify(artist,track_words$token[1])
    }
    if(is.na(retval[1])){
      # Try 4 Reduce song name to second longest word in song name. End there.
      retval <- my_search_spotify(artist,track_words$token[2])
      if (is.na(retval[1])) return(retval)
    }
  }
  # We got a match. Now check for false positives
  correct = adist(str_sub(track,end=10),
                  str_sub(str_remove_all(retval$spot_track,"[[:punct:]]"),end=10),
                  ignore.case = TRUE)
  # if title dissimilarity index is too great, reject.
  if(correct > 5) return(my_search_spotify("bogus artist","bogus song"))
  cat("success!\n")
  return(retval)
}

