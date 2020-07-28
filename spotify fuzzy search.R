# spotify track fuzzy search



artist = "Kid Gulliver"
track = "I Wanna Be a Popstar"

# Return a tibble with a subset of spotify metadata -----------------
my_search_spotify <- function(artist,track){
  search_string = paste0("track:",track," artist:",artist)
  result <- search_spotify(search_string,market="US")
  metadata <- result$tracks$items %>% as_tibble()
  if(nrow(metadata)==0){
    # nothing found
    return(NA)
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
fuzzy_search_spotify <- function(artist,track){
  # spotify won't do fuzzy searchs but does partial completions well
  # so we can fake fuzzy
  # Try 1
  retval <- my_search_spotify(artist,track)
  if(is.na(retval[1])){
    # Try 2
    # order words in track name by length
    track_words <- tibble(token = unlist(strsplit(track,split = " "))) %>% 
      mutate(len = str_length(token)) %>% 
      arrange(desc(len))
    retval <- my_search_spotify(artist,track_words$token[1])
  }
  # try the next work but that's it.
  if(is.na(retval[1])){
    # Try 3
    retval <- my_search_spotify(artist,track_words$token[2])
    if (is.na(retval[1])) return(NA)
  }
  # We got a match. Now check for false positives
  correct = adist(str_sub(track,end=10),
                  str_sub(str_remove_all(retval$spot_track,"[[:punct:]]"),end=10),
                  ignore.case = TRUE)
  if(correct > 5) return(NA)
  return(retval)
}

