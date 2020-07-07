# make band name dictionary
library(tidyverse)
library(rvest)
library(tokenizers)

url_base <- "https://rock-bands.com/"  #suffix e.g.  r.htm

get_page <- function(letter){
   url <- paste0(url_base,letter,".htm")
   raw_page <- read_html(url)
   page <- html_text(raw_page) %>% 
      tokenize_lines() %>% 
      unlist(recursive = FALSE) %>% 
      .[-1:-26]
   page <- page[1:(grep("Rock Band Names",page)-3)]
   page <- page[grep("  ",page,invert = TRUE)]
   band_words <- page %>% 
      tokenize_words(stopwords = c("the","and")) %>% unlist()
   return(band_words)
}
all_urls <- letters %>% map(get_page)
num_url <- "0-9" %>% map(get_page)
all_urls <- c(all_urls,num_url)
write_csv()

band_words <- all_urls %>% 
   unlist() %>% 
   sort() %>% 
   unique() %>% 
   enframe(name=NULL)
write_csv(band_words,"band_words.dic",col_names = FALSE)
