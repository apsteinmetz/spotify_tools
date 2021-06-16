# parse radio playlist
library(tidyverse)
library(tesseract)
library(magick)
library(imager)
library(imagerExtra)
library(tokenizers)
library(tidytext)
library(hunspell)


ocr_engine <- tesseract(options = list(preserve_interword_spaces=1))
ocr_lines <- function(file,engine = ocr_engine){
   tesseract::ocr(file,engine = engine) %>%
      tokenize_regex(pattern = "\\n") %>%
      unlist() %>%
      enframe(name = "line_num",value="line")
}

wrko <- ocr_lines("img/WRKO Now 30 1976.jpg") %>% 
   mutate(line=str_trim(str_remove_all(line,"[^a-zA-Z ]"))) %>% 
   separate(line,into = c("title","artist","label","weeks"),sep="  {2,}") %>% 
   select(title,artist,label) %>%
   # slicing is dumb. you have to specify bad rows
   slice(-(1:5)) %>% 
   slice(-c(8,32,33))

#manual cleanup
fix(wrko)
         
save(wrko,file="wrko.rdata")         
