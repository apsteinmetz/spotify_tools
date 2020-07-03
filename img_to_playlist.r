# playlist image to text
library(tidyverse)
library(tesseract)
library(magick)
library(tokenizers)
library(hunspell)

ocr_lines <- function(file){
   tesseract::ocr(file) %>%
      tokenize_regex(pattern = "\\n") %>%
      unlist() %>%
      enframe(name = "line_num",value="line")
}

file_name = "bk_80_04_20_trim.jpg"
path = "img/"
pf = paste0(path,file_name)
# see what tesseract sees
new_eng <- tesseract(options = list(tessedit_write_images="1"))
ocr(pf,engine = new_eng)


# load image
pl_img_raw <- image_read(pf)
# enhance

pl_img <- pl_img_raw %>%
   image_border(geometry = "30x30",color="white") %>%
   image_quantize(colorspace = "gray") %>%
   #image_modulate(brightness = 120) %>%
   #image_normalize() %>%
   #image_shear(geometry = geometry_area(x_off = 2014,y_off=2000)) %>%
   {.}

pl_img_raw
pl_text_raw <- ocr_lines(pl_img_raw)
pl_text_raw

words <- tokenizers::tokenize_words(pl_text_raw$line,strip_punct = TRUE) %>%
   unlist(recursive = FALSE)


# USE HUNSPELL SPELL CHECKER
words_df <- enframe(words,name=NULL,value="word") %>%
   mutate(correct=hunspell_check(word)) %>%
   mutate(suggestions=hunspell_suggest(word))

# get most likely term
best_suggestion <- words_df$suggestions %>%
   map(pluck,1,.default=NA) %>%
   unlist() %>%
   enframe(name=NULL,value="suggestion") %>%
   bind_cols(words_df) %>%
   filter(correct == FALSE) %>%
   select(word,suggestion) %>%
   mutate(suggestion=tolower(suggestion))

# choose alternate word
spell_fix_line <- function(line){
   for (n in 1:nrow(best_suggestion)){
      line <- if_else(str_detect(line,best_suggestion$word[n]),
                      str_replace(line,best_suggestion$word[n],best_suggestion$suggestion[n]),
                      line)
   }
   return(line)
}


pl_text <- pl_text_raw %>% 
   select(-line_num) %>%
   mutate(line = tolower(line)) %>% 
   unlist() %>% 
   map(spell_fix_line) %>% 
   unname() %>% 
   unlist() %>% 
   str_to_title() %>% 
   enframe(name = NULL,value="corrected") %>% 
   bind_cols(pl_text_raw) %>%
   select(line_num,line,corrected) %>% 
   separate(col = corrected,into = c("title","artist"),sep = "[-—~]+",remove = FALSE) %>% 
   {.}


