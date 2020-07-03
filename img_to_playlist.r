# playlist image to text
library(tidyverse)
library(tesseract)
library(magick)
library(tokenizers)

# see what tesseract sees
new_eng <- tesseract(options = list(tessedit_write_images="1"))
ocr(pl_img_raw,engine = new_eng)

ocr_lines <- function(file){
   tesseract::ocr(file) %>%
      tokenize_regex(pattern = "\\n") %>%
      unlist() %>%
      enframe(name = "line_num",value="line")
}

file_name = "bk_80_04_20_trim.jpg"
path = "img/"
file = paste0(path,file_name)


# load image
pl_img_raw <- image_read(file)
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
