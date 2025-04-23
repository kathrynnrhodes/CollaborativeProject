library(tidyverse)
f <- file.choose()
body_count <- read.csv(f, header = TRUE)
section_list <- select(body_count, group_cols())

install.packages("magick")
library(magick)
f <- file.choose()
image1 <- image_read(f)
image1

analyze_image <- function(image, threshold = "60%") {
  split_image <- image_separate(image)
  split_image <- image_threshold(split_image, type = "white", threshold = threshold)
  final <- image_combine(split_image)
  blank <- image_threshold(image, type = "white", threshold = "0%")
  comp <- image_compare_dist(final, blank, metric = "AE")
  info <- image_info(image)
  percent <- (comp$distortion/(info$width * info$height))*100
  paste0(percent, "%")
}
analyze_image(image1, threshold = "40%")

split_image <- image_separate(image1)
split_image
split_image <- image_threshold(split_image, type = "white", threshold = "60%")
split_image
split_image <- image_threshold(split_image, type = "black", threshold = "60%")
split_image
final_image <- image_combine(split_image)
final_image
blank <- image_threshold(image1, type = "white", threshold = "0%")
blank
fill <- image_threshold(image1, type = "black", threshold = "1000%")
fill
image_com <- image_compare_dist(final_image, blank, metric = "AE")
print(image_com)
1208*842
(image_com$distortion / (1208*842)) *100
