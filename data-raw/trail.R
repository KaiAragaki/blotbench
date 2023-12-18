## code to prepare `trail` dataset goes here
library(bladdr)

trail <- get_gbci(
  "Raw Data/ChemiDoc/aragaki-kai/2022-03-23_erda-tc/parp-trail.tif"
) |>
  magick::image_read()

usethis::use_data(trail, overwrite = TRUE)
