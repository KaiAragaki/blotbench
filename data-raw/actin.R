## code to prepare `actin` dataset goes here
library(bladdr)

actin <- get_gbci(
  "Raw Data/ChemiDoc/aragaki-kai/2022-03-23_erda-tc/parp-trail-actin.tif"
) |>
  magick::image_read()

usethis::use_data(actin, overwrite = TRUE)
