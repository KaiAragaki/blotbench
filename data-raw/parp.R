## code to prepare `parp` dataset goes here
library(bladdr)

parp <- get_gbci(
  "Raw Data/ChemiDoc/aragaki-kai/2022-03-23_erda-tc/parp.tif"
) |>
  magick::image_read()

usethis::use_data(parp, overwrite = TRUE)
