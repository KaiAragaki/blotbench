test_wb <- function() {
  img <- bladdr::get_gbci(
    "Raw Data/ChemiDoc/aragaki-kai/2022-03-23_erda-dr/trail-parp-actin.tif"
  ) |>
    magick::image_read()
  ca <- data.frame(
    drug = "erda",
    conc_nm = c(0, 1, 10, 100, 1000, 10000)
  )
  ra <- data.frame(name = "TRAIL")
  wb(img, ca, ra)
}
# Note - if a flip occurs in the editor, it should suggest a flip for coldata

test_input <- function() {
  list(
    width = 0,
    height = 0,
    xpos = 0,
    ypos = 0,
    rotation = 0,
    flip = TRUE
  )
}
