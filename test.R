tg <- function() {
  library(bladdr)
  library(magick)
  parp <- image_read(
    system.file("extdata", "parp.tif", package = "blotbench")
  )
  trail <- image_read(
    system.file("extdata", "trail.tif", package = "blotbench")
  )
  actin <- image_read(
    system.file("extdata", "actin.tif", package = "blotbench")
  )
  ca <- data.frame(
    drug = c("DMSO", "Erdafitinib", "Erdafitinib", "Erdafitinib"),
    time_hr = c(0, 24, 48, 72)
  )
  wb <- wb(
    imgs = c(parp, trail, actin),
    col_annot = ca,
    row_annot = c("PARP", "TRAIL", "Actin")
  )
  wb
}
