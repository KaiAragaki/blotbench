
mis <- list(imgs = c(logo, logo))

library(magick)

cutter <- function(imgs) {
  for (i in seq_along(imgs)) {
    img <- magick::image_crop(imgs[i], "100")
    imgs[i] <- img
  }
  imgs
}

image_info(logo)
image_info(cutter(logo))
image_info(logo)

cutter2 <- function(imgs) {
  lapply(imgs, \(x) image_crop(x, "100"))
}


make_col_annot_layout <- function() {
  # One col for the actual annotations, the other for the names of of each
  # column
  grid.layout(
    nrow = rows,
    ncol = 2,
    widths = unit(c(1, 1), c("null", "grobwidth"), list(NULL, label))
  )
}

present_wb_test <- function(wb) {
  # Annotation "block" dimensions cannot be calculated in isolation
  # They all need to be calculated with each other. With those dimensions,
  # we can then create annotation blocks individually.
  dims <- get_dimensions(wb)
  img <- apply_transform(wb)
  side_annot <- make_side_annot(wb)
  top_annot <- make_top_annot(wb)
  layout <- grid.layout(
    nrow = 2,
    ncol = 2,
    widths = unit(
      c(1, 1), c("grobwidth", "grobwidth"), list(side_annots, image)
    ),
    heights = unit(
      c(1, 1), c("grobheight", "grobheight"), list(top_annots, image)
    )
  )
  frame <- frameGrob(layout = layout)
  frame <- placeGrob(frame, )
}

produce_wb_img <- function(wb) {
  layout <- make_layout(get_dimensions(wb))
  fg <- frameGrob(
    layout = layout,
    vp = viewport(width = unit(0.8, "npc"), height = unit(0.8, "npc"))
  )
  fg <- placeGrob(fg, finalize_wb(wb) |> rasterGrob(), row = 2, col = 2)
  # temp:
  fg <- placeGrob(
    fg,
    top_annot(wb),
    row = 1, col = 2
  )
  # temp - content is right but formatting is not
  fg <- placeGrob(
    fg,
    textGrob(
      col_annot(wb) |> colnames() |> paste(collapse = "\n"),
      hjust = 1,
      x = unit(1, "npc")
    ),
    row = 1, col = 1
  )
  # temp - content is right but formatting is not
  fg <- placeGrob(
    fg,
    textGrob(
      row_annot(wb)$name |> paste(collapse = "\n"),
      hjust = 1,
      x = unit(1, "npc")
    ),
    row = 2, col = 1
  )
  fg
}

