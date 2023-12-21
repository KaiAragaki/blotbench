get_blot_w <- function(wb) {
  grid::grobWidth(grid::rasterGrob(apply_transform(wb)))
}

get_blot_hs <- function(wb) {
  tf <- apply_transform(wb) |> imgs()

  info <- magick::image_info(tf)
  # old_w * ? = max(info$width)
  info$scaling_factor <- max(info$width) / info$width
  info$new_height <- info$height * info$scaling_factor
  hs <- info$new_height / sum(info$new_height)
  grid::unit(hs, "npc")
}

get_blot_layout <- function(wb) {
  grid::grid.layout(
    nrow = length(imgs(wb)),
    ncol = 1,
    heights = get_blot_hs(wb),
    widths = get_blot_w(wb)
  )
}

make_blot <- function(wb) {
  frame <- grid::frameGrob(layout = get_blot_layout(wb))
  for (i in seq_along(imgs(wb))) {
    frame <- grid::placeGrob(
      frame, grid::rasterGrob(imgs(wb)[i]), row = i, col = 1
    )
  }
  frame
}

get_widest_img_size <- function(wb) {
  info <- magick::image_info(imgs(wb))
  max(info$width)
}

finalize_blot <- function(wb) {
  wb |>
    apply_transform() |>
    imgs() |>
    magick::image_apply(magick::image_resize, get_widest_img_size(wb)) |>
    magick::image_append(stack = TRUE)
}
