# It's not necessarily the most efficient way to code, but creating and calling
# separate get_blot_x functions keeps it consistent between other functions

get_blot_dims <- function(wb) {
  list(
    w = get_blot_w(wb),
    h = get_blot_h(wb)
  )
}

get_blot_w <- function(wb) {
  get_blot_w <-
  #TEMP
  grid::grobWidth(grid::rasterGrob(apply_transform(wb)))
}

get_blot_h <- function(wb) {
  tf <- apply_transform(wb)
  lapply(
    tf,
    \(x) grid::grobHeight(grid::rasterGrob(x))
  ) |>
    Reduce(grid::unit.c, x = _) |>
    sum()
}

get_blot_hs <- function(wb) {
  tf <- apply_transform(wb) |> imgs()

  # Get the height they'll be when both stretched to the same width:
  # make sure this is applicable to many different imgs
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

finalize_blot <- function(wb) {
  wb |>
    apply_transform() |>
    imgs() |>
    magick::image_apply(magick::image_resize, get_widest_img_size(wb)) |>
    magick::image_append(stack = TRUE)
}
