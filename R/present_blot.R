# It's not necessarily the most efficient way to code, but creating and calling
# separate get_blot_x functions keeps it consistent between other functions

get_blot_dims <- function(wb) {
  list(
    w = get_blot_w(wb),
    h = get_blot_h(wb)
  )
}

get_blot_w <- function(wb) {
  grobWidth(rasterGrob(finalize_blot(wb)))
}

get_blot_h <- function(wb) {
  sum(get_blot_hs(wb))
}

get_blot_hs <- function(wb) {
  tf <- apply_transform(wb)
  lapply(
    tf,
    \(x) grid::grobHeight(grid::rasterGrob(x))
  ) |>
    Reduce(grid::unit.c, x = _)
}

finalize_blot <- function(wb) {
  wb |>
    apply_transform() |>
    magick::image_append(stack = TRUE)
}
