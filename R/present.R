# Will produce a list of dimensions
get_dimensions <- function(wb) {
  blot <- get_blot_dims(wb)
  top <- get_header_dims(wb)
  side <- get_side_dims(wb)
  list(blot = blot, top = top, side = side)
}

vec_to_grob_heights <- function(vec) {
  lapply(vec, \(x) grid::grobHeight(textGrob(x))) |>
    Reduce(unit.c, x = _)
}

make_layout <- function(dims) {
  grid.layout(
    2, 2,
    widths = unit.c(dims$side$w, dims$blot$w),
    heights = unit.c(dims$top$h, dims$blot$h)
  )
}
