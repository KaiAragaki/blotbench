get_side_w <- function(wb, extra = grid::unit(0.5, "lines")) {
  ra <- row_annot(wb)
  ca <- col_annot(wb)
  all_text_vec <- c(ra$name, colnames(ca))
  max <- lapply(
    all_text_vec,
    \(x) grid::grobWidth(grid::textGrob(x))
  ) |>
    Reduce(grid::unit.c, x = _) |>
    max()
  max + extra
}

get_side_hs <- function(wb) {
  get_blot_hs(wb)
}

get_side_layout <- function(wb) {
  grid::grid.layout(
    nrow = nrow(row_annot(wb)),
    ncol = 1,
    heights = get_side_hs(wb),
    widths = get_side_w(wb)
  )
}

make_side <- function(wb) {
  names <- row_annot(wb)$name
  frame <- grid::frameGrob(layout = get_side_layout(wb))
  for (i in seq_along(names)) {
    frame <- grid::placeGrob(
      frame, grid::rectGrob(), row = i, col = 1
    )
    frame <- grid::placeGrob(
      frame, grid::textGrob(names[i], just = "right", x = 0.9), row = i, col = 1
    )
  }
  frame
}
