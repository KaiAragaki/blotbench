get_side_dims <- function(wb) {
  list(
    w = get_side_w(wb),
    h = get_side_h(wb)
  )
}

get_side_w <- function(wb, extra = grid::unit(0.2, "lines")) {
  ra <- row_annot(wb)
  ca <- col_annot(wb)
  all_text_vec <- c(ra$name, colnames(ca))
  max <- lapply(all_text_vec, \(x) grid::grobWidth(grid::textGrob(x))) |>
    Reduce(grid::unit.c, x = _) |>
    max()
  max + extra
}

get_side_h <- function(wb) {
  get_blot_h(wb)
}

get_side_hs <- function(wb) {
  get_blot_hs(wb)
}
