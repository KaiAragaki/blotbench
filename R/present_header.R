get_header_hs <- function(wb) {
  lapply(
    seq_len(ncol(col_annot(wb))),
    get_header_row_h,
    wb = wb
  ) |>
    Reduce(grid::unit.c, x = _)
}

# Considers both the given row title and row contents, and uses the maximum
# height.
get_header_row_h <- function(wb, i, extra = grid::unit(0.5, "lines")) {
  ca <- col_annot(wb)
  col <- ca[, i, drop = FALSE]
  all_text_vec <- c(colnames(col), col[[1]])
  all_text <- paste0(all_text_vec, collapse = "")
  grid::grobHeight(grid::textGrob(all_text)) + extra
}

get_header_w <- function(wb) {
  get_blot_w(wb)
}

get_header_layout <- function(wb) {
  grid::grid.layout(
    nrow = ncol(col_annot(wb)),
    ncol = 1,
    heights = get_header_hs(wb)
  )
}

make_header_row <- function(wb, i) {
  ca <- col_annot(wb)
  rle <- rle(ca[[i]])
  sizes <- rle$lengths / sum(rle$lengths)
  boundaries <- cumsum(sizes)
  mids <- boundaries - sizes / 2
  grid::gList(
    grid::rectGrob(),
    grid::segmentsGrob(x0 = boundaries, x1 = boundaries),
    grid::textGrob(rle$values, x = mids)
  )
}

make_header_row_title <- function(wb, i) {
  title <- colnames(col_annot(wb))[i]
  grid::gList(
    grid::textGrob(title, just = "right", x = 0.9)
  )
}

make_header_f <- function(wb, f) {
  function(wb) {
    frame <- grid::frameGrob(layout = get_header_layout(wb))
    for (i in seq_along(col_annot(wb))) {
      row_grobs <- f(wb, i)
      frame <- frame |>
        grid::placeGrob(grid::grobTree(row_grobs), row = i, col = 1)
    }
    frame
  }
}

make_header_titles <- make_header_f(wb, make_header_row_title)
make_header <- make_header_f(wb, make_header_row)
