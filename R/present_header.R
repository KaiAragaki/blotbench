# Row height should also include column name
#
# I'm making a design choice that everything should take wb as an argument (at
# least) rather than a derivative (ca, eg)
get_header_dims <- function(wb) {
  list(
    w = get_header_w(wb),
    h = get_header_h(wb)
  )
}

get_header_h <- function(wb) {
  sum(get_header_hs(wb))
}

get_header_hs <- function(wb) {
  lapply(
    seq_len(ncol(col_annot(wb))),
    get_header_row_h,
    wb = wb
  ) |>
    Reduce(grid::unit.c, x = _)
}

get_header_row_h <- function(wb, i, extra = grid::unit(0.2, "lines")) {
  ca <- col_annot(wb)
  col <- ca[, i, drop = FALSE]
  all_text_vec <- c(colnames(col), col[[1]])
  # I only want one value to determine row height
  all_text <- paste0(all_text_vec, collapse = "")
  grid::grobHeight(grid::textGrob(all_text)) + extra
}

get_header_w <- function(wb) {
  get_blot_w(wb)
}
