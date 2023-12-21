#' Get or set row annotation for a wb object
#' @param x A `wb` object
#' @export
row_annot <- function(x) {
  UseMethod("row_annot")
}

#' @export
row_annot.wb <- function(x) {
  x$row_annot
}

#' @rdname row_annot
#' @param value Either a `data.frame` with a single column `name`, or a
#'   character vector
#' @export
`row_annot<-` <- function(x, value) {
  UseMethod("row_annot<-")
}

#' @export
`row_annot<-.wb` <- function(x, value) {
  x$row_annot <- value
  x
}

make_row_annot <- function(wb) {
  annot <- row_annot(wb)
  y <- seq(from = 0, to = 1, by = 1 / nrow(annot))
  diff <- diff(y)
  y <- y[1:(length(y) - 1)]
  y <- y + diff / 2
  y <- rev(y) # to be same order as wb
  grid.newpage()
  grid.text(
    row_annot(wb)[[1]],
    just = "right",
    y,
    x = 0.9
  )
}
