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

}
