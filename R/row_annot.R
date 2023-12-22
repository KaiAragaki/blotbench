#' Get or set row annotation for a `wb` object
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
  stopifnot(is.vector(value) || is.data.frame(value) || is.null(value))

  if (is.vector(value)) value <- data.frame(name = value)

  check_row_annot(value)

  x$row_annot <- value

  validate_wb(x)

  x
}

check_row_annot <- function(ra) {
  if (!is.null(ra) && (ncol(ra) != 1 || colnames(ra) != "name"))
    rlang::abort("row_annot should only have 1 column, named `name`")
}
