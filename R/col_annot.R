#' Get or set column annotation for a wb object
#' @param x A `wb` object
#' @export
col_annot <- function(x) {
  UseMethod("col_annot")
}

#' @export
col_annot.wb <- function(x) {
  x$col_annot
}

#' @rdname col_annot
#' @param value A data.frame with each column representing a variable, and each
#'   row representing a lane in the blot. NOTE: the order of the rows should
#'   annotate the lanes left-to-right AFTER image manipulation, which might
#'   include mirroring.
#' @export
`col_annot<-` <- function(x, value) {
  UseMethod("col_annot<-")
}

#' @export
`col_annot<-.wb` <- function(x, value) {
  x$col_annot <- value
  x
}
