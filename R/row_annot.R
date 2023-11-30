#' @export
row_annot <- function(x) {
  UseMethod("row_annot")
}

#' @export
row_annot.wb <- function(x) {
  x$row_annot
}

#' @export
`row_annot<-` <- function(x, value) {
  UseMethod("row_annot<-")
}

#' @export
`row_annot<-.wb` <- function(x, value) {
  x$row_annot <- value
  x
}
