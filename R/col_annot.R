#' @export
col_annot <- function(x) {
  UseMethod("col_annot")
}

#' @export
col_annot.wb <- function(x) {
   x$col_annot
}

#' @export
`col_annot<-` <- function(x, value) {
  UseMethod("col_annot<-")
}

#' @export
`col_annot<-.wb` <- function(x, value) {
  x$col_annot <- value
}
