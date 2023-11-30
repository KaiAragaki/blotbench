default_transforms <- function(imgs) {
  info <- magick::image_info(imgs)
  data.frame(
    width = info$width,
    height = info$height,
    xpos = 0,
    ypos = 0,
    rotate = 0,
    flip = FALSE
  )
}

#' @export
transforms <- function(x) {
  UseMethod("transforms")
}

#' @export
transforms.wb <- function(x) {
  x$transforms
}

#' @export
`transforms<-` <- function(x, value) {
  UseMethod("transforms<-")
}

#' @export
`transforms<-.wb` <- function(x, value) {
  x$transforms <- value
  x
}
