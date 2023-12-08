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

apply_transform <- function(wb) {
  transforms <- transforms(wb)
  imgs <- imgs(wb)

  # Applying in magick is weird
  # This is the easiest way:
  new_imgs <- c()
  for (i in seq_along(imgs(wb))) {
    new_imgs <- image_join(new_imgs, transform_blot(imgs[i], transforms[i, ]))
  }
  new_imgs
}

transform_blot <- function(img, tf) {
  crop_geom <- magick::geometry_area(
    width = tf$width, height = tf$height, x_off = tf$xpos, y_off = tf$ypos
  )
  img <- magick::image_rotate(
    img, degrees = tf$rotate
  ) |>
    magick::image_crop(crop_geom)

  if (tf$flip) img <- magick::image_flop(img)

  img
}
