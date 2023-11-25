new_wb <- function(x = list(imgs = list(),
                            col_annot = data.frame(),
                            row_annot = data.frame())) {
  stopifnot(
    is.list(x),
    is(x$imgs, "magick-image"),
    is.data.frame(x$col_annot),
    is.data.frame(x$row_annot)
  )
  structure(x, class = "wb")
}

validate_wb <- function(x) {
  x
}


#' Create a wb object
#'
#' A wb object has several fields:
#' - `imgs`: A `magick-image`, which may include one more images
#' - `col_annot`: A `data.frame`, where each row is a lane and each
#'   column is a condition
#' - `row_annot`: A `data.frame`, where each row is a band and the first column
#'   contains band names
#'
#' @export
wb <- function(imgs, col_annot, row_annot) {
  list(imgs = imgs, col_annot = col_annot, row_annot = row_annot) |>
    new_wb() |>
    validate_wb()
}

#' @export
`[.wb` <- function(x, i = NULL, j = NULL) {

  ra <- x$row_annot
  ca <- x$col_annot

  if (!is.null(i)) ra <- ra[i, , drop = FALSE]
  if (!is.null(j)) ca <- ca[j, , drop = FALSE]

  imgs <- col_index_imgs(x, j)
  if (!is.null(i)) imgs <- imgs[i]

  new_wb(list(imgs = imgs, row_annot = ra, col_annot = ca))
}

get_lane_width <- function(wb) {
  get_widest_img_size(wb) / get_nlanes(wb)
}

get_nlanes <- function(wb) {
  nrow(col_annot(wb))
}

get_widest_img_size <- function(wb) {
  info <- magick::image_info(imgs(wb))
  max(info$width)
}
