new_wb <- function(x = list(imgs = list(),
                            col_annot = data.frame(),
                            row_annot = data.frame(),
                            transform = data.frame())) {
  stopifnot(
    is.list(x),
    is(x$imgs, "magick-image"),
    is.data.frame(x$col_annot),
    is.data.frame(x$row_annot),
    is.data.frame(x$transform)
  )
  structure(x, class = "wb")
}

validate_wb <- function(x) {
  # Check nrow(row_annot) == nrow(transform) == length(imgs)
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
#' - `transform`: A `data.frame`, where each row contains image transformation
#'   parameters for an item in `imgs`. Usually left blank unless you know what
#'   you're doing
#' @export
wb <- function(imgs,
               col_annot = data.frame(),
               row_annot,
               transforms = NULL) {

  if (is.null(transforms)) transforms <- default_transforms(imgs)

  list(
    imgs = imgs,
    col_annot = col_annot,
    row_annot = row_annot,
    transforms = transforms
  ) |>
    new_wb() |>
    validate_wb()
}

#' @export
`[.wb` <- function(x, i = NULL, j = NULL) {

  ra <- x$row_annot
  ca <- x$col_annot
  tf <- x$transform

  if (!is.null(i)) ra <- ra[i, , drop = FALSE]
  if (!is.null(j)) ca <- ca[j, , drop = FALSE]
  if (!is.null(i)) tf <- tf[i, , drop = FALSE]

  imgs <- col_index_imgs(x, j)
  if (!is.null(i)) imgs <- imgs[i]

  wb(imgs = imgs,
     row_annot = ra,
     col_annot = ca)
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

wb_view <- function(wb) {
  magick::image_browse(imgs(wb))
}

wb_present <- function(wb) {
  imgs <- apply_transform(wb)
  img <- magick::image_append(imgs, stack = TRUE)
  info <- magick::image_info(img)
  ca_path <- tempfile()
  png(ca_path, width = info$width, res = 30)
  make_col_annot(wb)
  dev.off()
  ca <- magick::image_read(ca_path)
  ra <- make_row_annot(wb)
}
