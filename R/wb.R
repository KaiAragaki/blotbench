new_wb <- function(x = list(imgs = list(),
                            col_annot = data.frame(),
                            row_annot = data.frame(),
                            transforms = data.frame())) {
  stopifnot(
    is.list(x),
    is(x$imgs, "magick-image"),
    is.data.frame(x$col_annot) || is.null(x$col_annot),
    is.data.frame(x$row_annot) || is.null(x$row_annot),
    is.data.frame(x$transforms)
  )
  structure(x, class = "wb")
}

validate_wb <- function(x) {
  ra <- row_annot(x)
  ca <- col_annot(x)
  tf <- transforms(x)

  if (!any(c(is.null(ra), is.null(ca)))) {
    if (!(nrow(ra)) == nrow(tf) && nrow(tf) == length(imgs(x))) {
      rlang::abort(
        "nrow(row_annot(x)) must equal nrow(transforms(x)) and length(imgs(x))"
      )
    }
  }
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
#' @param transforms A `data.frame`, where each row contains image transformation
#'   parameters for an item in `imgs`. Typically left blank unless you know what
#'   you're doing
#' @export
wb <- function(imgs,
               col_annot = NULL,
               row_annot = NULL,
               transforms = NULL) {

  if (is.null(transforms)) transforms <- default_transforms(imgs)
  if (is.vector(row_annot)) row_annot <- data.frame(name = row_annot)

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
  tf <- x$transforms

  if (!is.null(i) && !is.null(ra)) ra <- ra[i, , drop = FALSE]
  if (!is.null(j)) {
    if (is.null(ca)) {
      rlang::abort("Column indexing not possible if col_annot is NULL")
    }
    ca <- ca[j, , drop = FALSE]
  }
  if (!is.null(i)) tf <- tf[i, , drop = FALSE]

  x <- apply_transforms(x)
  imgs <- col_index_imgs(x, j)
  if (!is.null(i)) imgs <- imgs[i]

  # transforms is not suppled to force recalculation
  wb(imgs = imgs, row_annot = ra, col_annot = ca)
}

get_lane_width <- function(wb) {
  get_widest_img_size(wb) / get_nlanes(wb)
}

get_nlanes <- function(wb) {
  nrow(col_annot(wb))
}
