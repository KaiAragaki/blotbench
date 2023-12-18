#' @export
imgs <- function(x) {
  UseMethod("imgs")
}

#' @export
imgs.wb <- function(x) {
  x$imgs
}

#' @export
`imgs<-` <- function(x, value) {
  UseMethod("imgs<-")
}

#' @export
`imgs<-.wb` <- function(x, value) {
  x$imgs <- value
  x
}

#' Add an image to a wb object
#'
#' @param imgs A `magick-image` containing one or more images
#' @param names A character vector of names with lenght equal to imgs
wb_add_img <- function(x, imgs, names) {
  UseMethod("add_img")
}

#' @export
wb_add_img.wb <- function(x, imgs, names) {
  stopifnot(
    is(imgs, "magick-image"),
    length(imgs) == length(names),
  )

  wb |>
    check_all_names_unique(names) |>
    check_all_imgs_same_width(imgs) |>
    add_names(names) |>
    add_imgs(imgs)
}

#' Drop an image and its band data by name or index
#'
#' @param x A `wb` object
#' @param name Either a numeric representing the index of the image, or a name
#'   identifying the row_annot row associated with the image.
#'
#' @export
wb_rm_img <- function(x, name) {
  UseMethod("wb_rm_img")
}

#' @export
wb_rm_img.wb <- function(x, name) {
  stopifnot(is.numeric(name) | is.character(name))

  if (is.numeric(name)) {
    if (name > length(imgs(x)))
      cli::cli_abort("Subscript out of bounds")
    imgs(x) <- imgs(x)[-name]
    row_annot(x) <- row_annot(x)[-name]
  }

  if (is.character(name)) {
    if (!name %in% row_annot(x)$name)
      cli::cli_abort("{.var name} is not a name in the row_annot of this wb")
    idx <- which(row_annot(x)$name == name)
    imgs(x) <- imgs(x)[-idx]
    row_annot(x) <- row_annot(x)[-idx, ]
  }

  wb
}

check_all_names_unique <- function(wb, names) {
  stopifnot(!any(duplicated(names)))

  if (is.null(wb$row_annot)) return(wb)

  if (any(duplicated(c(wb$row_annot[, 1], names))))
    cli::cli_abort("Duplicate name found between {.code wb} and {.code names}")

  wb
}

check_all_imgs_same_width <- function(wb, imgs) {
  all_info <- c(wb$imgs, imgs) |>
    magick::image_info()
  if (unique(all_info$width) > 1) {
    cli::cli_warn(
      c("Some of images have different widths.",
        "This may results in strange (and incorrect) annotation.")
    )
  }
  wb
}

add_names <- function(wb, names) {
  row_annot(wb) <- rbind(row_annot(wb), data.frame(name = names))
  wb
}

add_imgs <- function(wb, imgs) {
  wb$imgs <- c(wb$imgs, imgs)
  wb
}

col_index_imgs <- function(wb, j) {
  if (is.null(j)) return(imgs(wb))

  lanes <- get_nlanes(wb)

  if (any(j < 0)) j <- setdiff(1:lanes, abs(j))

  new_imgs <- imgs(wb)
  out <- lapply(new_imgs, \(x, j, lanes) col_index_img(x, j, lanes), j = j, lanes = lanes)
  Reduce(c, out)
}

col_index_img <- function(img, j, lanes) {
  w <- magick::image_info(img)$width
  lw <- w / lanes
  lapply(
    (j - 1) * lw,
    \(x, img) magick::image_crop(img, paste0(lw, "x+", x)),
    img = img
  ) |>
    Reduce(c, x = _) |>
    magick::image_append()
}
