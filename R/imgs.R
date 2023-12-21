#' Get and set images from a wb object
#' @param x A `wb` object
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
  rlang::abort(
    "To add an image, use `wb_add_img`. To remove an image, use row indexing."
  )
}

#' @param imgs A `magick-image` containing one or more images
#' @param names A character vector of names with length equal to `imgs`. Can be
#'   `NULL` if row_annot is `NULL`
#' @rdname imgs
wb_add_img <- function(x, imgs, names) {
  UseMethod("add_img")
}

#' @export
wb_add_img.wb <- function(x, imgs, names) {
  stopifnot(
    is(imgs, "magick-image"),
    is.null(names) || length(imgs) == length(names),
  )

  wb |>
    check_all_names_unique(names) |>
    check_all_imgs_same_width(imgs) |>
    add_names(names) |>
    add_imgs(imgs)
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
  if (is.null(names)) {
    if (!is.null(row_annot(wb)))
      rlang::abort("Argument `names` is NULL but `row_annot` not")
    return(wb)
  }
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
