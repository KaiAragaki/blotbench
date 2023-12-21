#' Render a wb object into an image
#'
#' This function takes a wb object and (optionally) row and column annotations
#' and produces an image from them.
#'
#' @param wb A `wb` object with at least images and transformations
#' @export
present_wb <- function(wb) {
  wb <- apply_transform(wb)
  img <- finalize_blot(wb)

  if (is.null(col_annot(wb)) || is.null(row_annot(wb))) {
    return(magick::image_browse(img))
  }

  header <- make_header(wb)
  header_titles <- make_header_titles(wb)
  side <- make_side(wb)
  info <- magick::image_info(img)

  img <- grid::rasterGrob(
    img,
    x = grid::grobWidth(side),
    y = grid::unit(1, "native") - grid::grobHeight(header),
    just = c(0, 1),
    width = grid::unit(1, "native") - grid::grobWidth(side),
    height = grid::unit(1, "native") - grid::grobHeight(header),
    default.units = "native"
  )
  header$vp <- grid::viewport(
    x = grid::grobWidth(side),
    y = 1,
    width = grid::unit(1, "native") - grid::grobWidth(side),
    height = grid::grobHeight(header),
    just = c(0, 1),
    default.units = "native"
  )
  side$vp <- grid::viewport(
    x = 0,
    y = grid::unit(1, "native") - grid::grobHeight(header),
    width = grid::grobWidth(side),
    height = grid::unit(1, "native") - grid::grobHeight(header),
    just = c(0, 1),
    default.units = "native"
  )
  header_titles$vp <- grid::viewport(
    x = 0,
    y = 1,
    width = grid::grobWidth(side),
    height = grid::grobHeight(header),
    just = c(0, 1),
    default.units = "native"
  )

  grid::gTree(
    ar = info$width / info$height,
    children = grid::gList(img, header, side, header_titles),
    cl = "western_img", vp = grid::viewport()
  ) |>
    grid::grid.draw()
}


#' @importFrom grid makeContext
#' @export
makeContext.western_img <- function(x) {
  vp_w <- grid::convertWidth(
    x$vp$width, "in", valueOnly = TRUE
  )
  vp_h <- grid::convertHeight(
    x$vp$height, "in", valueOnly = TRUE
  )
  header_h <- grid::convertUnit(
    grid::grobHeight(x$children[[2]]), "in", valueOnly = TRUE
  )
  side_w <- grid::convertUnit(
    grid::grobWidth(x$children[[3]]), "in", valueOnly = TRUE
  )
  img_ratio <- x$ar
  # w/h = ratio => w = ratio * h
  expected_w <- (img_ratio * vp_h) + side_w
  expected_h <- (vp_w / img_ratio) + header_h
  if (vp_w > expected_w) {
    x$vp$width <- grid::unit(expected_w, "in")
    x$vp$height <- grid::unit(vp_h, "in")
  }

  if (vp_h > expected_h) {
    x$vp$width <- grid::unit(vp_w, "in")
    x$vp$height <- grid::unit(expected_h, "in")
  }
  x
}
