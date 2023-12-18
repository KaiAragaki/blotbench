# Will produce a list of dimensions
get_dimensions <- function(wb) {
  blot <- get_blot_dims(wb)
  top <- get_header_dims(wb)
  side <- get_side_dims(wb)
  list(blot = blot, top = top, side = side)
}

vec_to_grob_heights <- function(vec) {
  lapply(vec, \(x) grid::grobHeight(textGrob(x))) |>
    Reduce(unit.c, x = _)
}

get_layout <- function(wb) {
  dims <- get_dimensions(wb)
  grid::grid.layout(
    2, 2,
    widths = grid::unit.c(dims$side$w, get_blot_w(wb)),
    heights = grid::unit.c(dims$top$h, unit(1, "null"))
  )
}

present_wb <- function(wb) {
  img <- finalize_blot(wb)
  header <- make_header(wb)
  header_titles <- make_header_titles(wb)
  side <- make_side(wb)
  info <- magick::image_info(img)

  img <- grid::rasterGrob(
    img,
    x = grobWidth(side),
    y = unit(1, "native") - grobHeight(header),
    just = c(0, 1),
    width = unit(1, "native") - grobWidth(side),
    height = unit(1, "native") - grobHeight(header),
    default.units = "native"
  )
  header$vp <- viewport(
    x = grobWidth(side),
    y = 1,
    width = unit(1, "native") - grobWidth(side),
    height = grobHeight(header),
    just = c(0, 1),
    default.units = "native"
  )
  side$vp <- viewport(
    x = 0,
    y = unit(1, "native") - grobHeight(header),
    width = grobWidth(side),
    height = unit(1, "native") - grobHeight(header),
    just = c(0, 1),
    default.units = "native"
  )
  header_titles$vp <- viewport(
    x = 0,
    y = 1,
    width = grobWidth(side),
    height = grobHeight(header),
    just = c(0, 1),
    default.units = "native"
  )

  grid::gTree(
    ar = info$width / info$height,
    children = gList(img, header, side, header_titles),
    cl = "western_img", vp = viewport()
  )
}

#' @export
makeContext.western_img <- function(x) {
  vp_w <- convertWidth(x$vp$width, "in", valueOnly = TRUE)
  vp_h <- convertHeight(x$vp$height, "in", valueOnly = TRUE)
  header_h <- convertUnit(grobHeight(x$children[[2]]), "in", valueOnly = TRUE)
  side_w <- convertUnit(grobWidth(x$children[[3]]), "in", valueOnly = TRUE)
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
