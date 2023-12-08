# grobWidth and grobHeight is able to capture graphical parameters like font
# size and face. Might be useful in conjunction with convertWidth.
# Finding largest grobWidth text might help set font face for all others
#
# frameGrob with a layout and placeGrob might be a way to go about this

# At the simplest level:
# A single cell in the col_annot

cell <- function(text,
                 x = 0.5, y = 0.5,
                 units = "npc",
                 just = "center",
                 angle = 0,
                 name = "cell") {
  if (!grid::is.unit(x)) x <- grid::unit(x, units)
  if (!grid::is.unit(x)) y <- grid::unit(y, units)
  child <- cell_children(text, name)
  child_vp <- cell_children_vp(text, x, y, name = paste0(name, "_vp"))
  gTree(
    label = text, x = x, y = y, just = just, angle = angle,
    children = child, childrenvp = child_vp, cl = "cell", name = name
  )
}

cell_children <- function(text, name) {
  t <- textGrob(
    label = text, name = paste0(name, "_text"), vp = paste0(name, "_vp")
  )
  r <- rectGrob(
    name = paste0(name, "_rect"),
    gp = gpar(col = "red"),
    vp = paste0(name, "_vp")
  )
  gList(t, r)
}

cell_children_vp <- function(text, x, y, name = "cell_vp") {
  viewport(
    x, y, width = stringWidth(text), height = unit(1, "lines"), name = name
  )
}

row <- function(cell_gt, name = "cell_row") {

}

make_col_annot_layout <- function() {
  # One col for the actual annotations, the other for the names of of each
  # column
  grid.layout(
    nrow = rows,
    ncol = 2,
    widths = unit(c(1, 1), c("null", "grobwidth"), list(NULL, label))
  )
}

# The ultimate width of this thing needs to be the width of the blot + the width
# of the label
# Using rasterGrob could be one way of getting some relative width of the image


present_wb <- function(wb) {
  # Annotation "block" dimensions cannot be calculated in isolation
  # They all need to be calculated with each other. With those dimensions,
  # we can then create annotation blocks individually.
  dims <- get_dimensions(wb)


  img <- apply_transform(wb)
  side_annot <- make_side_annot(wb)
  top_annot <- make_top_annot(wb)
  layout <- grid.layout(
    nrow = 2,
    ncol = 2,
    widths = unit(
      c(1, 1), c("grobwidth", "grobwidth"), list(side_annots, image)
    ),
    heights = unit(
      c(1, 1), c("grobheight", "grobheight"), list(top_annots, image)
    )
  )
  frame <- frameGrob(layout = layout)
  frame <- placeGrob(frame, )
}

get_dimensions <- function(wb) {

  blot <- wb |>
    prepare_wb_img() |>
    rasterGrob()
  blot_w <- grobWidth(blot)
  blot_h <- grobHeight(blot)

  side_text <- get_all_side_text(wb)
  side_annot_w <- calc_max_width(side_text)

  top_annot_w <- blot_w + side_annot_w
  top_annot_h <- get_top_annot_height(wb)

  side_annot_h <- blot_h + top_annot_h

  list(
    blot_w = blot_w,
    blot_h = blot_h,
    side_annot_w = side_annot_w,
    side_annot_h = side_annot_h,
    top_annot_w = top_annot_w,
    top_annot_h = top_annot_h
  )
}

make_layout <- function(dims) {
  grid.layout(
    2, 2,
    widths = unit.c(dims$side_annot_w, dims$blot_w),
    heights = unit.c(dims$top_annot_h, dims$blot_h)
  )
}

get_all_side_text <- function(wb) {
  ca_txt <- colnames(col_annot(wb))
  ra_txt <- row_annot(wb)$name
  c(ca_txt, ra_txt)
}

get_top_annot_height <- function(wb) {
  top_annot_layout(wb)$heights |> sum()
}

calc_max_width <- function(txt) {
  textGrob("conc_nm") |> grobWidth()
}

produce_wb_img <- function(wb) {
  layout <- make_layout(get_dimensions(wb))
  fg <- frameGrob(
    layout = layout,
    vp = viewport(width = unit(0.8, "npc"), height = unit(0.8, "npc"))
  )
  fg <- placeGrob(fg, prepare_wb_img(wb) |> rasterGrob(), row = 2, col = 2)
  # temp:
  fg <- placeGrob(
    fg,
    top_annot(wb),
    row = 1, col = 2
  )
  # temp - content is right but formatting is not
  fg <- placeGrob(
    fg,
    textGrob(
      col_annot(wb) |> colnames() |> paste(collapse = "\n"),
      hjust = 1,
      x = unit(1, "npc")
    ),
    row = 1, col = 1
  )
  # temp - content is right but formatting is not
  fg <- placeGrob(
    fg,
    textGrob(
      row_annot(wb)$name |> paste(collapse = "\n"),
      hjust = 1,
      x = unit(1, "npc")
    ),
    row = 2, col = 1
  )
  fg
}


png("test.png")
grid.draw(produce_wb_img(wb))
dev.off()

prepare_wb_img <- function(wb) {
  wb |>
    apply_transform() |>
    magick::image_append(stack = TRUE)
}

top_annot_layout <- function(wb) {
  ca <- col_annot(wb)
  heights <- vec_to_grob_heights(colnames(ca)) + unit(0.2, "lines")
  grid.layout(nrow = ncol(col_annot(wb)), ncol = 1, heights = heights)
}

vec_to_grob_heights <- function(vec) {
  lapply(vec, \(x) grobHeight(textGrob(x))) |>
    Reduce(unit.c, x = _)
}

top_annot <- function(wb) {
  fg <- frameGrob(layout = top_annot_layout(wb))
  for (i in seq_len(ncol(col_annot(wb)))) {
    tar <- top_annot_row(col_annot(wb)[[i]])
    fg <- placeGrob(
      fg,
      rectGrob(),
      row = i,
      col = 1
    )
    fg <- placeGrob(
      fg,
      tar[[1]],
      row = i,
      col = 1
    )
    fg <- placeGrob(
      fg,
      tar[[2]],
      row = i,
      col = 1
    )
  }
  fg
}

top_annot_row <- function(ca_col) {
  rle <- rle(ca_col)
  sizes <- rle$lengths / sum(rle$lengths)
  boundaries <- cumsum(sizes)
  mids <- boundaries - sizes / 2
  gList(
    segmentsGrob(x0 = boundaries, x1 = boundaries),
    textGrob(rle$values, x = mids)
  )
}


test_transforms <- function(wb) {
  transforms(wb) <- tibble::tribble(
    ~width, ~height, ~xpos, ~ypos, ~rotate, ~flip,
    270L,     50L,   260,    55,     1.5,  TRUE,
    220L,    445L,     0,     0,       0, FALSE
  )
  wb
}
