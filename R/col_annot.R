#' @export
col_annot <- function(x) {
  UseMethod("col_annot")
}

#' @export
col_annot.wb <- function(x) {
  x$col_annot
}

#' @export
`col_annot<-` <- function(x, value) {
  UseMethod("col_annot<-")
}

#' @export
`col_annot<-.wb` <- function(x, value) {
  x$col_annot <- value
  x
}

# Generate header for colname annotation
# Should probably use montage generation for putting it all together

#' @param img A `wb`
#' @param n_lanes Numeric. The number of lanes in the image
#' @param text_size Size of text. Automatically downscaled if too big to fit.
#'
#' @return A `magick-image`
wb_annot_lanes <- function(wb, text_size = 20) {
  col_annot <- col_annot(wb)
  n_lanes <- nrow(col_annot)
  for (i in seq_len(ncol(col_annot))) {
    img <- make_blocks(
      img = img,
      labels = col_annot[[i]],
      n_lanes = n_lanes,
      text_size
    )
  }

  add_block_titles(img, annot, text_size)
}

get_new_text_size <- function(img, labels, block_sizes, text_size) {
  img <- magick::image_draw(img, res = 30)

  # THIS IS SLOW! A binary search for optimal size would probably be better.
  while (any((block_sizes - strwidth(labels, cex = text_size)) < 0) & text_size > 1) {
    text_size <- text_size - 1
  }
  dev.off()
  text_size
}

get_text_height <- function(img, labels, text_size) {
  img <- magick::image_draw(img, res = 30)
  height <- max(strheight(labels, cex = text_size, units = "figure")) * magick::image_info(img)$height
  dev.off()
  height
}

make_col_annot <- function(wb) {
  annot <- col_annot(wb)
  row_height <- 1 / ncol(annot)

  grid.newpage()
  # each row is a condiion in col_annot
  # two cols - one is to hold the lane labels, the other labels the rows
  pushViewport(viewport(layout = grid.layout(ncol(annot), 2)))

  for (i in seq_len(ncol(annot))) {
    labels <- annot[[i]]
    rle <- rle(labels)
    block_sizes <- rle$lengths
    block_labels <- rle$values
    block_sizes <- block_sizes / sum(block_sizes)
    block_borders <- cumsum(block_sizes)
    block_borders <- c(0, block_borders)[seq_along(block_borders)]

    pushViewport(viewport(layout.pos.row = i, layout.pos.col = 1))
    grid.rect(
      block_borders,
      width = block_sizes,
      just = "left",
      gp = gpar(col = "red")
    )

    grid.text(
      block_labels,
      x = block_borders + (block_sizes / 2),
      y = unit(0.5, "npc")
    )
    upViewport()

    pushViewport(viewport(layout.pos.row = i, layout.pos.col = 2))
    grid.text(
      annot[i] |> names(),
      just = "left",
      x = 0.1
    )
    upViewport()
  }
}

get_block_sizes <- function(wb, labels) {
  rle(labels)$lengths * get_lane_width(wb)
}

make_block_title_space <- function(img, annotation, text_size) {
  titles <- colnames(annotation)
  img <- magick::image_draw(img, res = 30)
  width <- max(strwidth(titles, cex = text_size))
  dev.off()
  add_side(img, width, side = "right")
}

add_block_titles <- function(img, annotation, text_size) {
  pre_width <- magick::image_info(img)$width
  img <- make_block_title_space(img, annotation, text_size)
  height <- get_text_height(img, unlist(annotation), text_size)
  img <- magick::image_draw(img, res = 30)
  annotation <- rev(annotation)
  for (i in 1:ncol(annotation)) {
    text(
      x = pre_width,
      y = height * (i - 1),
      labels = colnames(annotation)[i],
      cex = text_size,
      adj = c(0, 0.9)
    )
  }
  dev.off()

  img
}
