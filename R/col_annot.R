#' Get or set column annotation for a wb object
#' @param x A `wb` object
#' @export
col_annot <- function(x) {
  UseMethod("col_annot")
}

#' @export
col_annot.wb <- function(x) {
  x$col_annot
}

#' @rdname col_annot
#' @param value A data.frame with each column representing a variable, and each
#'   row representing a lane in the blot. NOTE: the order of the rows should
#'   annotate the lanes left-to-right AFTER image manipulation, which might
#'   include mirroring.
#' @export
`col_annot<-` <- function(x, value) {
  UseMethod("col_annot<-")
}

#' @export
`col_annot<-.wb` <- function(x, value) {
  x$col_annot <- value
  x
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
    rect <- grid.rect(
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

