controlsUI <- function(id) {
  shiny::tagList(
    shiny::numericInput(
      shiny::NS(id, "width"), "width", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "height"), "height", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "xpos"), "xpos", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "ypos"), "ypos", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "rotate"), "rotate", min = 0, value = 0
    ),
    shiny::checkboxInput(
      shiny::NS(id, "flip"), "flip", value = FALSE
    )
  )
}

curIdxUI <- function(id, usr_wb) {
  shiny::selectInput(
    shiny::NS(id, "selection"),
    "Select Blot",
    choices = row_annot(usr_wb)$name
  )
}



#' @importFrom shiny NS
blotUI <- function(id) {
  shiny::imageOutput(NS(id, "img"), height = 500)
}

make_ui <- function(df, id) {
  list(
    shiny::numericInput(
      id, "width", min = 0, value = df$width
    ),
    shiny::numericInput(
      id, "height", min = 0, value = df$height
    ),
    shiny::numericInput(
      id, "xpos", min = 0, value = df$xpos
    ),
    shiny::numericInput(
      id, "ypos", min = 0, value = df$ypos
    ),
    shiny::numericInput(
      id, "rotate", min = 0, value = df$rotate
    ),
    shiny::checkboxInput(
      id, "flip", value = df$flip
    )
  )
}
