controlsUI <- function(id) {
  shiny::tagList(
    shiny::numericInput(
      shiny::NS(id, "width"), "Width", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "height"), "Height", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "xpos"), "X-offset (from left)", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "ypos"), "Y-offset (from top)", min = 0, value = 0
    ),
    shiny::numericInput(
      shiny::NS(id, "rotate"), "Rotation", min = 0, value = 0
    ),
    shiny::checkboxInput(
      shiny::NS(id, "flip"), "Flip? (Horizontal)", value = FALSE
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

blotUI <- function(id) {
  shiny::imageOutput(shiny::NS(id, "img"), height = 500)
}

doneUI <- function(id) {
  shiny::actionButton(
    shiny::NS(id, "done"), "Done"
  )
}
