edit_blot <- function(usr_wb) {
  usr_wb <- usr_wb
  ui <- shiny::fluidPage(
    shiny::titlePanel("Blot Editor"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        curIdxUI("curIdx", usr_wb),
        controlsUI("controls")
      ),
      shiny::mainPanel(
        shiny::imageOutput("imgs", height = 500),
      )
    )
  )
  server <- function(input, output, session) {
    curIdx <- curIdxServer("curIdx", usr_wb)
    controls <- controlsServer("controls")
    allTrans <- allTransServer("allTrans", usr_wb, curIdx, controls)
    curTrans <- curTransServer("controls", curIdx, allTrans)
  }
  shiny::shinyApp(ui, server)
}

compose_edit <- function(input) {
  rlang::expr(
    wb_dicer(
      img,
      width    = !!input$width,
      height   = !!input$height,
      xpos     = !!input$xpos,
      ypos     = !!input$ypos,
      rotation = !!input$rotation,
      flip     = !!input$flip
    )
  )
}

#' @export
wb_dicer <- function(img, width, height, xpos, ypos, rotation, flip) {
  crop_geom <- magick::geometry_area(
    width = width, height = height, x_off = xpos, y_off = ypos
  )
  out <- img |>
    magick::image_rotate(rotation) |>
    magick::image_crop(crop_geom)
  if (flip) out <- magick::image_flop(out)
  out
}
