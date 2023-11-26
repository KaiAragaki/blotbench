edit_blot <- function(mwb) {
  server <- function(input, output, session) {
    # https://shiny.posit.co/r/gallery/advanced-shiny/image-output/
    output$img <- renderImage({
      # A temp file to save the output.
      # This file will be automatically removed later by
      # renderImage, because of the deleteFile=TRUE argument.
      outfile <- tempfile(fileext = ".png")

      crop_geom <- magick::geometry_area(width = input$width,
                                         height = input$height,
                                         x_off = input$xpos,
                                         y_off = input$ypos)

      img <- imgs(mwb)[1]
      img <- magick::image_rotate(img, degrees = input$rotation) |>
        magick::image_crop(geometry = crop_geom)

      if (input$flip) img <- magick::image_flop(img)

      magick::image_write(img, format = "png", path = outfile)
      # Return a list containing information about the image
      list(src = outfile,
           contentType = "image/png",
           alt = "This is alternate text")
    }, deleteFile = TRUE)
  }
  ui <- fluidPage(
    titlePanel("Blot Editor"),
    sidebarLayout(
      sidebarPanel(
        # Might be better as numeric input, we'll see
        selectInput("blot", "Blot", c("choice 1", "choice 2")),
        numericInput("rotation", "Rotation", min = -180, max = 180, value = 0),
        numericInput("width", "Width", min = 0, value = 0),
        numericInput("height", "Height", min = 0, value = 0),
        numericInput("xpos", "X-position", min = 0, value = 0),
        numericInput("ypos", "Y-position", min = 0, value = 0),
        checkboxInput("flip", "Flip?"),
        actionButton("done", "Done")
      ),
      mainPanel(
        imageOutput("img", height = 500)
      )
    )
  )
  shinyApp(ui, server)
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
