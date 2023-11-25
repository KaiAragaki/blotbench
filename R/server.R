library(png)

function(input, output, session) {

  # https://shiny.posit.co/r/gallery/advanced-shiny/image-output/

  output$img <- renderImage({
    width  <- session$clientData$output_img_width
    height <- session$clientData$output_img_height

    # A temp file to save the output.
    # This file will be automatically removed later by
    # renderImage, because of the deleteFile=TRUE argument.
    outfile <- tempfile(fileext = ".png")

    img <- imgs(wb)[1]

    writePNG(img, target = outfile)

    # Return a list containing information about the image
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
}
