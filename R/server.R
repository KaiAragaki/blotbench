# The index is the most useful, rather than the name
curIdxServer <- function(id, usr_wb) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(which(row_annot(usr_wb)$name %in% input$selection))
  })
}

controlsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(
      data.frame(
        width = input$width,
        height = input$height,
        xpos = input$xpos,
        ypos = input$ypos,
        rotate = input$rotate,
        flip = input$flip
      )
    )
  })
}

allTransServer <- function(id, usr_wb, curIdx, controls) {
  shiny::moduleServer(id, function(input, output, session) {
    tfs <- reactiveValues(tf = transforms(usr_wb))
    observeEvent({
      controls()
    },{
      tfs$tf[curIdx(), ] <- controls()
      tfs
    })
    tfs
  })
}

curTransServer <- function(id, usr_wb, curIdx, allTrans) {
  shiny::moduleServer(id, function(input, output, session) {
    curTrans <- shiny::reactive({
      at <- allTrans$tf
      at[curIdx(), ]
    })

    shiny::observeEvent(curTrans(), {
      shiny::updateNumericInput(session, "width", value = curTrans()[[1]])
      shiny::updateNumericInput(session, "height", value = curTrans()[[2]])
      shiny::updateNumericInput(session, "xpos", value = curTrans()[[3]])
      shiny::updateNumericInput(session, "ypos", value = curTrans()[[4]])
      shiny::updateNumericInput(session, "rotate", value = curTrans()[[5]])
      shiny::updateCheckboxInput(session, "flip", value = curTrans()[[6]])
    })

    output$img <- shiny::renderImage({
      outfile <- tempfile(fileext = ".png")
      crop_geom <- magick::geometry_area(
        width = input$width, height = input$height,
        x_off = input$xpos, y_off = input$ypos
      )

      img <- imgs(usr_wb)[curIdx()]
      img <- magick::image_rotate(
        img, degrees = input$rotate
      ) |>
        magick::image_crop(geometry = crop_geom)

      if (input$flip) img <- magick::image_flop(img)

      magick::image_write(img, format = "png", path = outfile)
      # Return a list containing information about the image
      list(src = outfile,
           contentType = "image/png",
           alt = "This is alternate text")
    }, deleteFile = TRUE)
  })
}

curBlotImgServer <- function(id, wbData, curBlotIdx) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(imgs(wbData())[curBlotIdx()])
  })
}

doneServer <- function(id, allTrans) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$done, {
      shiny::stopApp(allTrans$tf)
    })
  })
}
