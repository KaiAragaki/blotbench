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
    # Update transforms from controls...
    # At the current index...
    # (if the order is important one may need to appear before the other)
    # Update vs retrieval?
    # Start with default value
    tfs <- reactiveValues(tf = transforms(usr_wb))

    # Possible thought if necessary:
    # What if updating only occurs on curIdx change, and
    # all of the cropping came from curTrans (or controls?)
    observeEvent({
      controls()
    },{
      tfs$tf[curIdx(), ] <- controls()
      tfs
      print(tfs$tf)
    })
    # Need to update controls first, the update allTrans
    tfs
  })
}

curTransServer <- function(id, curIdx, allTrans) {
  shiny::moduleServer(id, function(input, output, session) {
    curTrans <- shiny::reactive({
      at <- allTrans$tf
      print(curIdx())
      at[curIdx(), ]
    })

    shiny::observeEvent(curTrans(), {
      print(curTrans())
      shiny::updateNumericInput(session, "width", value = curTrans()[[1]])
      shiny::updateNumericInput(session, "height", value = curTrans()[[2]])
      shiny::updateNumericInput(session, "xpos", value = curTrans()[[3]])
      shiny::updateNumericInput(session, "ypos", value = curTrans()[[4]])
      shiny::updateNumericInput(session, "rotate", value = curTrans()[[5]])
      shiny::updateCheckboxInput(session, "flip", value = curTrans()[[6]])
    })

  })
}

curBlotImgServer <- function(id, wbData, curBlotIdx) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(imgs(wbData())[curBlotIdx()])
  })
}
