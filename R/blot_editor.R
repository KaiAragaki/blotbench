#' Interactively edit blots
#'
#' This function helps generate code needed to transform a blot to your
#' specifications via a Shiny app
#'
#' @param wb A `wb` object
edit_blot <- function(wb) {
  usr_wb <- wb
  ui <- shiny::fluidPage(
    shiny::titlePanel("Blot Editor"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        curIdxUI("curIdx", usr_wb),
        controlsUI("controls"),
        doneUI("done")
      ),
      shiny::mainPanel(
        blotUI("controls")
      )
    )
  )
  server <- function(input, output, session) {
    curIdx <- curIdxServer("curIdx", usr_wb)
    controls <- controlsServer("controls")
    allTrans <- allTransServer("allTrans", usr_wb, curIdx, controls)
    curTrans <- curTransServer("controls", usr_wb, curIdx, allTrans)
    done <- doneServer("done", allTrans)
  }

  transforms_to_apply <- shiny::runApp(shiny::shinyApp(ui, server))
  cat(
    "Paste in your script to crop the images as seen in the app:\n",
    "transforms(", deparse(substitute(wb)), ") <- ",
    datapasta::tribble_construct(transforms_to_apply),
    sep = ""
  )
}
