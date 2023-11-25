ui <- fluidPage(
  titlePanel("Blot Editor"),
  sidebarLayout(
    sidebarPanel(
      # Might be better as numeric input, we'll see
      selectInput("blot", "Blot", c("choice 1", "choice 2")),
      #sliderInput("rotation", "Rotation"),
      #sliderInput("width", "Width"),
      #sliderInput("height", "Height"),
      #sliderInput("xpos", "X-position"),
      #sliderInput("ypos", "Y-position"),
      checkboxInput("flip", "Flip?"),
      actionButton("done", "Done")
    ),
    mainPanel(
      plotOutput("img")
    )
  )
)
